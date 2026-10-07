#!/usr/bin/env python3
"""Convert an Org document to a standalone HTML page, plus optional plain text.

Reads the .org source directly and emits a self-contained file: no external
CSS, no table of contents, no <pre> blocks and no ASCII table borders, so the
result stays readable in a browser and pastes as clean text into a rich-text
editor (LinkedIn, Google Docs, Word).

Usage:
    org2html.py FILE.org                  -> FILE.html
    org2html.py FILE.org -o out.html      -> out.html
    org2html.py FILE.org -t               -> FILE.txt alongside FILE.html
    org2html.py FILE.org -t -o out.html   -> out.html and out.txt
    org2html.py FILE.org -d ~/some/dir    -> ~/some/dir/FILE.html
    org2html.py FILE.org -l Tools         -> "Tools: ..." becomes metadata

Conventions:
    * A "place - type - dates" line right under a heading renders as muted
      metadata (detected by a surrounded separator character).
    * A bullet labelled "Stack: a, b, c" is metadata, not a list item: it is
      rendered as a muted line below the list, so the technology list reads
      apart from the achievements. Add more labels with -l/--label.

Scope:
    This is not an Org interpreter. It handles the subset that regular
    documents use: keyword lines, :PROPERTIES:/:END: drawers, headings,
    paragraph text, -/+ bullet lists, |pipe| tables and =verbatim= spans.
    Footnotes, src blocks, nested lists, links, tags and scheduled timestamps
    are not interpreted and may render oddly. For full fidelity use Emacs:
        emacs --batch -f org-html-export-to-file FILE.org
    Requires Python 3.6+, no third-party packages.
"""
import argparse
import html
import os
import re
import sys

CSS = """
:root { --ink: #1a1a1a; --muted: #6b6b6b; --rule: #d8d8d8; }
* { box-sizing: border-box; }
body {
  margin: 0; padding: 3rem 1.25rem 5rem;
  background: #fff; color: var(--ink);
  font: 16px/1.6 -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
        "Helvetica Neue", Arial, sans-serif;
}
main { max-width: 46rem; margin: 0 auto; }
h1 {
  font-size: 1.9rem; line-height: 1.25; margin: 0 0 .35rem;
  letter-spacing: -.01em;
}
h1 + .sub { color: var(--muted); margin: 0 0 2.75rem; font-size: .95rem; }
h2 {
  font-size: 1.05rem; text-transform: uppercase; letter-spacing: .09em;
  margin: 2.75rem 0 .9rem; padding-bottom: .35rem;
  border-bottom: 1px solid var(--rule);
}
h3 { font-size: 1.12rem; margin: 2rem 0 .3rem; }
h4 { font-size: 1rem; margin: 1.4rem 0 .25rem; font-weight: 600; }
p { margin: .5rem 0; }
.meta { color: var(--muted); font-size: .9rem; margin: .15rem 0 .5rem; }
.stack {
  font-size: .9rem; color: var(--muted); margin-top: .6rem;
  padding-top: .5rem; border-top: 1px dotted var(--rule);
}
ul { margin: .5rem 0 1rem; padding-left: 1.35rem; }
li { margin: .3rem 0; }
code {
  font: .88em ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
  background: #f4f4f4; padding: .1em .35em; border-radius: 3px;
}
table { border-collapse: collapse; width: 100%; margin: .75rem 0; font-size: .93rem; }
th, td { text-align: left; padding: .45rem .6rem; border-bottom: 1px solid var(--rule); }
th { font-size: .8rem; text-transform: uppercase; letter-spacing: .06em; color: var(--muted); }
td:last-child, th:last-child { white-space: nowrap; }
"""


# Labels whose bullets are metadata lines rather than list items. A bullet
# starting with one of these ("Stack: a, b, c") is rendered as a muted line
# below the list it belongs to. Extend with --label.
DEFAULT_LABELS = ("stack",)


def label_pattern(labels):
    """Match '^<label>: ' case-insensitively, longest label first."""
    alts = "|".join(re.escape(l) for l in sorted(labels, key=len, reverse=True))
    return re.compile(r"^(?:%s):\s+" % alts, re.I)


def inline(text):
    """Escape HTML, then turn every second chunk of =verbatim= into <code>."""
    out = []
    for i, chunk in enumerate(text.split("=")):
        if i % 2:
            out.append("<code>%s</code>" % html.escape(chunk))
        else:
            out.append(html.escape(chunk))
    return "".join(out)


def is_meta(line):
    """True for a 'place · type · dates' line, i.e. metadata under a heading.

    Detected by a middot-separated line that is not a list, heading or table.
    """
    return " · " in line and not line.startswith(("-", "*", "|", " "))


def is_block_start(line):
    """True for lines that must not be absorbed as paragraph continuation."""
    return (
        not line.strip()
        or re.match(r"^(\*+)\s", line)
        or re.match(r"^\s*[-+]\s", line)
        or line.lstrip().startswith("|")
        or line.startswith(":")
    )


def parse_table(rows):
    """Split pipe rows, dropping the |---+---| separator row."""
    cells = []
    for r in rows:
        parts = [c.strip() for c in r.strip().strip("|").split("|")]
        filled = [c for c in parts if c]
        if filled and all(re.fullmatch(r"[+:\-]+", c) for c in filled):
            continue  # horizontal rule, e.g. "---+---" or ":---:"
        cells.append(parts)
    return cells


def strip_keyword_lines(text):
    """Drop #+keyword lines and :drawer: lines; return (title, subtitle, body)."""
    title = subtitle = None
    body = []
    for ln in text.split("\n"):
        if ln.startswith("#+"):
            m = re.match(r"#\+title:\s*(.+)", ln)
            if m:
                title = m.group(1).strip()
            m = re.match(r"#\+subtitle:\s*(.+)", ln)
            if m:
                subtitle = m.group(1).strip()
            continue
        if ln.startswith(":"):
            continue  # :PROPERTIES:, :END:, any drawer keyword
        body.append(ln)
    return title, subtitle, body


def convert(body, label_re):
    """Turn the stripped Org body lines into an HTML fragment."""
    out = []
    i, n = 0, len(body)

    while i < n:
        line = body[i].rstrip()

        if not line.strip():
            i += 1
            continue

        # |pipe| table
        if line.lstrip().startswith("|"):
            rows = []
            while i < n and body[i].lstrip().startswith("|"):
                rows.append(body[i].strip())
                i += 1
            cells = parse_table(rows)
            if cells:
                out.append("<table>")
                out.append(
                    "<tr>%s</tr>" % "".join("<th>%s</th>" % inline(c) for c in cells[0])
                )
                for row in cells[1:]:
                    out.append(
                        "<tr>%s</tr>" % "".join("<td>%s</td>" % inline(c) for c in row)
                    )
                out.append("</table>")
            continue

        # heading
        m = re.match(r"^(\*+)\s+(.*)$", line)
        if m:
            tag = {1: "h2", 2: "h3", 3: "h4"}.get(len(m.group(1)), "h4")
            out.append("<%s>%s</%s>" % (tag, inline(m.group(2).strip()), tag))
            i += 1
            # a 'place · type · dates' line directly under the heading
            if i < n and is_meta(body[i].strip()):
                out.append('<p class="meta">%s</p>' % inline(body[i].strip()))
                i += 1
            continue

        # -/+ bullet list
        if re.match(r"^\s*[-+]\s+", line):
            items = []
            labels = []
            while i < n and re.match(r"^\s*[-+]\s+", body[i].rstrip()):
                item = re.sub(r"^\s*[-+]\s+", "", body[i].rstrip())
                i += 1
                # absorb wrapped continuation lines of this bullet
                while i < n and not is_block_start(body[i].rstrip()):
                    item += " " + body[i].strip()
                    i += 1
                # a "Label: ..." bullet is metadata, not a list item
                if label_re.match(item):
                    labels.append(item)
                    continue
                items.append("<li>%s</li>" % inline(item))
            if items:
                out.append("<ul>%s</ul>" % "".join(items))
            for lab in labels:
                out.append('<p class="stack">%s</p>' % inline(lab))
            continue

        # paragraph: join wrapped lines
        para = [line.strip()]
        i += 1
        while i < n and not is_block_start(body[i].rstrip()):
            para.append(body[i].strip())
            i += 1
        joined = " ".join(para)
        cls = ' class="meta"' if is_meta(joined) else ""
        out.append("<p%s>%s</p>" % (cls, inline(joined)))

    return "\n".join(out)


def to_text(doc_html):
    """Flatten the generated HTML into copy-pasteable plain text."""
    s = re.sub(r"<head.*?</head>", "", doc_html, flags=re.S)
    s = re.sub(r"<style.*?</style>", "", s, flags=re.S)
    s = re.sub(r"<h1[^>]*>(.*?)</h1>", r"\n\n[1] \1", s, flags=re.S)
    s = re.sub(r"<h2[^>]*>(.*?)</h2>", r"\n\n[1] \1", s, flags=re.S)
    s = re.sub(r"<h3[^>]*>(.*?)</h3>", r"\n\n[2] \1", s, flags=re.S)
    s = re.sub(r"<h4[^>]*>(.*?)</h4>", r"\n\n[3] \1", s, flags=re.S)
    s = re.sub(r"<li[^>]*>", "  - ", s)
    s = re.sub(r"</li>", "\n", s)
    s = re.sub(r"<tr[^>]*>", "\n  | ", s)
    s = re.sub(r"</t[hd]>", " | ", s)
    s = re.sub(r"<p[^>]*>", "\n", s)
    s = re.sub(r"<br\s*/?>", "\n", s)
    s = re.sub(r"<[^>]+>", "", s)
    s = html.unescape(s)
    s = re.sub(r"\n{3,}", "\n\n", s)
    return s.strip() + "\n"


def parse_args(argv):
    ap = argparse.ArgumentParser(
        description="Convert an Org document to standalone HTML.",
        epilog="Requires Python 3.6+, no third-party packages.",
    )
    ap.add_argument("source", help="path to the input .org file")
    ap.add_argument(
        "-o",
        "--output",
        help="output .html path (default: alongside the source)",
    )
    ap.add_argument(
        "-d",
        "--outdir",
        help="directory to write into, created if absent (default: alongside the source)",
    )
    ap.add_argument(
        "-t",
        "--text",
        action="store_true",
        help="also write a .txt copy next to the .html, for pasting as plain text",
    )
    ap.add_argument(
        "-l",
        "--label",
        action="append",
        metavar="NAME",
        help=(
            "treat a bullet labelled 'NAME: ...' as metadata instead of a list "
            "item; repeatable (default: %s)" % ", ".join(DEFAULT_LABELS)
        ),
    )
    return ap.parse_args(argv)


def main(argv=None):
    args = parse_args(argv if argv is not None else sys.argv[1:])

    src = os.path.abspath(args.source)
    if not os.path.isfile(src):
        sys.exit("error: no such file: %s" % src)

    if args.output:
        out = os.path.abspath(args.output)
    elif args.outdir:
        d = os.path.abspath(args.outdir)
        os.makedirs(d, exist_ok=True)
        out = os.path.join(d, os.path.splitext(os.path.basename(src))[0] + ".html")
    else:
        out = os.path.splitext(src)[0] + ".html"

    with open(src, encoding="utf-8") as f:
        text = f.read()

    title, subtitle, body = strip_keyword_lines(text)
    title = title or os.path.splitext(os.path.basename(src))[0]

    labels = tuple(args.label) if args.label else DEFAULT_LABELS
    label_re = label_pattern(labels)

    head = "<h1>%s</h1>" % inline(title)
    if subtitle:
        head += '\n<p class="sub">%s</p>' % inline(subtitle)

    doc = (
        "<!DOCTYPE html>\n"
        '<html lang="en">\n<head>\n<meta charset="utf-8" />\n'
        '<meta name="viewport" content="width=device-width, initial-scale=1" />\n'
        "<title>%s</title>\n<style>%s</style>\n</head>\n<body>\n<main>\n%s\n%s\n</main>\n</body>\n</html>\n"
        % (html.escape(title), CSS, head, convert(body, label_re))
    )

    parent = os.path.dirname(out)
    if parent:
        os.makedirs(parent, exist_ok=True)
    with open(out, "w", encoding="utf-8") as f:
        f.write(doc)
    print("%s -> %s (%d bytes)" % (src, out, len(doc.encode("utf-8"))))

    if args.text:
        txt = os.path.splitext(out)[0] + ".txt"
        with open(txt, "w", encoding="utf-8") as f:
            f.write(to_text(doc))
        print("%s -> %s (%d bytes)" % (out, txt, os.path.getsize(txt)))

    return 0


if __name__ == "__main__":
    sys.exit(main())