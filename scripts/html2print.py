#!/usr/bin/env python3
"""Add print-oriented CSS to a standalone HTML page, for PDF export.

Takes the screen-sized page emitted by org2html.py (16px type, screen
margins) and re-emits it with an extra <style> block appended, so the same
document prints as a compact A4 resume: ~10.5pt type, 14mm margins and
page-break hints that keep each heading with the block it introduces.

The original file is left untouched; write the result to a separate path and
print that one. Companion to org2html.py:

    org2html.py NOTE.org -o /tmp/NOTE.html
    html2print.py /tmp/NOTE.html /tmp/NOTE.print.html
    <browser> /tmp/NOTE.print.html        # Ctrl+P -> Save as PDF

Usage:
    html2print.py IN.html OUT.html        -> OUT.html with print CSS injected
"""
import os
import sys

PRINT_CSS = """
/* --- print tuning: compact A4 resume --- */
@page { size: A4; margin: 14mm 13mm; }
html { font-size: 10.5pt; }
body { padding: 0; font-size: 10.5pt; line-height: 1.35; }
main { max-width: none; }
h1 { font-size: 1.55rem; margin: 0 0 .2rem; }
h1 + .sub { margin: 0 0 1rem; font-size: .85rem; }
h2 {
  font-size: .95rem; margin: 1.15rem 0 .35rem; padding-bottom: .15rem;
  page-break-after: avoid;
}
h3 { font-size: 1rem; margin: .7rem 0 .15rem; page-break-after: avoid; }
h4 { font-size: .95rem; margin: .5rem 0 .1rem; page-break-after: avoid; }
p { margin: .2rem 0; }
.meta { margin: .05rem 0 .2rem; font-size: .85rem; }
.stack { margin-top: .3rem; padding-top: .25rem; font-size: .85rem; }
ul { margin: .2rem 0 .5rem; padding-left: 1.1rem; }
li { margin: .1rem 0; }
table { margin: .35rem 0; font-size: .88rem; }
th, td { padding: .25rem .45rem; }
/* keep each job block together where it fits on one page */
h3, h3 + p, h3 + p + ul, h3 + p + ul + p { page-break-inside: avoid; }
"""


def main(argv=None):
    argv = sys.argv[1:] if argv is None else argv
    if len(argv) != 2:
        sys.exit("usage: html2print.py IN.html OUT.html")

    src, dst = os.path.abspath(argv[0]), os.path.abspath(argv[1])
    if not os.path.isfile(src):
        sys.exit("error: no such file: %s" % src)
    if src == dst:
        sys.exit("error: IN and OUT must differ (the source is left untouched)")
    if not dst.endswith(".html"):
        sys.exit("error: OUT must end in .html: %s" % dst)

    with open(src, encoding="utf-8") as f:
        doc = f.read()
    if "</head>" not in doc:
        sys.exit("error: no </head> in %s (not an HTML document?)" % src)

    doc = doc.replace("</head>", "<style>%s</style>\n</head>" % PRINT_CSS, 1)

    parent = os.path.dirname(dst)
    if parent:
        os.makedirs(parent, exist_ok=True)
    with open(dst, "w", encoding="utf-8") as f:
        f.write(doc)
    print("%s -> %s (%d bytes)" % (src, dst, len(doc.encode("utf-8"))))
    return 0


if __name__ == "__main__":
    sys.exit(main())
