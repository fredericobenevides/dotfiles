pragma Singleton
pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    readonly property real hotTempThreshold: 85

    property real cpuPercent: 0
    property real memPercent: 0
    property real diskPercent: 0
    property real cpuTemp: 0
    property string hostname: ""
    property string uptimeText: ""
    readonly property string username: Quickshell.env("USER") || ""
    property var processes: []
    property int totalProcessCount: 0
    property real totalMemKB: 0
    property real usedMemKB: 0
    property real totalSwapKB: 0
    property real usedSwapKB: 0
    property real diskUsedKB: 0
    property real diskTotalKB: 0
    property real cpuFreqMHz: 0
    property bool processesActive: false
    property bool processesLoading: false

    onProcessesActiveChanged: {
        if (processesActive) {
            processesLoading = true;
            processProcess.running = true;
            processTimer.restart();
        } else {
            processTimer.stop();
        }
    }

    Timer {
        id: processTimer

        interval: 5000
        repeat: true
        running: false
        onTriggered: {
            if (!processProcess.running)
                processProcess.running = true;
        }
    }
    property var _procTicksPrev: ({})
    property real _totalPrev: 0

    property real _cpuTotalPrev: 0
    property real _cpuIdlePrev: 0

    Timer {
        interval: 3000
        repeat: true
        running: true
        onTriggered: root.refresh()
    }

    Component.onCompleted: refresh()

    function refresh() {
        cpuProcess.running = true;
        memProcess.running = true;
        diskProcess.running = true;
        tempProcess.running = true;
        hostnameProcess.running = true;
        uptimeProcess.running = true;
        freqProcess.running = true;
        swapProcess.running = true;
    }

    Process {
        id: cpuProcess

        command: ["sh", "-c", "grep -m1 '^cpu ' /proc/stat"]

        stdout: StdioCollector {
            onStreamFinished: {
                const parts = text.trim().split(/\s+/);
                if (parts.length < 8)
                    return;

                let idle = parseFloat(parts[4]) + parseFloat(parts[5]);
                let total = 0;
                for (let i = 1; i < parts.length; i++)
                    total += parseFloat(parts[i]);

                if (root._cpuTotalPrev > 0) {
                    const dTotal = total - root._cpuTotalPrev;
                    const dIdle = idle - root._cpuIdlePrev;
                    if (dTotal > 0)
                        root.cpuPercent = Math.round((1 - dIdle / dTotal) * 100);
                }

                root._cpuTotalPrev = total;
                root._cpuIdlePrev = idle;
            }
        }
    }

    Process {
        id: memProcess

        command: ["sh", "-c", "awk '/MemTotal|MemAvailable/ {print $1,$2}' /proc/meminfo"]

        stdout: StdioCollector {
            onStreamFinished: {
                const lines = text.trim().split("\n");
                let total = 0;
                let avail = 0;
                for (let i = 0; i < lines.length; i++) {
                    if (lines[i].startsWith("MemTotal"))
                        total = parseFloat(lines[i].split(/\s+/)[1]);
                    else if (lines[i].startsWith("MemAvailable"))
                        avail = parseFloat(lines[i].split(/\s+/)[1]);
                }
                if (total > 0) {
                    root.totalMemKB = total;
                    root.usedMemKB = total - avail;
                    root.memPercent = Math.round((1 - avail / total) * 100);
                }
            }
        }
    }

    Process {
        id: diskProcess

        command: ["df", "-P", "/"]

        stdout: StdioCollector {
            onStreamFinished: {
                const lines = text.trim().split("\n");
                if (lines.length < 2)
                    return;

                const fields = lines[1].trim().split(/\s+/);
                if (fields.length >= 3) {
                    root.diskTotalKB = parseFloat(fields[1]);
                    root.diskUsedKB = parseFloat(fields[2]);
                }
                if (fields.length >= 5)
                    root.diskPercent = parseFloat(fields[4].replace("%", ""));
            }
        }
    }

    Process {
        id: freqProcess

        command: ["sh", "-c", "f=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq 2>/dev/null); if [ -n \"$f\" ]; then echo $((f / 1000)); else awk -F': ' '/cpu MHz/{print int($2); exit}' /proc/cpuinfo; fi"]

        stdout: StdioCollector {
            onStreamFinished: {
                const value = parseFloat(text.trim());
                if (!isNaN(value) && value > 0)
                    root.cpuFreqMHz = value;
            }
        }
    }

    Process {
        id: swapProcess

        command: ["sh", "-c", "awk '/SwapTotal|SwapFree/ {print $1,$2}' /proc/meminfo"]

        stdout: StdioCollector {
            onStreamFinished: {
                const lines = text.trim().split("\n");
                let total = 0;
                let free = 0;
                for (let i = 0; i < lines.length; i++) {
                    if (lines[i].startsWith("SwapTotal"))
                        total = parseFloat(lines[i].split(/\s+/)[1]);
                    else if (lines[i].startsWith("SwapFree"))
                        free = parseFloat(lines[i].split(/\s+/)[1]);
                }
                if (total > 0) {
                    root.totalSwapKB = total;
                    root.usedSwapKB = total - free;
                }
            }
        }
    }

    Process {
        id: tempProcess

        command: ["sh", "-c", "for d in /sys/class/hwmon/hwmon*; do [ -d \"$d\" ] || continue; n=$(cat \"$d/name\" 2>/dev/null); case \"$n\" in coretemp|k10temp|zenpower|cpu_thermal|x86_pkg_temp) for l in \"$d\"/temp*_label; do [ -r \"$l\" ] || continue; t=$(cat \"$l\" 2>/dev/null); case \"$t\" in *Package*|*Tctl*|*Tdie*|*CPU*) i=\"${l%_label}_input\"; [ -r \"$i\" ] && cat \"$i\" && exit 0;; esac; done; for i in \"$d\"/temp*_input; do [ -r \"$i\" ] && cat \"$i\" && exit 0; done;; esac; done; for i in /sys/class/hwmon/hwmon*/temp*_input; do [ -r \"$i\" ] && cat \"$i\" && exit 0; done"]

        stdout: StdioCollector {
            onStreamFinished: {
                const raw = parseFloat(text.trim());
                if (isNaN(raw))
                    return;

                root.cpuTemp = Math.round(raw / 1000);
            }
        }
    }

    Process {
        id: hostnameProcess

        command: ["uname", "-n", "-r"]

        stdout: StdioCollector {
            onStreamFinished: {
                const value = text.trim();
                if (value)
                    root.hostname = value;
            }
        }
    }

    Process {
        id: uptimeProcess

        command: ["sh", "-c", "awk '{d=int($1/86400); h=int($1%86400/3600); m=int($1%3600/60); printf \"%dd %02dh %02dm\", d, h, m}' /proc/uptime"]

        stdout: StdioCollector {
            onStreamFinished: {
                const value = text.trim();
                if (value)
                    root.uptimeText = value;
            }
        }
    }

    Process {
        id: processProcess

        command: ["sh", "-c", "awk '/^cpu /{for(i=2;i<=10;i++)t+=$i} END{print \"@TOTAL \" t}' /proc/stat; for p in /proc/[0-9]*/stat; do pid=${p#/proc/}; pid=${pid%/stat}; [ -r \"$p\" ] || continue; line=$(cat \"$p\") || continue; line=${line#*)}; set -- $line; ticks=$(( ${12:-0} + ${13:-0} )); ppid=$2; user=$(stat -c %U /proc/$pid 2>/dev/null); pss=$(awk 'NR==2{print $2}' /proc/$pid/smaps_rollup 2>/dev/null); comm=$(cat /proc/$pid/comm 2>/dev/null); args=$(tr '\\0' ' ' < /proc/$pid/cmdline 2>/dev/null); printf '%s|%s|%s|%s|%s|%s|%s\\n' \"$pid\" \"$user\" \"$pss\" \"$ticks\" \"$ppid\" \"$comm\" \"$args\"; done"]

        stdout: StdioCollector {
            onStreamFinished: {
                const lines = text.split("\n");
                let total = 0;
                const list = [];
                const prev = root._procTicksPrev;
                for (let i = 0; i < lines.length; i++) {
                    const line = lines[i].trim();
                    if (!line)
                        continue;

                    if (line.indexOf("@TOTAL ") === 0) {
                        total = parseFloat(line.substring(7));
                        continue;
                    }

                    const parts = line.split("|");
                    if (parts.length < 7)
                        continue;

                    const pid = parts[0];
                    const ticks = parseFloat(parts[3]) || 0;
                    let cpu = 0;
                    if (root._totalPrev > 0 && prev[pid] != null) {
                        const dProc = ticks - prev[pid];
                        const dTotal = total - root._totalPrev;
                        if (dTotal > 0)
                            cpu = Math.max(0, (dProc / dTotal) * 100);
                    }

                    list.push({
                        "pid": pid,
                        "user": parts[1],
                        "cpu": cpu,
                        "rss": parseFloat(parts[2]),
                        "ppid": parts[4],
                        "name": parts[5],
                        "fullCommand": parts[6].trim() || parts[5],
                        "ticks": ticks
                    });
                }

                const map = {};
                for (let i = 0; i < list.length; i++)
                    map[list[i].pid] = list[i].ticks;

                root._procTicksPrev = map;
                root._totalPrev = total;
                root.totalProcessCount = list.length;

                const grouped = {};
                for (let i = 0; i < list.length; i++) {
                    const p = list[i];
                    const key = p.name || "unknown";
                    if (!grouped[key])
                        grouped[key] = {
                            "members": []
                        };
                    grouped[key].members.push(p);
                }

                const result = [];
                for (const key in grouped) {
                    const members = grouped[key].members;

                    let rep = members[0];
                    for (let i = 1; i < members.length; i++) {
                        if (parseInt(members[i].pid) < parseInt(rep.pid))
                            rep = members[i];
                    }

                    let cpu = 0;
                    let rss = 0;
                    for (let i = 0; i < members.length; i++) {
                        cpu += members[i].cpu || 0;
                        rss += members[i].rss || 0;
                    }

                    result.push({
                        "pid": rep.pid,
                        "ppid": rep.ppid,
                        "user": rep.user,
                        "cpu": cpu,
                        "rss": rss,
                        "name": key,
                        "fullCommand": rep.fullCommand,
                        "count": members.length
                    });
                }

                result.sort((a, b) => (b.cpu || 0) - (a.cpu || 0));
                root.processes = result;
            }
        }

        onExited: {
            root.processesLoading = false;
        }
    }

    function killProcess(pid, signal) {
        if (!pid)
            return;
        killProc.command = ["kill", "-" + signal, String(pid)];
        killProc.running = true;
    }

    Process {
        id: killProc
    }
}
