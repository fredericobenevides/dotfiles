pragma Singleton
pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    property int repoCount: 0
    property int aurCount: 0
    readonly property int count: root.repoCount + root.aurCount
    readonly property bool available: root.count > 0
    property var repoPackages: []
    property var aurPackages: []
    property bool repoLoading: false
    property bool aurLoading: false

    Timer {
        interval: 1800000
        repeat: true
        running: true
        onTriggered: root.refresh()
    }

    Component.onCompleted: refresh()

    function refresh() {
        if (!updatesProcess.running)
            updatesProcess.running = true;
    }

    function loadRepoPackages() {
        if (root.repoLoading)
            return;
        root.repoLoading = true;
        root.repoPackages = [];
        repoListProcess.running = true;
    }

    function loadAurPackages() {
        if (root.aurLoading)
            return;
        root.aurLoading = true;
        root.aurPackages = [];
        aurListProcess.running = true;
    }

    Process {
        id: repoListProcess

        command: ["sh", "-c", "flock -w 60 /tmp/checkupdates-qs.lock checkupdates 2>/dev/null"]

        stdout: StdioCollector {
            onStreamFinished: {
                const lines = text.split("\n");
                const list = [];
                for (let i = 0; i < lines.length; i++) {
                    const line = lines[i].trim();
                    if (!line)
                        continue;
                    list.push(line.split(/\s+/)[0]);
                }
                root.repoPackages = list;
                root.repoLoading = false;
            }
        }
    }

    Process {
        id: aurListProcess

        command: ["sh", "-c", "yay -Qua 2>/dev/null"]

        stdout: StdioCollector {
            onStreamFinished: {
                const lines = text.split("\n");
                const list = [];
                for (let i = 0; i < lines.length; i++) {
                    const line = lines[i].trim();
                    if (!line)
                        continue;
                    list.push(line);
                }
                root.aurPackages = list;
                root.aurLoading = false;
            }
        }
    }

    Process {
        id: updatesProcess

        command: ["sh", "-c", "repo=$(flock -w 60 /tmp/checkupdates-qs.lock checkupdates 2>/dev/null | wc -l); aur=0; if command -v yay >/dev/null 2>&1; then aur=$(timeout 15 yay -Qua 2>/dev/null | wc -l); fi; echo \"$repo $aur\""]

        stdout: StdioCollector {
            onStreamFinished: {
                const parts = text.trim().split(/\s+/);
                const repo = parseInt(parts[0]);
                const aur = parseInt(parts[1]);
                root.repoCount = isNaN(repo) ? 0 : repo;
                root.aurCount = isNaN(aur) ? 0 : aur;
            }
        }
    }
}
