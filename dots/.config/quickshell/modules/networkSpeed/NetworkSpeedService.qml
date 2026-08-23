import QtQuick
import Quickshell
import Quickshell.Io
pragma Singleton

Singleton {
    id: root

    property real downSpeed: 0
    property real upSpeed: 0

    function formatSpeed(kbytes) {
        if (kbytes >= 1024)
            return (kbytes / 1024).toFixed(1) + " MB";

        return Math.round(kbytes) + " KB";
    }

    function refresh() {
        speedProc.running = true;
    }

    Component.onCompleted: root.refresh()

    Timer {
        id: speedTimer

        interval: 1000
        repeat: true
        running: true
        onTriggered: root.refresh()
    }

    Process {
        id: speedProc

        property string cmd: "R1=$(cat /proc/net/dev | grep -vE 'lo:|docker|Inter|face' | awk '{s+=$2} END{print s}'); T1=$(cat /proc/net/dev | grep -vE 'lo:|docker|Inter|face' | awk '{s+=$10} END{print s}'); sleep 1; R2=$(cat /proc/net/dev | grep -vE 'lo:|docker|Inter|face' | awk '{s+=$2} END{print s}'); T2=$(cat /proc/net/dev | grep -vE 'lo:|docker|Inter|face' | awk '{s+=$10} END{print s}'); echo \"$((R2-R1)) $((T2-T1))\""

        command: ["sh", "-c", cmd]

        stdout: SplitParser {
            onRead: (data) => {
                const parts = data.trim().split(/\s+/);
                if (parts.length < 2)
                    return ;

                root.downSpeed = (parseInt(parts[0]) || 0) / 1024;
                root.upSpeed = (parseInt(parts[1]) || 0) / 1024;
            }
        }

    }

}
