import QtQuick
import Quickshell
import Quickshell.Io
pragma Singleton

Singleton {
    id: root

    property bool connected: false
    property bool connecting: false

    function refresh() {
        statusProc.running = true;
    }

    function toggle() {
        if (root.connected || root.connecting) {
            root.connecting = false;
            connectTimeout.stop();
            disconnectProc.running = true;
        } else {
            root.connecting = true;
            connectTimeout.start();
            connectProc.running = true;
        }
    }

    Component.onCompleted: root.refresh()

    Timer {
        id: connectTimeout

        interval: 15000
        onTriggered: {
            root.connecting = false;
            root.connected = false;
        }
    }

    Timer {
        id: statusTimer

        interval: 5000
        repeat: true
        running: true
        onTriggered: root.refresh()
    }

    Process {
        id: statusProc

        command: ["warp-cli", "status"]

        stdout: SplitParser {
            onRead: (data) => {
                // Disconnected during an in-progress connect → keep amber

                const t = data.trim();
                if (!t.startsWith("Status update:"))
                    return ;

                if (t.indexOf("Connected") !== -1) {
                    root.connected = true;
                    root.connecting = false;
                    connectTimeout.stop();
                } else if (t.indexOf("Connecting") !== -1) {
                    root.connecting = true;
                } else if (root.connecting && connectTimeout.running) {
                } else {
                    root.connected = false;
                    root.connecting = false;
                }
            }
        }

    }

    Process {
        id: connectProc

        command: ["warp-cli", "connect"]
        onRunningChanged: {
            if (!running)
                root.refresh();

        }
    }

    Process {
        id: disconnectProc

        command: ["warp-cli", "disconnect"]
        onRunningChanged: {
            if (!running)
                root.refresh();

        }
    }

}
