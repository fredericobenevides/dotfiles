import QtQuick
import Quickshell
import Quickshell.Io
pragma Singleton

Singleton {
    id: root

    readonly property bool active: setProc.running
    property int temperature: 3500
    property bool _restartPending: false

    function toggle() {
        if (setProc.running) {
            setProc.signal(15);
        } else {
            setProc.command = ["gammastep", "-m", "wayland", "-O", String(root.temperature)];
            setProc.running = true;
        }
    }

    function setTemperature(temp) {
        root.temperature = temp;
        if (setProc.running) {
            root._restartPending = true;
            setProc.signal(15);
        } else {
            setProc.command = ["gammastep", "-m", "wayland", "-O", String(root.temperature)];
            setProc.running = true;
        }
    }

    Process {
        id: setProc

        onRunningChanged: {
            if (!running && root._restartPending) {
                root._restartPending = false;
                setProc.command = ["gammastep", "-m", "wayland", "-O", String(root.temperature)];
                setProc.running = true;
            }
        }
    }

}
