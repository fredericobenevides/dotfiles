import QtQuick
import Quickshell
import Quickshell.Io
pragma Singleton

Singleton {
    id: root

    readonly property bool active: setProc.running
    readonly property string cachePath: Quickshell.env("HOME") + "/.cache/quickshell/nightlight.json"
    property int temperature: 3500
    property bool _restartPending: false

    function loadCache(text) {
        if (!text || text.trim() === "")
            return ;

        try {
            const data = JSON.parse(text);
            if (data && typeof data.temperature === "number") {
                const clamped = Math.max(2000, Math.min(6500, Math.round(data.temperature)));
                root.temperature = clamped;
            }
        } catch (e) {
        }
    }

    function saveCache() {
        cacheFile.setText(JSON.stringify({
            "temperature": root.temperature
        }));
    }

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
        root.saveCache();
        if (setProc.running) {
            root._restartPending = true;
            setProc.signal(15);
        } else {
            setProc.command = ["gammastep", "-m", "wayland", "-O", String(root.temperature)];
            setProc.running = true;
        }
    }

    Component.onCompleted: {
        Quickshell.execDetached(["mkdir", "-p", Quickshell.env("HOME") + "/.cache/quickshell"]);
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

    FileView {
        id: cacheFile

        path: root.cachePath
        atomicWrites: true
        watchChanges: true
        onLoaded: root.loadCache(text())
        onLoadFailed: (error) => {
        }
    }

}
