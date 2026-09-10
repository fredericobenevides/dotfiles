import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Services.Pipewire
pragma Singleton

Singleton {
    id: root

    property int fps: 60
    property string quality: "high"
    property bool audioSystem: true
    property bool audioMic: false
    property bool recording: false
    property bool saving: false
    property bool paused: false
    property int elapsed: 0
    property string lastFile: ""
    property string presetName: "Default"
    property var presetNames: ["Default"]
    property var presetData: ({
        "Default": {
            "fps": 60,
            "quality": "high",
            "audioSystem": true,
            "audioMic": false
        }
    })
    readonly property string cachePath: Quickshell.env("HOME") + "/.cache/quickshell/recorder.json"

    function pad2(n) {
        return String(100 + n).substr(1);
    }

    function timestamp() {
        const d = new Date();
        return d.getFullYear() + "-" + root.pad2(d.getMonth() + 1) + "-" + root.pad2(d.getDate()) + "_" + root.pad2(d.getHours()) + "-" + root.pad2(d.getMinutes()) + "-" + root.pad2(d.getSeconds());
    }

    function formatElapsed() {
        const m = Math.floor(root.elapsed / 60);
        const s = root.elapsed % 60;
        return root.pad2(m) + ":" + root.pad2(s);
    }

    function buildAudio() {
        const parts = [];
        if (root.audioSystem)
            parts.push("default_output");

        if (root.audioMic) {
            const src = Pipewire.defaultAudioSource;
            parts.push(src && src.name ? "device:" + src.name : "default_input");
        }
        if (parts.length === 0)
            return "";

        return "-a \"" + parts.join("|") + "\"";
    }

    function startScreen() {
        if (root.recording)
            return ;

        const file = "$HOME/Videos/gsr-" + root.timestamp() + ".mp4";
        const cmd = "gpu-screen-recorder -w portal -f " + root.fps + " -q " + root.quality + " " + root.buildAudio() + " -c mp4 -k h264 -cursor yes -ipc \"$XDG_RUNTIME_DIR/gsr-qsh.sock\" -o " + file;
        root.elapsed = 0;
        root.paused = false;
        root.saving = false;
        root.recording = true;
        recorderProc.command = ["sh", "-c", cmd];
        recorderProc.running = true;
    }

    function stop() {
        if (!root.recording || root.saving)
            return ;

        root.saving = true;
        root.paused = false;
        stopProc.command = ["sh", "-c", "gsr-cli -ipc \"$XDG_RUNTIME_DIR/gsr-qsh.sock\" stop"];
        stopProc.running = true;
    }

    function setPaused(value) {
        if (!root.recording)
            return ;

        pauseProc.command = ["sh", "-c", "gsr-cli -ipc \"$XDG_RUNTIME_DIR/gsr-qsh.sock\" set-paused " + (value ? "true" : "false")];
        pauseProc.running = true;
    }

    function togglePause() {
        root.setPaused(!root.paused);
    }

    function toggleShortcut() {
        if (root.recording) {
            root.stop();
            return ;
        }
        root.startScreen();
    }

    function openVideos() {
        openFolderProc.running = true;
    }

    function refreshLastFile() {
        lastFileProc.command = ["sh", "-c", "R=$(ls -1t \"$HOME/Videos\"/gsr-*.mp4 2>/dev/null | head -1); printf '%s\\n' \"${R:-none}\""];
        lastFileProc.running = true;
    }

    function notify(msg) {
        notifyProc.command = ["notify-send", "-a", "GPU Screen Recorder", msg];
        notifyProc.running = true;
    }

    function defaultPreset() {
        return {
            "fps": 60,
            "quality": "high",
            "audioSystem": true,
            "audioMic": false
        };
    }

    function sortPresetNames() {
        const names = Object.keys(root.presetData);
        names.sort((a, b) => {
            if (a === "Default")
                return -1;

            if (b === "Default")
                return 1;

            return a.localeCompare(b);
        });
        return names;
    }

    function applyPreset(name) {
        const data = root.presetData[name];
        if (!data)
            return ;

        root.fps = data.fps;
        root.quality = data.quality;
        root.audioSystem = data.audioSystem;
        root.audioMic = data.audioMic;
        root.presetName = name;
        root.savePresetCache();
    }

    function createPreset(name) {
        const trimmed = name.trim();
        if (trimmed === "" || trimmed === "Default")
            return false;

        root.presetData[trimmed] = {
            "fps": root.fps,
            "quality": root.quality,
            "audioSystem": root.audioSystem,
            "audioMic": root.audioMic
        };
        root.presetNames = root.sortPresetNames();
        root.applyPreset(trimmed);
        return true;
    }

    function deletePreset(name) {
        if (name === "Default" || !root.presetData[name])
            return ;

        delete root.presetData[name];
        root.presetNames = root.sortPresetNames();
        if (root.presetName === name)
            root.applyPreset("Default");
        else
            root.savePresetCache();
    }

    function loadPresetCache(text) {
        const loaded = {
            "presets": {
            },
            "lastPreset": "Default"
        };
        try {
            if (text && text.trim() !== "") {
                const data = JSON.parse(text);
                if (data && typeof data === "object") {
                    if (data.presets && typeof data.presets === "object")
                        loaded.presets = data.presets;

                    if (typeof data.lastPreset === "string")
                        loaded.lastPreset = data.lastPreset;

                }
            }
        } catch (e) {
        }
        if (!loaded.presets.Default)
            loaded.presets.Default = root.defaultPreset();

        root.presetData = loaded.presets;
        root.presetNames = root.sortPresetNames();
        if (!loaded.presets[loaded.lastPreset])
            loaded.lastPreset = "Default";

        root.applyPreset(loaded.lastPreset);
    }

    function savePresetCache() {
        cacheFile.setText(JSON.stringify({
            "lastPreset": root.presetName,
            "presets": root.presetData
        }));
    }

    Component.onCompleted: {
        Quickshell.execDetached(["mkdir", "-p", Quickshell.env("HOME") + "/.cache/quickshell"]);
    }

    Timer {
        interval: 1000
        repeat: true
        running: root.recording
        onTriggered: {
            if (root.recording)
                root.elapsed += 1;

        }
    }

    Process {
        id: recorderProc

        onExited: (exitCode) => {
            if (exitCode !== 0 && root.lastFile === "")
                root.notify("Failed to start recording");

            root.recording = false;
            root.paused = false;
            root.saving = false;
        }

        stdout: SplitParser {
            onRead: (data) => {
                const t = data.trim();
                if (t !== "")
                    console.log("gsr:", t);

            }
        }

        stderr: SplitParser {
            onRead: (data) => {
                const t = data.trim();
                if (t !== "")
                    console.log("gsr-err:", t);

            }
        }

    }

    Process {
        id: stopProc

        onExited: (exitCode) => {
            root.saving = false;
            if (exitCode !== 0)
                root.notify("Failed to stop recording");
            else if (root.lastFile !== "")
                root.notify("Recording saved to\n" + root.lastFile);
        }

        stdout: SplitParser {
            onRead: (data) => {
                const t = data.trim();
                if (t !== "")
                    root.lastFile = t;

            }
        }

        stderr: SplitParser {
            onRead: (data) => {
                const t = data.trim();
                if (t !== "")
                    console.log("gsr-err:", t);

            }
        }

    }

    Process {
        id: pauseProc
    }

    FileView {
        id: cacheFile

        path: root.cachePath
        atomicWrites: true
        watchChanges: true
        onLoaded: root.loadPresetCache(text())
        onLoadFailed: (error) => {
            root.loadPresetCache("");
        }
    }

    Process {
        id: openFolderProc

        command: ["sh", "-c", "thunar \"$HOME/Videos\""]
    }

    Process {
        id: lastFileProc

        stdout: SplitParser {
            onRead: (data) => {
                const t = data.trim();
                root.lastFile = t === "none" ? "" : t;
            }
        }

    }

    Process {
        id: notifyProc
    }

}
