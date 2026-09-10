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
    property bool capturing: false
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
    property var region: null
    property bool regionEnabled: true
    readonly property bool hasRegion: region !== null
    readonly property string regionLabel: hasRegion ? region.w + "x" + region.h + "@" + region.x + "," + region.y : ""
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
        root.capturing = false;
        root.recording = true;
        recorderProc.command = ["sh", "-c", cmd];
        recorderProc.running = true;
    }

    function startRegion() {
        if (root.recording || !root.hasRegion)
            return ;

        const r = root.region;
        const file = "$HOME/Videos/gsr-" + root.timestamp() + ".mp4";
        const cmd = "gpu-screen-recorder -w " + r.w + "x" + r.h + "+" + r.x + "+" + r.y + " -f " + root.fps + " -q " + root.quality + " " + root.buildAudio() + " -c mp4 -k h264 -cursor yes -ipc \"$XDG_RUNTIME_DIR/gsr-qsh.sock\" -o " + file;
        root.elapsed = 0;
        root.paused = false;
        root.saving = false;
        root.capturing = false;
        root.recording = true;
        recorderProc.command = ["sh", "-c", cmd];
        recorderProc.running = true;
    }

    function selectRegion() {
        if (root.recording)
            return ;

        regionProc.command = ["sh", "-c", "rm -f /tmp/qs-region.txt; kitty --title \"Select recording region\" sh -c \"slurp -f '%o %x %y %w %h' > /tmp/qs-region.txt\"; cat /tmp/qs-region.txt 2>/dev/null"];
        regionProc.running = true;
    }

    function clearRegion() {
        const p = root.presetData[root.presetName];
        if (p) {
            p.region = null;
            p.regionEnabled = false;
        }
        root.region = null;
        root.regionEnabled = false;
        root.savePresetCache();
    }

    function toggleRegionEnabled() {
        if (!root.hasRegion)
            return ;

        root.regionEnabled = !root.regionEnabled;
        const p = root.presetData[root.presetName];
        if (p)
            p.regionEnabled = root.regionEnabled;

        root.savePresetCache();
    }

    function toggleRecording() {
        if (root.recording) {
            root.stop();
            return ;
        }
        if (root.hasRegion && root.regionEnabled)
            root.startRegion();
        else
            root.startScreen();
    }

    function stop() {
        if (!root.recording || root.saving)
            return ;

        root.saving = true;
        root.capturing = false;
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
        root.toggleRecording();
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
            "audioMic": false,
            "region": null,
            "regionEnabled": false
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
        root.region = data.region || null;
        root.regionEnabled = root.region !== null && data.regionEnabled === false ? false : true;
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
            "audioMic": root.audioMic,
            "region": root.region,
            "regionEnabled": root.regionEnabled
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

                    if (data.region && typeof data.region === "object" && data.region.w > 0 && data.region.h > 0) {
                        loaded.legacyRegion = data.region;
                        loaded.legacyRegionEnabled = data.regionEnabled;
                    }

                }
            }
        } catch (e) {
        }
        if (!loaded.presets.Default)
            loaded.presets.Default = root.defaultPreset();

        if (loaded.legacyRegion && loaded.presets.Default && loaded.presets.Default.region === undefined) {
            loaded.presets.Default.region = loaded.legacyRegion;
            loaded.presets.Default.regionEnabled = loaded.legacyRegionEnabled;
        }

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
            if (root.capturing && !root.paused)
                root.elapsed += 1;

        }
    }

    Process {
        id: recorderProc

        onExited: (exitCode) => {
            if (exitCode !== 0 && root.lastFile === "")
                root.notify("Failed to start recording");

            root.recording = false;
            root.capturing = false;
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

                if (!root.capturing && (t.indexOf("update fps:") >= 0 || t.indexOf("new state: \"streaming\"") >= 0))
                    root.capturing = true;

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
        id: regionProc

        stdout: SplitParser {
            onRead: (data) => {
                const t = data.trim();
                const m = t.match(/^(\S+) (\-?\d+) (\-?\d+) (\d+) (\d+)$/);
                if (!m)
                    return;

                const x = parseInt(m[2]);
                const y = parseInt(m[3]);
                const w = parseInt(m[4]);
                const h = parseInt(m[5]);
                if (w < 16 || h < 16)
                    return;

                if (!root.hasRegion || x !== root.region.x || y !== root.region.y || w !== root.region.w || h !== root.region.h) {
                    root.region = {
                        "x": x,
                        "y": y,
                        "w": w,
                        "h": h
                    };
                    root.regionEnabled = true;
                    const p = root.presetData[root.presetName];
                    if (p) {
                        p.region = {
                            "x": x,
                            "y": y,
                            "w": w,
                            "h": h
                        };
                        p.regionEnabled = true;
                    }
                    root.savePresetCache();
                }
            }
        }

    }

    Process {
        id: notifyProc
    }

}
