import QtQuick
import Quickshell
import Quickshell.Io
pragma Singleton

Singleton {
    id: root

    property var entries: []
    property int count: entries.length
    property string searchText: ""
    property int selectedIndex: 0
    property bool loading: false
    property var _pendingEntries: []
    readonly property var filteredEntries: {
        if (searchText.trim().length === 0)
            return entries;

        const term = searchText.trim().toLowerCase();
        return entries.filter(function(e) {
            return e.preview.toLowerCase().includes(term);
        });
    }

    signal refreshDone()
    signal copyDone()
    signal deleteDone()
    signal clearDone()

    function refresh() {
        root.loading = true;
        root._pendingEntries = [];
        listProc.running = true;
    }

    function copyEntry(entry) {
        copyProc.command = ["sh", "-c", "cliphist decode " + entry.id + " | wl-copy"];
        copyProc.running = true;
    }

    function deleteEntry(entry) {
        deleteProc.command = ["bash", "-c", "echo '" + entry.id + "' | cliphist delete"];
        deleteProc.running = true;
    }

    function clearAll() {
        clearProc.running = true;
    }

    function parseListOutput(text) {
        if (!text || text.trim().length === 0)
            return [];

        const lines = text.split("\n");
        const result = [];
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i].trim();
            if (line.length === 0)
                continue;

            const tabIdx = line.indexOf("\t");
            const id = tabIdx >= 0 ? line.substring(0, tabIdx) : line;
            const preview = tabIdx >= 0 ? line.substring(tabIdx + 1) : line;
            result.push({
                "id": id,
                "preview": preview,
                "type": getEntryType(preview)
            });
        }
        return result;
    }

    function getEntryType(preview) {
        if (preview.startsWith("[png]") || preview.startsWith("[jpeg]") || preview.startsWith("[image"))
            return "image";

        if (preview.length > 200)
            return "long_text";

        return "text";
    }

    function selectNext() {
        const list = root.filteredEntries;
        if (list.length === 0)
            return ;

        root.selectedIndex = Math.min(root.selectedIndex + 1, list.length - 1);
    }

    function selectPrev() {
        if (root.filteredEntries.length === 0)
            return ;

        root.selectedIndex = Math.max(root.selectedIndex - 1, 0);
    }

    function reset() {
        root.searchText = "";
        root.selectedIndex = 0;
    }

    Component.onCompleted: root.refresh()

    Process {
        id: listProc

        command: ["cliphist", "list"]
        onRunningChanged: {
            if (!running && root.loading) {
                root.entries = root._pendingEntries;
                root._pendingEntries = [];
                root.loading = false;
                root.refreshDone();
            }
        }

        stdout: SplitParser {
            onRead: (data) => {
                if (data.trim().length === 0)
                    return ;

                const result = root.parseListOutput(data);
                if (result.length > 0)
                    root._pendingEntries = root._pendingEntries.concat(result);

            }
        }

    }

    Process {
        id: copyProc

        onRunningChanged: {
            if (!running)
                root.copyDone();

        }
    }

    Process {
        id: deleteProc

        onRunningChanged: {
            if (!running) {
                root.deleteDone();
                root.refresh();
            }
        }
    }

    Process {
        id: clearProc

        command: ["cliphist", "wipe"]
        onRunningChanged: {
            if (!running) {
                root.entries = [];
                root.clearDone();
            }
        }
    }

}
