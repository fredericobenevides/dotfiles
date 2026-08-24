pragma Singleton
pragma ComponentBehavior: Bound

import QtQuick
import QtQml
import Quickshell
import Quickshell.Io
import Quickshell.Services.Notifications

Singleton {
    id: root

    signal popupRequested(var notif)
    signal popupClosed(var notif)

    property var items: []
    property int count: items.length
    property var history: []
    readonly property string historyPath: Quickshell.env("HOME") + "/.cache/quickshell/notifications-history.json"

    Component.onCompleted: {
        Quickshell.execDetached(["mkdir", "-p", Quickshell.env("HOME") + "/.cache/quickshell"]);
    }

    NotificationServer {
        keepOnReload: true
        persistenceSupported: true
        bodySupported: true
        bodyMarkupSupported: true
        bodyHyperlinksSupported: true
        bodyImagesSupported: true
        imageSupported: true
        actionsSupported: true
        inlineReplySupported: true

        onNotification: notif => {
            notif.tracked = true;
            const watcher = watcherComponent.createObject(root, {
                "watched": notif
            });
            root.items = [notif].concat(root.items);
            if (!notif.lastGeneration) {
                root.addToHistory(notif);
                root.popupRequested(notif);
                Quickshell.execDetached(["paplay", Quickshell.env("HOME") + "/.config/quickshell/assets/sounds/message.oga"]);
            }
        }
    }

    Component {
        id: watcherComponent

        Connections {
            property var watched: null
            property bool isInitial: true
            property bool soundCooldown: false

            target: watched

            function onBodyChanged() {
                if (!isInitial && !soundCooldown) {
                    soundCooldown = true;
                    Quickshell.execDetached(["paplay", Quickshell.env("HOME") + "/.config/quickshell/assets/sounds/message.oga"]);
                    Qt.callLater(function() { soundCooldown = false; });
                }
            }

            function onClosed() {
                NotificationsService.remove(watched);
                NotificationsService.popupClosed(watched);
            }

            Component.onCompleted: {
                Qt.callLater(function() { isInitial = false; });
            }
        }
    }

    function remove(n) {
        const arr = root.items.slice();
        const idx = arr.indexOf(n);
        if (idx !== -1) {
            arr.splice(idx, 1);
            root.items = arr;
        }
    }

    function dismiss(n) {
        try {
            n.dismiss();
        } catch (e) {
            root.remove(n);
        }
    }

    function dismissApp(appName) {
        const arr = root.items.slice();
        for (let i = 0; i < arr.length; i++) {
            if (arr[i].appName === appName)
                root.dismiss(arr[i]);
        }
    }

    function dismissAll() {
        const arr = root.items.slice();
        root.items = [];
        for (let i = 0; i < arr.length; i++) {
            try {
                arr[i].dismiss();
            } catch (e) {
            }
        }
    }

    function loadHistory(text) {
        if (!text || text.trim() === "")
            return;

        try {
            const data = JSON.parse(text);
            if (Array.isArray(data))
                root.history = data;
        } catch (e) {
        }
    }

    function saveHistory() {
        historyFileView.setText(JSON.stringify(root.history));
    }

    function addToHistory(notif) {
        const entry = {
            "appName": notif.appName || "",
            "summary": notif.summary || "",
            "body": notif.body || "",
            "urgency": notif.urgency,
            "time": Date.now()
        };
        const arr = root.history.slice();
        arr.unshift(entry);
        if (arr.length > 50)
            arr.length = 50;
        root.history = arr;
        root.saveHistory();
    }

    function removeFromHistory(entry) {
        const arr = root.history.slice();
        const idx = arr.findIndex(e => e.appName === entry.appName && e.summary === entry.summary && e.time === entry.time);
        if (idx !== -1) {
            arr.splice(idx, 1);
            root.history = arr;
            root.saveHistory();
        }
    }

    function clearHistory() {
        root.history = [];
        root.saveHistory();
    }

    FileView {
        id: historyFileView

        path: root.historyPath
        atomicWrites: true
        watchChanges: true
        onLoaded: root.loadHistory(text())
        onLoadFailed: error => {
        }
    }
}
