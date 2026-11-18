import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import qs.modules.systemInfo
import qs.theme

PanelWindow {
    id: systemInfoModal

    property real fontScale: 1.0
    property var targetScreen: null
    property string processFilter: "all"
    property string sortField: "cpu"
    property bool sortAscending: false
    property string searchText: ""
    property string expandedPid: ""
    property bool _tempFlashOn: false
    readonly property real colName: 220
    readonly property real colCpu: 48
    readonly property real colGap: 54
    readonly property real colMem: 38
    readonly property real colPid: 60
    readonly property real colArrow: 16
    readonly property real colSpacing: 14
    readonly property var visibleProcesses: {
        const procs = SystemInfoStats.processes;
        let filtered = procs;
        if (processFilter === "user")
            filtered = procs.filter((p) => {
            return p.user === SystemInfoStats.username;
        });
        else if (processFilter === "system")
            filtered = procs.filter((p) => {
            return p.user !== SystemInfoStats.username;
        });
        if (searchText.length > 0) {
            const term = searchText.toLowerCase();
            filtered = filtered.filter((p) => {
                return (p.name || "").toLowerCase().includes(term);
            });
        }
        const field = sortField;
        const asc = sortAscending;
        return filtered.slice().sort((a, b) => {
            if (field === "name") {
                const va = (a.name || "").toLowerCase();
                const vb = (b.name || "").toLowerCase();
                return asc ? va.localeCompare(vb) : vb.localeCompare(va);
            }
            if (field === "pid") {
                const va = parseInt(a.pid) || 0;
                const vb = parseInt(b.pid) || 0;
                return asc ? va - vb : vb - va;
            }
            const key = field === "mem" ? "rss" : field;
            const va = a[key] || 0;
            const vb = b[key] || 0;
            return asc ? va - vb : vb - va;
        });
    }

    function formatMem(kb) {
        if (kb == null || isNaN(kb))
            return "--";

        if (kb >= 1.04858e+06)
            return (kb / 1.04858e+06).toFixed(2) + " GB";

        return Math.round(kb / 1024) + " MB";
    }

    function setSort(field) {
        if (sortField === field) {
            sortAscending = !sortAscending;
        } else {
            sortField = field;
            sortAscending = false;
        }
    }

    function open() {
        visible = true;
    }

    function closeModal() {
        visible = false;
    }

    onVisibleChanged: {
        SystemInfoStats.processesActive = visible;
        if (visible)
            bg.forceActiveFocus();

    }
    focusable: true
    visible: false
    screen: targetScreen
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    color: "transparent"

    Timer {
        interval: 500
        repeat: true
        running: SystemInfoStats.cpuTemp >= SystemInfoStats.hotTempThreshold
        onTriggered: systemInfoModal._tempFlashOn = !systemInfoModal._tempFlashOn
        onRunningChanged: {
            if (!running)
                systemInfoModal._tempFlashOn = false;

        }
    }

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    MouseArea {
        anchors.fill: parent
        onClicked: systemInfoModal.closeModal()
    }

    Rectangle {
        id: bg

        width: Theme.scaledSize(600, systemInfoModal.fontScale)
        height: content.implicitHeight + Theme.scaledSize(32, systemInfoModal.fontScale)
        anchors.centerIn: parent
        radius: Theme.scaledSize(16, systemInfoModal.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: Theme.scaledSize(1, systemInfoModal.fontScale)
        Keys.onPressed: (event) => {
            const filters = ["all", "user", "system"];
            const idx = filters.indexOf(systemInfoModal.processFilter);
            if (event.key === Qt.Key_Escape) {
                systemInfoModal.closeModal();
                event.accepted = true;
            } else if (event.key === Qt.Key_Left) {
                systemInfoModal.processFilter = filters[(idx - 1 + filters.length) % filters.length];
                event.accepted = true;
            } else if (event.key === Qt.Key_Right) {
                systemInfoModal.processFilter = filters[(idx + 1) % filters.length];
                event.accepted = true;
            } else if (event.key === Qt.Key_Tab) {
                searchField.forceActiveFocus();
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: content

            anchors.fill: parent
            anchors.margins: Theme.scaledSize(16, systemInfoModal.fontScale)
            spacing: Theme.scaledSize(0, systemInfoModal.fontScale)

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, systemInfoModal.fontScale)

                Text {
                    text: "\uE8F8"
                    font.family: materialSymbols.name
                    font.pixelSize: Theme.scaledFont(18, systemInfoModal.fontScale)
                    color: Theme.primary
                }

                Text {
                    Layout.fillWidth: true
                    text: "System"
                    font.pixelSize: Theme.scaledFont(18, systemInfoModal.fontScale)
                    font.bold: true
                    color: Theme.surfaceText
                }

            }

            RowLayout {
                Layout.fillWidth: true
                Layout.topMargin: Theme.scaledSize(15, systemInfoModal.fontScale)
                spacing: Theme.scaledSize(12, systemInfoModal.fontScale)

                Repeater {
                    model: [{
                        "icon": "\uE322",
                        "label": "CPU",
                        "display": Math.round(SystemInfoStats.cpuPercent) + "%",
                        "percent": SystemInfoStats.cpuPercent,
                        "hot": false
                    }, {
                        "icon": "\uE30D",
                        "label": "Memory",
                        "display": Math.round(SystemInfoStats.memPercent) + "%",
                        "percent": SystemInfoStats.memPercent,
                        "hot": false
                    }, {
                        "icon": "\uF80E",
                        "label": "HD",
                        "display": Math.round(SystemInfoStats.diskPercent) + "%",
                        "percent": SystemInfoStats.diskPercent,
                        "hot": false
                    }, {
                        "icon": "\uE1FF",
                        "label": "Temperature",
                        "display": Math.round(SystemInfoStats.cpuTemp) + "°C",
                        "percent": Math.min(100, SystemInfoStats.cpuTemp),
                        "hot": SystemInfoStats.cpuTemp >= SystemInfoStats.hotTempThreshold
                    }]

                    delegate: Item {
                        Layout.fillWidth: true
                        Layout.preferredHeight: Theme.scaledSize(148, systemInfoModal.fontScale)

                        Item {
                            id: ringWrap

                            width: Theme.scaledSize(108, systemInfoModal.fontScale)
                            height: Theme.scaledSize(108, systemInfoModal.fontScale)
                            anchors.horizontalCenter: parent.horizontalCenter
                            anchors.top: parent.top

                            Canvas {
                                property real gaugeValue: modelData.percent

                                anchors.fill: parent
                                antialiasing: true
                                onPaint: {
                                    const ctx = getContext("2d");
                                    ctx.reset();
                                    const cx = width / 2;
                                    const cy = height / 2;
                                    const r = width / 2 - 8;
                                    const value = Math.min(1, Math.max(0, gaugeValue / 100));
                                    ctx.lineWidth = 8;
                                    ctx.strokeStyle = Theme.surfaceContainerHighest;
                                    ctx.beginPath();
                                    ctx.arc(cx, cy, r, 0, Math.PI * 2);
                                    ctx.stroke();
                                    ctx.strokeStyle = modelData.hot ? (systemInfoModal._tempFlashOn ? "#ff8080" : Theme.error) : Theme.primary;
                                    ctx.beginPath();
                                    ctx.arc(cx, cy, r, -Math.PI / 2, -Math.PI / 2 + Math.PI * 2 * value);
                                    ctx.stroke();
                                }
                                onGaugeValueChanged: requestPaint()
                            }

                            ColumnLayout {
                                anchors.centerIn: parent
                                spacing: Theme.scaledSize(0, systemInfoModal.fontScale)

                                Text {
                                    Layout.alignment: Qt.AlignHCenter
                                    text: modelData.display
                                    font.pixelSize: Theme.scaledFont(18, systemInfoModal.fontScale)
                                    font.bold: true
                                    color: modelData.hot ? (systemInfoModal._tempFlashOn ? "#ffffff" : Theme.error) : Theme.surfaceText
                                }

                                Text {
                                    Layout.alignment: Qt.AlignHCenter
                                    text: modelData.label
                                    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                                    color: Theme.surfaceVariantText
                                }

                            }

                        }

                        Row {
                            anchors.horizontalCenter: parent.horizontalCenter
                            anchors.top: ringWrap.bottom
                            anchors.topMargin: Theme.scaledSize(4, systemInfoModal.fontScale)
                            spacing: Theme.scaledSize(3, systemInfoModal.fontScale)

                            Text {
                                text: modelData.icon
                                font.family: materialSymbols.name
                                font.pixelSize: Theme.scaledFont(18, systemInfoModal.fontScale)
                                color: modelData.hot ? Theme.error : Theme.primary
                                anchors.verticalCenter: parent.verticalCenter
                            }

                            Text {
                                text: modelData.label
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                                font.bold: true
                                color: Theme.surfaceVariantText
                                anchors.verticalCenter: parent.verticalCenter
                            }

                        }

                    }

                }

            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(2, systemInfoModal.fontScale)

                Row {
                    spacing: Theme.scaledSize(4, systemInfoModal.fontScale)
                    Layout.fillWidth: true

                    Text {
                        text: "Info:"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: SystemInfoStats.hostname
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

            }

            Rectangle {
                Layout.fillWidth: true
                Layout.topMargin: Theme.scaledSize(8, systemInfoModal.fontScale)
                height: Theme.scaledSize(1, systemInfoModal.fontScale)
                color: Theme.outlineVariant
            }

            ColumnLayout {
                Layout.fillWidth: true
                Layout.topMargin: Theme.scaledSize(8, systemInfoModal.fontScale)
                spacing: Theme.scaledSize(2, systemInfoModal.fontScale)

                Row {
                    spacing: Theme.scaledSize(4, systemInfoModal.fontScale)
                    Layout.fillWidth: true

                    Text {
                        text: "Uptime:"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: SystemInfoStats.uptimeText
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

                Row {
                    spacing: Theme.scaledSize(4, systemInfoModal.fontScale)
                    Layout.fillWidth: true

                    Text {
                        text: "CPU Frequency:"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: SystemInfoStats.cpuFreqMHz > 0 ? (SystemInfoStats.cpuFreqMHz / 1000).toFixed(2) + " GHz" : "--"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

                Row {
                    spacing: Theme.scaledSize(4, systemInfoModal.fontScale)
                    Layout.fillWidth: true

                    Text {
                        text: "RAM Usage:"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: systemInfoModal.formatMem(SystemInfoStats.usedMemKB) + " / " + systemInfoModal.formatMem(SystemInfoStats.totalMemKB)
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

                Row {
                    spacing: Theme.scaledSize(4, systemInfoModal.fontScale)
                    Layout.fillWidth: true

                    Text {
                        text: "Swap Usage:"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: systemInfoModal.formatMem(SystemInfoStats.usedSwapKB) + " / " + systemInfoModal.formatMem(SystemInfoStats.totalSwapKB)
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

                Row {
                    spacing: Theme.scaledSize(4, systemInfoModal.fontScale)
                    Layout.fillWidth: true

                    Text {
                        text: "Processes:"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: SystemInfoStats.totalProcessCount
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

            }

            Rectangle {
                Layout.fillWidth: true
                Layout.topMargin: Theme.scaledSize(8, systemInfoModal.fontScale)
                height: Theme.scaledSize(1, systemInfoModal.fontScale)
                color: Theme.outlineVariant
            }

            ColumnLayout {
                Layout.fillWidth: true
                Layout.topMargin: Theme.scaledSize(8, systemInfoModal.fontScale)
                spacing: Theme.scaledSize(2, systemInfoModal.fontScale)

                Row {
                    spacing: Theme.scaledSize(4, systemInfoModal.fontScale)
                    Layout.fillWidth: true

                    Text {
                        text: "File systems:"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                        color: Theme.surfaceVariantText
                    }

                }

                Row {
                    spacing: Theme.scaledSize(4, systemInfoModal.fontScale)
                    Layout.fillWidth: true

                    Text {
                        text: "/ :"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: systemInfoModal.formatMem(SystemInfoStats.diskUsedKB) + " / " + systemInfoModal.formatMem(SystemInfoStats.diskTotalKB)
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

            }

            Rectangle {
                Layout.fillWidth: true
                Layout.topMargin: Theme.scaledSize(8, systemInfoModal.fontScale)
                height: Theme.scaledSize(1, systemInfoModal.fontScale)
                color: Theme.outlineVariant
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.topMargin: Theme.scaledSize(15, systemInfoModal.fontScale)
                spacing: Theme.scaledSize(8, systemInfoModal.fontScale)

                Rectangle {
                    Layout.preferredWidth: Theme.scaledSize(48, systemInfoModal.fontScale)
                    Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                    radius: Theme.scaledSize(12, systemInfoModal.fontScale)
                    color: systemInfoModal.processFilter === "all" ? Theme.primary : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "ALL"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                        font.bold: true
                        color: systemInfoModal.processFilter === "all" ? Theme.primaryText : Theme.surfaceVariantText
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: systemInfoModal.processFilter = "all"
                    }

                }

                Rectangle {
                    Layout.preferredWidth: Theme.scaledSize(52, systemInfoModal.fontScale)
                    Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                    radius: Theme.scaledSize(12, systemInfoModal.fontScale)
                    color: systemInfoModal.processFilter === "user" ? Theme.primary : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "USER"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                        font.bold: true
                        color: systemInfoModal.processFilter === "user" ? Theme.primaryText : Theme.surfaceVariantText
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: systemInfoModal.processFilter = "user"
                    }

                }

                Rectangle {
                    Layout.preferredWidth: Theme.scaledSize(62, systemInfoModal.fontScale)
                    Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                    radius: Theme.scaledSize(12, systemInfoModal.fontScale)
                    color: systemInfoModal.processFilter === "system" ? Theme.primary : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "System"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                        font.bold: true
                        color: systemInfoModal.processFilter === "system" ? Theme.primaryText : Theme.surfaceVariantText
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: systemInfoModal.processFilter = "system"
                    }

                }

                Item {
                    Layout.fillWidth: true
                }

                Rectangle {
                    Layout.preferredWidth: Theme.scaledSize(220, systemInfoModal.fontScale)
                    Layout.preferredHeight: Theme.scaledSize(30, systemInfoModal.fontScale)
                    radius: Theme.scaledSize(15, systemInfoModal.fontScale)
                    color: Theme.surfaceContainerHigh
                    border.color: searchField.activeFocus ? Theme.primary : "transparent"
                    border.width: Theme.scaledSize(1, systemInfoModal.fontScale)

                    RowLayout {
                        anchors.fill: parent
                        anchors.leftMargin: Theme.scaledSize(8, systemInfoModal.fontScale)
                        anchors.rightMargin: Theme.scaledSize(8, systemInfoModal.fontScale)
                        spacing: Theme.scaledSize(4, systemInfoModal.fontScale)

                        Text {
                            text: "\uE8B6"
                            font.family: materialSymbols.name
                            font.pixelSize: Theme.scaledFont(12, systemInfoModal.fontScale)
                            color: Theme.surfaceVariantText
                        }

                        Item {
                            Layout.fillWidth: true
                            Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                            Layout.alignment: Qt.AlignVCenter

                            Text {
                                anchors.left: parent.left
                                anchors.verticalCenter: parent.verticalCenter
                                text: "Search processes..."
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelLarge, systemInfoModal.fontScale)
                                color: Theme.surfaceVariantText
                                visible: searchField.text === "" && !searchField.activeFocus
                            }

                            TextInput {
                                id: searchField

                                anchors.fill: parent
                                z: 1
                                clip: true
                                color: Theme.surfaceText
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelLarge, systemInfoModal.fontScale)
                                selectByMouse: true
                                verticalAlignment: Text.AlignVCenter
                                onTextChanged: systemInfoModal.searchText = text
                                Keys.onPressed: (event) => {
                                    if (event.key === Qt.Key_Escape) {
                                        searchField.text = "";
                                        searchField.focus = false;
                                        event.accepted = true;
                                    } else if (event.key === Qt.Key_Tab) {
                                        bg.forceActiveFocus();
                                        event.accepted = true;
                                    }
                                }
                            }

                        }

                    }

                }

            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: Theme.scaledSize(320, systemInfoModal.fontScale)
                Layout.topMargin: Theme.scaledSize(12, systemInfoModal.fontScale)
                radius: Theme.scaledSize(14, systemInfoModal.fontScale)
                color: Theme.surfaceContainerHigh
                border.color: Theme.surfaceContainerHighest
                border.width: Theme.scaledSize(1, systemInfoModal.fontScale)

                ColumnLayout {
                    anchors.fill: parent
                    anchors.margins: Theme.scaledSize(12, systemInfoModal.fontScale)
                    spacing: Theme.scaledSize(8, systemInfoModal.fontScale)

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: systemInfoModal.colSpacing

                        Item {
                            Layout.preferredWidth: systemInfoModal.colName
                            Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                            Layout.alignment: Qt.AlignVCenter

                            Text {
                                anchors.fill: parent
                                text: "Name " + (systemInfoModal.sortField === "name" ? (systemInfoModal.sortAscending ? "\u25B4" : "\u25BE") : "")
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                                font.bold: true
                                color: systemInfoModal.sortField === "name" ? Theme.primary : Theme.surfaceVariantText
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: systemInfoModal.setSort("name")
                            }

                        }

                        Item {
                            Layout.preferredWidth: systemInfoModal.colCpu
                            Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                            Layout.alignment: Qt.AlignVCenter

                            Text {
                                anchors.centerIn: parent
                                anchors.horizontalCenterOffset: -18
                                text: "CPU " + (systemInfoModal.sortField === "cpu" ? (systemInfoModal.sortAscending ? "\u25B4" : "\u25BE") : "")
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                                font.bold: true
                                color: systemInfoModal.sortField === "cpu" ? Theme.primary : Theme.surfaceVariantText
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: systemInfoModal.setSort("cpu")
                            }

                        }

                        Item {
                            Layout.preferredWidth: systemInfoModal.colGap
                            Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                        }

                        Item {
                            Layout.preferredWidth: systemInfoModal.colMem
                            Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                            Layout.alignment: Qt.AlignVCenter

                            Text {
                                anchors.centerIn: parent
                                anchors.horizontalCenterOffset: -38
                                text: "Memory " + (systemInfoModal.sortField === "mem" ? (systemInfoModal.sortAscending ? "\u25B4" : "\u25BE") : "")
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                                font.bold: true
                                color: systemInfoModal.sortField === "mem" ? Theme.primary : Theme.surfaceVariantText
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: systemInfoModal.setSort("mem")
                            }

                        }

                        Item {
                            Layout.preferredWidth: systemInfoModal.colPid
                            Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                            Layout.alignment: Qt.AlignVCenter

                            Text {
                                anchors.centerIn: parent
                                anchors.horizontalCenterOffset: -16
                                text: "PID " + (systemInfoModal.sortField === "pid" ? (systemInfoModal.sortAscending ? "\u25B4" : "\u25BE") : "")
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                                font.bold: true
                                color: systemInfoModal.sortField === "pid" ? Theme.primary : Theme.surfaceVariantText
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: systemInfoModal.setSort("pid")
                            }

                        }

                        Item {
                            Layout.fillWidth: true
                            Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                        }

                    }

                    Rectangle {
                        Layout.fillWidth: true
                        Layout.preferredHeight: Theme.scaledSize(1, systemInfoModal.fontScale)
                        color: Theme.surfaceContainerHighest
                    }

                    Item {
                        Layout.fillWidth: true
                        Layout.fillHeight: true

                        ListView {
                            anchors.fill: parent
                            clip: true
                            visible: !SystemInfoStats.processesLoading
                            model: systemInfoModal.visibleProcesses
                            spacing: Theme.scaledSize(2, systemInfoModal.fontScale)

                            ScrollBar.vertical: ScrollBar {
                                policy: ScrollBar.AsNeeded
                                width: Theme.scaledSize(14, systemInfoModal.fontScale)
                                minimumSize: 0.25

                                contentItem: Rectangle {
                                    implicitWidth: Theme.scaledSize(14, systemInfoModal.fontScale)
                                    radius: Theme.scaledSize(7, systemInfoModal.fontScale)
                                    color: parent.pressed ? Theme.surfaceVariant : Theme.surfaceContainerHighest
                                }

                            }

                            delegate: Column {
                                width: ListView.view.width
                                spacing: Theme.scaledSize(2, systemInfoModal.fontScale)

                                Item {
                                    width: parent.width
                                    height: Theme.scaledSize(24, systemInfoModal.fontScale)

                                    RowLayout {
                                        width: parent.width
                                        height: Theme.scaledSize(24, systemInfoModal.fontScale)
                                        spacing: systemInfoModal.colSpacing
                                        z: rowMouse.containsMouse ? 1 : 0

                                        Text {
                                            Layout.preferredWidth: systemInfoModal.colName
                                            Layout.alignment: Qt.AlignVCenter
                                            text: modelData.name
                                            font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                                            color: rowMouse.containsMouse ? Theme.primary : Theme.surfaceText
                                            elide: Text.ElideRight
                                        }

                                        Item {
                                            Layout.preferredWidth: systemInfoModal.colCpu
                                            Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                                            Layout.alignment: Qt.AlignVCenter

                                            Text {
                                                anchors.centerIn: parent
                                                anchors.horizontalCenterOffset: -38
                                                text: modelData.cpu.toFixed(1) + "%"
                                                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                                                color: rowMouse.containsMouse ? Theme.primary : Theme.surfaceVariantText
                                            }

                                        }

                                        Item {
                                            Layout.preferredWidth: systemInfoModal.colGap
                                            Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                                            Layout.alignment: Qt.AlignVCenter
                                        }

                                        Item {
                                            Layout.preferredWidth: systemInfoModal.colMem
                                            Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                                            Layout.alignment: Qt.AlignVCenter

                                            Text {
                                                anchors.centerIn: parent
                                                anchors.horizontalCenterOffset: -68
                                                text: systemInfoModal.formatMem(modelData.rss)
                                                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                                                color: rowMouse.containsMouse ? Theme.primary : Theme.surfaceVariantText
                                            }

                                        }

                                        Item {
                                            Layout.preferredWidth: systemInfoModal.colPid
                                            Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                                            Layout.alignment: Qt.AlignVCenter

                                            Text {
                                                anchors.centerIn: parent
                                                anchors.horizontalCenterOffset: -46
                                                text: modelData.pid
                                                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                                                color: rowMouse.containsMouse ? Theme.primary : Theme.surfaceText
                                            }

                                        }

                                        Item {
                                            Layout.fillWidth: true
                                            Layout.preferredHeight: Theme.scaledSize(24, systemInfoModal.fontScale)
                                            Layout.alignment: Qt.AlignVCenter

                                            Text {
                                                anchors.centerIn: parent
                                                anchors.horizontalCenterOffset: -18
                                                text: systemInfoModal.expandedPid === modelData.pid ? "\uE5CE" : "\uE5CF"
                                                font.family: materialSymbols.name
                                                font.pixelSize: Theme.scaledFont(14, systemInfoModal.fontScale)
                                                color: systemInfoModal.expandedPid === modelData.pid ? Theme.primary : Theme.surfaceVariantText
                                                horizontalAlignment: Text.AlignHCenter
                                                verticalAlignment: Text.AlignVCenter
                                            }

                                            MouseArea {
                                                anchors.fill: parent
                                                hoverEnabled: true
                                                cursorShape: Qt.PointingHandCursor
                                                onClicked: systemInfoModal.expandedPid = (systemInfoModal.expandedPid === modelData.pid) ? "" : modelData.pid
                                            }

                                        }

                                    }

                                    Rectangle {
                                        anchors.fill: parent
                                        radius: Theme.scaledSize(4, systemInfoModal.fontScale)
                                        color: rowMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent"
                                        z: -1
                                    }

                                    MouseArea {
                                        id: rowMouse

                                        anchors.fill: parent
                                        hoverEnabled: true
                                        cursorShape: Qt.PointingHandCursor
                                        onClicked: systemInfoModal.expandedPid = (systemInfoModal.expandedPid === modelData.pid) ? "" : modelData.pid
                                    }

                                }

                                Rectangle {
                                    width: parent.width
                                    visible: systemInfoModal.expandedPid === modelData.pid
                                    radius: Theme.scaledSize(8, systemInfoModal.fontScale)
                                    color: Theme.surfaceContainer
                                    border.color: Theme.surfaceContainerHighest
                                    border.width: Theme.scaledSize(1, systemInfoModal.fontScale)
                                    implicitHeight: expandedContent.implicitHeight + Theme.scaledSize(24, systemInfoModal.fontScale)

                                    RowLayout {
                                        id: expandedContent

                                        anchors.fill: parent
                                        anchors.margins: Theme.scaledSize(14, systemInfoModal.fontScale)
                                        spacing: Theme.scaledSize(16, systemInfoModal.fontScale)

                                        ColumnLayout {
                                            Layout.fillWidth: true
                                            Layout.fillHeight: true
                                            Layout.alignment: Qt.AlignVCenter
                                            spacing: Theme.scaledSize(6, systemInfoModal.fontScale)

                                            Text {
                                                Layout.fillWidth: true
                                                text: "<b>Full command:</b> " + (modelData.fullCommand || modelData.name)
                                                textFormat: Text.RichText
                                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                                                color: Theme.surfaceText
                                                wrapMode: Text.Wrap
                                            }

                                            Item {
                                                Layout.preferredHeight: Theme.scaledSize(2, systemInfoModal.fontScale)
                                            }

                                            RowLayout {
                                                Layout.fillWidth: true
                                                spacing: Theme.scaledSize(16, systemInfoModal.fontScale)

                                                Text {
                                                    text: "<b>PPID:</b> " + modelData.ppid
                                                    textFormat: Text.RichText
                                                    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                                                    color: Theme.surfaceVariantText
                                                }

                                                Text {
                                                    text: "<b>Memory:</b> " + (SystemInfoStats.totalMemKB > 0 ? ((modelData.rss / SystemInfoStats.totalMemKB) * 100).toFixed(1) + "%" : "--")
                                                    textFormat: Text.RichText
                                                    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                                                    color: Theme.surfaceVariantText
                                                }

                                            }

                                            Item {
                                                Layout.fillHeight: true
                                            }

                                        }

                                        RowLayout {
                                            Layout.fillHeight: true
                                            Layout.alignment: Qt.AlignVCenter
                                            spacing: Theme.scaledSize(6, systemInfoModal.fontScale)

                                            Rectangle {
                                                Layout.preferredWidth: Theme.scaledSize(48, systemInfoModal.fontScale)
                                                Layout.preferredHeight: Theme.scaledSize(22, systemInfoModal.fontScale)
                                                radius: Theme.scaledSize(11, systemInfoModal.fontScale)
                                                color: killMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                                                Text {
                                                    anchors.centerIn: parent
                                                    text: "Kill"
                                                    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                                                    font.bold: true
                                                    color: Theme.surfaceText
                                                }

                                                MouseArea {
                                                    id: killMouse

                                                    anchors.fill: parent
                                                    hoverEnabled: true
                                                    cursorShape: Qt.PointingHandCursor
                                                    onClicked: SystemInfoStats.killProcess(modelData.pid, 15)
                                                }

                                            }

                                            Rectangle {
                                                Layout.preferredWidth: Theme.scaledSize(76, systemInfoModal.fontScale)
                                                Layout.preferredHeight: Theme.scaledSize(22, systemInfoModal.fontScale)
                                                radius: Theme.scaledSize(11, systemInfoModal.fontScale)
                                                color: forceMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                                                Text {
                                                    anchors.centerIn: parent
                                                    text: "Force kill"
                                                    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemInfoModal.fontScale)
                                                    font.bold: true
                                                    color: Theme.error
                                                }

                                                MouseArea {
                                                    id: forceMouse

                                                    anchors.fill: parent
                                                    hoverEnabled: true
                                                    cursorShape: Qt.PointingHandCursor
                                                    onClicked: SystemInfoStats.killProcess(modelData.pid, 9)
                                                }

                                            }

                                        }

                                    }

                                }

                            }

                        }

                        RowLayout {
                            anchors.centerIn: parent
                            spacing: Theme.scaledSize(10, systemInfoModal.fontScale)
                            visible: SystemInfoStats.processesLoading

                            BusyIndicator {
                                Layout.preferredWidth: Theme.scaledSize(26, systemInfoModal.fontScale)
                                Layout.preferredHeight: Theme.scaledSize(26, systemInfoModal.fontScale)
                                running: SystemInfoStats.processesLoading
                                palette.text: Theme.primary
                            }

                            Text {
                                text: "Loading processes..."
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemInfoModal.fontScale)
                                color: Theme.surfaceVariantText
                            }

                        }

                    }

                }

            }

        }

    }

}
