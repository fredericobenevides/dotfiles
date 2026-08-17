import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import qs.modules.system
import qs.theme

PanelWindow {
    id: systemModal

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
        const procs = SystemStats.processes;
        let filtered = procs;
        if (processFilter === "user")
            filtered = procs.filter((p) => {
            return p.user === SystemStats.username;
        });
        else if (processFilter === "system")
            filtered = procs.filter((p) => {
            return p.user !== SystemStats.username;
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
        SystemStats.processesActive = visible;
    }
    focusable: true
    visible: false
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    color: "transparent"

    Timer {
        interval: 500
        repeat: true
        running: SystemStats.cpuTemp >= SystemStats.hotTempThreshold
        onTriggered: systemModal._tempFlashOn = !systemModal._tempFlashOn
        onRunningChanged: {
            if (!running)
                systemModal._tempFlashOn = false;

        }
    }

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/quickshell/dms/assets/fonts/material-design-icons/variablefont/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    MouseArea {
        anchors.fill: parent
        onClicked: systemModal.closeModal()
    }

    Rectangle {
        id: bg

        width: 600
        height: content.implicitHeight + 32
        anchors.top: parent.top
        anchors.topMargin: 6
        anchors.horizontalCenter: parent.horizontalCenter
        radius: 16
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: 1

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: content

            anchors.fill: parent
            anchors.margins: 16
            spacing: 0

            RowLayout {
                Layout.fillWidth: true
                spacing: 8

                Text {
                    text: "\uE8F8"
                    font.family: materialSymbols.name
                    font.pixelSize: 18
                    color: Theme.primary
                }

                Text {
                    Layout.fillWidth: true
                    text: "System"
                    font.pixelSize: 18
                    font.bold: true
                    color: Theme.surfaceText
                }

            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 12

                Repeater {
                    model: [{
                        "icon": "\uE322",
                        "label": "CPU",
                        "display": Math.round(SystemStats.cpuPercent) + "%",
                        "percent": SystemStats.cpuPercent,
                        "hot": false
                    }, {
                        "icon": "\uE30D",
                        "label": "Memory",
                        "display": Math.round(SystemStats.memPercent) + "%",
                        "percent": SystemStats.memPercent,
                        "hot": false
                    }, {
                        "icon": "\uF80E",
                        "label": "HD",
                        "display": Math.round(SystemStats.diskPercent) + "%",
                        "percent": SystemStats.diskPercent,
                        "hot": false
                    }, {
                        "icon": "\uE1FF",
                        "label": "Temperature",
                        "display": Math.round(SystemStats.cpuTemp) + "°C",
                        "percent": Math.min(100, SystemStats.cpuTemp),
                        "hot": SystemStats.cpuTemp >= SystemStats.hotTempThreshold
                    }]

                    delegate: Item {
                        Layout.fillWidth: true
                        Layout.preferredHeight: 148

                        Item {
                            id: ringWrap

                            width: 108
                            height: 108
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
                                    ctx.strokeStyle = modelData.hot ? (systemModal._tempFlashOn ? "#ff8080" : Theme.error) : Theme.primary;
                                    ctx.beginPath();
                                    ctx.arc(cx, cy, r, -Math.PI / 2, -Math.PI / 2 + Math.PI * 2 * value);
                                    ctx.stroke();
                                }
                                onGaugeValueChanged: requestPaint()
                            }

                            ColumnLayout {
                                anchors.centerIn: parent
                                spacing: 0

                                Text {
                                    Layout.alignment: Qt.AlignHCenter
                                    text: modelData.display
                                    font.pixelSize: 18
                                    font.bold: true
                                    color: modelData.hot ? (systemModal._tempFlashOn ? "#ffffff" : Theme.error) : Theme.surfaceText
                                }

                                Text {
                                    Layout.alignment: Qt.AlignHCenter
                                    text: modelData.label
                                    font.pixelSize: Theme.fontLabelSmall
                                    color: Theme.surfaceVariantText
                                }

                            }

                        }

                        Row {
                            anchors.horizontalCenter: parent.horizontalCenter
                            anchors.top: ringWrap.bottom
                            anchors.topMargin: 4
                            spacing: 3

                            Text {
                                text: modelData.icon
                                font.family: materialSymbols.name
                                font.pixelSize: 18
                                color: modelData.hot ? Theme.error : Theme.primary
                                anchors.verticalCenter: parent.verticalCenter
                            }

                            Text {
                                text: modelData.label
                                font.pixelSize: Theme.fontLabelSmall
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
                spacing: 2

                Row {
                    spacing: 4
                    Layout.fillWidth: true

                    Text {
                        text: "Info:"
                        font.pixelSize: Theme.fontLabelMedium
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: SystemStats.hostname
                        font.pixelSize: Theme.fontLabelMedium
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

            }

            Rectangle {
                Layout.fillWidth: true
                Layout.topMargin: 8
                height: 1
                color: Theme.outlineVariant
            }

            ColumnLayout {
                Layout.fillWidth: true
                Layout.topMargin: 8
                spacing: 2

                Row {
                    spacing: 4
                    Layout.fillWidth: true

                    Text {
                        text: "Uptime:"
                        font.pixelSize: Theme.fontLabelSmall
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: SystemStats.uptimeText
                        font.pixelSize: Theme.fontLabelSmall
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

                Row {
                    spacing: 4
                    Layout.fillWidth: true

                    Text {
                        text: "CPU Frequency:"
                        font.pixelSize: Theme.fontLabelSmall
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: SystemStats.cpuFreqMHz > 0 ? (SystemStats.cpuFreqMHz / 1000).toFixed(2) + " GHz" : "--"
                        font.pixelSize: Theme.fontLabelSmall
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

                Row {
                    spacing: 4
                    Layout.fillWidth: true

                    Text {
                        text: "RAM Usage:"
                        font.pixelSize: Theme.fontLabelSmall
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: systemModal.formatMem(SystemStats.usedMemKB) + " / " + systemModal.formatMem(SystemStats.totalMemKB)
                        font.pixelSize: Theme.fontLabelSmall
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

                Row {
                    spacing: 4
                    Layout.fillWidth: true

                    Text {
                        text: "Swap Usage:"
                        font.pixelSize: Theme.fontLabelSmall
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: systemModal.formatMem(SystemStats.usedSwapKB) + " / " + systemModal.formatMem(SystemStats.totalSwapKB)
                        font.pixelSize: Theme.fontLabelSmall
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

                Row {
                    spacing: 4
                    Layout.fillWidth: true

                    Text {
                        text: "Processes:"
                        font.pixelSize: Theme.fontLabelSmall
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: SystemStats.totalProcessCount
                        font.pixelSize: Theme.fontLabelSmall
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

            }

            Rectangle {
                Layout.fillWidth: true
                Layout.topMargin: 8
                height: 1
                color: Theme.outlineVariant
            }

            ColumnLayout {
                Layout.fillWidth: true
                Layout.topMargin: 8
                spacing: 2

                Row {
                    spacing: 4
                    Layout.fillWidth: true

                    Text {
                        text: "File systems:"
                        font.pixelSize: Theme.fontLabelMedium
                        color: Theme.surfaceVariantText
                    }

                }

                Row {
                    spacing: 4
                    Layout.fillWidth: true

                    Text {
                        text: "/ :"
                        font.pixelSize: Theme.fontLabelSmall
                        color: Theme.surfaceVariantText
                    }

                    Text {
                        text: systemModal.formatMem(SystemStats.diskUsedKB) + " / " + systemModal.formatMem(SystemStats.diskTotalKB)
                        font.pixelSize: Theme.fontLabelSmall
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

            }

            Rectangle {
                Layout.fillWidth: true
                Layout.topMargin: 8
                height: 1
                color: Theme.outlineVariant
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.topMargin: 15
                spacing: 8

                Rectangle {
                    Layout.preferredWidth: 48
                    Layout.preferredHeight: 24
                    radius: 12
                    color: systemModal.processFilter === "all" ? Theme.primary : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "ALL"
                        font.pixelSize: Theme.fontLabelMedium
                        font.bold: true
                        color: systemModal.processFilter === "all" ? Theme.primaryText : Theme.surfaceVariantText
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: systemModal.processFilter = "all"
                    }

                }

                Rectangle {
                    Layout.preferredWidth: 52
                    Layout.preferredHeight: 24
                    radius: 12
                    color: systemModal.processFilter === "user" ? Theme.primary : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "USER"
                        font.pixelSize: Theme.fontLabelMedium
                        font.bold: true
                        color: systemModal.processFilter === "user" ? Theme.primaryText : Theme.surfaceVariantText
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: systemModal.processFilter = "user"
                    }

                }

                Rectangle {
                    Layout.preferredWidth: 62
                    Layout.preferredHeight: 24
                    radius: 12
                    color: systemModal.processFilter === "system" ? Theme.primary : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "System"
                        font.pixelSize: Theme.fontLabelMedium
                        font.bold: true
                        color: systemModal.processFilter === "system" ? Theme.primaryText : Theme.surfaceVariantText
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: systemModal.processFilter = "system"
                    }

                }

                Item {
                    Layout.fillWidth: true
                }

                Rectangle {
                    Layout.preferredWidth: 220
                    Layout.preferredHeight: 30
                    radius: 15
                    color: Theme.surfaceContainerHigh
                    border.color: searchField.activeFocus ? Theme.primary : "transparent"
                    border.width: 1

                    RowLayout {
                        anchors.fill: parent
                        anchors.leftMargin: 8
                        anchors.rightMargin: 8
                        spacing: 4

                        Text {
                            text: "\uE8B6"
                            font.family: materialSymbols.name
                            font.pixelSize: 12
                            color: Theme.surfaceVariantText
                        }

                        TextInput {
                            id: searchField

                            Layout.fillWidth: true
                            clip: true
                            color: Theme.surfaceText
                            font.pixelSize: Theme.fontLabelLarge
                            selectByMouse: true
                            onTextChanged: systemModal.searchText = text
                            Keys.onPressed: (event) => {
                                if (event.key === Qt.Key_Escape) {
                                    searchField.text = "";
                                    searchField.focus = false;
                                    event.accepted = true;
                                }
                            }
                        }

                    }

                }

            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 320
                Layout.topMargin: 12
                radius: 14
                color: Theme.surfaceContainerHigh
                border.color: Theme.surfaceContainerHighest
                border.width: 1

                ColumnLayout {
                    anchors.fill: parent
                    anchors.margins: 12
                    spacing: 8

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: systemModal.colSpacing

                        Item {
                            Layout.preferredWidth: systemModal.colName
                            Layout.preferredHeight: 24
                            Layout.alignment: Qt.AlignVCenter

                            Text {
                                anchors.fill: parent
                                text: "Name " + (systemModal.sortField === "name" ? (systemModal.sortAscending ? "\u25B4" : "\u25BE") : "")
                                font.pixelSize: Theme.fontLabelMedium
                                font.bold: true
                                color: systemModal.sortField === "name" ? Theme.primary : Theme.surfaceVariantText
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: systemModal.setSort("name")
                            }

                        }

                        Item {
                            Layout.preferredWidth: systemModal.colCpu
                            Layout.preferredHeight: 24
                            Layout.alignment: Qt.AlignVCenter

                            Text {
                                anchors.centerIn: parent
                                anchors.horizontalCenterOffset: -18
                                text: "CPU " + (systemModal.sortField === "cpu" ? (systemModal.sortAscending ? "\u25B4" : "\u25BE") : "")
                                font.pixelSize: Theme.fontLabelMedium
                                font.bold: true
                                color: systemModal.sortField === "cpu" ? Theme.primary : Theme.surfaceVariantText
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: systemModal.setSort("cpu")
                            }

                        }

                        Item {
                            Layout.preferredWidth: systemModal.colGap
                            Layout.preferredHeight: 24
                        }

                        Item {
                            Layout.preferredWidth: systemModal.colMem
                            Layout.preferredHeight: 24
                            Layout.alignment: Qt.AlignVCenter

                            Text {
                                anchors.centerIn: parent
                                anchors.horizontalCenterOffset: -38
                                text: "Memory " + (systemModal.sortField === "mem" ? (systemModal.sortAscending ? "\u25B4" : "\u25BE") : "")
                                font.pixelSize: Theme.fontLabelMedium
                                font.bold: true
                                color: systemModal.sortField === "mem" ? Theme.primary : Theme.surfaceVariantText
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: systemModal.setSort("mem")
                            }

                        }

                        Item {
                            Layout.preferredWidth: systemModal.colPid
                            Layout.preferredHeight: 24
                            Layout.alignment: Qt.AlignVCenter

                            Text {
                                anchors.centerIn: parent
                                anchors.horizontalCenterOffset: -16
                                text: "PID " + (systemModal.sortField === "pid" ? (systemModal.sortAscending ? "\u25B4" : "\u25BE") : "")
                                font.pixelSize: Theme.fontLabelMedium
                                font.bold: true
                                color: systemModal.sortField === "pid" ? Theme.primary : Theme.surfaceVariantText
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: systemModal.setSort("pid")
                            }

                        }

                        Item {
                            Layout.fillWidth: true
                            Layout.preferredHeight: 24
                        }

                    }

                    Rectangle {
                        Layout.fillWidth: true
                        Layout.preferredHeight: 1
                        color: Theme.surfaceContainerHighest
                    }

                    Item {
                        Layout.fillWidth: true
                        Layout.fillHeight: true

                        ListView {
                            anchors.fill: parent
                            clip: true
                            visible: !SystemStats.processesLoading
                            model: systemModal.visibleProcesses
                            spacing: 2

                            ScrollBar.vertical: ScrollBar {
                                policy: ScrollBar.AsNeeded
                                width: 14
                                minimumSize: 0.25

                                contentItem: Rectangle {
                                    implicitWidth: 14
                                    radius: 7
                                    color: parent.pressed ? Theme.surfaceVariant : Theme.surfaceContainerHighest
                                }

                            }

                            delegate: Column {
                                width: ListView.view.width
                                spacing: 2

                                RowLayout {
                                    width: parent.width
                                    height: 24
                                    spacing: systemModal.colSpacing

                                    Text {
                                        Layout.preferredWidth: systemModal.colName
                                        Layout.alignment: Qt.AlignVCenter
                                        text: modelData.name
                                        font.pixelSize: Theme.fontLabelMedium
                                        color: Theme.surfaceText
                                        elide: Text.ElideRight
                                    }

                                    Item {
                                        Layout.preferredWidth: systemModal.colCpu
                                        Layout.preferredHeight: 24
                                        Layout.alignment: Qt.AlignVCenter

                                        Text {
                                            anchors.centerIn: parent
                                            anchors.horizontalCenterOffset: -38
                                            text: modelData.cpu.toFixed(1) + "%"
                                            font.pixelSize: Theme.fontLabelMedium
                                            color: Theme.surfaceVariantText
                                        }

                                    }

                                    Item {
                                        Layout.preferredWidth: systemModal.colGap
                                        Layout.preferredHeight: 24
                                        Layout.alignment: Qt.AlignVCenter
                                    }

                                    Item {
                                        Layout.preferredWidth: systemModal.colMem
                                        Layout.preferredHeight: 24
                                        Layout.alignment: Qt.AlignVCenter

                                        Text {
                                            anchors.centerIn: parent
                                            anchors.horizontalCenterOffset: -68
                                            text: systemModal.formatMem(modelData.rss)
                                            font.pixelSize: Theme.fontLabelMedium
                                            color: Theme.surfaceVariantText
                                        }

                                    }

                                    Item {
                                        Layout.preferredWidth: systemModal.colPid
                                        Layout.preferredHeight: 24
                                        Layout.alignment: Qt.AlignVCenter

                                        Text {
                                            anchors.centerIn: parent
                                            anchors.horizontalCenterOffset: -46
                                            text: modelData.pid
                                            font.pixelSize: Theme.fontLabelMedium
                                            color: Theme.surfaceText
                                        }

                                    }

                                    Item {
                                        Layout.fillWidth: true
                                        Layout.preferredHeight: 24
                                        Layout.alignment: Qt.AlignVCenter

                                        Text {
                                            anchors.centerIn: parent
                                            anchors.horizontalCenterOffset: -18
                                            text: systemModal.expandedPid === modelData.pid ? "\uE5CE" : "\uE5CF"
                                            font.family: materialSymbols.name
                                            font.pixelSize: 14
                                            color: systemModal.expandedPid === modelData.pid ? Theme.primary : Theme.surfaceVariantText
                                            horizontalAlignment: Text.AlignHCenter
                                            verticalAlignment: Text.AlignVCenter
                                        }

                                        MouseArea {
                                            anchors.fill: parent
                                            hoverEnabled: true
                                            cursorShape: Qt.PointingHandCursor
                                            onClicked: systemModal.expandedPid = (systemModal.expandedPid === modelData.pid) ? "" : modelData.pid
                                        }

                                    }

                                }

                                Rectangle {
                                    width: parent.width
                                    visible: systemModal.expandedPid === modelData.pid
                                    radius: 8
                                    color: Theme.surfaceContainer
                                    border.color: Theme.surfaceContainerHighest
                                    border.width: 1
                                    implicitHeight: expandedContent.implicitHeight + 24

                                    RowLayout {
                                        id: expandedContent

                                        anchors.fill: parent
                                        anchors.margins: 14
                                        spacing: 16

                                        ColumnLayout {
                                            Layout.fillWidth: true
                                            Layout.fillHeight: true
                                            Layout.alignment: Qt.AlignVCenter
                                            spacing: 6

                                            Text {
                                                Layout.fillWidth: true
                                                text: "<b>Full command:</b> " + (modelData.fullCommand || modelData.name)
                                                textFormat: Text.RichText
                                                font.pixelSize: Theme.fontLabelSmall
                                                color: Theme.surfaceText
                                                wrapMode: Text.Wrap
                                            }

                                            Item {
                                                Layout.preferredHeight: 2
                                            }

                                            RowLayout {
                                                Layout.fillWidth: true
                                                spacing: 16

                                                Text {
                                                    text: "<b>PPID:</b> " + modelData.ppid
                                                    textFormat: Text.RichText
                                                    font.pixelSize: Theme.fontLabelSmall
                                                    color: Theme.surfaceVariantText
                                                }

                                                Text {
                                                    text: "<b>Memory:</b> " + (SystemStats.totalMemKB > 0 ? ((modelData.rss / SystemStats.totalMemKB) * 100).toFixed(1) + "%" : "--")
                                                    textFormat: Text.RichText
                                                    font.pixelSize: Theme.fontLabelSmall
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
                                            spacing: 6

                                            Rectangle {
                                                Layout.preferredWidth: 48
                                                Layout.preferredHeight: 22
                                                radius: 11
                                                color: killMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                                                Text {
                                                    anchors.centerIn: parent
                                                    text: "Kill"
                                                    font.pixelSize: Theme.fontLabelSmall
                                                    font.bold: true
                                                    color: Theme.surfaceText
                                                }

                                                MouseArea {
                                                    id: killMouse

                                                    anchors.fill: parent
                                                    hoverEnabled: true
                                                    cursorShape: Qt.PointingHandCursor
                                                    onClicked: SystemStats.killProcess(modelData.pid, 15)
                                                }

                                            }

                                            Rectangle {
                                                Layout.preferredWidth: 76
                                                Layout.preferredHeight: 22
                                                radius: 11
                                                color: forceMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                                                Text {
                                                    anchors.centerIn: parent
                                                    text: "Force kill"
                                                    font.pixelSize: Theme.fontLabelSmall
                                                    font.bold: true
                                                    color: Theme.error
                                                }

                                                MouseArea {
                                                    id: forceMouse

                                                    anchors.fill: parent
                                                    hoverEnabled: true
                                                    cursorShape: Qt.PointingHandCursor
                                                    onClicked: SystemStats.killProcess(modelData.pid, 9)
                                                }

                                            }

                                        }

                                    }

                                }

                            }

                        }

                        RowLayout {
                            anchors.centerIn: parent
                            spacing: 10
                            visible: SystemStats.processesLoading

                            BusyIndicator {
                                Layout.preferredWidth: 26
                                Layout.preferredHeight: 26
                                running: SystemStats.processesLoading
                                palette.text: Theme.primary
                            }

                            Text {
                                text: "Loading processes..."
                                font.pixelSize: Theme.fontLabelMedium
                                color: Theme.surfaceVariantText
                            }

                        }

                    }

                }

            }

        }

    }

}
