import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import qs.theme

PanelWindow {
    id: clockMenu

    property real fontScale: 1.0
    property date selectedDate: new Date()
    property date viewDate: new Date(selectedDate.getFullYear(), selectedDate.getMonth(), 1)
    readonly property var monthNames: ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    readonly property var weekDayNames: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
    property var monthCells: (() => {
        const cells = [];
        const first = viewDate.getDay();
        const days = new Date(viewDate.getFullYear(), viewDate.getMonth() + 1, 0).getDate();
        const prevDays = new Date(viewDate.getFullYear(), viewDate.getMonth(), 0).getDate();
        const total = Math.ceil((first + days) / 7) * 7;
        for (let i = 0; i < total; i++) {
            let day;
            let inView;
            let offset = 0;
            if (i < first) {
                day = prevDays - first + 1 + i;
                inView = false;
                offset = -1;
            } else if (i >= first + days) {
                day = i - first - days + 1;
                inView = false;
                offset = 1;
            } else {
                day = i - first + 1;
                inView = true;
            }
            cells.push({
                "day": day,
                "inView": inView,
                "offset": offset
            });
        }
        return cells;
    })()

    function prevMonth() {
        viewDate = new Date(viewDate.getFullYear(), viewDate.getMonth() - 1, 1);
    }

    function nextMonth() {
        viewDate = new Date(viewDate.getFullYear(), viewDate.getMonth() + 1, 1);
    }

    focusable: true
    visible: false
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    color: "transparent"
    onVisibleChanged: {
        if (visible) {
            viewDate = new Date(selectedDate.getFullYear(), selectedDate.getMonth(), 1);
            bg.forceActiveFocus();
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: clockMenu.visible = false
    }

    Rectangle {
        id: bg

        width: Theme.scaledSize(300, clockMenu.fontScale)
        height: content.implicitHeight + Theme.scaledSize(24, clockMenu.fontScale)
        anchors.top: parent.top
        anchors.topMargin: Theme.scaledSize(6, clockMenu.fontScale)
        anchors.right: parent.right
        anchors.rightMargin: Theme.scaledSize(42, clockMenu.fontScale)
        radius: Theme.scaledSize(16, clockMenu.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: Theme.scaledSize(1, clockMenu.fontScale)
        Keys.onPressed: (event) => {
            if (event.key === Qt.Key_Escape) {
                clockMenu.visible = false;
                event.accepted = true;
            } else if (event.key === Qt.Key_Left) {
                clockMenu.prevMonth();
                event.accepted = true;
            } else if (event.key === Qt.Key_Right) {
                clockMenu.nextMonth();
                event.accepted = true;
            } else if (event.key === Qt.Key_Up) {
                viewDate = new Date(viewDate.getFullYear() + 1, viewDate.getMonth(), 1);
                event.accepted = true;
            } else if (event.key === Qt.Key_Down) {
                viewDate = new Date(viewDate.getFullYear() - 1, viewDate.getMonth(), 1);
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: content

            anchors.fill: parent
            anchors.margins: Theme.scaledSize(12, clockMenu.fontScale)
            spacing: Theme.scaledSize(14, clockMenu.fontScale)

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, clockMenu.fontScale)

                Rectangle {
                    Layout.alignment: Qt.AlignVCenter
                    width: Theme.scaledSize(24, clockMenu.fontScale)
                    height: Theme.scaledSize(24, clockMenu.fontScale)
                    radius: Theme.scaledSize(7, clockMenu.fontScale)
                    color: prevMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        text: "‹"
                        anchors.centerIn: parent
                        font.pixelSize: Theme.scaledFont(16, clockMenu.fontScale)
                        color: Theme.surfaceText
                    }

                    MouseArea {
                        id: prevMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: clockMenu.prevMonth()
                    }

                }

                Item {
                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    RowLayout {
                        anchors.centerIn: parent
                        spacing: Theme.scaledSize(10, clockMenu.fontScale)

                        Text {
                            Layout.alignment: Qt.AlignVCenter
                            text: clockMenu.monthNames[viewDate.getMonth()] + " " + viewDate.getFullYear()
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelLarge, clockMenu.fontScale)
                            font.bold: true
                            color: Theme.surfaceText
                        }

                        RowLayout {
                            Layout.alignment: Qt.AlignVCenter
                            spacing: Theme.scaledSize(5, clockMenu.fontScale)

                            Rectangle {
                                Layout.alignment: Qt.AlignVCenter
                                width: Theme.scaledSize(16, clockMenu.fontScale)
                                height: Theme.scaledSize(16, clockMenu.fontScale)
                                radius: Theme.scaledSize(5, clockMenu.fontScale)
                                color: upYearMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                                Text {
                                    text: "▲"
                                    anchors.centerIn: parent
                                    font.pixelSize: Theme.scaledFont(8, clockMenu.fontScale)
                                    color: Theme.surfaceText
                                }

                                MouseArea {
                                    id: upYearMouse

                                    anchors.fill: parent
                                    hoverEnabled: true
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: viewDate = new Date(viewDate.getFullYear() + 1, viewDate.getMonth(), 1)
                                }

                            }

                            Rectangle {
                                Layout.alignment: Qt.AlignVCenter
                                width: Theme.scaledSize(16, clockMenu.fontScale)
                                height: Theme.scaledSize(16, clockMenu.fontScale)
                                radius: Theme.scaledSize(5, clockMenu.fontScale)
                                color: downYearMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                                Text {
                                    text: "▼"
                                    anchors.centerIn: parent
                                    font.pixelSize: Theme.scaledFont(8, clockMenu.fontScale)
                                    color: Theme.surfaceText
                                }

                                MouseArea {
                                    id: downYearMouse

                                    anchors.fill: parent
                                    hoverEnabled: true
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: viewDate = new Date(viewDate.getFullYear() - 1, viewDate.getMonth(), 1)
                                }

                            }

                        }

                    }

                }

                Rectangle {
                    Layout.alignment: Qt.AlignVCenter
                    width: Theme.scaledSize(24, clockMenu.fontScale)
                    height: Theme.scaledSize(24, clockMenu.fontScale)
                    radius: Theme.scaledSize(7, clockMenu.fontScale)
                    color: nextMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        text: "›"
                        anchors.centerIn: parent
                        font.pixelSize: Theme.scaledFont(16, clockMenu.fontScale)
                        color: Theme.surfaceText
                    }

                    MouseArea {
                        id: nextMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: clockMenu.nextMonth()
                    }

                }

            }

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(4, clockMenu.fontScale)

                Repeater {
                    model: clockMenu.weekDayNames

                    Text {
                        Layout.preferredWidth: Theme.scaledSize(36, clockMenu.fontScale)
                        horizontalAlignment: Text.AlignHCenter
                        text: modelData
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, clockMenu.fontScale)
                        color: Theme.surfaceVariantText
                    }

                }

            }

            Grid {
                id: calendarGrid

                Layout.alignment: Qt.AlignHCenter
                columns: 7
                columnSpacing: Theme.scaledSize(4, clockMenu.fontScale)
                rowSpacing: Theme.scaledSize(4, clockMenu.fontScale)

                Repeater {
                    model: clockMenu.monthCells

                    delegate: Rectangle {
                        id: cell

                        readonly property bool isInMonth: modelData.inView
                        readonly property bool isToday: isInMonth && modelData.day === new Date().getDate() && viewDate.getMonth() === new Date().getMonth() && viewDate.getFullYear() === new Date().getFullYear()
                        readonly property bool isSelected: isInMonth && modelData.day === clockMenu.selectedDate.getDate() && viewDate.getMonth() === clockMenu.selectedDate.getMonth() && viewDate.getFullYear() === clockMenu.selectedDate.getFullYear()

                        width: Theme.scaledSize(36, clockMenu.fontScale)
                        height: Theme.scaledSize(30, clockMenu.fontScale)
                        radius: Theme.scaledSize(7, clockMenu.fontScale)
                        color: isSelected ? Theme.primary : (cellMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent")
                        border.color: isToday ? Theme.primary : "transparent"
                        border.width: isToday ? Theme.scaledSize(1, clockMenu.fontScale) : Theme.scaledSize(0, clockMenu.fontScale)

                        Text {
                            anchors.centerIn: parent
                            text: modelData.day
                            font.pixelSize: Theme.scaledFont(isInMonth ? Theme.fontLabelMedium : Theme.fontLabelSmall, clockMenu.fontScale)
                            font.bold: isSelected || isToday
                            color: isSelected ? Theme.primaryText : (isInMonth ? Theme.surfaceText : Theme.surfaceVariantText)
                        }

                        MouseArea {
                            id: cellMouse

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: {
                                clockMenu.selectedDate = new Date(viewDate.getFullYear(), viewDate.getMonth() + modelData.offset, modelData.day);
                                clockMenu.visible = false;
                            }
                        }

                    }

                }

            }

        }

    }

}
