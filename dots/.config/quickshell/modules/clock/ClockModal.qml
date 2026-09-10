import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import qs.theme

PanelWindow {
    id: clockMenu

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

        width: 300
        height: content.implicitHeight + 24
        anchors.top: parent.top
        anchors.topMargin: 6
        anchors.right: parent.right
        anchors.rightMargin: 42
        radius: 16
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: 1
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
            anchors.margins: 12
            spacing: 14

            RowLayout {
                Layout.fillWidth: true
                spacing: 8

                Rectangle {
                    Layout.alignment: Qt.AlignVCenter
                    width: 24
                    height: 24
                    radius: 7
                    color: prevMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        text: "‹"
                        anchors.centerIn: parent
                        font.pixelSize: 16
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
                        spacing: 10

                        Text {
                            Layout.alignment: Qt.AlignVCenter
                            text: clockMenu.monthNames[viewDate.getMonth()] + " " + viewDate.getFullYear()
                            font.pixelSize: Theme.fontLabelLarge
                            font.bold: true
                            color: Theme.surfaceText
                        }

                        RowLayout {
                            Layout.alignment: Qt.AlignVCenter
                            spacing: 5

                            Rectangle {
                                Layout.alignment: Qt.AlignVCenter
                                width: 16
                                height: 16
                                radius: 5
                                color: upYearMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                                Text {
                                    text: "▲"
                                    anchors.centerIn: parent
                                    font.pixelSize: 8
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
                                width: 16
                                height: 16
                                radius: 5
                                color: downYearMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                                Text {
                                    text: "▼"
                                    anchors.centerIn: parent
                                    font.pixelSize: 8
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
                    width: 24
                    height: 24
                    radius: 7
                    color: nextMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        text: "›"
                        anchors.centerIn: parent
                        font.pixelSize: 16
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
                spacing: 4

                Repeater {
                    model: clockMenu.weekDayNames

                    Text {
                        Layout.preferredWidth: 36
                        horizontalAlignment: Text.AlignHCenter
                        text: modelData
                        font.pixelSize: Theme.fontLabelSmall
                        color: Theme.surfaceVariantText
                    }

                }

            }

            Grid {
                id: calendarGrid

                Layout.alignment: Qt.AlignHCenter
                columns: 7
                columnSpacing: 4
                rowSpacing: 4

                Repeater {
                    model: clockMenu.monthCells

                    delegate: Rectangle {
                        id: cell

                        readonly property bool isInMonth: modelData.inView
                        readonly property bool isToday: isInMonth && modelData.day === new Date().getDate() && viewDate.getMonth() === new Date().getMonth() && viewDate.getFullYear() === new Date().getFullYear()
                        readonly property bool isSelected: isInMonth && modelData.day === clockMenu.selectedDate.getDate() && viewDate.getMonth() === clockMenu.selectedDate.getMonth() && viewDate.getFullYear() === clockMenu.selectedDate.getFullYear()

                        width: 36
                        height: 30
                        radius: 7
                        color: isSelected ? Theme.primary : (cellMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent")
                        border.color: isToday ? Theme.primary : "transparent"
                        border.width: isToday ? 1 : 0

                        Text {
                            anchors.centerIn: parent
                            text: modelData.day
                            font.pixelSize: isInMonth ? Theme.fontLabelMedium : Theme.fontLabelSmall
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
