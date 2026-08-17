import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs.theme

PanelWindow {
    id: powerMenu

    function execute(action) {
        switch (action) {
        case "lock":
            actionProcess.command = ["sh", "-c", "pidof hyprlock || hyprlock"];
            break;
        case "logout":
            actionProcess.command = ["sh", "-c", "loginctl kill-session \"$XDG_SESSION_ID\""];
            break;
        case "reboot":
            actionProcess.command = ["systemctl", "reboot"];
            break;
        case "poweroff":
            actionProcess.command = ["systemctl", "poweroff"];
            break;
        case "restart":
            powerMenu.visible = false;
            Quickshell.reload(true);
            return ;
        }
        actionProcess.running = true;
        powerMenu.visible = false;
    }

    focusable: true

    visible: false
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    color: "transparent"
    onVisibleChanged: {
        if (visible)
            optionsList.forceActiveFocus();

    }

    ListModel {
        id: optionsModel

        ListElement {
            label: "Lock"
            icon: "\uF023"
            action: "lock"
        }

        ListElement {
            label: "Logout"
            icon: "\uF08B"
            action: "logout"
        }

        ListElement {
            label: "Reboot"
            icon: "\uF01E"
            action: "reboot"
        }

        ListElement {
            label: "Power off"
            icon: "\uF011"
            action: "poweroff"
        }

        ListElement {
            label: "Restart Quickshell"
            icon: "\uF021"
            action: "restart"
        }

    }

    Process {
        id: actionProcess
    }

    MouseArea {
        anchors.fill: parent
        onClicked: powerMenu.visible = false
    }

    Rectangle {
        id: bg

        width: 300
        height: headerRow.height + 14 + optionsList.height + 36
        anchors.top: parent.top
        anchors.topMargin: 6
        anchors.right: parent.right
        anchors.rightMargin: 6
        radius: 16
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: 1

        MouseArea {
            anchors.fill: parent
        }

        RowLayout {
            id: headerRow

            anchors.top: parent.top
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.topMargin: 14
            spacing: 8

            Text {
                text: "\uF011"
                font.pixelSize: 20
                color: Theme.surfaceVariantText
            }

            Text {
                text: "Power"
                font.pixelSize: 20
                font.bold: true
                color: Theme.surfaceText
            }

        }

        ListView {
            id: optionsList

            anchors.horizontalCenter: parent.horizontalCenter
            anchors.top: headerRow.bottom
            anchors.topMargin: 14
            width: bg.width - 16
            height: contentHeight
            model: optionsModel
            spacing: 10
            keyNavigationEnabled: true
            highlightFollowsCurrentItem: true
            focus: true
            Keys.onPressed: (event) => {
                if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                    const option = optionsModel.get(currentIndex);
                    if (option)
                        execute(option.action);

                    event.accepted = true;
                } else if (event.key === Qt.Key_Escape) {
                    powerMenu.visible = false;
                    event.accepted = true;
                }
            }

            delegate: Rectangle {
                id: optionItem

                readonly property bool isCurrent: ListView.isCurrentItem && optionsList.activeFocus

                width: optionsList.width
                height: 44
                radius: 7
                color: (optionMouse.containsMouse || isCurrent) ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                border.color: isCurrent ? Theme.primary : "transparent"
                border.width: isCurrent ? 1 : 0

                RowLayout {
                    anchors.fill: parent
                    anchors.leftMargin: 14
                    anchors.rightMargin: 14
                    spacing: 12

                    Text {
                        text: model.icon
                        font.pixelSize: 18
                        color: isCurrent ? Theme.primary : Theme.surfaceVariantText
                        Layout.alignment: Qt.AlignVCenter
                    }

                    Text {
                        text: model.label
                        font.pixelSize: Theme.fontLabelLarge
                        color: Theme.surfaceText
                        Layout.alignment: Qt.AlignVCenter
                    }

                }

                MouseArea {
                    id: optionMouse

                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: {
                        optionsList.currentIndex = index;
                        execute(model.action);
                    }
                }

            }

        }

    }

}
