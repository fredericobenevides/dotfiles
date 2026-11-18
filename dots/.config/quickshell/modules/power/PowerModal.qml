import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs.theme

PanelWindow {
    id: powerMenu

    property real fontScale: 1.0
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

        width: Theme.scaledSize(300, powerMenu.fontScale)
        height: headerRow.height + Theme.scaledSize(14, powerMenu.fontScale) + optionsList.height + Theme.scaledSize(36, powerMenu.fontScale)
        anchors.top: parent.top
        anchors.topMargin: Theme.scaledSize(6, powerMenu.fontScale)
        anchors.right: parent.right
        anchors.rightMargin: Theme.scaledSize(6, powerMenu.fontScale)
        radius: Theme.scaledSize(16, powerMenu.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: Theme.scaledSize(1, powerMenu.fontScale)

        MouseArea {
            anchors.fill: parent
        }

        RowLayout {
            id: headerRow

            anchors.top: parent.top
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.topMargin: Theme.scaledSize(14, powerMenu.fontScale)
            spacing: Theme.scaledSize(8, powerMenu.fontScale)

            Text {
                text: "\uF011"
                font.pixelSize: Theme.scaledFont(20, powerMenu.fontScale)
                color: Theme.surfaceVariantText
            }

            Text {
                text: "Power"
                font.pixelSize: Theme.scaledFont(20, powerMenu.fontScale)
                font.bold: true
                color: Theme.surfaceText
            }

        }

        ListView {
            id: optionsList

            anchors.horizontalCenter: parent.horizontalCenter
            anchors.top: headerRow.bottom
            anchors.topMargin: Theme.scaledSize(14, powerMenu.fontScale)
            width: bg.width - Theme.scaledSize(16, powerMenu.fontScale)
            height: contentHeight
            model: optionsModel
            spacing: Theme.scaledSize(10, powerMenu.fontScale)
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
                height: Theme.scaledSize(44, powerMenu.fontScale)
                radius: Theme.scaledSize(7, powerMenu.fontScale)
                color: (optionMouse.containsMouse || isCurrent) ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                border.color: isCurrent ? Theme.primary : "transparent"
                border.width: isCurrent ? Theme.scaledSize(1, powerMenu.fontScale) : Theme.scaledSize(0, powerMenu.fontScale)

                RowLayout {
                    anchors.fill: parent
                    anchors.leftMargin: Theme.scaledSize(14, powerMenu.fontScale)
                    anchors.rightMargin: Theme.scaledSize(14, powerMenu.fontScale)
                    spacing: Theme.scaledSize(12, powerMenu.fontScale)

                    Text {
                        text: model.icon
                        font.pixelSize: Theme.scaledFont(18, powerMenu.fontScale)
                        color: isCurrent ? Theme.primary : Theme.surfaceVariantText
                        Layout.alignment: Qt.AlignVCenter
                    }

                    Text {
                        text: model.label
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelLarge, powerMenu.fontScale)
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
