import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.theme

PanelWindow {
    id: root

    property var systemInfoModal
    property var systemUpdatesModal
    property var nightLightModal
    property var networkModal
    property var weatherModal
    property var recorderModal
    property var clipboardModal
    property var targetScreen

    function openModal(modal) {
        root.visible = false;
        if (!modal)
            return ;

        if (modal === root.systemInfoModal || modal === root.systemUpdatesModal || modal === root.weatherModal || modal === root.recorderModal)
            modal.targetScreen = root.targetScreen;

        modal.visible = true;
        if (modal.open)
            modal.open();

    }

    visible: false
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    color: "transparent"

    MouseArea {
        anchors.fill: parent
        onClicked: root.visible = false
    }

    Rectangle {
        width: 330
        height: controls.implicitHeight + 32
        anchors.top: parent.top
        anchors.topMargin: 50
        anchors.right: parent.right
        anchors.rightMargin: 8
        radius: 12
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: controls

            anchors.centerIn: parent
            width: parent.width - 32
            spacing: 10

            Text {
                text: "Controls"
                font.pixelSize: Theme.fontLabelMedium
                font.bold: true
                color: Theme.surfaceText
            }

            GridLayout {
                columns: 2
                columnSpacing: 10
                rowSpacing: 10

                CompactControlButton {
                    icon: "󰒓"
                    text: "System"
                    onClicked: root.openModal(root.systemInfoModal)
                }

                CompactControlButton {
                    icon: "󰚰"
                    text: "Updates"
                    onClicked: root.openModal(root.systemUpdatesModal)
                }

                CompactControlButton {
                    icon: "󰖨"
                    text: "Night light"
                    onClicked: root.openModal(root.nightLightModal)
                }

                CompactControlButton {
                    icon: "󰈀"
                    text: "Network"
                    onClicked: root.openModal(root.networkModal)
                }

                CompactControlButton {
                    icon: "󰖐"
                    text: "Weather"
                    onClicked: root.openModal(root.weatherModal)
                }

                CompactControlButton {
                    icon: "󰻂"
                    text: "Recording"
                    onClicked: root.openModal(root.recorderModal)
                }

                CompactControlButton {
                    icon: "󰅍"
                    text: "Clipboard"
                    onClicked: root.openModal(root.clipboardModal)
                }

            }

        }

    }

}
