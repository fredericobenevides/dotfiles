import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.theme

PanelWindow {
    id: root

    property real fontScale: 1.0
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

        modal.fontScale = root.fontScale;
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
        width: Theme.scaledSize(330, root.fontScale)
        height: controls.implicitHeight + Theme.scaledSize(32, root.fontScale)
        anchors.top: parent.top
        anchors.topMargin: Theme.scaledSize(50, root.fontScale)
        anchors.right: parent.right
        anchors.rightMargin: Theme.scaledSize(8, root.fontScale)
        radius: Theme.scaledSize(12, root.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: controls

            anchors.centerIn: parent
            width: parent.width - Theme.scaledSize(32, root.fontScale)
            spacing: Theme.scaledSize(10, root.fontScale)

            Text {
                text: "Controls"
                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, root.fontScale)
                font.bold: true
                color: Theme.surfaceText
            }

            GridLayout {
                columns: 2
                columnSpacing: Theme.scaledSize(10, root.fontScale)
                rowSpacing: Theme.scaledSize(10, root.fontScale)

                CompactControlButton {
                    fontScale: root.fontScale
                    icon: "󰒓"
                    text: "System"
                    onClicked: root.openModal(root.systemInfoModal)
                }

                CompactControlButton {
                    fontScale: root.fontScale
                    icon: "󰚰"
                    text: "Updates"
                    onClicked: root.openModal(root.systemUpdatesModal)
                }

                CompactControlButton {
                    fontScale: root.fontScale
                    icon: "󰖨"
                    text: "Night light"
                    onClicked: root.openModal(root.nightLightModal)
                }

                CompactControlButton {
                    fontScale: root.fontScale
                    icon: "󰈀"
                    text: "Network"
                    onClicked: root.openModal(root.networkModal)
                }

                CompactControlButton {
                    fontScale: root.fontScale
                    icon: "󰖐"
                    text: "Weather"
                    onClicked: root.openModal(root.weatherModal)
                }

                CompactControlButton {
                    fontScale: root.fontScale
                    icon: "󰻂"
                    text: "Recording"
                    onClicked: root.openModal(root.recorderModal)
                }

                CompactControlButton {
                    fontScale: root.fontScale
                    icon: "󰅍"
                    text: "Clipboard"
                    onClicked: root.openModal(root.clipboardModal)
                }

            }

        }

    }

}
