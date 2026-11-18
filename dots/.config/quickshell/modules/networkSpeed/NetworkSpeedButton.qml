import QtQuick
import QtQuick.Layouts
import qs.modules.networkSpeed
import qs.modules.vpn
import qs.theme

Rectangle {
    id: root

    property real fontScale: 1.0
    property bool hovered: false
    property var modal
    property bool downFlash: false
    property bool upFlash: false
    readonly property bool vpnActive: VpnService.connected
    readonly property bool vpnConnecting: VpnService.connecting

    implicitWidth: speedRow.implicitWidth + Theme.scaledSize(16, root.fontScale)
    implicitHeight: Theme.scaledSize(24, root.fontScale)
    radius: Theme.scaledSize(12, root.fontScale)
    color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

    Timer {
        id: flashTimer

        interval: 800
        repeat: true
        running: NetworkSpeedService.downSpeed > 0 || NetworkSpeedService.upSpeed > 0
        onTriggered: {
            root.downFlash = !root.downFlash;
            root.upFlash = !root.upFlash;
        }
    }

    RowLayout {
        id: speedRow

        anchors.centerIn: parent
        spacing: Theme.scaledSize(6, root.fontScale)

        Text {
            text: "\uF1EB"
            font.family: "JetBrainsMono Nerd Font"
            font.pixelSize: Theme.scaledFont(15, root.fontScale)
            color: root.vpnConnecting ? "#f9e2af" : (root.vpnActive ? Theme.success : (root.hovered ? Theme.surfaceText : Theme.surfaceVariantText))

            Behavior on color {
                ColorAnimation {
                    duration: 160
                }

            }

        }

        Text {
            text: "\u2193"
            font.pixelSize: Theme.scaledFont(14, root.fontScale)
            opacity: Math.round(NetworkSpeedService.downSpeed) > 0 ? (root.downFlash ? 1 : 0.4) : 1
            color: Math.round(NetworkSpeedService.downSpeed) > 0 ? "#89b4fa" : Theme.surfaceVariantText

            Behavior on opacity {
                NumberAnimation {
                    duration: 350
                }

            }

        }

        Text {
            text: NetworkSpeedService.formatSpeed(NetworkSpeedService.downSpeed)
            font.pixelSize: Theme.scaledFont(11, root.fontScale)
            font.family: "JetBrainsMono Nerd Font"
            color: Theme.surfaceVariantText
            horizontalAlignment: Text.AlignRight
        }

        Text {
            text: "\u2191"
            font.pixelSize: Theme.scaledFont(14, root.fontScale)
            opacity: Math.round(NetworkSpeedService.upSpeed) > 0 ? (root.upFlash ? 1 : 0.4) : 1
            color: Math.round(NetworkSpeedService.upSpeed) > 0 ? "#fab387" : Theme.surfaceVariantText

            Behavior on opacity {
                NumberAnimation {
                    duration: 350
                }

            }

        }

        Text {
            text: NetworkSpeedService.formatSpeed(NetworkSpeedService.upSpeed)
            font.pixelSize: Theme.scaledFont(11, root.fontScale)
            font.family: "JetBrainsMono Nerd Font"
            color: Theme.surfaceVariantText
            horizontalAlignment: Text.AlignRight
        }

    }

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        hoverEnabled: true
        onEntered: root.hovered = true
        onExited: root.hovered = false
        onClicked: {
            if (!root.modal)
                return ;

            root.modal.fontScale = root.fontScale;
            root.modal.visible = !root.modal.visible;
            if (root.modal.visible && root.modal.open)
                root.modal.open();

        }
    }

}
