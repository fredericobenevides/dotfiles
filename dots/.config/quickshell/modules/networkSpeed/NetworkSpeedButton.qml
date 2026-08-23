import QtQuick
import QtQuick.Layouts
import qs.modules.networkSpeed
import qs.modules.vpn
import qs.theme

Rectangle {
    id: root

    property bool hovered: false
    property var modal
    property bool downFlash: false
    property bool upFlash: false
    readonly property bool vpnActive: VpnService.connected
    readonly property bool vpnConnecting: VpnService.connecting

    implicitWidth: speedRow.implicitWidth + 16
    implicitHeight: 24
    radius: 12
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
        spacing: 6

        Text {
            text: "\uF1EB"
            font.family: "JetBrainsMono Nerd Font"
            font.pixelSize: 15
            color: root.vpnConnecting ? "#f9e2af" : (root.vpnActive ? Theme.success : (root.hovered ? Theme.surfaceText : Theme.surfaceVariantText))

            Behavior on color {
                ColorAnimation {
                    duration: 160
                }

            }

        }

        Text {
            text: "\u2193"
            font.pixelSize: 14
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
            font.pixelSize: 11
            font.family: "JetBrainsMono Nerd Font"
            color: Theme.surfaceVariantText
            horizontalAlignment: Text.AlignRight
        }

        Text {
            text: "\u2191"
            font.pixelSize: 14
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
            font.pixelSize: 11
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

            root.modal.visible = !root.modal.visible;
            if (root.modal.visible && root.modal.open)
                root.modal.open();

        }
    }

}
