import QtQuick
import qs.modules.vpn
import qs.theme

Item {
    id: root

    readonly property string wifiIcon: "\uF1EB"
    readonly property int iconSize: 15
    property bool hovered: false
    property var modal
    readonly property bool vpnActive: VpnService.connected
    readonly property bool vpnConnecting: VpnService.connecting

    implicitWidth: 24
    implicitHeight: 24

    Text {
        anchors.centerIn: parent
        text: root.wifiIcon
        font.family: "JetBrainsMono Nerd Font"
        font.pixelSize: root.iconSize
        color: root.vpnConnecting ? "#f9e2af" : (root.vpnActive ? Theme.success : (root.hovered ? Theme.surfaceText : Theme.surfaceVariantText))

        Behavior on color {
            ColorAnimation {
                duration: 160
            }

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
