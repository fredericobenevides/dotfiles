import QtQuick
import qs.modules.nightLight
import qs.theme

Rectangle {
    id: root

    property real fontScale: 1.0
    readonly property string inactiveIcon: "\uF186"
    readonly property int iconSize: 15
    readonly property bool active: NightLightService.active
    property bool hovered: false
    property var modal

    function toggle() {
        NightLightService.toggle();
    }

    Component.onCompleted: shell.nightLightRef = root
    implicitWidth: Theme.scaledSize(24, root.fontScale)
    implicitHeight: Theme.scaledSize(24, root.fontScale)
    radius: Theme.scaledSize(12, root.fontScale)
    color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

    Text {
        anchors.centerIn: parent
        text: root.inactiveIcon
        font.family: "JetBrainsMono Nerd Font"
        font.pixelSize: Theme.scaledFont(root.iconSize, root.fontScale)
        color: root.active ? "#f9e2af" : (root.hovered ? Theme.surfaceText : Theme.surfaceVariantText)

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

            root.modal.fontScale = root.fontScale;
            root.modal.visible = !root.modal.visible;
            if (root.modal.visible && root.modal.open)
                root.modal.open();

        }
    }

}
