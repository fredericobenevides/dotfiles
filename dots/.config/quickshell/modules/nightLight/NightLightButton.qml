import QtQuick
import qs.modules.nightLight
import qs.theme

Rectangle {
    id: root

    readonly property string inactiveIcon: "\uF186"
    readonly property int iconSize: 15
    readonly property bool active: NightLightService.active
    property bool hovered: false
    property var modal

    function toggle() {
        NightLightService.toggle();
    }

    Component.onCompleted: shell.nightLightRef = root
    implicitWidth: 24
    implicitHeight: 24
    radius: 12
    color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

    Text {
        anchors.centerIn: parent
        text: root.inactiveIcon
        font.family: "JetBrainsMono Nerd Font"
        font.pixelSize: root.iconSize
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

            root.modal.visible = !root.modal.visible;
            if (root.modal.visible && root.modal.open)
                root.modal.open();

        }
    }

}
