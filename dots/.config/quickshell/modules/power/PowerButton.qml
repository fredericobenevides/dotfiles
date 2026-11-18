import QtQuick
import qs.theme

Item {
    id: root

    property real fontScale: 1.0
    readonly property string powerIcon: ""
    readonly property int iconSize: 15
    property bool hovered: false
    property var modal

    implicitWidth: bg.width
    implicitHeight: bg.height

    Rectangle {
        id: bg

        width: Theme.scaledSize(24, root.fontScale)
        height: Theme.scaledSize(24, root.fontScale)
        radius: Theme.scaledSize(7, root.fontScale)
        color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

        Text {
            anchors.centerIn: parent
            text: root.powerIcon
            font.pixelSize: Theme.scaledFont(root.iconSize, root.fontScale)
            color: Theme.surfaceVariantText

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

}
