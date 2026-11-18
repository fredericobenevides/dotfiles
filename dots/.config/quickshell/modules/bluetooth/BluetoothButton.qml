import QtQuick
import qs.theme

Item {
    id: root

    property real fontScale: 1.0
    readonly property string bluetoothIcon: "\uF293"
    readonly property int iconSize: 15
    property bool hovered: false
    property var modal

    implicitWidth: Theme.scaledSize(24, root.fontScale)
    implicitHeight: Theme.scaledSize(24, root.fontScale)

    Text {
        anchors.centerIn: parent
        text: root.bluetoothIcon
        font.pixelSize: Theme.scaledFont(root.iconSize, root.fontScale)
        color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText

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
