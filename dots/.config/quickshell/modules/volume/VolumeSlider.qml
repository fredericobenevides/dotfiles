import QtQuick
import qs.theme

Item {
    id: root

    property real fontScale: 1.0
    property real currentValue: 0
    signal valueChange(real value)

    readonly property real safeValue: Number.isFinite(currentValue) ? currentValue : 0

    implicitHeight: Theme.scaledSize(20, root.fontScale)
    implicitWidth: Theme.scaledSize(100, root.fontScale)

    Rectangle {
        id: track

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter
        height: Theme.scaledSize(6, root.fontScale)
        radius: Theme.scaledSize(3, root.fontScale)
        color: Theme.surfaceContainerHighest

        Rectangle {
            anchors.left: parent.left
            anchors.verticalCenter: parent.verticalCenter
            width: Math.max(0, Math.min(parent.width, parent.width * root.safeValue))
            height: parent.height
            radius: Theme.scaledSize(3, root.fontScale)
            color: Theme.primary
        }

    }

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        onPressed: (mouse) => root.valueChange(clamp(mouse.x / track.width))
        onPositionChanged: (mouse) => {
            if (pressed) {
                root.valueChange(clamp(mouse.x / track.width));
            }
        }

        function clamp(value) {
            return Math.max(0, Math.min(1, value));
        }

    }

}
