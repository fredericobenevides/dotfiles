import QtQuick
import qs.theme

Item {
    id: root

    property real currentValue: 0
    signal valueChange(real value)

    readonly property real safeValue: Number.isFinite(currentValue) ? currentValue : 0

    implicitHeight: 20
    implicitWidth: 100

    Rectangle {
        id: track

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter
        height: 6
        radius: 3
        color: Theme.surfaceContainerHighest

        Rectangle {
            anchors.left: parent.left
            anchors.verticalCenter: parent.verticalCenter
            width: Math.max(0, Math.min(parent.width, parent.width * root.safeValue))
            height: parent.height
            radius: 3
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
