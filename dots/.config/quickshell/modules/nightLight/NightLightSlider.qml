import QtQuick
import qs.theme

Item {
    id: root

    property real fontScale: 1.0
    readonly property int minTemp: 2000
    readonly property int maxTemp: 6500
    property int currentValue: 3500
    readonly property int safeValue: Number.isFinite(currentValue) ? currentValue : root.minTemp

    signal valueChange(int value)

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
            width: Math.max(0, Math.min(parent.width, parent.width * (root.safeValue - root.minTemp) / (root.maxTemp - root.minTemp)))
            height: parent.height
            radius: Theme.scaledSize(3, root.fontScale)
            color: Theme.primary
        }

    }

    MouseArea {
        function toTemp(fraction) {
            const clamped = Math.max(0, Math.min(1, fraction));
            return Math.round(root.minTemp + clamped * (root.maxTemp - root.minTemp));
        }

        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        onPressed: (mouse) => {
            return root.valueChange(toTemp(mouse.x / track.width));
        }
        onPositionChanged: (mouse) => {
            if (pressed)
                root.valueChange(toTemp(mouse.x / track.width));

        }
    }

}
