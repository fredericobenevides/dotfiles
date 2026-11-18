import QtQuick
import qs.theme

Rectangle {
    id: root

    property real fontScale: 1.0
    property string icon: ""
    property string text: ""
    property bool hovered: false
    signal clicked

    implicitWidth: Theme.scaledSize(140, root.fontScale)
    implicitHeight: Theme.scaledSize(40, root.fontScale)
    radius: Theme.scaledSize(8, root.fontScale)
    color: hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

    Row {
        anchors.centerIn: parent
        spacing: Theme.scaledSize(8, root.fontScale)

        Text {
            text: root.icon
            font.pixelSize: Theme.scaledFont(18, root.fontScale)
            color: Theme.primary
        }

        Text {
            text: root.text
            font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, root.fontScale)
            color: Theme.surfaceText
        }

    }

    MouseArea {
        anchors.fill: parent
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onEntered: root.hovered = true
        onExited: root.hovered = false
        onClicked: root.clicked()
    }

}
