import QtQuick
import qs.theme

Rectangle {
    id: root

    property string icon: ""
    property string text: ""
    property bool hovered: false
    signal clicked

    implicitWidth: 140
    implicitHeight: 40
    radius: 8
    color: hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

    Row {
        anchors.centerIn: parent
        spacing: 8

        Text {
            text: root.icon
            font.pixelSize: 18
            color: Theme.primary
        }

        Text {
            text: root.text
            font.pixelSize: Theme.fontLabelMedium
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
