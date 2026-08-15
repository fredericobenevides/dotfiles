import QtQuick
import qs.theme

Item {
    id: root

    readonly property string bluetoothIcon: "\uF293"
    readonly property int iconSize: 15
    property bool hovered: false

    implicitWidth: 24
    implicitHeight: 24

    Text {
        anchors.centerIn: parent
        text: root.bluetoothIcon
        font.pixelSize: root.iconSize
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
            shell.toggleMenu(bluetoothMenu);
        }
    }

}
