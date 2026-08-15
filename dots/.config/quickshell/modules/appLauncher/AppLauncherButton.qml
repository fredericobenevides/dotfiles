import QtQuick
import qs.theme

Item {
    id: root

    readonly property string launcherIcon: "󰀻"
    readonly property int iconSize: 18
    property bool hovered: false

    implicitWidth: bg.width
    implicitHeight: bg.height

    Rectangle {
        id: bg

        width: 24
        height: 24
        radius: 12
        color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

        Text {
            anchors.centerIn: parent
            text: root.launcherIcon
            font.pixelSize: root.iconSize
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
                shell.toggleMenu(launcherMenu);
            }
        }

    }

}
