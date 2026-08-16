import QtQuick
import Quickshell.Io
import qs.theme

Rectangle {
    id: root

    readonly property string activeIcon: "\uF06E"
    readonly property string inactiveIcon: "\uF070"
    readonly property int iconSize: 15
    readonly property bool active: inhibitProcess.running
    property bool hovered: false

    implicitWidth: 24
    implicitHeight: 24
    radius: 12
    color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

    Process {
        id: inhibitProcess

        command: ["systemd-inhibit", "--what=idle", "--who=Quickshell", "--why=User requested inhibition", "sleep", "infinity"]
    }

    Text {
        anchors.centerIn: parent
        text: root.active ? root.activeIcon : root.inactiveIcon
        font.family: "JetBrainsMono Nerd Font"
        font.pixelSize: root.iconSize
        color: root.active ? Theme.primary : (root.hovered ? Theme.surfaceText : Theme.surfaceVariantText)

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
            if (inhibitProcess.running)
                inhibitProcess.signal(15);
            else
                inhibitProcess.running = true;
        }
    }

    Behavior on color {
        ColorAnimation {
            duration: 160
        }

    }

}
