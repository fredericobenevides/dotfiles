import QtQuick
import Quickshell.Io
import qs.theme

Rectangle {
    id: root

    property real fontScale: 1.0
    readonly property string activeIcon: "\uF06E"
    readonly property string inactiveIcon: "\uF070"
    readonly property int iconSize: 15
    readonly property bool active: inhibitProcess.running
    property bool hovered: false

    function toggle() {
        if (inhibitProcess.running)
            inhibitProcess.signal(15);
        else
            inhibitProcess.running = true;
    }

    Component.onCompleted: shell.idleInhibitorRef = root
    implicitWidth: Theme.scaledSize(24, root.fontScale)
    implicitHeight: Theme.scaledSize(24, root.fontScale)
    radius: Theme.scaledSize(12, root.fontScale)
    color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

    Process {
        id: inhibitProcess

        command: ["systemd-inhibit", "--what=idle", "--who=Quickshell", "--why=User requested inhibition", "sleep", "infinity"]
    }

    Text {
        anchors.centerIn: parent
        text: root.active ? root.activeIcon : root.inactiveIcon
        font.family: "JetBrainsMono Nerd Font"
        font.pixelSize: Theme.scaledFont(root.iconSize, root.fontScale)
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
        onClicked: root.toggle()
    }

    Behavior on color {
        ColorAnimation {
            duration: 160
        }

    }

}
