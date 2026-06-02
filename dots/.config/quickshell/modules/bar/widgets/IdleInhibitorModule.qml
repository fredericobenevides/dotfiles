import QtQuick
import Quickshell.Io

Item {
    id: idleInhibitor

    property int fontSize: 14
    property string fontFamily: "JetBrainsMono Nerd Font"
    property color iconActive: "white"
    property color iconActiveReverse: "#2d3436"
    property color bgIconActive: "#ecf0f1"

    width: 30
    height: 30

    Process {
        id: inhibitProcess

        command: ["systemd-inhibit", "--what=idle", "--who=Quickshell", "--why=User requested inhibition", "sleep", "infinity"]
    }

    Rectangle {
        anchors.fill: parent
        radius: 4
        color: inhibitProcess.running ? bgIconActive : "transparent"

        Text {
            text: inhibitProcess.running ? " " : " "
            anchors.centerIn: parent
            font.pixelSize: fontSize
            font.family: fontFamily
            color: inhibitProcess.running ? iconActiveReverse : iconActive
        }

    }

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        onClicked: {
            inhibitProcess.running = !inhibitProcess.running;
        }
    }

}
