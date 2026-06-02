import QtQuick
import Quickshell
import Quickshell.Io

Item {
    property int fontSize: 14
    property string fontFamily: "JetBrainsMono Nerd Font"
    property color textActive: "white"

    width: 30
    height: 30

    Process {
        id: powerMenuProcess

        command: ["sh", "-c", "~/.dotfiles/scripts/power-menu.sh"]
    }

    Text {
        text: "⏻"
        anchors.centerIn: parent
        font.pixelSize: fontSize
        font.family: fontFamily
        color: iconActive
    }

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        onClicked: {
            powerMenuProcess.running = true;
        }
    }

}
