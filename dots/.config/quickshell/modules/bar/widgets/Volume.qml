import QtQuick
import Quickshell
import Quickshell.Io

Item {
    id: volume

    readonly property int maxVolume: 150
    readonly property int scrollStep: 10
    property int fontSize: 14
    property string fontFamily: "JetBrainsMono Nerd Font"
    property color textActive: "white"
    property int volumeLevel: 0
    property bool isMuted: false

    width: 30
    height: 30

    Process {
        id: getVolumeProcess

        command: ["sh", "-c", "pactl get-sink-volume @DEFAULT_SINK@ | awk '{print $5}' | sed 's/%//'; pactl get-sink-mute @DEFAULT_SINK@ | awk '{print $2}'"]
        running: true

        stdout: SplitParser {
            onRead: (line) => {
                var cleanLine = line.trim();
                if (cleanLine === "yes")
                    volume.isMuted = true;
                else if (cleanLine === "no")
                    volume.isMuted = false;
                else if (cleanLine !== "" && !isNaN(cleanLine))
                    volume.volumeLevel = parseInt(cleanLine);
            }
        }

    }

    Timer {
        interval: 1000
        running: true
        repeat: true
        onTriggered: getVolumeProcess.running = true
    }

    Process {
        id: volumeUpCmd

        command: ["pactl", "set-sink-volume", "@DEFAULT_SINK@", "+" + volume.scrollStep + "%"]
    }

    Process {
        id: volumeDownCmd

        command: ["pactl", "set-sink-volume", "@DEFAULT_SINK@", "-" + volume.scrollStep + "%"]
    }

    Process {
        id: volumeMuteCmd

        command: ["pactl", "set-sink-mute", "@DEFAULT_SINK@", "toggle"]
    }

    Process {
        id: pavucontrolCmd

        command: ["pavucontrol"]
    }

    Text {
        anchors.centerIn: parent
        font.pixelSize: volume.fontSize
        font.family: volume.fontFamily
        color: volume.textActive
        text: {
            if (volume.isMuted)
                return "  ";

            if (volume.volumeLevel === 0)
                return "  ";

            if (volume.volumeLevel > 0 && volume.volumeLevel <= 50)
                return "  ";

            return "   ";
        }
    }

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: (mouse) => {
            if (mouse.button === Qt.LeftButton) {
                pavucontrolCmd.running = true;
            } else if (mouse.button === Qt.RightButton) {
                volumeMuteCmd.running = true;
                getVolumeProcess.running = true;
            }
        }
        onWheel: (wheel) => {
            if (wheel.angleDelta.y > 0) {
                if (volume.volumeLevel + volume.scrollStep <= volume.maxVolume)
                    volumeUpCmd.running = true;

            } else {
                volumeDownCmd.running = true;
            }
            getVolumeProcess.running = true;
        }
    }

}
