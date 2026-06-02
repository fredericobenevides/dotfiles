import QtQuick
import Quickshell

Column {
    property int fontSize: 14
    property string fontFamily: "JetBrainsMono Nerd Font"
    property color textActive: "white"

    SystemClock {
        id: clock

        precision: SystemClock.Seconds
    }

    Text {
        text: Qt.formatDateTime(clock.date, "hh:mm")
        font.pixelSize: fontSize - 2
        font.family: fontFamily
        color: textActive
        anchors.right: parent.right
    }

    Text {
        text: Qt.formatDateTime(clock.date, "dd/MM/yyyy")
        font.pixelSize: fontSize - 2
        font.family: fontFamily
        color: textActive
        anchors.right: parent.right
    }

}
