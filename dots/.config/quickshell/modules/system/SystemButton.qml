import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.modules.system
import qs.theme

Item {
    id: root

    property var modal
    property bool hovered: false
    readonly property bool tempHot: SystemStats.cpuTemp >= SystemStats.hotTempThreshold
    property bool _tempFlashOn: false

    implicitWidth: contentRow.implicitWidth + 16
    implicitHeight: 24

    Timer {
        interval: 500
        repeat: true
        running: root.tempHot
        onTriggered: root._tempFlashOn = !root._tempFlashOn
        onRunningChanged: {
            if (!running)
                root._tempFlashOn = false;

        }
    }

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Rectangle {
        anchors.fill: parent
        radius: 12
        color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

        Behavior on color {
            ColorAnimation {
                duration: 160
            }

        }

    }

    Row {
        id: contentRow

        z: 1
        anchors.left: parent.left
        anchors.leftMargin: 8
        anchors.right: parent.right
        anchors.rightMargin: 8
        anchors.verticalCenter: parent.verticalCenter
        spacing: 10

        Row {
            spacing: 3

            Text {
                text: "\uE322"
                font.family: materialSymbols.name
                font.pixelSize: 14
                color: Theme.primary
                anchors.verticalCenter: parent.verticalCenter
            }

            Text {
                text: Math.round(SystemStats.cpuPercent) + "%"
                font.pixelSize: Theme.fontLabelMedium
                font.bold: true
                color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText
                anchors.verticalCenter: parent.verticalCenter
            }

        }

        Row {
            spacing: 3

            Text {
                text: "\uE30D"
                font.family: materialSymbols.name
                font.pixelSize: 14
                color: Theme.primary
                anchors.verticalCenter: parent.verticalCenter
            }

            Text {
                text: Math.round(SystemStats.memPercent) + "%"
                font.pixelSize: Theme.fontLabelMedium
                font.bold: true
                color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText
                anchors.verticalCenter: parent.verticalCenter
            }

        }

        Row {
            spacing: 3

            Text {
                text: "\uF80E"
                font.family: materialSymbols.name
                font.pixelSize: 14
                color: Theme.primary
                anchors.verticalCenter: parent.verticalCenter
            }

            Text {
                text: Math.round(SystemStats.diskPercent) + "%"
                font.pixelSize: Theme.fontLabelMedium
                font.bold: true
                color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText
                anchors.verticalCenter: parent.verticalCenter
            }

        }

        Row {
            spacing: 3

            Text {
                text: "\uE1FF"
                font.family: materialSymbols.name
                font.pixelSize: 14
                color: root.tempHot ? Theme.error : Theme.primary
                anchors.verticalCenter: parent.verticalCenter
            }

            Text {
                text: Math.round(SystemStats.cpuTemp) + "°C"
                font.pixelSize: Theme.fontLabelMedium
                font.bold: true
                color: root.tempHot ? (root._tempFlashOn ? "#ffffff" : Theme.error) : (root.hovered ? Theme.surfaceText : Theme.surfaceVariantText)
                anchors.verticalCenter: parent.verticalCenter
            }

        }

    }

    MouseArea {
        z: 0
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        hoverEnabled: true
        onEntered: root.hovered = true
        onExited: root.hovered = false
        onClicked: {
            if (!root.modal)
                return ;

            root.modal.visible = !root.modal.visible;
            if (root.modal.visible && root.modal.open)
                root.modal.open();

        }
    }

}
