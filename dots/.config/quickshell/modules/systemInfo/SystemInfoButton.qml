import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.modules.systemInfo
import qs.theme

Item {
    id: root

    property real fontScale: 1.0
    property var modal
    property var targetScreen
    property bool hovered: false
    readonly property bool tempHot: SystemInfoStats.cpuTemp >= SystemInfoStats.hotTempThreshold
    property bool _tempFlashOn: false

    implicitWidth: contentRow.implicitWidth + Theme.scaledSize(16, root.fontScale)
    implicitHeight: Theme.scaledSize(24, root.fontScale)

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
        radius: Theme.scaledSize(12, root.fontScale)
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
        anchors.leftMargin: Theme.scaledSize(8, root.fontScale)
        anchors.right: parent.right
        anchors.rightMargin: Theme.scaledSize(8, root.fontScale)
        anchors.verticalCenter: parent.verticalCenter
        spacing: Theme.scaledSize(10, root.fontScale)

        Row {
            spacing: Theme.scaledSize(3, root.fontScale)

            Text {
                text: "\uE322"
                font.family: materialSymbols.name
                font.pixelSize: Theme.scaledFont(14, root.fontScale)
                color: Theme.primary
                anchors.verticalCenter: parent.verticalCenter
            }

            Text {
                text: Math.round(SystemInfoStats.cpuPercent) + "%"
                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, root.fontScale)
                font.bold: true
                color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText
                anchors.verticalCenter: parent.verticalCenter
            }

        }

        Row {
            spacing: Theme.scaledSize(3, root.fontScale)

            Text {
                text: "\uE30D"
                font.family: materialSymbols.name
                font.pixelSize: Theme.scaledFont(14, root.fontScale)
                color: Theme.primary
                anchors.verticalCenter: parent.verticalCenter
            }

            Text {
                text: Math.round(SystemInfoStats.memPercent) + "%"
                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, root.fontScale)
                font.bold: true
                color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText
                anchors.verticalCenter: parent.verticalCenter
            }

        }

        Row {
            spacing: Theme.scaledSize(3, root.fontScale)

            Text {
                text: "\uF80E"
                font.family: materialSymbols.name
                font.pixelSize: Theme.scaledFont(14, root.fontScale)
                color: Theme.primary
                anchors.verticalCenter: parent.verticalCenter
            }

            Text {
                text: Math.round(SystemInfoStats.diskPercent) + "%"
                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, root.fontScale)
                font.bold: true
                color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText
                anchors.verticalCenter: parent.verticalCenter
            }

        }

        Row {
            spacing: Theme.scaledSize(3, root.fontScale)

            Text {
                text: "\uE1FF"
                font.family: materialSymbols.name
                font.pixelSize: Theme.scaledFont(14, root.fontScale)
                color: root.tempHot ? Theme.error : Theme.primary
                anchors.verticalCenter: parent.verticalCenter
            }

            Text {
                text: Math.round(SystemInfoStats.cpuTemp) + "°C"
                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, root.fontScale)
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

            root.modal.targetScreen = root.targetScreen;
            root.modal.fontScale = root.fontScale;
            root.modal.visible = !root.modal.visible;
            if (root.modal.visible && root.modal.open)
                root.modal.open();

        }
    }

}
