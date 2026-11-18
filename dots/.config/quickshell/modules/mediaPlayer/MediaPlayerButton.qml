import QtQuick
import QtQuick.Layouts
import Quickshell.Services.Mpris
import qs.theme

Rectangle {
    id: root

    property real fontScale: 1.0
    property var modal
    property bool hovered: false
    readonly property var player: MprisController.activePlayer
    readonly property bool hasPlayer: player !== null
    readonly property string trackTitle: hasPlayer ? (MprisController.stableTitle || player.trackTitle || "No media") : "No media"

    function toggleModal() {
        if (!root.modal)
            return ;

        root.modal.fontScale = root.fontScale;
        root.modal.visible = !root.modal.visible;
        if (root.modal.visible && root.modal.open)
            root.modal.open();

    }

    width: implicitWidth
    height: implicitHeight
    implicitHeight: Theme.scaledSize(24, root.fontScale)
    implicitWidth: Math.max(Theme.scaledSize(240, root.fontScale), contentRow.implicitWidth + buttonsRow.implicitWidth + Theme.scaledSize(36, root.fontScale)) + Theme.scaledSize(16, root.fontScale)
    radius: Theme.scaledSize(12, root.fontScale)
    color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Row {
        id: contentRow

        z: 1
        anchors.left: parent.left
        anchors.leftMargin: Theme.scaledSize(12, root.fontScale)
        anchors.verticalCenter: parent.verticalCenter
        spacing: Theme.scaledSize(8, root.fontScale)

        Text {
            text: "\uE405"
            font.family: materialSymbols.name
            font.pixelSize: Theme.scaledFont(16, root.fontScale)
            color: Theme.primary
            anchors.verticalCenter: parent.verticalCenter
        }

        MarqueeText {
            width: Theme.scaledSize(128, root.fontScale)
            text: root.trackTitle
            fontSize: Theme.fontLabelMedium
            fontScale: root.fontScale
            color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText
            anchors.verticalCenter: parent.verticalCenter
            onClicked: root.toggleModal()
        }

    }

    Row {
        id: buttonsRow

        z: 1
        anchors.right: parent.right
        anchors.rightMargin: Theme.scaledSize(8, root.fontScale)
        anchors.verticalCenter: parent.verticalCenter
        spacing: Theme.scaledSize(4, root.fontScale)

        Rectangle {
            width: Theme.scaledSize(16, root.fontScale)
            height: Theme.scaledSize(16, root.fontScale)
            radius: Theme.scaledSize(8, root.fontScale)
            anchors.verticalCenter: parent.verticalCenter
            color: prevMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
            opacity: hasPlayer && player.canGoPrevious ? 1 : 0.45

            Text {
                anchors.centerIn: parent
                text: "\uE045"
                font.family: materialSymbols.name
                font.pixelSize: Theme.scaledFont(15, root.fontScale)
                color: Theme.surfaceText
            }

            MouseArea {
                id: prevMouse

                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                preventStealing: true
                acceptedButtons: Qt.LeftButton
                enabled: hasPlayer && player.canGoPrevious
                onClicked: MprisController.previousOrRewind()
            }

        }

        Rectangle {
            width: Theme.scaledSize(16, root.fontScale)
            height: Theme.scaledSize(16, root.fontScale)
            radius: Theme.scaledSize(8, root.fontScale)
            anchors.verticalCenter: parent.verticalCenter
            color: rewindMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
            opacity: hasPlayer && player.positionSupported ? 1 : 0.45

            Text {
                anchors.centerIn: parent
                text: "\uE020"
                font.family: materialSymbols.name
                font.pixelSize: Theme.scaledFont(15, root.fontScale)
                color: Theme.surfaceText
            }

            MouseArea {
                id: rewindMouse

                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                preventStealing: true
                acceptedButtons: Qt.LeftButton
                enabled: hasPlayer && player.positionSupported
                onClicked: MprisController.seekBy(-5)
            }

        }

        Rectangle {
            width: Theme.scaledSize(24, root.fontScale)
            height: Theme.scaledSize(24, root.fontScale)
            radius: Theme.scaledSize(12, root.fontScale)
            anchors.verticalCenter: parent.verticalCenter
            color: playMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.primary
            opacity: hasPlayer && player.canTogglePlaying ? 1 : 0.55

            Text {
                anchors.centerIn: parent
                text: root.player && root.player.playbackState === MprisPlaybackState.Playing ? "\uE034" : "\uE037"
                font.family: materialSymbols.name
                font.pixelSize: Theme.scaledFont(17, root.fontScale)
                color: Theme.surfaceContainer
            }

            MouseArea {
                id: playMouse

                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                preventStealing: true
                acceptedButtons: Qt.LeftButton
                enabled: hasPlayer && player.canTogglePlaying
                onClicked: {
                    if (player)
                        player.togglePlaying();

                }
            }

        }

        Rectangle {
            width: Theme.scaledSize(16, root.fontScale)
            height: Theme.scaledSize(16, root.fontScale)
            radius: Theme.scaledSize(8, root.fontScale)
            anchors.verticalCenter: parent.verticalCenter
            color: forwardMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
            opacity: hasPlayer && player.positionSupported ? 1 : 0.45

            Text {
                anchors.centerIn: parent
                text: "\uE01F"
                font.family: materialSymbols.name
                font.pixelSize: Theme.scaledFont(15, root.fontScale)
                color: Theme.surfaceText
            }

            MouseArea {
                id: forwardMouse

                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                preventStealing: true
                acceptedButtons: Qt.LeftButton
                enabled: hasPlayer && player.positionSupported
                onClicked: MprisController.seekBy(5)
            }

        }

        Rectangle {
            width: Theme.scaledSize(16, root.fontScale)
            height: Theme.scaledSize(16, root.fontScale)
            radius: Theme.scaledSize(8, root.fontScale)
            anchors.verticalCenter: parent.verticalCenter
            color: nextMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
            opacity: hasPlayer && player.canGoNext ? 1 : 0.45

            Text {
                anchors.centerIn: parent
                text: "\uE044"
                font.family: materialSymbols.name
                font.pixelSize: Theme.scaledFont(15, root.fontScale)
                color: Theme.surfaceText
            }

            MouseArea {
                id: nextMouse

                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                preventStealing: true
                acceptedButtons: Qt.LeftButton
                enabled: hasPlayer && player.canGoNext
                onClicked: MprisController.next()
            }

        }

    }

    MouseArea {
        z: 0
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        hoverEnabled: true
        acceptedButtons: Qt.LeftButton
        onEntered: root.hovered = true
        onExited: root.hovered = false
        onClicked: root.toggleModal()
    }

    Behavior on color {
        ColorAnimation {
            duration: 160
        }

    }

}
