import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Services.Mpris
import Quickshell.Wayland
import qs.theme

PanelWindow {
    id: mediaPlayerModal

    property real fontScale: 1.0
    property Item anchorItem: null
    readonly property var player: MprisController.activePlayer
    readonly property string title: MprisController.stableTitle || (player ? player.trackTitle : "No media")
    readonly property string artist: MprisController.stableArtist || (player ? player.trackArtist : "")
    readonly property string album: player ? (player.trackAlbum || "") : ""
    readonly property string artUrl: MprisController.stableArtUrl

    function open() {
        visible = true;
    }

    function closeModal() {
        visible = false;
    }

    focusable: true
    visible: false
    color: "transparent"
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    onVisibleChanged: {
        if (visible)
            bg.forceActiveFocus();

    }

    MouseArea {
        anchors.fill: parent
        onClicked: closeModal()
    }

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Rectangle {
        id: bg

        width: Theme.scaledSize(580, mediaPlayerModal.fontScale)
        height: Theme.scaledSize(140, mediaPlayerModal.fontScale)
        anchors.top: parent.top
        anchors.topMargin: Theme.scaledSize(6, mediaPlayerModal.fontScale)
        anchors.horizontalCenter: parent.horizontalCenter
        radius: Theme.scaledSize(20, mediaPlayerModal.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: Theme.scaledSize(1, mediaPlayerModal.fontScale)
        Keys.onPressed: (event) => {
            if (event.key === Qt.Key_Escape) {
                mediaPlayerModal.closeModal();
                event.accepted = true;
            } else if (event.key === Qt.Key_Space) {
                if (mediaPlayerModal.player && mediaPlayerModal.player.canTogglePlaying)
                    mediaPlayerModal.player.togglePlaying();

                event.accepted = true;
            } else if (event.key === Qt.Key_Left && (event.modifiers & Qt.ShiftModifier)) {
                MprisController.seekBy(-5);
                event.accepted = true;
            } else if (event.key === Qt.Key_Left) {
                MprisController.previousOrRewind();
                event.accepted = true;
            } else if (event.key === Qt.Key_Right && (event.modifiers & Qt.ShiftModifier)) {
                MprisController.seekBy(5);
                event.accepted = true;
            } else if (event.key === Qt.Key_Right) {
                MprisController.next();
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.NoButton
        }

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: Theme.scaledSize(16, mediaPlayerModal.fontScale)
            anchors.rightMargin: Theme.scaledSize(16, mediaPlayerModal.fontScale)
            anchors.topMargin: Theme.scaledSize(0, mediaPlayerModal.fontScale)
            anchors.bottomMargin: Theme.scaledSize(0, mediaPlayerModal.fontScale)
            spacing: Theme.scaledSize(0, mediaPlayerModal.fontScale)

            Rectangle {
                Layout.preferredWidth: Theme.scaledSize(120, mediaPlayerModal.fontScale)
                Layout.preferredHeight: Theme.scaledSize(120, mediaPlayerModal.fontScale)
                radius: Theme.scaledSize(20, mediaPlayerModal.fontScale)
                color: Theme.surfaceContainerHigh
                clip: true

                Image {
                    id: artworkImage

                    anchors.fill: parent
                    source: mediaPlayerModal.artUrl
                    fillMode: Image.PreserveAspectCrop
                    asynchronous: true
                    cache: true
                    visible: source !== ""
                }

                Text {
                    anchors.centerIn: parent
                    text: "\uE405"
                    font.family: materialSymbols.name
                    font.pixelSize: Theme.scaledFont(32, mediaPlayerModal.fontScale)
                    color: Theme.surfaceVariantText
                    visible: !artworkImage.visible
                }

            }

            Item {
                width: Theme.scaledSize(12, mediaPlayerModal.fontScale)
            }

            ColumnLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                Layout.topMargin: Theme.scaledSize(0, mediaPlayerModal.fontScale)
                spacing: Theme.scaledSize(2, mediaPlayerModal.fontScale)

                MarqueeText {
                    Layout.fillWidth: true
                    text: mediaPlayerModal.title
                    fontSize: 15
                    fontScale: mediaPlayerModal.fontScale
                    bold: true
                }

                Item {
                    Layout.preferredHeight: Theme.scaledSize(4, mediaPlayerModal.fontScale)
                }

                Text {
                    Layout.fillWidth: true
                    text: mediaPlayerModal.artist
                    font.pixelSize: Theme.scaledFont(12, mediaPlayerModal.fontScale)
                    color: Theme.surfaceVariantText
                    elide: Text.ElideRight
                }

                Item {
                    Layout.preferredHeight: Theme.scaledSize(18, mediaPlayerModal.fontScale)
                }

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: Theme.scaledSize(5, mediaPlayerModal.fontScale)
                    radius: Theme.scaledSize(5, mediaPlayerModal.fontScale)
                    color: Theme.surfaceContainerHigh

                    Rectangle {
                        width: (mediaPlayerModal.player && mediaPlayerModal.player.lengthSupported && mediaPlayerModal.player.length > 0) ? Math.max(10, parent.width * Math.max(0, Math.min(1, mediaPlayerModal.player.position / mediaPlayerModal.player.length))) : 0
                        height: parent.height
                        radius: Theme.scaledSize(5, mediaPlayerModal.fontScale)
                        color: Theme.primary
                    }

                    MouseArea {
                        id: seekArea

                        function seekTo(mx) {
                            if (!mediaPlayerModal.player || !mediaPlayerModal.player.lengthSupported || mediaPlayerModal.player.length <= 0)
                                return ;

                            const fraction = mx / Math.max(1, parent.width);
                            MprisController.seekToFraction(fraction);
                        }

                        anchors.fill: parent
                        anchors.topMargin: -Theme.scaledSize(8, mediaPlayerModal.fontScale)
                        anchors.bottomMargin: -Theme.scaledSize(8, mediaPlayerModal.fontScale)
                        cursorShape: Qt.PointingHandCursor
                        onClicked: seekTo(mouse.x)
                        onPositionChanged: {
                            if (pressed)
                                seekTo(mouse.x);

                        }
                    }

                }

                Item {
                    Layout.preferredHeight: Theme.scaledSize(8, mediaPlayerModal.fontScale)
                }

                RowLayout {
                    Layout.fillWidth: true
                    spacing: Theme.scaledSize(8, mediaPlayerModal.fontScale)

                    Text {
                        text: MprisController.positionText
                        font.pixelSize: Theme.scaledFont(11, mediaPlayerModal.fontScale)
                        color: Theme.surfaceVariantText
                    }

                    Item {
                        Layout.fillWidth: true
                    }

                    Rectangle {
                        width: Theme.scaledSize(16, mediaPlayerModal.fontScale)
                        height: Theme.scaledSize(16, mediaPlayerModal.fontScale)
                        radius: Theme.scaledSize(8, mediaPlayerModal.fontScale)
                        color: prevMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                        opacity: mediaPlayerModal.player && mediaPlayerModal.player.canGoPrevious ? 1 : 0.45

                        Text {
                            anchors.centerIn: parent
                            text: "\uE045"
                            font.family: materialSymbols.name
                            font.pixelSize: Theme.scaledFont(15, mediaPlayerModal.fontScale)
                            color: Theme.surfaceText
                        }

                        MouseArea {
                            id: prevMouse

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            enabled: mediaPlayerModal.player && mediaPlayerModal.player.canGoPrevious
                            onClicked: MprisController.previousOrRewind()
                        }

                    }

                    Rectangle {
                        width: Theme.scaledSize(16, mediaPlayerModal.fontScale)
                        height: Theme.scaledSize(16, mediaPlayerModal.fontScale)
                        radius: Theme.scaledSize(8, mediaPlayerModal.fontScale)
                        color: rewindModalMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent"
                        opacity: mediaPlayerModal.player && mediaPlayerModal.player.positionSupported ? (rewindModalMouse.containsMouse ? 1 : 0.7) : 0.3

                        Text {
                            anchors.centerIn: parent
                            text: "\uE020"
                            font.family: materialSymbols.name
                            font.pixelSize: Theme.scaledFont(15, mediaPlayerModal.fontScale)
                            color: Theme.surfaceText
                        }

                        MouseArea {
                            id: rewindModalMouse

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            enabled: mediaPlayerModal.player && mediaPlayerModal.player.positionSupported
                            onClicked: MprisController.seekBy(-5)
                        }

                    }

                    Rectangle {
                        width: Theme.scaledSize(24, mediaPlayerModal.fontScale)
                        height: Theme.scaledSize(24, mediaPlayerModal.fontScale)
                        radius: Theme.scaledSize(12, mediaPlayerModal.fontScale)
                        color: playMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.primary
                        opacity: mediaPlayerModal.player && mediaPlayerModal.player.canTogglePlaying ? 1 : 0.55

                        Text {
                            anchors.centerIn: parent
                            text: mediaPlayerModal.player && mediaPlayerModal.player.playbackState === MprisPlaybackState.Playing ? "\uE034" : "\uE037"
                            font.family: materialSymbols.name
                            font.pixelSize: Theme.scaledFont(17, mediaPlayerModal.fontScale)
                            color: Theme.surfaceContainer
                        }

                        MouseArea {
                            id: playMouse

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            enabled: mediaPlayerModal.player && mediaPlayerModal.player.canTogglePlaying
                            onClicked: mediaPlayerModal.player.togglePlaying()
                        }

                    }

                    Rectangle {
                        width: Theme.scaledSize(16, mediaPlayerModal.fontScale)
                        height: Theme.scaledSize(16, mediaPlayerModal.fontScale)
                        radius: Theme.scaledSize(8, mediaPlayerModal.fontScale)
                        color: forwardModalMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent"
                        opacity: mediaPlayerModal.player && mediaPlayerModal.player.positionSupported ? (forwardModalMouse.containsMouse ? 1 : 0.7) : 0.3

                        Text {
                            anchors.centerIn: parent
                            text: "\uE01F"
                            font.family: materialSymbols.name
                            font.pixelSize: Theme.scaledFont(15, mediaPlayerModal.fontScale)
                            color: Theme.surfaceText
                        }

                        MouseArea {
                            id: forwardModalMouse

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            enabled: mediaPlayerModal.player && mediaPlayerModal.player.positionSupported
                            onClicked: MprisController.seekBy(5)
                        }

                    }

                    Rectangle {
                        width: Theme.scaledSize(16, mediaPlayerModal.fontScale)
                        height: Theme.scaledSize(16, mediaPlayerModal.fontScale)
                        radius: Theme.scaledSize(8, mediaPlayerModal.fontScale)
                        color: nextMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                        opacity: mediaPlayerModal.player && mediaPlayerModal.player.canGoNext ? 1 : 0.45

                        Text {
                            anchors.centerIn: parent
                            text: "\uE044"
                            font.family: materialSymbols.name
                            font.pixelSize: Theme.scaledFont(15, mediaPlayerModal.fontScale)
                            color: Theme.surfaceText
                        }

                        MouseArea {
                            id: nextMouse

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            enabled: mediaPlayerModal.player && mediaPlayerModal.player.canGoNext
                            onClicked: MprisController.next()
                        }

                    }

                    Item {
                        Layout.fillWidth: true
                    }

                    Text {
                        text: MprisController.lengthText
                        font.pixelSize: Theme.scaledFont(11, mediaPlayerModal.fontScale)
                        color: Theme.surfaceVariantText
                    }

                }

            }

        }

    }

}
