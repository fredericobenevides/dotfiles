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

        width: 580
        height: 140
        anchors.top: parent.top
        anchors.topMargin: 6
        anchors.horizontalCenter: parent.horizontalCenter
        radius: 20
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: 1
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
            anchors.leftMargin: 16
            anchors.rightMargin: 16
            anchors.topMargin: 0
            anchors.bottomMargin: 0
            spacing: 0

            Rectangle {
                Layout.preferredWidth: 120
                Layout.preferredHeight: 120
                radius: 20
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
                    font.pixelSize: 32
                    color: Theme.surfaceVariantText
                    visible: !artworkImage.visible
                }

            }

            Item {
                width: 12
            }

            ColumnLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                Layout.topMargin: 0
                spacing: 2

                MarqueeText {
                    Layout.fillWidth: true
                    text: mediaPlayerModal.title
                    fontSize: 15
                    bold: true
                }

                Item {
                    Layout.preferredHeight: 4
                }

                Text {
                    Layout.fillWidth: true
                    text: mediaPlayerModal.artist
                    font.pixelSize: 12
                    color: Theme.surfaceVariantText
                    elide: Text.ElideRight
                }

                Item {
                    Layout.preferredHeight: 18
                }

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 5
                    radius: 5
                    color: Theme.surfaceContainerHigh

                    Rectangle {
                        width: (mediaPlayerModal.player && mediaPlayerModal.player.lengthSupported && mediaPlayerModal.player.length > 0) ? Math.max(10, parent.width * Math.max(0, Math.min(1, mediaPlayerModal.player.position / mediaPlayerModal.player.length))) : 0
                        height: parent.height
                        radius: 5
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
                        anchors.topMargin: -8
                        anchors.bottomMargin: -8
                        cursorShape: Qt.PointingHandCursor
                        onClicked: seekTo(mouse.x)
                        onPositionChanged: {
                            if (pressed)
                                seekTo(mouse.x);

                        }
                    }

                }

                Item {
                    Layout.preferredHeight: 8
                }

                RowLayout {
                    Layout.fillWidth: true
                    spacing: 8

                    Text {
                        text: MprisController.positionText
                        font.pixelSize: 11
                        color: Theme.surfaceVariantText
                    }

                    Item {
                        Layout.fillWidth: true
                    }

                    Rectangle {
                        width: 16
                        height: 16
                        radius: 8
                        color: prevMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                        opacity: mediaPlayerModal.player && mediaPlayerModal.player.canGoPrevious ? 1 : 0.45

                        Text {
                            anchors.centerIn: parent
                            text: "\uE045"
                            font.family: materialSymbols.name
                            font.pixelSize: 15
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
                        width: 16
                        height: 16
                        radius: 8
                        color: rewindModalMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent"
                        opacity: mediaPlayerModal.player && mediaPlayerModal.player.positionSupported ? (rewindModalMouse.containsMouse ? 1 : 0.7) : 0.3

                        Text {
                            anchors.centerIn: parent
                            text: "\uE020"
                            font.family: materialSymbols.name
                            font.pixelSize: 15
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
                        width: 24
                        height: 24
                        radius: 12
                        color: playMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.primary
                        opacity: mediaPlayerModal.player && mediaPlayerModal.player.canTogglePlaying ? 1 : 0.55

                        Text {
                            anchors.centerIn: parent
                            text: mediaPlayerModal.player && mediaPlayerModal.player.playbackState === MprisPlaybackState.Playing ? "\uE034" : "\uE037"
                            font.family: materialSymbols.name
                            font.pixelSize: 17
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
                        width: 16
                        height: 16
                        radius: 8
                        color: forwardModalMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent"
                        opacity: mediaPlayerModal.player && mediaPlayerModal.player.positionSupported ? (forwardModalMouse.containsMouse ? 1 : 0.7) : 0.3

                        Text {
                            anchors.centerIn: parent
                            text: "\uE01F"
                            font.family: materialSymbols.name
                            font.pixelSize: 15
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
                        width: 16
                        height: 16
                        radius: 8
                        color: nextMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                        opacity: mediaPlayerModal.player && mediaPlayerModal.player.canGoNext ? 1 : 0.45

                        Text {
                            anchors.centerIn: parent
                            text: "\uE044"
                            font.family: materialSymbols.name
                            font.pixelSize: 15
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
                        font.pixelSize: 11
                        color: Theme.surfaceVariantText
                    }

                }

            }

        }

    }

}
