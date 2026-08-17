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

        source: Qt.resolvedUrl("/usr/share/quickshell/dms/assets/fonts/material-design-icons/variablefont/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Rectangle {
        id: bg

        width: 341
        height: 96
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
            }
        }

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.NoButton
        }

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: 12
            anchors.rightMargin: 12
            anchors.topMargin: 0
            anchors.bottomMargin: 0
            spacing: 0

            Rectangle {
                Layout.preferredWidth: 83
                Layout.preferredHeight: 83
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
                width: 8
            }

            ColumnLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                spacing: 2

                MarqueeText {
                    Layout.fillWidth: true
                    text: mediaPlayerModal.title
                    fontSize: 13
                    bold: true
                }

                Text {
                    Layout.fillWidth: true
                    text: mediaPlayerModal.artist
                    font.pixelSize: 10
                    color: Theme.surfaceVariantText
                    elide: Text.ElideRight
                }

                Item {
                    Layout.preferredHeight: 5
                }

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 2
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
                        anchors.topMargin: -7
                        anchors.bottomMargin: -7
                        cursorShape: Qt.PointingHandCursor
                        onClicked: seekTo(mouse.x)
                        onPositionChanged: {
                            if (pressed)
                                seekTo(mouse.x);

                        }
                    }

                }

                Item {
                    Layout.preferredHeight: 5
                }

                RowLayout {
                    Layout.fillWidth: true
                    spacing: 8

                    Text {
                        text: MprisController.positionText
                        font.pixelSize: 10
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
                            font.pixelSize: 14
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
                        width: 18
                        height: 18
                        radius: 9
                        color: playMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.primary
                        opacity: mediaPlayerModal.player && mediaPlayerModal.player.canTogglePlaying ? 1 : 0.55

                        Text {
                            anchors.centerIn: parent
                            text: mediaPlayerModal.player && mediaPlayerModal.player.playbackState === MprisPlaybackState.Playing ? "\uE034" : "\uE037"
                            font.family: materialSymbols.name
                            font.pixelSize: 16
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
                        color: nextMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                        opacity: mediaPlayerModal.player && mediaPlayerModal.player.canGoNext ? 1 : 0.45

                        Text {
                            anchors.centerIn: parent
                            text: "\uE044"
                            font.family: materialSymbols.name
                            font.pixelSize: 14
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
                        font.pixelSize: 10
                        color: Theme.surfaceVariantText
                    }

                }

            }

        }

    }

}
