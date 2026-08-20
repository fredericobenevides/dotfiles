import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Pipewire
import Quickshell.Wayland
import qs.theme

PanelWindow {
    id: volumeOSD

    readonly property var outputSink: Pipewire.defaultAudioSink
    readonly property real currentVolume: outputSink && outputSink.audio ? outputSink.audio.volume : 0
    readonly property bool isMuted: outputSink && outputSink.audio ? outputSink.audio.muted : false

    function pct(value) {
        return Number.isFinite(value) ? Math.round(value * 100) + "%" : "--";
    }

    function show() {
        visible = true;
        hideTimer.restart();
    }

    focusable: false
    visible: false
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    color: "transparent"
    exclusiveZone: 0

    PwObjectTracker {
        objects: Pipewire.defaultAudioSink ? [Pipewire.defaultAudioSink] : []
    }

    Timer {
        id: hideTimer

        interval: 2000
        onTriggered: volumeOSD.visible = false
    }

    Connections {
        function onCurrentVolumeChanged() {
            volumeOSD.show();
        }

        function onIsMutedChanged() {
            volumeOSD.show();
        }

        target: volumeOSD
    }

    MouseArea {
        anchors.fill: parent
        onClicked: volumeOSD.visible = false
    }

    Rectangle {
        id: bg

        width: 260
        height: contentColumn.implicitHeight + 20
        anchors.top: parent.top
        anchors.topMargin: 6
        anchors.right: parent.right
        anchors.rightMargin: 240
        radius: 16
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: 1

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: contentColumn

            anchors.fill: parent
            anchors.margins: 12
            spacing: 8

            RowLayout {
                Layout.fillWidth: true
                spacing: 8

                Text {
                    text: volumeOSD.isMuted ? "\uE04E" : "\uE050"
                    font.family: materialSymbols.name
                    font.pixelSize: 18
                    color: Theme.primary
                }

                Text {
                    Layout.fillWidth: true
                    text: volumeOSD.isMuted ? "Muted" : "Volume"
                    font.pixelSize: Theme.fontLabelLarge
                    font.bold: true
                    color: Theme.surfaceText
                }

                Text {
                    text: volumeOSD.pct(volumeOSD.currentVolume)
                    font.pixelSize: Theme.fontLabelMedium
                    color: Theme.surfaceVariantText
                }

            }

            VolumeSlider {
                Layout.fillWidth: true
                currentValue: volumeOSD.currentVolume
                onValueChange: (value) => {
                    if (volumeOSD.outputSink && volumeOSD.outputSink.audio)
                        volumeOSD.outputSink.audio.volume = value;

                }
            }

        }

        FontLoader {
            id: materialSymbols

            source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
        }

    }

}
