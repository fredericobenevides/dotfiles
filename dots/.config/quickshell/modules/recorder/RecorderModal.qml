import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs.modules.recorder
import qs.theme

PanelWindow {
    id: recorderModal

    property bool createMode: false
    property bool presetOpen: false

    function closeModal() {
        visible = false;
    }

    function toggleRecording() {
        if (GSRService.saving)
            return ;

        if (GSRService.recording)
            GSRService.stop();
        else
            GSRService.startScreen();
    }

    function statusText() {
        if (GSRService.saving)
            return "Saving...";

        if (GSRService.recording)
            return "Recording " + GSRService.formatElapsed();

        return "Ready";
    }

    focusable: true
    visible: false
    color: "transparent"
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    onVisibleChanged: {
        if (visible) {
            GSRService.refreshLastFile();
            bg.forceActiveFocus();
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: recorderModal.closeModal()
    }

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Rectangle {
        id: bg

        width: 390
        implicitHeight: column.implicitHeight + 32
        anchors.top: parent.top
        anchors.topMargin: 6
        anchors.horizontalCenter: parent.horizontalCenter
        radius: 20
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: 1
        Keys.onPressed: (event) => {
            if (event.key === Qt.Key_Escape) {
                recorderModal.closeModal();
                event.accepted = true;
            } else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                recorderModal.toggleRecording();
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.NoButton
        }

        ColumnLayout {
            id: column

            anchors.fill: parent
            anchors.margins: 16
            spacing: 12

            RowLayout {
                Layout.fillWidth: true
                spacing: 8

                Text {
                    text: "\uE04B"
                    font.family: materialSymbols.name
                    font.pixelSize: 18
                    color: GSRService.recording ? Theme.error : Theme.surfaceVariantText
                }

                Text {
                    text: "Recorder"
                    font.pixelSize: Theme.fontLabelLarge
                    font.bold: true
                    color: Theme.surfaceText
                }

                Item {
                    Layout.fillWidth: true
                }

                OptionButton {
                    visible: GSRService.recording
                    implicitWidth: 64
                    text: GSRService.paused ? "Resume" : "Pause"
                    checked: GSRService.paused
                    activeColor: GSRService.paused ? "#f9e2af" : Theme.surfaceVariantText
                    onClicked: GSRService.togglePause()
                }

                OptionButton {
                    implicitWidth: 32
                    text: "✕"
                    onClicked: recorderModal.closeModal()
                }

            }

            Rectangle {
                Layout.fillWidth: true
                height: 1
                color: Theme.surfaceContainerHighest
            }

            Text {
                text: "Preset"
                font.pixelSize: Theme.fontLabelSmall
                color: Theme.surfaceVariantText
            }

            Item {
                Layout.fillWidth: true
                implicitHeight: 26
                z: 10
                enabled: !GSRService.recording

                RowLayout {
                    anchors.fill: parent
                    spacing: 6

                    Rectangle {
                        id: presetBox

                        Layout.fillWidth: true
                        implicitHeight: 26
                        radius: 6
                        color: presetBoxMA.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                        border.color: Theme.surfaceContainerHighest
                        border.width: 1

                        RowLayout {
                            anchors.fill: parent
                            spacing: 0

                            Text {
                                Layout.fillWidth: true
                                Layout.leftMargin: 10
                                text: GSRService.presetName
                                font.pixelSize: Theme.fontLabelSmall
                                font.bold: true
                                color: Theme.surfaceText
                                elide: Text.ElideRight
                            }

                            Text {
                                Layout.rightMargin: 8
                                text: "▾"
                                font.pixelSize: Theme.fontLabelSmall
                                color: Theme.surfaceVariantText
                            }

                            MouseArea {
                                id: presetBoxMA

                                anchors.fill: parent
                                hoverEnabled: true
                                onClicked: {
                                    recorderModal.presetOpen = !recorderModal.presetOpen;
                                    if (recorderModal.presetOpen)
                                        recorderModal.createMode = false;

                                }
                            }

                        }

                    }

                    OptionButton {
                        visible: GSRService.presetName !== "Default" && !recorderModal.createMode
                        Layout.preferredWidth: 26
                        Layout.alignment: Qt.AlignVCenter
                        text: "－"
                        onClicked: GSRService.deletePreset(GSRService.presetName)
                    }

                    OptionButton {
                        visible: !recorderModal.createMode
                        Layout.preferredWidth: 26
                        Layout.alignment: Qt.AlignVCenter
                        text: "＋"
                        onClicked: {
                            recorderModal.presetOpen = false;
                            recorderModal.createMode = true;
                        }
                    }

                }

                Rectangle {
                    visible: recorderModal.presetOpen
                    anchors.top: parent.top
                    anchors.topMargin: 30
                    anchors.left: parent.left
                    anchors.right: parent.right
                    z: 5
                    implicitHeight: chips.implicitHeight + 8
                    radius: 8
                    color: Theme.surfaceContainerHigh
                    border.color: Theme.surfaceContainerHighest
                    border.width: 1

                    ColumnLayout {
                        id: chips

                        anchors.fill: parent
                        anchors.margins: 4
                        spacing: 4

                        Repeater {
                            model: GSRService.presetNames

                            delegate: OptionButton {
                                required property string modelData

                                Layout.fillWidth: true
                                text: modelData
                                checked: GSRService.presetName === modelData
                                onClicked: {
                                    GSRService.applyPreset(modelData);
                                    recorderModal.presetOpen = false;
                                }
                            }

                        }

                    }

                }

            }

            RowLayout {
                visible: recorderModal.createMode
                Layout.fillWidth: true
                spacing: 6

                TextField {
                    id: nameField

                    Layout.fillWidth: true
                    placeholderText: "Nome..."
                    onVisibleChanged: {
                        if (visible)
                            forceActiveFocus();

                    }
                    Keys.onEscapePressed: recorderModal.createMode = false
                    onAccepted: {
                        if (GSRService.createPreset(text))
                            recorderModal.createMode = false;

                        text = "";
                    }
                }

                OptionButton {
                    text: "Save"
                    onClicked: {
                        if (GSRService.createPreset(nameField.text))
                            recorderModal.createMode = false;

                        nameField.text = "";
                    }
                }

            }

            Text {
                text: "FPS"
                font.pixelSize: Theme.fontLabelSmall
                color: Theme.surfaceVariantText
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 6
                enabled: !GSRService.recording

                OptionButton {
                    text: "30"
                    checked: GSRService.fps === 30
                    onClicked: GSRService.fps = 30
                }

                OptionButton {
                    text: "60"
                    checked: GSRService.fps === 60
                    onClicked: GSRService.fps = 60
                }

                OptionButton {
                    text: "120"
                    checked: GSRService.fps === 120
                    onClicked: GSRService.fps = 120
                }

                OptionButton {
                    text: "144"
                    checked: GSRService.fps === 144
                    onClicked: GSRService.fps = 144
                }

                Item {
                    Layout.fillWidth: true
                }

            }

            Text {
                text: "Quality"
                font.pixelSize: Theme.fontLabelSmall
                color: Theme.surfaceVariantText
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 6
                enabled: !GSRService.recording

                OptionButton {
                    Layout.fillWidth: true
                    text: "Low"
                    checked: GSRService.quality === "medium"
                    onClicked: GSRService.quality = "medium"
                }

                OptionButton {
                    Layout.fillWidth: true
                    text: "Medium"
                    checked: GSRService.quality === "high"
                    onClicked: GSRService.quality = "high"
                }

                OptionButton {
                    Layout.fillWidth: true
                    text: "High"
                    checked: GSRService.quality === "very_high"
                    onClicked: GSRService.quality = "very_high"
                }

                OptionButton {
                    Layout.fillWidth: true
                    text: "Ultra"
                    checked: GSRService.quality === "ultra"
                    onClicked: GSRService.quality = "ultra"
                }

            }

            Text {
                text: "Audio"
                font.pixelSize: Theme.fontLabelSmall
                color: Theme.surfaceVariantText
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 6
                enabled: !GSRService.recording

                OptionButton {
                    Layout.fillWidth: true
                    text: "System"
                    checked: GSRService.audioSystem
                    activeColor: Theme.success
                    onClicked: GSRService.audioSystem = !GSRService.audioSystem
                }

                OptionButton {
                    Layout.fillWidth: true
                    text: "Microphone"
                    checked: GSRService.audioMic
                    activeColor: Theme.success
                    onClicked: GSRService.audioMic = !GSRService.audioMic
                }

            }

            Rectangle {
                Layout.fillWidth: true
                height: 1
                color: Theme.surfaceContainerHighest
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 6

                Rectangle {
                    width: 8
                    height: 8
                    radius: 4
                    color: GSRService.recording ? Theme.error : (GSRService.saving ? "#f9e2af" : Theme.surfaceVariantText)
                    visible: !GSRService.paused || GSRService.saving
                }

                Text {
                    Layout.fillWidth: true
                    text: recorderModal.statusText()
                    font.pixelSize: Theme.fontLabelMedium
                    font.bold: true
                    color: GSRService.recording ? Theme.error : (GSRService.saving ? "#f9e2af" : Theme.surfaceText)
                }

            }

            Button {
                Layout.fillWidth: true
                Layout.preferredHeight: 38
                text: GSRService.recording ? "Stop and save" : "Start recording"
                enabled: !GSRService.saving
                onClicked: recorderModal.toggleRecording()

                background: Rectangle {
                    radius: 9
                    color: !GSRService.recording ? Theme.primary : Theme.error

                    Behavior on color {
                        ColorAnimation {
                            duration: 180
                        }

                    }

                }

                contentItem: Text {
                    text: parent.text
                    font.pixelSize: Theme.fontLabelMedium
                    font.bold: true
                    color: Theme.primaryText
                    horizontalAlignment: Text.AlignHCenter
                    verticalAlignment: Text.AlignVCenter
                }

            }

            Rectangle {
                Layout.fillWidth: true
                height: 1
                color: Theme.surfaceContainerHighest
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 8

                Text {
                    Layout.fillWidth: true
                    text: GSRService.lastFile !== "" ? "Last: " + GSRService.lastFile : "No recordings yet"
                    elide: Text.ElideMiddle
                    font.pixelSize: Theme.fontLabelSmall
                    color: Theme.surfaceVariantText
                }

                OptionButton {
                    text: "Open folder"
                    implicitWidth: 76
                    onClicked: GSRService.openVideos()
                }

            }

        }

    }

}
