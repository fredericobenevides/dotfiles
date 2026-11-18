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

    property real fontScale: 1.0
    property var targetScreen: null
    property bool createMode: false
    property bool presetOpen: false

    function closeModal() {
        visible = false;
    }

    function toggleRecording() {
        if (GSRService.saving)
            return ;

        GSRService.toggleRecording();
    }

    function statusText() {
        if (GSRService.saving)
            return "Saving...";

        if (GSRService.recording && !GSRService.capturing)
            return "Waiting for selection...";

        if (GSRService.recording)
            return "Recording " + GSRService.formatElapsed();

        return "Ready";
    }

    focusable: true
    visible: false
    screen: targetScreen
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

        width: Theme.scaledSize(390, recorderModal.fontScale)
        implicitHeight: column.implicitHeight + Theme.scaledSize(32, recorderModal.fontScale)
        anchors.centerIn: parent
        radius: Theme.scaledSize(20, recorderModal.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: Theme.scaledSize(1, recorderModal.fontScale)
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
            anchors.margins: Theme.scaledSize(16, recorderModal.fontScale)
            spacing: Theme.scaledSize(12, recorderModal.fontScale)

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, recorderModal.fontScale)

                Text {
                    text: "\uE04B"
                    font.family: materialSymbols.name
                    font.pixelSize: Theme.scaledFont(18, recorderModal.fontScale)
                    color: GSRService.recording ? Theme.error : Theme.surfaceVariantText
                }

                Text {
                    text: "Recorder"
                    font.pixelSize: Theme.scaledFont(Theme.fontLabelLarge, recorderModal.fontScale)
                    font.bold: true
                    color: Theme.surfaceText
                }

                Item {
                    Layout.fillWidth: true
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
                    visible: GSRService.recording
                    implicitWidth: Theme.scaledSize(64, recorderModal.fontScale)
                    text: GSRService.paused ? "Resume" : "Pause"
                    checked: GSRService.paused
                    activeColor: GSRService.paused ? "#f9e2af" : Theme.surfaceVariantText
                    onClicked: GSRService.togglePause()
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
                    implicitWidth: Theme.scaledSize(32, recorderModal.fontScale)
                    text: "✕"
                    onClicked: recorderModal.closeModal()
                }

            }

            Rectangle {
                Layout.fillWidth: true
                height: Theme.scaledSize(1, recorderModal.fontScale)
                color: Theme.surfaceContainerHighest
            }

            Text {
                text: "Preset"
                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, recorderModal.fontScale)
                color: Theme.surfaceVariantText
            }

            Item {
                Layout.fillWidth: true
                implicitHeight: Theme.scaledSize(26, recorderModal.fontScale)
                z: 10
                enabled: !GSRService.recording

                RowLayout {
                    anchors.fill: parent
                    spacing: Theme.scaledSize(6, recorderModal.fontScale)

                    Rectangle {
                        id: presetBox

                        Layout.fillWidth: true
                        implicitHeight: Theme.scaledSize(26, recorderModal.fontScale)
                        radius: Theme.scaledSize(6, recorderModal.fontScale)
                        color: presetBoxMA.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                        border.color: Theme.surfaceContainerHighest
                        border.width: Theme.scaledSize(1, recorderModal.fontScale)

                        RowLayout {
                            anchors.fill: parent
                            spacing: Theme.scaledSize(0, recorderModal.fontScale)

                            Text {
                                Layout.fillWidth: true
                                Layout.leftMargin: Theme.scaledSize(10, recorderModal.fontScale)
                                text: GSRService.presetName
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, recorderModal.fontScale)
                                font.bold: true
                                color: Theme.surfaceText
                                elide: Text.ElideRight
                            }

                            Text {
                                Layout.rightMargin: Theme.scaledSize(8, recorderModal.fontScale)
                                text: "▾"
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, recorderModal.fontScale)
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

                        fontScale: recorderModal.fontScale
                        visible: GSRService.presetName !== "Default" && !recorderModal.createMode
                        Layout.preferredWidth: Theme.scaledSize(26, recorderModal.fontScale)
                        Layout.alignment: Qt.AlignVCenter
                        text: "－"
                        onClicked: GSRService.deletePreset(GSRService.presetName)
                    }

                    OptionButton {

                        fontScale: recorderModal.fontScale
                        visible: !recorderModal.createMode
                        Layout.preferredWidth: Theme.scaledSize(26, recorderModal.fontScale)
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
                    anchors.topMargin: Theme.scaledSize(30, recorderModal.fontScale)
                    anchors.left: parent.left
                    anchors.right: parent.right
                    z: 5
                    implicitHeight: chips.implicitHeight + Theme.scaledSize(8, recorderModal.fontScale)
                    radius: Theme.scaledSize(8, recorderModal.fontScale)
                    color: Theme.surfaceContainerHigh
                    border.color: Theme.surfaceContainerHighest
                    border.width: Theme.scaledSize(1, recorderModal.fontScale)

                    ColumnLayout {
                        id: chips

                        anchors.fill: parent
                        anchors.margins: Theme.scaledSize(4, recorderModal.fontScale)
                        spacing: Theme.scaledSize(4, recorderModal.fontScale)

                        Repeater {
                            model: GSRService.presetNames

                            delegate: OptionButton {

                                fontScale: recorderModal.fontScale
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
                spacing: Theme.scaledSize(6, recorderModal.fontScale)

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

                    fontScale: recorderModal.fontScale
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
                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, recorderModal.fontScale)
                color: Theme.surfaceVariantText
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(6, recorderModal.fontScale)
                enabled: !GSRService.recording

                OptionButton {

                    fontScale: recorderModal.fontScale
                    text: "30"
                    checked: GSRService.fps === 30
                    onClicked: GSRService.fps = 30
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
                    text: "60"
                    checked: GSRService.fps === 60
                    onClicked: GSRService.fps = 60
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
                    text: "120"
                    checked: GSRService.fps === 120
                    onClicked: GSRService.fps = 120
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
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
                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, recorderModal.fontScale)
                color: Theme.surfaceVariantText
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(6, recorderModal.fontScale)
                enabled: !GSRService.recording

                OptionButton {

                    fontScale: recorderModal.fontScale
                    Layout.fillWidth: true
                    text: "Low"
                    checked: GSRService.quality === "medium"
                    onClicked: GSRService.quality = "medium"
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
                    Layout.fillWidth: true
                    text: "Medium"
                    checked: GSRService.quality === "high"
                    onClicked: GSRService.quality = "high"
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
                    Layout.fillWidth: true
                    text: "High"
                    checked: GSRService.quality === "very_high"
                    onClicked: GSRService.quality = "very_high"
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
                    Layout.fillWidth: true
                    text: "Ultra"
                    checked: GSRService.quality === "ultra"
                    onClicked: GSRService.quality = "ultra"
                }

            }

            Text {
                text: "Audio"
                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, recorderModal.fontScale)
                color: Theme.surfaceVariantText
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(6, recorderModal.fontScale)
                enabled: !GSRService.recording

                OptionButton {

                    fontScale: recorderModal.fontScale
                    Layout.fillWidth: true
                    text: "System"
                    checked: GSRService.audioSystem
                    activeColor: Theme.success
                    onClicked: GSRService.audioSystem = !GSRService.audioSystem
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
                    Layout.fillWidth: true
                    text: "Microphone"
                    checked: GSRService.audioMic
                    activeColor: Theme.success
                    onClicked: GSRService.audioMic = !GSRService.audioMic
                }

            }

            Rectangle {
                Layout.fillWidth: true
                height: Theme.scaledSize(1, recorderModal.fontScale)
                color: Theme.surfaceContainerHighest
            }

            Text {
                text: "Area"
                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, recorderModal.fontScale)
                color: Theme.surfaceVariantText
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(6, recorderModal.fontScale)
                enabled: !GSRService.recording && !GSRService.saving

                OptionButton {

                    fontScale: recorderModal.fontScale
                    Layout.fillWidth: true
                    text: "Select area"
                    onClicked: GSRService.selectRegion()
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
                    Layout.fillWidth: true
                    text: GSRService.regionLabel
                    visible: GSRService.hasRegion
                    checked: GSRService.regionEnabled
                    onClicked: GSRService.toggleRegionEnabled()
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
                    Layout.fillWidth: true
                    text: "No area saved"
                    visible: !GSRService.hasRegion
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
                    text: "✕"
                    visible: GSRService.hasRegion
                    onClicked: GSRService.clearRegion()
                }

            }

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(6, recorderModal.fontScale)

                Rectangle {
                    width: Theme.scaledSize(8, recorderModal.fontScale)
                    height: Theme.scaledSize(8, recorderModal.fontScale)
                    radius: Theme.scaledSize(4, recorderModal.fontScale)
                    color: GSRService.recording ? Theme.error : (GSRService.saving ? "#f9e2af" : Theme.surfaceVariantText)
                    visible: !GSRService.paused || GSRService.saving
                }

                Text {
                    Layout.fillWidth: true
                    text: recorderModal.statusText()
                    font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, recorderModal.fontScale)
                    font.bold: true
                    color: GSRService.recording ? Theme.error : (GSRService.saving ? "#f9e2af" : Theme.surfaceText)
                }

            }

            Button {
                Layout.fillWidth: true
                Layout.preferredHeight: Theme.scaledSize(38, recorderModal.fontScale)
                text: GSRService.recording ? "Stop and save" : (GSRService.hasRegion && GSRService.regionEnabled ? "Start Recording with Selected Area" : "Start recording")
                enabled: !GSRService.saving
                onClicked: recorderModal.toggleRecording()

                background: Rectangle {
                    radius: Theme.scaledSize(9, recorderModal.fontScale)
                    color: !GSRService.recording ? Theme.primary : Theme.error

                    Behavior on color {
                        ColorAnimation {
                            duration: 180
                        }

                    }

                }

                contentItem: Text {
                    text: parent.text
                    font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, recorderModal.fontScale)
                    font.bold: true
                    color: Theme.primaryText
                    horizontalAlignment: Text.AlignHCenter
                    verticalAlignment: Text.AlignVCenter
                }

            }

            Rectangle {
                Layout.fillWidth: true
                height: Theme.scaledSize(1, recorderModal.fontScale)
                color: Theme.surfaceContainerHighest
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, recorderModal.fontScale)

                Text {
                    Layout.fillWidth: true
                    text: GSRService.lastFile !== "" ? "Last: " + GSRService.lastFile : "No recordings yet"
                    elide: Text.ElideMiddle
                    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, recorderModal.fontScale)
                    color: Theme.surfaceVariantText
                }

                OptionButton {

                    fontScale: recorderModal.fontScale
                    text: "Open folder"
                    implicitWidth: Theme.scaledSize(76, recorderModal.fontScale)
                    onClicked: GSRService.openVideos()
                }

            }

        }

    }

}
