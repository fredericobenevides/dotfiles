import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Pipewire
import Quickshell.Wayland
import qs.theme

PanelWindow {
    id: volumeMenu

    readonly property var outputSink: Pipewire.defaultAudioSink
    readonly property var inputSource: Pipewire.defaultAudioSource
    property string pickerMode: ""
    property var pickerModel: []

    function clamp(value) {
        return Math.max(0, Math.min(1, value));
    }

    function pct(value) {
        return Number.isFinite(value) ? Math.round(value * 100) + "%" : "--";
    }

    function nodeLabel(node) {
        if (!node)
            return "";

        return node.description || node.nickname || node.name || "";
    }

    function openPicker(mode) {
        pickerMode = mode;
        if (mode === "")
            return ;

        const isSink = mode === "sink";
        const nodes = Pipewire.nodes.values;
        const result = [];
        for (let i = 0; i < nodes.length; i++) {
            const node = nodes[i];
            if (!node || !node.audio || node.isStream)
                continue;

            if (node.isSink === isSink)
                result.push(node);

        }
        result.sort((a, b) => {
            return volumeMenu.nodeLabel(a).localeCompare(volumeMenu.nodeLabel(b));
        });
        pickerModel = result;
    }

    function selectDevice(node) {
        if (pickerMode === "sink")
            Pipewire.preferredDefaultAudioSink = node;
        else if (pickerMode === "source")
            Pipewire.preferredDefaultAudioSource = node;
        pickerMode = "";
    }

    focusable: true
    exclusionMode: PanelWindow.None
    visible: false
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    color: "transparent"
    onVisibleChanged: {
        if (visible) {
            pickerMode = "";
            bg.forceActiveFocus();
        }
    }

    PwObjectTracker {
        objects: Pipewire.defaultAudioSink && Pipewire.defaultAudioSource ? [Pipewire.defaultAudioSink, Pipewire.defaultAudioSource] : Pipewire.defaultAudioSink ? [Pipewire.defaultAudioSink] : Pipewire.defaultAudioSource ? [Pipewire.defaultAudioSource] : []
    }

    MouseArea {
        anchors.fill: parent
        onClicked: volumeMenu.visible = false
    }

    Rectangle {
        id: bg

        width: 590
        height: content.implicitHeight + 24
        anchors.top: parent.top
        anchors.topMargin: 6
        anchors.right: parent.right
        anchors.rightMargin: 118
        radius: 16
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: 1
        Keys.onPressed: (event) => {
            if (event.key === Qt.Key_Escape) {
                volumeMenu.visible = false;
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: content

            anchors.fill: parent
            anchors.margins: 12
            spacing: 10

            RowLayout {
                Layout.fillWidth: true
                spacing: 12

                ColumnLayout {
                    Layout.fillWidth: true
                    Layout.preferredWidth: 1
                    spacing: 10

                    RowLayout {
                        Layout.fillWidth: true

                        Text {
                            text: "\uF028"
                            font.pixelSize: 14
                            color: Theme.surfaceVariantText
                        }

                        Text {
                            Layout.fillWidth: true
                            text: "Volume"
                            font.pixelSize: Theme.fontLabelLarge
                            font.bold: true
                            color: Theme.surfaceText
                        }

                        Text {
                            text: volumeMenu.pct(outputSink && outputSink.audio ? outputSink.audio.volume : NaN)
                            font.pixelSize: Theme.fontLabelMedium
                            color: Theme.surfaceVariantText
                        }

                    }

                    VolumeSlider {
                        Layout.fillWidth: true
                        currentValue: outputSink && outputSink.audio ? outputSink.audio.volume : 0
                        onValueChange: (value) => {
                            if (outputSink && outputSink.audio)
                                outputSink.audio.volume = value;

                        }
                    }

                    Rectangle {
                        id: outputButton

                        Layout.fillWidth: true
                        height: 40
                        radius: 7
                        color: outputButtonMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                        RowLayout {
                            anchors.fill: parent
                            anchors.leftMargin: 12
                            anchors.rightMargin: 8
                            spacing: 8

                            Text {
                                text: "\uF028"
                                font.pixelSize: 14
                                color: Theme.surfaceVariantText
                            }

                            Text {
                                Layout.fillWidth: true
                                text: volumeMenu.nodeLabel(outputSink) || "No device"
                                elide: Text.ElideRight
                                font.pixelSize: Theme.fontLabelSmall
                                color: Theme.surfaceText
                            }

                            Text {
                                text: "\uF078"
                                font.pixelSize: 10
                                color: Theme.surfaceVariantText
                            }

                        }

                        MouseArea {
                            id: outputButtonMouse

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: openPicker(pickerMode === "sink" ? "" : "sink")
                        }

                    }

                }

                ColumnLayout {
                    Layout.fillWidth: true
                    Layout.preferredWidth: 1
                    spacing: 10

                    RowLayout {
                        Layout.fillWidth: true

                        Text {
                            text: "\uF130"
                            font.pixelSize: 14
                            color: Theme.surfaceVariantText
                        }

                        Text {
                            Layout.fillWidth: true
                            text: "Microphone"
                            font.pixelSize: Theme.fontLabelLarge
                            font.bold: true
                            color: Theme.surfaceText
                        }

                        Text {
                            text: volumeMenu.pct(inputSource && inputSource.audio ? inputSource.audio.volume : NaN)
                            font.pixelSize: Theme.fontLabelMedium
                            color: Theme.surfaceVariantText
                        }

                    }

                    VolumeSlider {
                        Layout.fillWidth: true
                        currentValue: inputSource && inputSource.audio ? inputSource.audio.volume : 0
                        onValueChange: (value) => {
                            if (inputSource && inputSource.audio)
                                inputSource.audio.volume = value;

                        }
                    }

                    Rectangle {
                        id: inputButton

                        Layout.fillWidth: true
                        height: 40
                        radius: 7
                        color: inputButtonMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                        RowLayout {
                            anchors.fill: parent
                            anchors.leftMargin: 12
                            anchors.rightMargin: 8
                            spacing: 8

                            Text {
                                text: "\uF130"
                                font.pixelSize: 14
                                color: Theme.surfaceVariantText
                            }

                            Text {
                                Layout.fillWidth: true
                                text: volumeMenu.nodeLabel(inputSource) || "No device"
                                elide: Text.ElideRight
                                font.pixelSize: Theme.fontLabelSmall
                                color: Theme.surfaceText
                            }

                            Text {
                                text: "\uF078"
                                font.pixelSize: 10
                                color: Theme.surfaceVariantText
                            }

                        }

                        MouseArea {
                            id: inputButtonMouse

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: openPicker(pickerMode === "source" ? "" : "source")
                        }

                    }

                }

            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 4
                visible: pickerMode !== ""

                Repeater {
                    model: volumeMenu.pickerModel

                    delegate: Rectangle {
                        id: deviceItem

                        required property var modelData
                        readonly property bool isDefault: (pickerMode === "sink" && outputSink && outputSink.id === modelData.id) || (pickerMode === "source" && inputSource && inputSource.id === modelData.id)

                        Layout.fillWidth: true
                        height: 36
                        radius: 7
                        color: deviceMouse.containsMouse || isDefault ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                        border.color: isDefault ? Theme.primary : "transparent"
                        border.width: isDefault ? 1 : 0

                        RowLayout {
                            anchors.fill: parent
                            anchors.leftMargin: 12
                            anchors.rightMargin: 12
                            spacing: 8

                            Text {
                                text: modelData.isSink ? "\uF028" : "\uF130"
                                font.pixelSize: 12
                                color: Theme.surfaceVariantText
                            }

                            Text {
                                Layout.fillWidth: true
                                text: volumeMenu.nodeLabel(modelData)
                                elide: Text.ElideRight
                                font.pixelSize: Theme.fontLabelSmall
                                color: Theme.surfaceText
                            }

                            Text {
                                visible: isDefault
                                text: "\uF00C"
                                font.pixelSize: 12
                                color: Theme.primary
                            }

                        }

                        MouseArea {
                            id: deviceMouse

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: selectDevice(modelData)
                        }

                    }

                }

            }

        }

    }

}
