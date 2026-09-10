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
    property var osd: null

    function clamp(value) {
        return Math.max(0, Math.min(1, value));
    }

    function pct(value) {
        return Number.isFinite(value) ? Math.round(value * 100) + "%" : "--";
    }

    function levelColor(level) {
        if (level >= 0.8)
            return Theme.error;

        if (level >= 0.5)
            return "#f9e2af";

        return Theme.success;
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
        const active = isSink ? Pipewire.defaultAudioSink : Pipewire.defaultAudioSource;
        const nodes = Pipewire.nodes.values;
        const seen = new Set();
        const result = [];
        for (let i = 0; i < nodes.length; i++) {
            const node = nodes[i];
            if (!node || !node.audio || node.isStream)
                continue;

            if (node.isSink !== isSink)
                continue;

            const label = volumeMenu.nodeLabel(node);
            if (!label || seen.has(label))
                continue;

            seen.add(label);
            result.push(node);
        }
        result.sort((a, b) => {
            const aActive = active && active.id === a.id;
            const bActive = active && active.id === b.id;
            if (aActive !== bActive)
                return aActive ? -1 : 1;

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
    visible: false
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    color: "transparent"
    onVisibleChanged: {
        if (volumeMenu.osd)
            volumeMenu.osd.suppress = visible;

        if (visible) {
            pickerMode = "";
            bg.forceActiveFocus();
            if (volumeMenu.osd)
                volumeMenu.osd.visible = false;

        }
    }

    PwObjectTracker {
        objects: Pipewire.defaultAudioSink && Pipewire.defaultAudioSource ? [Pipewire.defaultAudioSink, Pipewire.defaultAudioSource] : Pipewire.defaultAudioSink ? [Pipewire.defaultAudioSink] : Pipewire.defaultAudioSource ? [Pipewire.defaultAudioSource] : []
    }

    PwNodePeakMonitor {
        id: micPeakMonitor

        node: Pipewire.defaultAudioSource ? Pipewire.defaultAudioSource : null
        enabled: volumeMenu.visible && Pipewire.defaultAudioSource !== null
    }

    PwNodePeakMonitor {
        id: sinkPeakMonitor

        node: Pipewire.defaultAudioSink ? Pipewire.defaultAudioSink : null
        enabled: volumeMenu.visible && Pipewire.defaultAudioSink !== null
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

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 8

                        Text {
                            text: "\uF028"
                            font.pixelSize: 12
                            color: Theme.surfaceVariantText
                        }

                        Rectangle {
                            Layout.fillWidth: true
                            height: 8
                            radius: 4
                            color: Theme.surfaceContainerHighest

                            Rectangle {
                                id: sinkLevelFill

                                anchors.left: parent.left
                                anchors.top: parent.top
                                anchors.bottom: parent.bottom
                                width: parent.width * (sinkPeakMonitor.peak || 0)
                                radius: 4
                                color: volumeMenu.levelColor(sinkPeakMonitor.peak)

                                Behavior on width {
                                    NumberAnimation {
                                        duration: 60
                                        easing.type: Easing.OutQuad
                                    }

                                }

                                Behavior on color {
                                    ColorAnimation {
                                        duration: 100
                                    }

                                }

                            }

                        }

                        Text {
                            text: volumeMenu.pct(sinkPeakMonitor.peak)
                            font.pixelSize: Theme.fontLabelSmall
                            color: Theme.surfaceVariantText
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

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 8

                        Text {
                            text: "\uF130"
                            font.pixelSize: 12
                            color: Theme.surfaceVariantText
                        }

                        Rectangle {
                            Layout.fillWidth: true
                            height: 8
                            radius: 4
                            color: Theme.surfaceContainerHighest

                            Rectangle {
                                id: micLevelFill

                                anchors.left: parent.left
                                anchors.top: parent.top
                                anchors.bottom: parent.bottom
                                width: parent.width * (micPeakMonitor.peak || 0)
                                radius: 4
                                color: volumeMenu.levelColor(micPeakMonitor.peak)

                                Behavior on width {
                                    NumberAnimation {
                                        duration: 60
                                        easing.type: Easing.OutQuad
                                    }

                                }

                                Behavior on color {
                                    ColorAnimation {
                                        duration: 100
                                    }

                                }

                            }

                        }

                        Text {
                            text: volumeMenu.pct(micPeakMonitor.peak)
                            font.pixelSize: Theme.fontLabelSmall
                            color: Theme.surfaceVariantText
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
