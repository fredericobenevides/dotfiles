import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Pipewire
import Quickshell.Wayland
import qs.theme

PanelWindow {
    id: volumeMenu

    property real fontScale: 1.0
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

        width: Theme.scaledSize(590, volumeMenu.fontScale)
        height: content.implicitHeight + Theme.scaledSize(24, volumeMenu.fontScale)
        anchors.top: parent.top
        anchors.topMargin: Theme.scaledSize(6, volumeMenu.fontScale)
        anchors.right: parent.right
        anchors.rightMargin: Theme.scaledSize(118, volumeMenu.fontScale)
        radius: Theme.scaledSize(16, volumeMenu.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: Theme.scaledSize(1, volumeMenu.fontScale)
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
            anchors.margins: Theme.scaledSize(12, volumeMenu.fontScale)
            spacing: Theme.scaledSize(10, volumeMenu.fontScale)

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(12, volumeMenu.fontScale)

                ColumnLayout {
                    Layout.fillWidth: true
                    Layout.preferredWidth: Theme.scaledSize(1, volumeMenu.fontScale)
                    spacing: Theme.scaledSize(10, volumeMenu.fontScale)

                    RowLayout {
                        Layout.fillWidth: true

                        Text {
                            text: "\uF028"
                            font.pixelSize: Theme.scaledFont(14, volumeMenu.fontScale)
                            color: Theme.surfaceVariantText
                        }

                        Text {
                            Layout.fillWidth: true
                            text: "Volume"
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelLarge, volumeMenu.fontScale)
                            font.bold: true
                            color: Theme.surfaceText
                        }

                        Text {
                            text: volumeMenu.pct(outputSink && outputSink.audio ? outputSink.audio.volume : NaN)
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, volumeMenu.fontScale)
                            color: Theme.surfaceVariantText
                        }

                    }

                    VolumeSlider {
                        fontScale: volumeMenu.fontScale
                        Layout.fillWidth: true
                        currentValue: outputSink && outputSink.audio ? outputSink.audio.volume : 0
                        onValueChange: (value) => {
                            if (outputSink && outputSink.audio)
                                outputSink.audio.volume = value;

                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: Theme.scaledSize(8, volumeMenu.fontScale)

                        Text {
                            text: "\uF028"
                            font.pixelSize: Theme.scaledFont(12, volumeMenu.fontScale)
                            color: Theme.surfaceVariantText
                        }

                        Rectangle {
                            Layout.fillWidth: true
                            height: Theme.scaledSize(8, volumeMenu.fontScale)
                            radius: Theme.scaledSize(4, volumeMenu.fontScale)
                            color: Theme.surfaceContainerHighest

                            Rectangle {
                                id: sinkLevelFill

                                anchors.left: parent.left
                                anchors.top: parent.top
                                anchors.bottom: parent.bottom
                                width: parent.width * (sinkPeakMonitor.peak || 0)
                                radius: Theme.scaledSize(4, volumeMenu.fontScale)
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
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, volumeMenu.fontScale)
                            color: Theme.surfaceVariantText
                        }

                    }

                    Rectangle {
                        id: outputButton

                        Layout.fillWidth: true
                        height: Theme.scaledSize(40, volumeMenu.fontScale)
                        radius: Theme.scaledSize(7, volumeMenu.fontScale)
                        color: outputButtonMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                        RowLayout {
                            anchors.fill: parent
                            anchors.leftMargin: Theme.scaledSize(12, volumeMenu.fontScale)
                            anchors.rightMargin: Theme.scaledSize(8, volumeMenu.fontScale)
                            spacing: Theme.scaledSize(8, volumeMenu.fontScale)

                            Text {
                                text: "\uF028"
                                font.pixelSize: Theme.scaledFont(14, volumeMenu.fontScale)
                                color: Theme.surfaceVariantText
                            }

                            Text {
                                Layout.fillWidth: true
                                text: volumeMenu.nodeLabel(outputSink) || "No device"
                                elide: Text.ElideRight
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, volumeMenu.fontScale)
                                color: Theme.surfaceText
                            }

                            Text {
                                text: "\uF078"
                                font.pixelSize: Theme.scaledFont(10, volumeMenu.fontScale)
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
                    Layout.preferredWidth: Theme.scaledSize(1, volumeMenu.fontScale)
                    spacing: Theme.scaledSize(10, volumeMenu.fontScale)

                    RowLayout {
                        Layout.fillWidth: true

                        Text {
                            text: "\uF130"
                            font.pixelSize: Theme.scaledFont(14, volumeMenu.fontScale)
                            color: Theme.surfaceVariantText
                        }

                        Text {
                            Layout.fillWidth: true
                            text: "Microphone"
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelLarge, volumeMenu.fontScale)
                            font.bold: true
                            color: Theme.surfaceText
                        }

                        Text {
                            text: volumeMenu.pct(inputSource && inputSource.audio ? inputSource.audio.volume : NaN)
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, volumeMenu.fontScale)
                            color: Theme.surfaceVariantText
                        }

                    }

                    VolumeSlider {
                        fontScale: volumeMenu.fontScale
                        Layout.fillWidth: true
                        currentValue: inputSource && inputSource.audio ? inputSource.audio.volume : 0
                        onValueChange: (value) => {
                            if (inputSource && inputSource.audio)
                                inputSource.audio.volume = value;

                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: Theme.scaledSize(8, volumeMenu.fontScale)

                        Text {
                            text: "\uF130"
                            font.pixelSize: Theme.scaledFont(12, volumeMenu.fontScale)
                            color: Theme.surfaceVariantText
                        }

                        Rectangle {
                            Layout.fillWidth: true
                            height: Theme.scaledSize(8, volumeMenu.fontScale)
                            radius: Theme.scaledSize(4, volumeMenu.fontScale)
                            color: Theme.surfaceContainerHighest

                            Rectangle {
                                id: micLevelFill

                                anchors.left: parent.left
                                anchors.top: parent.top
                                anchors.bottom: parent.bottom
                                width: parent.width * (micPeakMonitor.peak || 0)
                                radius: Theme.scaledSize(4, volumeMenu.fontScale)
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
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, volumeMenu.fontScale)
                            color: Theme.surfaceVariantText
                        }

                    }

                    Rectangle {
                        id: inputButton

                        Layout.fillWidth: true
                        height: Theme.scaledSize(40, volumeMenu.fontScale)
                        radius: Theme.scaledSize(7, volumeMenu.fontScale)
                        color: inputButtonMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                        RowLayout {
                            anchors.fill: parent
                            anchors.leftMargin: Theme.scaledSize(12, volumeMenu.fontScale)
                            anchors.rightMargin: Theme.scaledSize(8, volumeMenu.fontScale)
                            spacing: Theme.scaledSize(8, volumeMenu.fontScale)

                            Text {
                                text: "\uF130"
                                font.pixelSize: Theme.scaledFont(14, volumeMenu.fontScale)
                                color: Theme.surfaceVariantText
                            }

                            Text {
                                Layout.fillWidth: true
                                text: volumeMenu.nodeLabel(inputSource) || "No device"
                                elide: Text.ElideRight
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, volumeMenu.fontScale)
                                color: Theme.surfaceText
                            }

                            Text {
                                text: "\uF078"
                                font.pixelSize: Theme.scaledFont(10, volumeMenu.fontScale)
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
                spacing: Theme.scaledSize(4, volumeMenu.fontScale)
                visible: pickerMode !== ""

                Repeater {
                    model: volumeMenu.pickerModel

                    delegate: Rectangle {
                        id: deviceItem

                        required property var modelData
                        readonly property bool isDefault: (pickerMode === "sink" && outputSink && outputSink.id === modelData.id) || (pickerMode === "source" && inputSource && inputSource.id === modelData.id)

                        Layout.fillWidth: true
                        height: Theme.scaledSize(36, volumeMenu.fontScale)
                        radius: Theme.scaledSize(7, volumeMenu.fontScale)
                        color: deviceMouse.containsMouse || isDefault ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                        border.color: isDefault ? Theme.primary : "transparent"
                        border.width: isDefault ? Theme.scaledSize(1, volumeMenu.fontScale) : Theme.scaledSize(0, volumeMenu.fontScale)

                        RowLayout {
                            anchors.fill: parent
                            anchors.leftMargin: Theme.scaledSize(12, volumeMenu.fontScale)
                            anchors.rightMargin: Theme.scaledSize(12, volumeMenu.fontScale)
                            spacing: Theme.scaledSize(8, volumeMenu.fontScale)

                            Text {
                                text: modelData.isSink ? "\uF028" : "\uF130"
                                font.pixelSize: Theme.scaledFont(12, volumeMenu.fontScale)
                                color: Theme.surfaceVariantText
                            }

                            Text {
                                Layout.fillWidth: true
                                text: volumeMenu.nodeLabel(modelData)
                                elide: Text.ElideRight
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, volumeMenu.fontScale)
                                color: Theme.surfaceText
                            }

                            Text {
                                visible: isDefault
                                text: "\uF00C"
                                font.pixelSize: Theme.scaledFont(12, volumeMenu.fontScale)
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
