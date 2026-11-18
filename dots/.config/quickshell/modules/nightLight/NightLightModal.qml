import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import qs.modules.nightLight
import qs.theme

PanelWindow {
    id: nightLightMenu

    property real fontScale: 1.0
    property bool editingTemp: false

    function open() {
        visible = true;
    }

    function closeModal() {
        visible = false;
    }

    focusable: true
    visible: false
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    color: "transparent"
    onVisibleChanged: {
        if (visible)
            bg.forceActiveFocus();

    }

    MouseArea {
        anchors.fill: parent
        onClicked: nightLightMenu.closeModal()
    }

    Rectangle {
        id: bg

        width: Theme.scaledSize(320, nightLightMenu.fontScale)
        height: content.implicitHeight + Theme.scaledSize(24, nightLightMenu.fontScale)
        anchors.top: parent.top
        anchors.topMargin: Theme.scaledSize(6, nightLightMenu.fontScale)
        anchors.right: parent.right
        anchors.rightMargin: Theme.scaledSize(118, nightLightMenu.fontScale)
        radius: Theme.scaledSize(16, nightLightMenu.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: Theme.scaledSize(1, nightLightMenu.fontScale)
        Keys.onPressed: (event) => {
            if (event.key === Qt.Key_Escape) {
                nightLightMenu.closeModal();
                event.accepted = true;
            } else if (event.key === Qt.Key_Left) {
                NightLightService.setTemperature(Math.max(2000, NightLightService.temperature - 100));
                event.accepted = true;
            } else if (event.key === Qt.Key_Right) {
                NightLightService.setTemperature(Math.min(6500, NightLightService.temperature + 100));
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: content

            anchors.fill: parent
            anchors.margins: Theme.scaledSize(16, nightLightMenu.fontScale)
            spacing: Theme.scaledSize(14, nightLightMenu.fontScale)

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, nightLightMenu.fontScale)

                Text {
                    text: "\uF186"
                    font.family: "JetBrainsMono Nerd Font"
                    font.pixelSize: Theme.scaledFont(18, nightLightMenu.fontScale)
                    color: NightLightService.active ? "#f9e2af" : Theme.surfaceVariantText
                }

                Text {
                    Layout.fillWidth: true
                    text: "Night Light"
                    font.pixelSize: Theme.scaledFont(18, nightLightMenu.fontScale)
                    font.bold: true
                    color: Theme.surfaceText
                }

                Item {
                    Layout.alignment: Qt.AlignVCenter
                    width: Theme.scaledSize(64, nightLightMenu.fontScale)
                    height: Theme.scaledSize(24, nightLightMenu.fontScale)

                    Text {
                        anchors.right: parent.right
                        anchors.verticalCenter: parent.verticalCenter
                        text: NightLightService.temperature + "K"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, nightLightMenu.fontScale)
                        color: NightLightService.active ? "#f9e2af" : Theme.surfaceVariantText
                        visible: !nightLightMenu.editingTemp
                    }

                    TextInput {
                        id: tempInput

                        anchors.right: parent.right
                        anchors.verticalCenter: parent.verticalCenter
                        width: parent.width
                        horizontalAlignment: Text.AlignRight
                        text: String(NightLightService.temperature)
                        visible: nightLightMenu.editingTemp
                        color: Theme.surfaceText
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, nightLightMenu.fontScale)
                        onAccepted: {
                            NightLightService.setTemperature(parseInt(text));
                            nightLightMenu.editingTemp = false;
                        }
                        Keys.onPressed: (event) => {
                            if (event.key === Qt.Key_Escape) {
                                nightLightMenu.editingTemp = false;
                                event.accepted = true;
                            }
                        }

                        validator: IntValidator {
                            bottom: 2000
                            top: 6500
                        }

                    }

                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor
                        visible: !nightLightMenu.editingTemp
                        onClicked: {
                            nightLightMenu.editingTemp = true;
                            tempInput.forceActiveFocus();
                            tempInput.selectAll();
                        }
                    }

                }

            }

            NightLightSlider {
                fontScale: nightLightMenu.fontScale
                Layout.fillWidth: true
                currentValue: NightLightService.temperature
                onValueChange: (value) => {
                    return NightLightService.setTemperature(value);
                }
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, nightLightMenu.fontScale)

                Text {
                    Layout.fillWidth: true
                    text: NightLightService.active ? "Ligado" : "Desligado"
                    font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, nightLightMenu.fontScale)
                    font.bold: true
                    color: NightLightService.active ? Theme.success : Theme.surfaceVariantText
                }

                Rectangle {
                    Layout.preferredWidth: Theme.scaledSize(34, nightLightMenu.fontScale)
                    Layout.preferredHeight: Theme.scaledSize(20, nightLightMenu.fontScale)
                    Layout.alignment: Qt.AlignRight
                    radius: Theme.scaledSize(10, nightLightMenu.fontScale)
                    color: NightLightService.active ? Theme.primary : Theme.surfaceContainerHigh
                    border.color: NightLightService.active ? "transparent" : Theme.outlineVariant
                    border.width: Theme.scaledSize(1, nightLightMenu.fontScale)

                    Rectangle {
                        width: Theme.scaledSize(16, nightLightMenu.fontScale)
                        height: Theme.scaledSize(16, nightLightMenu.fontScale)
                        radius: Theme.scaledSize(8, nightLightMenu.fontScale)
                        x: NightLightService.active ? parent.width - width - 2 : 2
                        anchors.verticalCenter: parent.verticalCenter
                        color: NightLightService.active ? Theme.primaryText : Theme.surfaceVariantText

                        Behavior on x {
                            NumberAnimation {
                                duration: 160
                                easing.type: Easing.OutCubic
                            }

                        }

                    }

                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor
                        onClicked: NightLightService.toggle()
                    }

                }

            }

        }

    }

}
