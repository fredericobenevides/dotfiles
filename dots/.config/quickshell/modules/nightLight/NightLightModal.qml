import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import qs.modules.nightLight
import qs.theme

PanelWindow {
    id: nightLightMenu

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

        width: 320
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
            anchors.margins: 16
            spacing: 14

            RowLayout {
                Layout.fillWidth: true
                spacing: 8

                Text {
                    text: "\uF186"
                    font.family: "JetBrainsMono Nerd Font"
                    font.pixelSize: 18
                    color: NightLightService.active ? "#f9e2af" : Theme.surfaceVariantText
                }

                Text {
                    Layout.fillWidth: true
                    text: "Night Light"
                    font.pixelSize: 18
                    font.bold: true
                    color: Theme.surfaceText
                }

                Item {
                    Layout.alignment: Qt.AlignVCenter
                    width: 64
                    height: 24

                    Text {
                        anchors.right: parent.right
                        anchors.verticalCenter: parent.verticalCenter
                        text: NightLightService.temperature + "K"
                        font.pixelSize: Theme.fontLabelMedium
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
                        font.pixelSize: Theme.fontLabelMedium
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
                Layout.fillWidth: true
                currentValue: NightLightService.temperature
                onValueChange: (value) => {
                    return NightLightService.setTemperature(value);
                }
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 8

                Text {
                    Layout.fillWidth: true
                    text: NightLightService.active ? "Ligado" : "Desligado"
                    font.pixelSize: Theme.fontLabelMedium
                    font.bold: true
                    color: NightLightService.active ? Theme.success : Theme.surfaceVariantText
                }

                Rectangle {
                    Layout.preferredWidth: 34
                    Layout.preferredHeight: 20
                    Layout.alignment: Qt.AlignRight
                    radius: 10
                    color: NightLightService.active ? Theme.primary : Theme.surfaceContainerHigh
                    border.color: NightLightService.active ? "transparent" : Theme.outlineVariant
                    border.width: 1

                    Rectangle {
                        width: 16
                        height: 16
                        radius: 8
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
