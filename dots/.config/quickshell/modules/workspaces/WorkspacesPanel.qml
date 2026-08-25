import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import Quickshell.Wayland
import qs.theme

Item {
    id: root

    property var currentMonitor

    implicitWidth: bg.width
    implicitHeight: bg.height

    Rectangle {
        id: bg

        width: row.width + 15
        height: row.height + 4
        radius: 12
        color: Theme.surfaceContainerHigh

        Row {
            id: row

            spacing: 8
            anchors.centerIn: parent

            Repeater {
                model: Hyprland.workspaces.values.filter((ws) => {
                    return ws && ws.monitor && ws.id >= 1 && ws.monitor.name === root.currentMonitor.name;
                })

                Rectangle {
                    id: wsBox

                    required property var modelData
                    property bool isActive: Hyprland.focusedWorkspace && Hyprland.focusedWorkspace.id === modelData.id
                    property bool isAttention: modelData.urgent && !isActive
                    property bool hovered: mouseArea.containsMouse

                    width: isActive ? 32 : 20
                    height: 20
                    radius: 10
                    color: isActive ? Theme.primary : (isAttention ? Theme.attention : Theme.outlineVariant)

                    Rectangle {
                        anchors.fill: parent
                        radius: 10
                        color: "white"
                        opacity: wsBox.hovered ? 0.15 : 0

                        Behavior on opacity {
                            NumberAnimation {
                                duration: 160
                            }

                        }

                    }

                    Text {
                        anchors.centerIn: parent
                        text: wsBox.modelData.name
                        font.pixelSize: Theme.fontLabelMedium
                        color: isActive ? Theme.primaryText : (wsBox.isAttention ? Theme.primaryText : Theme.surfaceVariantText)
                    }

                    MouseArea {
                        id: mouseArea

                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor
                        hoverEnabled: true
                        onClicked: Hyprland.dispatch("hl.dsp.focus({ workspace = " + wsBox.modelData.id + " })")
                    }

                }

            }

        }

    }

}
