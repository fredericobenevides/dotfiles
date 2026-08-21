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

                    width: isActive ? 32 : 20
                    height: 20
                    radius: 10
                    color: isActive ? Theme.primary : (isAttention ? Theme.attention : Theme.outlineVariant)

                    Text {
                        anchors.centerIn: parent
                        text: wsBox.modelData.name
                        font.pixelSize: Theme.fontLabelMedium
                        color: isActive ? Theme.primaryText : (wsBox.isAttention ? Theme.primaryText : Theme.surfaceVariantText)
                    }

                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor
                        onClicked: Hyprland.dispatch("hl.dsp.focus({ workspace = " + wsBox.modelData.id + " })")
                    }

                }

            }

        }

    }

}
