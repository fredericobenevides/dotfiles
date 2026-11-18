import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import Quickshell.Wayland
import qs.theme

Item {
    id: root

    property real fontScale: 1.0
    property var currentMonitor

    implicitWidth: bg.width
    implicitHeight: bg.height

    Rectangle {
        id: bg

        width: row.width + Theme.scaledSize(15, root.fontScale)
        height: row.height + Theme.scaledSize(4, root.fontScale)
        radius: Theme.scaledSize(12, root.fontScale)
        color: Theme.surfaceContainerHigh

        Row {
            id: row

            spacing: Theme.scaledSize(8, root.fontScale)
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

                    width: isActive ? Theme.scaledSize(32, root.fontScale) : Theme.scaledSize(20, root.fontScale)
                    height: Theme.scaledSize(20, root.fontScale)
                    radius: Theme.scaledSize(10, root.fontScale)
                    color: isActive ? Theme.primary : (isAttention ? Theme.attention : Theme.outlineVariant)

                    Rectangle {
                        anchors.fill: parent
                        radius: Theme.scaledSize(10, root.fontScale)
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
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, root.fontScale)
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
