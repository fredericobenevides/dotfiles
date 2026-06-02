import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import Quickshell.Wayland
import qs.modules.bar.widgets

Scope {
    readonly property color bgColor: Qt.rgba(44, 47, 58, 0.2)
    readonly property color borderColor: Qt.rgba(1, 1, 1, 0.2)
    readonly property color textActive: "white"
    readonly property color textInactive: Qt.rgba(1, 1, 1, 0.25)
    readonly property color iconActive: "white"
    readonly property color iconActiveReverse: "#2d3436"
    readonly property color bgIconActive: "#ecf0f1"
    readonly property string fontFamily: "JetBrainsMono Nerd Font"
    readonly property int fontSize: 14

    Variants {
        model: Quickshell.screens

        PanelWindow {
            property var modelData

            screen: modelData
            anchors.left: true
            anchors.right: true
            anchors.bottom: true
            implicitHeight: 30
            color: bgColor

            RowLayout {
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.verticalCenter: parent.verticalCenter
                anchors.leftMargin: 5
                anchors.rightMargin: 10
                spacing: 15

                Repeater {
                    model: Hyprland.workspaces.values.filter((ws) => {
                        return ws && ws.monitor && ws.id >= 1 && ws.monitor.name === modelData.name;
                    })

                    Item {
                        property var workspace: modelData
                        property bool isActive: Hyprland.focusedWorkspace && Hyprland.focusedWorkspace.id === workspace.id

                        width: 30
                        height: 30

                        Text {
                            text: parent.workspace.name
                            anchors.centerIn: parent
                            font.pixelSize: fontSize
                            font.family: fontFamily
                            font.bold: true
                            color: parent.isActive ? textActive : textInactive
                        }

                        Rectangle {
                            width: 30
                            height: parent.isActive ? 3 : 0
                            color: parent.isActive ? textActive : bgColor
                            anchors.horizontalCenter: parent.horizontalCenter
                            anchors.bottom: parent.bottom
                        }

                        MouseArea {
                            anchors.fill: parent
                            onClicked: Hyprland.dispatch("workspace " + parent.workspace.id)
                        }

                    }

                }

                Item {
                    Layout.fillWidth: true
                }

                IdleInhibitorModule {
                }

                Volume {
                }

                Clock {
                }

                PowerMenu {
                }

            }

            // Bottom border
            Rectangle {
                width: 100
                height: 1
                color: borderColor
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: parent.bottom
            }

        }

    }

}
