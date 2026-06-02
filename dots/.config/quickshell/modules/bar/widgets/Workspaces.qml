import QtQuick
import Quickshell.Hyprland

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
