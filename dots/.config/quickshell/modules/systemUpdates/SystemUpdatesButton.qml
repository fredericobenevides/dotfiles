import QtQuick
import QtQuick.Layouts
import qs.modules.systemUpdates
import qs.theme

Item {
    id: root

    property var modal
    property bool hovered: false

    implicitWidth: contentRow.implicitWidth + 20
    implicitHeight: 24
    visible: SystemUpdatesService.available

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/quickshell/dms/assets/fonts/material-design-icons/variablefont/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Rectangle {
        anchors.fill: parent
        radius: 12
        color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

        Behavior on color {
            ColorAnimation {
                duration: 160
            }

        }

    }

    Row {
        id: contentRow

        anchors.centerIn: parent
        spacing: 4

        Text {
            text: "\uE923"
            font.family: materialSymbols.name
            font.pixelSize: 14
            color: Theme.primary
            anchors.verticalCenter: parent.verticalCenter
        }

        Text {
            text: SystemUpdatesService.count
            font.pixelSize: Theme.fontLabelMedium
            font.bold: true
            color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText
            anchors.verticalCenter: parent.verticalCenter
        }

    }

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        hoverEnabled: true
        onEntered: root.hovered = true
        onExited: root.hovered = false
        onClicked: {
            if (!root.modal)
                return ;

            root.modal.visible = !root.modal.visible;
            if (root.modal.visible && root.modal.open)
                root.modal.open();

        }
    }

}
