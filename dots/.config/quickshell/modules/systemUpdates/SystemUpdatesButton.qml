import QtQuick
import QtQuick.Layouts
import qs.modules.systemUpdates
import qs.theme

Item {
    id: root

    property real fontScale: 1.0
    property var modal
    property var targetScreen
    property bool hovered: false

    implicitWidth: contentRow.implicitWidth + Theme.scaledSize(20, root.fontScale)
    implicitHeight: Theme.scaledSize(24, root.fontScale)
    visible: SystemUpdatesService.available

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Rectangle {
        anchors.fill: parent
        radius: Theme.scaledSize(12, root.fontScale)
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
        spacing: Theme.scaledSize(4, root.fontScale)

        Text {
            text: "\uE923"
            font.family: materialSymbols.name
            font.pixelSize: Theme.scaledFont(14, root.fontScale)
            color: Theme.primary
            anchors.verticalCenter: parent.verticalCenter
        }

        Text {
            text: SystemUpdatesService.count
            font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, root.fontScale)
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

            root.modal.targetScreen = root.targetScreen;
            root.modal.fontScale = root.fontScale;
            root.modal.visible = !root.modal.visible;
            if (root.modal.visible && root.modal.open)
                root.modal.open();

        }
    }

}
