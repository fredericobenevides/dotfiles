import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.modules.clipboard
import qs.theme

Rectangle {
    id: root

    property real fontScale: 1.0
    property var modal
    property bool hovered: false

    implicitWidth: Theme.scaledSize(24, root.fontScale)
    implicitHeight: Theme.scaledSize(24, root.fontScale)
    radius: Theme.scaledSize(12, root.fontScale)
    color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Text {
        anchors.centerIn: parent
        text: "\uE14F"
        font.family: materialSymbols.name
        font.pixelSize: Theme.scaledFont(15, root.fontScale)
        color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText
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

            root.modal.fontScale = root.fontScale;
            root.modal.visible = !root.modal.visible;
            if (root.modal.visible && root.modal.open)
                root.modal.open();

        }
    }

    Behavior on color {
        ColorAnimation {
            duration: 160
        }

    }

}
