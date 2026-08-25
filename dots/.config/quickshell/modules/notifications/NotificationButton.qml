import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.modules.notifications
import qs.theme

Rectangle {
    id: root

    property var modal
    property bool hovered: false

    implicitWidth: 24
    implicitHeight: 24
    radius: 12
    color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Text {
        anchors.centerIn: parent
        text: "\uE7F5"
        font.family: materialSymbols.name
        font.pixelSize: 15
        color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText
    }

    Rectangle {
        anchors.top: parent.top
        anchors.topMargin: 1
        anchors.right: parent.right
        anchors.rightMargin: 1
        visible: NotificationsService.count > 0
        width: 13
        height: 13
        radius: 7
        color: Theme.primary

        Text {
            anchors.centerIn: parent
            text: Math.min(99, NotificationsService.count)
            font.pixelSize: 8
            font.bold: true
            color: Theme.primaryText
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

    Behavior on color {
        ColorAnimation {
            duration: 160
        }

    }

}
