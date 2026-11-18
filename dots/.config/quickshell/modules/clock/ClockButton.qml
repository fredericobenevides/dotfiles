import QtQuick
import QtQuick.Layouts
import qs.theme

Item {
    id: root

    property real fontScale: 1.0
    property bool hovered: false
    property var modal

    function currentDate() {
        return Qt.formatDateTime(new Date(), "dd/MM/yyyy");
    }

    function currentTime() {
        return Qt.formatDateTime(new Date(), "hh:mm:ss");
    }

    implicitWidth: bg.width
    implicitHeight: bg.height

    Timer {
        interval: 1000
        running: true
        repeat: true
        onTriggered: {
            dateText.text = root.currentDate();
            timeText.text = root.currentTime();
        }
    }

    Rectangle {
        id: bg

        height: Theme.scaledSize(24, root.fontScale)
        width: rowLayout.implicitWidth + Theme.scaledSize(40, root.fontScale)
        radius: Theme.scaledSize(7, root.fontScale)
        color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

        RowLayout {
            id: rowLayout

            anchors.centerIn: parent
            spacing: Theme.scaledSize(12, root.fontScale)

            Text {
                id: dateText

                text: root.currentDate()
                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, root.fontScale)
                font.bold: true
                color: Theme.surfaceText
            }

            Text {
                id: timeText

                text: root.currentTime()
                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, root.fontScale)
                font.bold: true
                color: Theme.surfaceText
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

                root.modal.fontScale = root.fontScale;
                root.modal.visible = !root.modal.visible;
                if (root.modal.visible && root.modal.open)
                    root.modal.open();

            }
        }

    }

}
