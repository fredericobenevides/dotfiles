import QtQuick
import QtQuick.Layouts
import qs.theme

Item {
    id: root

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

        height: 24
        width: rowLayout.implicitWidth + 40
        radius: 7
        color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

        RowLayout {
            id: rowLayout

            anchors.centerIn: parent
            spacing: 12

            Text {
                id: dateText

                text: root.currentDate()
                font.pixelSize: Theme.fontLabelMedium
                font.bold: true
                color: Theme.surfaceText
            }

            Text {
                id: timeText

                text: root.currentTime()
                font.pixelSize: Theme.fontLabelMedium
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

                root.modal.visible = !root.modal.visible;
                if (root.modal.visible && root.modal.open)
                    root.modal.open();

            }
        }

    }

}
