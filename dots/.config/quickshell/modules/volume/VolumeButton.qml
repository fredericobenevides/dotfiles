import QtQuick
import qs.theme

Item {
    id: root

    readonly property string volumeIcon: "\uF028"
    readonly property int iconSize: 15
    property bool hovered: false
    property var modal

    implicitWidth: 24
    implicitHeight: 24

    Text {
        anchors.centerIn: parent
        text: root.volumeIcon
        font.pixelSize: root.iconSize
        color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText

        Behavior on color {
            ColorAnimation {
                duration: 160
            }

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
