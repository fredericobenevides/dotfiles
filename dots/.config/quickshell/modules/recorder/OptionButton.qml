import QtQuick
import QtQuick.Controls
import qs.theme

Button {
    id: root

    property color activeColor: Theme.primary

    font.pixelSize: Theme.fontLabelSmall
    font.bold: true
    implicitWidth: 44
    implicitHeight: 26
    padding: 0

    background: Rectangle {
        radius: 6
        color: root.checked ? Theme.surfaceContainerHighest : (root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh)
        border.color: root.checked ? root.activeColor : "transparent"
        border.width: 1
    }

    contentItem: Text {
        text: root.text
        font.pixelSize: Theme.fontLabelSmall
        font.bold: true
        color: root.checked ? root.activeColor : Theme.surfaceText
        horizontalAlignment: Text.AlignHCenter
        verticalAlignment: Text.AlignVCenter
    }

}
