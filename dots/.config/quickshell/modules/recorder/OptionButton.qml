import QtQuick
import QtQuick.Controls
import qs.theme

Button {
    id: root

    property real fontScale: 1.0
    property color activeColor: Theme.primary

    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, root.fontScale)
    font.bold: true
    implicitWidth: Theme.scaledSize(44, root.fontScale)
    implicitHeight: Theme.scaledSize(26, root.fontScale)
    padding: Theme.scaledSize(0, root.fontScale)

    background: Rectangle {
        radius: Theme.scaledSize(6, root.fontScale)
        color: root.checked ? Theme.surfaceContainerHighest : (root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh)
        border.color: root.checked ? root.activeColor : "transparent"
        border.width: Theme.scaledSize(1, root.fontScale)
    }

    contentItem: Text {
        text: root.text
        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, root.fontScale)
        font.bold: true
        color: root.checked ? root.activeColor : Theme.surfaceText
        horizontalAlignment: Text.AlignHCenter
        verticalAlignment: Text.AlignVCenter
    }

}
