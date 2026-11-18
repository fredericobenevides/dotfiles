import QtQuick
import QtQuick.Layouts
import qs.modules.bluetooth
import qs.modules.volume
import qs.theme

Rectangle {
    id: root

    property var bluetoothModal
    property var volumeModal
    property real fontScale: 1.0
    property bool hovered: bluetoothBtn.hovered || volumeBtn.hovered

    implicitWidth: buttonsRow.implicitWidth + Theme.scaledSize(15, root.fontScale)
    implicitHeight: Theme.scaledSize(24, root.fontScale)
    radius: Theme.scaledSize(12, root.fontScale)
    color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

    RowLayout {
        id: buttonsRow

        anchors.centerIn: parent
        spacing: Theme.scaledSize(4, root.fontScale)

        BluetoothButton {
            id: bluetoothBtn

            modal: root.bluetoothModal
            fontScale: root.fontScale
        }

        VolumeButton {
            id: volumeBtn

            modal: root.volumeModal
            fontScale: root.fontScale
        }

    }

    Behavior on color {
        ColorAnimation {
            duration: 160
        }

    }

}
