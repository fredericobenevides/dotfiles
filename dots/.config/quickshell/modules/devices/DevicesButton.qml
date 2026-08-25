import QtQuick
import QtQuick.Layouts
import qs.modules.bluetooth
import qs.modules.volume
import qs.theme

Rectangle {
    id: root

    property var bluetoothModal
    property var volumeModal
    property bool hovered: bluetoothBtn.hovered || volumeBtn.hovered

    implicitWidth: buttonsRow.implicitWidth + 15
    implicitHeight: 24
    radius: 12
    color: root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

    RowLayout {
        id: buttonsRow

        anchors.centerIn: parent
        spacing: 4

        BluetoothButton {
            id: bluetoothBtn

            modal: root.bluetoothModal
        }

        VolumeButton {
            id: volumeBtn

            modal: root.volumeModal
        }

    }

    Behavior on color {
        ColorAnimation {
            duration: 160
        }

    }

}
