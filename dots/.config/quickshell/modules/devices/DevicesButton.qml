import QtQuick
import QtQuick.Layouts
import qs.modules.bluetooth
import qs.modules.volume
import qs.theme

Rectangle {
    id: root

    property var bluetoothModal
    property var volumeModal

    implicitWidth: buttonsRow.implicitWidth + 15
    implicitHeight: 24
    radius: 12
    color: Theme.surfaceContainerHigh

    RowLayout {
        id: buttonsRow

        anchors.centerIn: parent
        spacing: 4

        BluetoothButton {
            modal: root.bluetoothModal
        }

        VolumeButton {
            modal: root.volumeModal
        }

    }

}
