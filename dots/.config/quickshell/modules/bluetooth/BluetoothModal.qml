import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Bluetooth
import Quickshell.Wayland
import qs.theme

PanelWindow {
    id: bluetoothMenu

    property real fontScale: 1.0
    function deviceIcon(device) {
        const name = ((device.name || device.deviceName || "") + " " + (device.icon || "")).toLowerCase();
        if (name.includes("headset") || name.includes("audio") || name.includes("headphone") || name.includes("airpod"))
            return "\uF01F";

        if (name.includes("mouse"))
            return "\uE323";

        if (name.includes("keyboard"))
            return "\uE312";

        if (name.includes("phone") || name.includes("iphone") || name.includes("android") || name.includes("samsung"))
            return "\uE7BA";

        if (name.includes("watch"))
            return "\uE334";

        if (name.includes("speaker"))
            return "\uE32D";

        if (name.includes("tv") || name.includes("display"))
            return "\uE63B";

        return "\uE1A7";
    }

    focusable: true
    visible: false
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    color: "transparent"
    onVisibleChanged: {
        if (visible)
            bg.forceActiveFocus();

    }

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    MouseArea {
        anchors.fill: parent
        onClicked: bluetoothMenu.visible = false
    }

    Rectangle {
        id: bg

        width: Theme.scaledSize(380, bluetoothMenu.fontScale)
        height: content.implicitHeight + Theme.scaledSize(24, bluetoothMenu.fontScale)
        anchors.top: parent.top
        anchors.topMargin: Theme.scaledSize(6, bluetoothMenu.fontScale)
        anchors.right: parent.right
        anchors.rightMargin: Theme.scaledSize(146, bluetoothMenu.fontScale)
        radius: Theme.scaledSize(16, bluetoothMenu.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: Theme.scaledSize(1, bluetoothMenu.fontScale)
        Keys.onPressed: (event) => {
            if (event.key === Qt.Key_Escape) {
                bluetoothMenu.visible = false;
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: content

            anchors.fill: parent
            anchors.margins: Theme.scaledSize(12, bluetoothMenu.fontScale)
            spacing: Theme.scaledSize(10, bluetoothMenu.fontScale)

            RowLayout {
                Layout.fillWidth: true

                Text {
                    text: "\uE1A7"
                    font.family: materialSymbols.name
                    font.pixelSize: Theme.scaledFont(18, bluetoothMenu.fontScale)
                    color: Theme.surfaceVariantText
                }

                Text {
                    Layout.fillWidth: true
                    text: "Bluetooth"
                    font.pixelSize: Theme.scaledFont(18, bluetoothMenu.fontScale)
                    font.bold: true
                    color: Theme.surfaceText
                }

            }

            Rectangle {
                Layout.fillWidth: true
                Layout.topMargin: Theme.scaledSize(8, bluetoothMenu.fontScale)
                Layout.preferredHeight: Theme.scaledSize(34, bluetoothMenu.fontScale)
                visible: Bluetooth.defaultAdapter !== null
                radius: Theme.scaledSize(7, bluetoothMenu.fontScale)
                color: scanMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                RowLayout {
                    anchors.centerIn: parent
                    spacing: Theme.scaledSize(8, bluetoothMenu.fontScale)

                    Text {
                        text: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.discovering ? "\uE047" : "\uE60F"
                        font.family: materialSymbols.name
                        font.pixelSize: Theme.scaledFont(14, bluetoothMenu.fontScale)
                        color: Theme.primary
                    }

                    Text {
                        text: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.discovering ? "Scanning" : "Scan"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelLarge, bluetoothMenu.fontScale)
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

                MouseArea {
                    id: scanMouse

                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: {
                        const adapter = Bluetooth.defaultAdapter;
                        if (adapter)
                            adapter.discovering = !adapter.discovering;

                    }
                }

            }

            ScriptModel {
                id: pairedModel

                objectProp: "address"
                values: {
                    const adapter = Bluetooth.defaultAdapter;
                    const devices = adapter ? adapter.devices : null;
                    const values = devices ? devices.values : [];
                    return values.filter((device) => {
                        return device && (device.paired || device.trusted);
                    });
                }
            }

            Text {
                Layout.fillWidth: true
                Layout.preferredHeight: Theme.scaledSize(176, bluetoothMenu.fontScale)
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter
                visible: Bluetooth.defaultAdapter !== null && !(Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.discovering) && pairedModel.values.length === 0
                text: "No paired devices"
                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, bluetoothMenu.fontScale)
                color: Theme.surfaceVariantText
            }

            ListView {
                id: pairedDevicesList

                Layout.fillWidth: true
                Layout.preferredHeight: Theme.scaledSize(176, bluetoothMenu.fontScale)
                visible: pairedModel.values.length > 0
                clip: true
                spacing: Theme.scaledSize(4, bluetoothMenu.fontScale)
                boundsBehavior: Flickable.StopAtBounds
                model: pairedModel

                delegate: Rectangle {
                    required property var modelData
                    readonly property bool isConnected: modelData.connected
                    readonly property bool isConnecting: modelData.state === BluetoothDeviceState.Connecting || modelData.pairing
                    readonly property bool isDisconnecting: modelData.state === BluetoothDeviceState.Disconnecting
                    readonly property string deviceName: modelData.name || modelData.deviceName || "Desconhecido"

                    width: pairedDevicesList.width
                    height: Theme.scaledSize(56, bluetoothMenu.fontScale)
                    radius: Theme.scaledSize(7, bluetoothMenu.fontScale)
                    color: itemMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    border.color: isConnected ? Theme.primary : "transparent"
                    border.width: isConnected ? Theme.scaledSize(1, bluetoothMenu.fontScale) : Theme.scaledSize(0, bluetoothMenu.fontScale)

                    MouseArea {
                        id: itemMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            if (isConnected) {
                                modelData.disconnect();
                            } else {
                                modelData.trusted = true;
                                modelData.connect();
                            }
                        }
                    }

                    RowLayout {
                        anchors.fill: parent
                        anchors.leftMargin: Theme.scaledSize(12, bluetoothMenu.fontScale)
                        anchors.rightMargin: Theme.scaledSize(8, bluetoothMenu.fontScale)
                        spacing: Theme.scaledSize(8, bluetoothMenu.fontScale)

                        Text {
                            text: bluetoothMenu.deviceIcon(modelData)
                            font.family: materialSymbols.name
                            font.pixelSize: Theme.scaledFont(16, bluetoothMenu.fontScale)
                            color: isConnected ? Theme.primary : Theme.surfaceVariantText
                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: Theme.scaledSize(2, bluetoothMenu.fontScale)

                            Text {
                                Layout.fillWidth: true
                                text: deviceName
                                elide: Text.ElideRight
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, bluetoothMenu.fontScale)
                                font.bold: true
                                color: Theme.surfaceText
                            }

                            RowLayout {
                                Layout.fillWidth: true
                                spacing: Theme.scaledSize(6, bluetoothMenu.fontScale)

                                Text {
                                    text: {
                                        if (isConnecting)
                                            return "Connecting...";

                                        if (isConnected)
                                            return "Connected";

                                        if (isDisconnecting)
                                            return "Disconnecting...";

                                        return "Paired";
                                    }
                                    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, bluetoothMenu.fontScale)
                                    color: isConnected ? Theme.primary : Theme.surfaceVariantText
                                }

                                Text {
                                    text: {
                                        if (modelData.batteryAvailable && modelData.battery > 0)
                                            return "• " + Math.round(modelData.battery * 100) + "%";

                                        return "";
                                    }
                                    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, bluetoothMenu.fontScale)
                                    color: Theme.surfaceVariantText
                                    visible: text.length > 0
                                }

                            }

                        }

                        Item {
                            Layout.preferredWidth: Theme.scaledSize(30, bluetoothMenu.fontScale)
                            Layout.preferredHeight: Theme.scaledSize(56, bluetoothMenu.fontScale)

                            Rectangle {
                                anchors.fill: parent
                                radius: Theme.scaledSize(7, bluetoothMenu.fontScale)
                                color: deviceMenuMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent"
                            }

                            Text {
                                anchors.centerIn: parent
                                text: "..."
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelLarge, bluetoothMenu.fontScale)
                                font.bold: true
                                color: deviceMenuMouse.containsMouse ? Theme.primary : Theme.surfaceVariantText
                            }

                            MouseArea {
                                id: deviceMenuMouse

                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: {
                                    if (deviceRowMenu.opened && deviceRowMenu.anchorItem === deviceMenuMouse.parent) {
                                        deviceRowMenu.close();
                                        return ;
                                    }
                                    const items = [];
                                    items.push({
                                        "label": isConnected ? "Disconnect" : "Connect",
                                        "action": function() {
                                            if (isConnected)
                                                modelData.disconnect();

                                        }
                                    });
                                    items.push({
                                        "label": modelData.trusted ? "Untrust" : "Trust",
                                        "action": function() {
                                            modelData.trusted = !modelData.trusted;
                                        }
                                    });
                                    items.push({
                                        "label": "Forget Device",
                                        "action": function() {
                                            modelData.forget();
                                        }
                                    });
                                    deviceRowMenu.items = items;
                                    deviceRowMenu.showFor(deviceMenuMouse.parent);
                                }
                            }

                        }

                    }

                }

            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: Theme.scaledSize(1, bluetoothMenu.fontScale)
                visible: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.discovering && availableModel.values.length > 0
                color: Theme.surfaceContainerHighest
            }

            Text {
                Layout.fillWidth: true
                Layout.preferredHeight: Theme.scaledSize(176, bluetoothMenu.fontScale)
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter
                visible: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.discovering && availableModel.values.length === 0
                text: "Searching for devices..."
                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, bluetoothMenu.fontScale)
                color: Theme.surfaceVariantText
            }

            ScriptModel {
                id: availableModel

                objectProp: "address"
                values: {
                    const adapter = Bluetooth.defaultAdapter;
                    const devices = adapter ? adapter.devices : null;
                    const values = devices ? devices.values : [];
                    return values.filter((device) => {
                        return device && !device.paired && !device.pairing && !device.blocked;
                    });
                }
            }

            ListView {
                id: availableDevicesList

                Layout.fillWidth: true
                Layout.preferredHeight: Theme.scaledSize(176, bluetoothMenu.fontScale)
                visible: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.discovering && availableModel.values.length > 0
                clip: true
                spacing: Theme.scaledSize(4, bluetoothMenu.fontScale)
                boundsBehavior: Flickable.StopAtBounds
                model: availableModel

                delegate: Rectangle {
                    required property var modelData
                    readonly property bool isBusy: modelData.pairing || modelData.state === BluetoothDeviceState.Connecting || modelData.state === BluetoothDeviceState.Disconnecting
                    readonly property string deviceName: modelData.name || modelData.deviceName || "Desconhecido"

                    width: availableDevicesList.width
                    height: Theme.scaledSize(56, bluetoothMenu.fontScale)
                    radius: Theme.scaledSize(7, bluetoothMenu.fontScale)
                    color: avMouse.containsMouse && !isBusy ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    opacity: isBusy ? 0.6 : 1

                    MouseArea {
                        id: avMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        enabled: !isBusy
                        onClicked: {
                            modelData.trusted = true;
                            modelData.pair();
                        }
                    }

                    RowLayout {
                        anchors.fill: parent
                        anchors.leftMargin: Theme.scaledSize(12, bluetoothMenu.fontScale)
                        anchors.rightMargin: Theme.scaledSize(8, bluetoothMenu.fontScale)
                        spacing: Theme.scaledSize(8, bluetoothMenu.fontScale)

                        Text {
                            text: bluetoothMenu.deviceIcon(modelData)
                            font.family: materialSymbols.name
                            font.pixelSize: Theme.scaledFont(16, bluetoothMenu.fontScale)
                            color: Theme.surfaceVariantText
                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: Theme.scaledSize(2, bluetoothMenu.fontScale)

                            Text {
                                Layout.fillWidth: true
                                text: deviceName
                                elide: Text.ElideRight
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, bluetoothMenu.fontScale)
                                font.bold: true
                                color: Theme.surfaceText
                            }

                            Text {
                                text: isBusy ? "Pairing..." : modelData.signalStrength > 0 ? modelData.signalStrength + "%" : "New device"
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, bluetoothMenu.fontScale)
                                color: Theme.surfaceVariantText
                            }

                        }

                        Text {
                            text: isBusy ? "Pairing..." : "Pair"
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, bluetoothMenu.fontScale)
                            font.bold: true
                            color: isBusy ? Theme.surfaceVariantText : Theme.primary
                        }

                    }

                }

            }

            Text {
                Layout.fillWidth: true
                Layout.preferredHeight: Theme.scaledSize(60, bluetoothMenu.fontScale)
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter
                visible: Bluetooth.defaultAdapter === null
                text: "Bluetooth unavailable"
                font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, bluetoothMenu.fontScale)
                color: Theme.surfaceVariantText
            }

        }

    }

    Popup {
        id: deviceRowMenu

        property var items: []
        property var anchorItem: null

        function showFor(anchor) {
            anchorItem = anchor;
            let target = bluetoothMenu.contentItem;
            if (!target) {
                let cur = anchor;
                while (cur) {
                    const w = cur.Window ? cur.Window.window : null;
                    if (w && w.contentItem) {
                        target = w.contentItem;
                        break;
                    }
                    cur = cur.parent;
                }
            }
            if (!target) {
                open();
                return ;
            }
            const p = anchor.mapToItem(target, 0, 0);
            x = Math.max(2, Math.min(target.width - width - 2, p.x - width - 8));
            y = Math.max(2, Math.min(target.height - height - 2, p.y + anchor.height / 2 - height / 2));
            open();
        }

        width: Theme.scaledSize(180, bluetoothMenu.fontScale)
        leftPadding: Theme.scaledSize(4, bluetoothMenu.fontScale)
        rightPadding: Theme.scaledSize(4, bluetoothMenu.fontScale)
        topPadding: Theme.scaledSize(4, bluetoothMenu.fontScale)
        bottomPadding: Theme.scaledSize(4, bluetoothMenu.fontScale)
        modal: true
        dim: false
        closePolicy: Popup.CloseOnPressOutside | Popup.CloseOnEscape

        background: Rectangle {
            color: Theme.surfaceContainerHigh
            radius: Theme.scaledSize(10, bluetoothMenu.fontScale)
            border.color: Theme.outlineVariant
            border.width: Theme.scaledSize(1, bluetoothMenu.fontScale)
        }

        contentItem: ColumnLayout {
            spacing: Theme.scaledSize(2, bluetoothMenu.fontScale)

            Repeater {
                model: deviceRowMenu.items

                delegate: Rectangle {
                    required property var modelData

                    Layout.fillWidth: true
                    Layout.preferredHeight: Theme.scaledSize(32, bluetoothMenu.fontScale)
                    radius: Theme.scaledSize(6, bluetoothMenu.fontScale)
                    color: rowItemMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent"

                    Text {
                        anchors.left: parent.left
                        anchors.leftMargin: Theme.scaledSize(12, bluetoothMenu.fontScale)
                        anchors.verticalCenter: parent.verticalCenter
                        text: modelData.label
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, bluetoothMenu.fontScale)
                        color: rowItemMouse.containsMouse ? Theme.primary : Theme.surfaceText
                    }

                    MouseArea {
                        id: rowItemMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            deviceRowMenu.close();
                            modelData.action();
                        }
                    }

                }

            }

        }

    }

}
