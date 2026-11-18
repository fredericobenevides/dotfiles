import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Notifications
import Quickshell.Wayland
import qs.modules.notifications
import qs.theme

PanelWindow {
    id: notificationsModal

    property real fontScale: 1.0
    property bool showHistory: false
    property int selectedIndex: -1
    property bool anyCardHovered: false
    readonly property var groupedItems: {
        const items = NotificationsService.items;
        const groups = {
        };
        const order = [];
        for (let i = 0; i < items.length; i++) {
            const app = items[i].appName || "Notification";
            if (!groups[app]) {
                groups[app] = {
                    "appName": app,
                    "count": 0,
                    "latest": items[i],
                    "notifications": []
                };
                order.push(app);
            }
            groups[app].notifications.push(items[i]);
            groups[app].count++;
            groups[app].latest = items[i];
        }
        const result = [];
        for (let i = 0; i < order.length; i++) result.push(groups[order[i]])
        return result;
    }

    function open() {
        visible = true;
        const list = showHistory ? NotificationsService.history : groupedItems;
        selectedIndex = list.length > 0 ? 0 : -1;
    }

    function closeModal() {
        visible = false;
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
            focusInput.forceActiveFocus();

    }

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    MouseArea {
        anchors.fill: parent
        onClicked: notificationsModal.closeModal()
    }

    Rectangle {
        id: bg

        width: Theme.scaledSize(400, notificationsModal.fontScale)
        height: Theme.scaledSize(420, notificationsModal.fontScale)
        anchors.top: parent.top
        anchors.topMargin: Theme.scaledSize(6, notificationsModal.fontScale)
        anchors.right: parent.right
        anchors.rightMargin: Theme.scaledSize(6, notificationsModal.fontScale)
        radius: Theme.scaledSize(16, notificationsModal.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: Theme.scaledSize(1, notificationsModal.fontScale)
        focus: true
        Keys.onPressed: function(event) {
            const list = notificationsModal.showHistory ? NotificationsService.history : notificationsModal.groupedItems;
            const count = list.length;
            if (event.key === Qt.Key_Escape) {
                notificationsModal.closeModal();
                event.accepted = true;
            } else if (event.key === Qt.Key_Down) {
                notificationsModal.selectedIndex = Math.min(notificationsModal.selectedIndex + 1, count - 1);
                event.accepted = true;
            } else if (event.key === Qt.Key_Up) {
                notificationsModal.selectedIndex = Math.max(notificationsModal.selectedIndex - 1, 0);
                event.accepted = true;
            } else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                if (notificationsModal.selectedIndex >= 0 && notificationsModal.selectedIndex < count) {
                    const item = list[notificationsModal.selectedIndex];
                    if (notificationsModal.showHistory)
                        NotificationsService.removeFromHistory(item);
                    else
                        NotificationsService.dismissApp(item.appName);
                    notificationsModal.selectedIndex = Math.max(0, notificationsModal.selectedIndex - 1);
                }
                event.accepted = true;
            } else if (event.key === Qt.Key_Delete && (event.modifiers & Qt.ControlModifier)) {
                if (notificationsModal.showHistory)
                    NotificationsService.clearHistory();
                else
                    NotificationsService.dismissAll();
                notificationsModal.selectedIndex = -1;
                event.accepted = true;
            } else if (event.key === Qt.Key_Delete) {
                if (notificationsModal.selectedIndex >= 0 && notificationsModal.selectedIndex < count) {
                    const item = list[notificationsModal.selectedIndex];
                    if (notificationsModal.showHistory)
                        NotificationsService.removeFromHistory(item);
                    else
                        NotificationsService.dismissApp(item.appName);
                    notificationsModal.selectedIndex = Math.max(0, notificationsModal.selectedIndex - 1);
                }
                event.accepted = true;
            } else if (event.key === Qt.Key_Left || event.key === Qt.Key_Right) {
                notificationsModal.showHistory = !notificationsModal.showHistory;
                const list = notificationsModal.showHistory ? NotificationsService.history : notificationsModal.groupedItems;
                notificationsModal.selectedIndex = list.length > 0 ? 0 : -1;
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
            hoverEnabled: true
            onExited: notificationsModal.anyCardHovered = false
        }

        TextInput {
            id: focusInput

            width: Theme.scaledSize(1, notificationsModal.fontScale)
            height: Theme.scaledSize(1, notificationsModal.fontScale)
            color: "transparent"
            focus: true
            Keys.onPressed: (event) => {
                if (event.key === Qt.Key_Delete && (event.modifiers & Qt.ControlModifier)) {
                    if (notificationsModal.showHistory)
                        NotificationsService.clearHistory();
                    else
                        NotificationsService.dismissAll();
                    notificationsModal.selectedIndex = -1;
                    event.accepted = true;
                } else if (event.key === Qt.Key_Delete) {
                    const list = notificationsModal.showHistory ? NotificationsService.history : notificationsModal.groupedItems;
                    const count = list.length;
                    if (notificationsModal.selectedIndex >= 0 && notificationsModal.selectedIndex < count) {
                        const item = list[notificationsModal.selectedIndex];
                        if (notificationsModal.showHistory)
                            NotificationsService.removeFromHistory(item);
                        else
                            NotificationsService.dismissApp(item.appName);
                        notificationsModal.selectedIndex = Math.max(0, notificationsModal.selectedIndex - 1);
                    }
                    event.accepted = true;
                }
            }
        }

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: Theme.scaledSize(12, notificationsModal.fontScale)
            spacing: Theme.scaledSize(10, notificationsModal.fontScale)

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, notificationsModal.fontScale)

                Text {
                    text: "\uE7F5"
                    font.family: materialSymbols.name
                    font.pixelSize: Theme.scaledFont(18, notificationsModal.fontScale)
                    color: Theme.primary
                }

                Text {
                    Layout.fillWidth: true
                    text: "Notifications"
                    font.pixelSize: Theme.scaledFont(18, notificationsModal.fontScale)
                    font.bold: true
                    color: Theme.surfaceText
                }

                Rectangle {
                    Layout.preferredWidth: Theme.scaledSize(36, notificationsModal.fontScale)
                    Layout.preferredHeight: Theme.scaledSize(36, notificationsModal.fontScale)
                    radius: Theme.scaledSize(18, notificationsModal.fontScale)
                    color: clearMouse.containsMouse ? Theme.error : "transparent"
                    visible: notificationsModal.showHistory ? NotificationsService.history.length > 0 : NotificationsService.count > 0

                    Text {
                        anchors.centerIn: parent
                        text: "\uE16C"
                        font.family: materialSymbols.name
                        font.pixelSize: Theme.scaledFont(22, notificationsModal.fontScale)
                        color: Theme.surfaceText
                    }

                    MouseArea {
                        id: clearMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            if (notificationsModal.showHistory)
                                NotificationsService.clearHistory();
                            else
                                NotificationsService.dismissAll();
                        }
                    }

                }

            }

            RowLayout {
                Layout.fillWidth: true
                Layout.topMargin: Theme.scaledSize(8, notificationsModal.fontScale)
                spacing: Theme.scaledSize(6, notificationsModal.fontScale)

                Rectangle {
                    Layout.preferredWidth: Theme.scaledSize(88, notificationsModal.fontScale)
                    Layout.preferredHeight: Theme.scaledSize(26, notificationsModal.fontScale)
                    radius: Theme.scaledSize(13, notificationsModal.fontScale)
                    color: !notificationsModal.showHistory ? Theme.primary : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "Current (" + NotificationsService.count + ")"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, notificationsModal.fontScale)
                        font.bold: true
                        color: !notificationsModal.showHistory ? Theme.primaryText : Theme.surfaceVariantText
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: notificationsModal.showHistory = false
                    }

                }

                Rectangle {
                    Layout.preferredWidth: Theme.scaledSize(88, notificationsModal.fontScale)
                    Layout.preferredHeight: Theme.scaledSize(26, notificationsModal.fontScale)
                    radius: Theme.scaledSize(13, notificationsModal.fontScale)
                    color: notificationsModal.showHistory ? Theme.primary : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "History (" + NotificationsService.history.length + ")"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, notificationsModal.fontScale)
                        font.bold: true
                        color: notificationsModal.showHistory ? Theme.primaryText : Theme.surfaceVariantText
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: notificationsModal.showHistory = true
                    }

                }

                Item {
                    Layout.fillWidth: true
                }

            }

            ListView {
                Layout.fillWidth: true
                Layout.fillHeight: true
                clip: true
                spacing: Theme.scaledSize(14, notificationsModal.fontScale)
                model: notificationsModal.showHistory ? NotificationsService.history : notificationsModal.groupedItems
                currentIndex: notificationsModal.selectedIndex
                highlightFollowsCurrentItem: true
                highlightMoveDuration: 100

                ScrollBar.vertical: ScrollBar {
                    policy: ScrollBar.AsNeeded
                }

                delegate: Rectangle {
                    id: notificationCard

                    readonly property bool isGroup: (modelData.count || 0) > 1
                    readonly property int groupCount: isGroup ? modelData.count : 0
                    readonly property var notif: modelData.latest !== undefined ? modelData.latest : modelData
                    readonly property bool critical: notificationsModal.showHistory ? (modelData.urgency === 2) : (notif.urgency === NotificationUrgency.Critical)
                    readonly property bool hovered: cardHover.containsMouse
                    property bool expandedGroup: false
                    property real collapsedHeight: 0

                    width: ListView.view.width
                    clip: true
                    radius: Theme.scaledSize(12, notificationsModal.fontScale)
                    color: ((ListView.isCurrentItem && !notificationsModal.anyCardHovered) || hovered) ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    border.color: ((ListView.isCurrentItem && !notificationsModal.anyCardHovered) || hovered) ? Theme.primary : (critical ? Theme.primary : Theme.surfaceContainerHighest)
                    border.width: Theme.scaledSize(1, notificationsModal.fontScale)
                    implicitHeight: cardContent.implicitHeight + Theme.scaledSize(40, notificationsModal.fontScale)

                    MouseArea {
                        id: cardHover

                        anchors.fill: parent
                        hoverEnabled: true
                        acceptedButtons: Qt.NoButton
                        onEntered: notificationsModal.anyCardHovered = true
                    }

                    Rectangle {
                        anchors.fill: parent
                        radius: Theme.scaledSize(12, notificationsModal.fontScale)
                        visible: notificationCard.critical

                        gradient: Gradient {
                            orientation: Gradient.Horizontal

                            GradientStop {
                                position: 0
                                color: Theme.primary
                            }

                            GradientStop {
                                position: 0.02
                                color: Theme.primary
                            }

                            GradientStop {
                                position: 0.021
                                color: "transparent"
                            }

                        }

                    }

                    MouseArea {
                        anchors.fill: parent
                        visible: notificationCard.isGroup
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            if (!notificationCard.expandedGroup)
                                notificationCard.collapsedHeight = notificationCard.height;

                            notificationCard.expandedGroup = !notificationCard.expandedGroup;
                        }
                    }

                    Rectangle {
                        anchors.right: parent.right
                        anchors.top: parent.top
                        anchors.margins: Theme.scaledSize(8, notificationsModal.fontScale)
                        width: Theme.scaledSize(18, notificationsModal.fontScale)
                        height: Theme.scaledSize(18, notificationsModal.fontScale)
                        radius: Theme.scaledSize(9, notificationsModal.fontScale)
                        color: dismissMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainer

                        Text {
                            anchors.centerIn: parent
                            text: "\uE5CD"
                            font.family: materialSymbols.name
                            font.pixelSize: Theme.scaledFont(12, notificationsModal.fontScale)
                            color: Theme.surfaceVariantText
                        }

                        MouseArea {
                            id: dismissMouse

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: {
                                if (notificationsModal.showHistory)
                                    NotificationsService.removeFromHistory(modelData);
                                else
                                    NotificationsService.dismissApp(notificationCard.notif.appName);
                            }
                        }

                    }

                    ColumnLayout {
                        id: cardContent

                        x: 18
                        width: parent.width - Theme.scaledSize(28, notificationsModal.fontScale)
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: Theme.scaledSize(4, notificationsModal.fontScale)

                        RowLayout {
                            Layout.fillWidth: true
                            spacing: Theme.scaledSize(8, notificationsModal.fontScale)

                            Text {
                                text: notif.appName || "Notification"
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, notificationsModal.fontScale)
                                font.bold: true
                                color: critical ? Theme.primary : Theme.surfaceVariantText
                                elide: Text.ElideRight
                                Layout.maximumWidth: Theme.scaledSize(260, notificationsModal.fontScale)
                            }

                            Rectangle {
                                visible: notificationCard.groupCount > 1
                                Layout.preferredWidth: Theme.scaledSize(18, notificationsModal.fontScale)
                                Layout.preferredHeight: Theme.scaledSize(18, notificationsModal.fontScale)
                                radius: Theme.scaledSize(9, notificationsModal.fontScale)
                                color: Theme.primary

                                Text {
                                    anchors.centerIn: parent
                                    text: notificationCard.groupCount
                                    font.pixelSize: Theme.scaledFont(9, notificationsModal.fontScale)
                                    font.bold: true
                                    color: Theme.primaryText
                                }

                            }

                        }

                        Rectangle {
                            Layout.fillWidth: true
                            Layout.preferredHeight: Theme.scaledSize(120, notificationsModal.fontScale)
                            visible: !!notificationCard.notif.image && notificationCard.notif.image !== ""
                            radius: Theme.scaledSize(8, notificationsModal.fontScale)
                            clip: true
                            color: Theme.surfaceContainer

                            Image {
                                anchors.fill: parent
                                source: notificationCard.notif.image
                                fillMode: Image.PreserveAspectCrop
                                asynchronous: true
                            }

                        }

                        Text {
                            Layout.fillWidth: true
                            visible: !notificationCard.expandedGroup
                            text: notificationCard.notif.summary
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, notificationsModal.fontScale)
                            font.bold: true
                            color: Theme.surfaceText
                            wrapMode: Text.Wrap
                        }

                        Text {
                            Layout.fillWidth: true
                            visible: !notificationCard.expandedGroup && notificationCard.notif.body && notificationCard.notif.body.length > 0
                            text: notificationCard.notif.body
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, notificationsModal.fontScale)
                            color: Theme.surfaceVariantText
                            wrapMode: Text.Wrap
                            maximumLineCount: 3
                            elide: Text.ElideRight
                        }

                        ListView {
                            Layout.fillWidth: true
                            Layout.preferredHeight: Math.max(0, modelData.notifications.length * (notificationCard.collapsedHeight + 14) - 14)
                            Layout.topMargin: Theme.scaledSize(15, notificationsModal.fontScale)
                            visible: notificationCard.isGroup && notificationCard.expandedGroup
                            spacing: Theme.scaledSize(10, notificationsModal.fontScale)
                            interactive: false
                            model: notificationCard.isGroup ? modelData.notifications : []

                            delegate: Rectangle {
                                width: ListView.view.width
                                height: notificationCard.collapsedHeight
                                radius: Theme.scaledSize(12, notificationsModal.fontScale)
                                color: Theme.surfaceContainerHigh
                                border.color: Theme.surfaceContainerHighest
                                border.width: Theme.scaledSize(1, notificationsModal.fontScale)
                                clip: true

                                ColumnLayout {
                                    id: msgContent

                                    x: 12
                                    width: parent.width - Theme.scaledSize(22, notificationsModal.fontScale)
                                    anchors.verticalCenter: parent.verticalCenter
                                    spacing: Theme.scaledSize(4, notificationsModal.fontScale)

                                    RowLayout {
                                        Layout.fillWidth: true
                                        spacing: Theme.scaledSize(8, notificationsModal.fontScale)

                                        Text {
                                            Layout.fillWidth: true
                                            text: modelData.appName || "Notification"
                                            font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, notificationsModal.fontScale)
                                            font.bold: true
                                            color: modelData.urgency === NotificationUrgency.Critical ? Theme.primary : Theme.surfaceVariantText
                                            elide: Text.ElideRight
                                        }

                                        Rectangle {
                                            Layout.preferredWidth: Theme.scaledSize(18, notificationsModal.fontScale)
                                            Layout.preferredHeight: Theme.scaledSize(18, notificationsModal.fontScale)
                                            radius: Theme.scaledSize(9, notificationsModal.fontScale)
                                            color: msgDismissMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainer

                                            Text {
                                                anchors.centerIn: parent
                                                text: "\uE5CD"
                                                font.family: materialSymbols.name
                                                font.pixelSize: Theme.scaledFont(12, notificationsModal.fontScale)
                                                color: Theme.surfaceVariantText
                                            }

                                            MouseArea {
                                                id: msgDismissMouse

                                                anchors.fill: parent
                                                hoverEnabled: true
                                                cursorShape: Qt.PointingHandCursor
                                                onClicked: NotificationsService.dismiss(modelData)
                                            }

                                        }

                                    }

                                    Text {
                                        Layout.fillWidth: true
                                        text: modelData.summary
                                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, notificationsModal.fontScale)
                                        font.bold: true
                                        color: Theme.surfaceText
                                        wrapMode: Text.Wrap
                                    }

                                    Text {
                                        Layout.fillWidth: true
                                        visible: modelData.body && modelData.body.length > 0
                                        text: modelData.body
                                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, notificationsModal.fontScale)
                                        color: Theme.surfaceVariantText
                                        wrapMode: Text.Wrap
                                    }

                                }

                            }

                        }

                    }

                }

            }

        }

    }

}
