import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Notifications
import Quickshell.Wayland
import qs.modules.notifications
import qs.theme

PanelWindow {
    id: notificationPopup

    property var popups: []

    function removePopup(notif) {
        const arr = notificationPopup.popups.slice();
        const idx = arr.indexOf(notif);
        if (idx !== -1) {
            arr.splice(idx, 1);
            notificationPopup.popups = arr;
        }
    }

    focusable: false
    visible: false
    anchors.top: true
    anchors.right: true
    margins.top: 6
    margins.right: 6
    color: "transparent"
    implicitWidth: popupColumn.width + 6
    implicitHeight: popupColumn.height + 6
    onPopupsChanged: visible = popups.length > 0

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Connections {
        function onPopupRequested(notif) {
            const arr = notificationPopup.popups.slice();
            arr.unshift(notif);
            notificationPopup.popups = arr;
            notificationPopup.visible = true;
            if (notif.urgency !== NotificationUrgency.Critical)
                popupTimerComponent.createObject(notificationPopup, {
                "targetNotif": notif
            });

        }

        target: NotificationsService
    }

    Component {
        id: popupTimerComponent

        Timer {
            property var targetNotif: null

            interval: 6000
            running: true
            onTriggered: notificationPopup.removePopup(targetNotif)
        }

    }

    Column {
        id: popupColumn

        anchors.top: parent.top
        anchors.topMargin: 3
        anchors.right: parent.right
        anchors.rightMargin: 3
        spacing: 8

        Repeater {
            model: notificationPopup.popups

            delegate: Rectangle {
                readonly property bool isCritical: modelData.urgency === NotificationUrgency.Critical

                width: 372
                radius: 12
                color: Theme.surfaceContainerHigh
                border.color: Theme.surfaceContainerHighest
                border.width: 1
                implicitHeight: cardContent.implicitHeight + 40

                Rectangle {
                    anchors.fill: parent
                    radius: 12
                    visible: isCritical

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

                ColumnLayout {
                    id: cardContent

                    x: 12
                    width: parent.width - 22
                    anchors.verticalCenter: parent.verticalCenter
                    spacing: 4

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 8

                        Text {
                            Layout.fillWidth: true
                            text: modelData.appName || "Notification"
                            font.pixelSize: Theme.fontLabelSmall
                            font.bold: true
                            color: isCritical ? Theme.primary : Theme.surfaceVariantText
                            elide: Text.ElideRight
                        }

                        Rectangle {
                            Layout.preferredWidth: 18
                            Layout.preferredHeight: 18
                            radius: 9
                            color: closeMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainer

                            Text {
                                anchors.centerIn: parent
                                text: "\uE5CD"
                                font.family: materialSymbols.name
                                font.pixelSize: 12
                                color: Theme.surfaceVariantText
                            }

                            MouseArea {
                                id: closeMouse

                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: {
                                    NotificationsService.dismiss(modelData);
                                    notificationPopup.removePopup(modelData);
                                }
                            }

                        }

                    }

                    Text {
                        Layout.fillWidth: true
                        text: modelData.summary
                        font.pixelSize: Theme.fontLabelMedium
                        font.bold: true
                        color: Theme.surfaceText
                        wrapMode: Text.Wrap
                    }

                    Text {
                        Layout.fillWidth: true
                        visible: modelData.body && modelData.body.length > 0
                        text: modelData.body
                        font.pixelSize: Theme.fontLabelSmall
                        color: Theme.surfaceVariantText
                        wrapMode: Text.Wrap
                        maximumLineCount: 3
                        elide: Text.ElideRight
                    }

                }

                MouseArea {
                    anchors.fill: parent
                    cursorShape: Qt.PointingHandCursor
                    onClicked: notificationPopup.removePopup(modelData)
                }

            }

        }

    }

}
