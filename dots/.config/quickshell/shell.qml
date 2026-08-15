import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Wayland
// import qs.modules.bar
import qs.modules.appLauncher
import qs.modules.bluetooth
import qs.modules.clock
import qs.modules.idleInhibitor
import qs.modules.mediaPlayer
import qs.modules.network
import qs.modules.notifications
import qs.modules.power
import qs.modules.system
import qs.modules.systemUpdates
import qs.modules.volume
import qs.modules.weather
import qs.modules.workspaces
import qs.theme

ShellRoot {
    id: shell

    function closeOtherModals(exclude) {
        const modals = [launcherMenu, bluetoothMenu, networkMenu, volumeMenu, powerMenu, clockMenu, mediaPlayerModal, weatherModal, systemModal, systemUpdatesModal, notificationsModal];
        for (let i = 0; i < modals.length; i++) {
            if (modals[i] !== exclude && modals[i].visible)
                modals[i].visible = false;

        }
    }

    function toggleMenu(menu) {
        if (menu.visible) {
            menu.visible = false;
            return ;
        }
        menu.visible = true;
    }

    MediaPlayerModal {
        id: mediaPlayerModal

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(mediaPlayerModal);

        }
    }

    WeatherModal {
        id: weatherModal

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(weatherModal);

        }
    }

    SystemModal {
        id: systemModal

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(systemModal);

        }
    }

    SystemUpdatesModal {
        id: systemUpdatesModal

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(systemUpdatesModal);

        }
    }

    NotificationsModal {
        id: notificationsModal

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(notificationsModal);

        }
    }

    NotificationPopup {
        id: notificationPopup
    }

    Variants {
        model: Quickshell.screens

        PanelWindow {
            id: topBar

            required property var modelData

            screen: modelData
            implicitHeight: 45
            anchors.top: true
            anchors.left: true
            anchors.right: true
            color: "transparent"

            Rectangle {
                anchors.top: parent.top
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: parent.bottom
                anchors.topMargin: 5
                anchors.leftMargin: 5
                anchors.rightMargin: 5
                color: Theme.surfaceContainer
                radius: 5

                Item {
                    anchors.fill: parent
                    anchors.leftMargin: 10
                    anchors.rightMargin: 10

                    // Left side
                    RowLayout {
                        anchors.left: parent.left
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: 8

                        AppLauncherButton {
                        }

                        WorkspacesPanel {
                            currentMonitor: topBar.modelData
                        }

                        SystemButton {
                            id: systemButton

                            modal: systemModal
                            Layout.leftMargin: 8
                        }

                        SystemUpdatesButton {
                            id: systemUpdatesButton

                            modal: systemUpdatesModal
                            Layout.leftMargin: 5
                            Layout.rightMargin: 5
                        }

                    }

                    // Center (fixed)
                    Item {
                        anchors.centerIn: parent
                        implicitHeight: mediaPlayerButtonBg.height
                        implicitWidth: mediaPlayerButtonBg.width

                        Rectangle {
                            id: mediaPlayerButtonBg

                            anchors.centerIn: parent
                            height: 24
                            width: mediaPlayerButton.implicitWidth + 16
                            radius: 12
                            color: Theme.surfaceContainerHigh

                            MediaPlayerButton {
                                id: mediaPlayerButton

                                modal: mediaPlayerModal
                                anchors.fill: parent
                            }

                        }

                    }

                    // Right side
                    RowLayout {
                        anchors.right: parent.right
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: 8

                        Rectangle {
                            height: 24
                            width: 24
                            radius: 12
                            color: idleInhibitorButton.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                            IdleInhibitorButton {
                                id: idleInhibitorButton

                                anchors.centerIn: parent
                            }

                            Behavior on color {
                                ColorAnimation {
                                    duration: 160
                                }

                            }

                        }

                        Rectangle {
                            height: 24
                            width: systemButtonsRow.implicitWidth + 8
                            radius: 12
                            color: Theme.surfaceContainerHigh

                            RowLayout {
                                id: systemButtonsRow

                                anchors.centerIn: parent
                                spacing: 4

                                BluetoothButton {
                                }

                                NetworkButton {
                                }

                                VolumeButton {
                                }

                            }

                        }

                        Rectangle {
                            height: 24
                            width: weatherButton.implicitWidth + 8
                            radius: 12
                            color: Theme.surfaceContainerHigh

                            WeatherButton {
                                id: weatherButton

                                modal: weatherModal
                                anchors.fill: parent
                            }

                        }

                        Rectangle {
                            height: 24
                            width: 24
                            radius: 12
                            color: Theme.surfaceContainerHigh

                            NotificationButton {
                                id: notificationButton

                                modal: notificationsModal
                                anchors.fill: parent
                            }

                        }

                        ClockButton {
                        }

                        PowerButton {
                        }

                    }

                }

            }

        }

    }

    AppLauncherModal {
        id: launcherMenu

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(launcherMenu);

        }
    }

    BluetoothModal {
        id: bluetoothMenu

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(bluetoothMenu);

        }
    }

    NetworkModal {
        id: networkMenu

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(networkMenu);

        }
    }

    VolumeModal {
        id: volumeMenu

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(volumeMenu);

        }
    }

    PowerModal {
        id: powerMenu

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(powerMenu);

        }
    }

    ClockModal {
        id: clockMenu

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(clockMenu);

        }
    }

    GlobalShortcut {
        name: "toggle-launcher"
        description: "Toggle app launcher"
        onPressed: shell.toggleMenu(launcherMenu)
    }

}
