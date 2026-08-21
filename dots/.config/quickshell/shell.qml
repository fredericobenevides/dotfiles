import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Wayland
import qs.modules.appLauncher
import qs.modules.bluetooth
import qs.modules.clock
import qs.modules.devices
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

    property var idleInhibitorRef

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
                opacity: 0.9

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
                            modal: launcherMenu
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
                    MediaPlayerButton {
                        id: mediaPlayerButton

                        modal: mediaPlayerModal
                        anchors.centerIn: parent
                    }

                    // Right side
                    RowLayout {
                        anchors.right: parent.right
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: 8

                        IdleInhibitorButton {
                            id: idleInhibitor
                        }

                        DevicesButton {
                            bluetoothModal: bluetoothMenu
                            networkModal: networkMenu
                            volumeModal: volumeMenu
                        }

                        WeatherButton {
                            modal: weatherModal
                        }

                        NotificationButton {
                            modal: notificationsModal
                        }

                        ClockButton {
                            modal: clockMenu
                        }

                        PowerButton {
                            modal: powerMenu
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

    VolumeOSD {
        id: volumeOSD
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

    GlobalShortcut {
        name: "toggle-notifications"
        description: "Toggle notifications modal"
        onPressed: {
            if (notificationsModal.visible && !notificationsModal.showHistory) {
                notificationsModal.visible = false;
            } else {
                notificationsModal.showHistory = false;
                notificationsModal.visible = true;
                shell.closeOtherModals(notificationsModal);
            }
        }
    }

    GlobalShortcut {
        name: "toggle-notifications-history"
        description: "Toggle notifications history"
        onPressed: {
            if (notificationsModal.visible && notificationsModal.showHistory) {
                notificationsModal.visible = false;
            } else {
                notificationsModal.showHistory = true;
                notificationsModal.visible = true;
                shell.closeOtherModals(notificationsModal);
            }
        }
    }

    GlobalShortcut {
        name: "toggle-power"
        description: "Toggle power menu"
        onPressed: shell.toggleMenu(powerMenu)
    }

    GlobalShortcut {
        name: "toggle-idle-inhibitor"
        description: "Toggle idle inhibitor"
        onPressed: {
            if (shell.idleInhibitorRef)
                shell.idleInhibitorRef.toggle();

        }
    }

}
