import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Wayland
import qs.modules.appLauncher
import qs.modules.bluetooth
import qs.modules.clipboard
import qs.modules.clock
import qs.modules.devices
import qs.modules.idleInhibitor
import qs.modules.mediaPlayer
import qs.modules.networkSpeed
import qs.modules.nightLight
import qs.modules.notifications
import qs.modules.power
import qs.modules.systemInfo
import qs.modules.systemUpdates
import qs.modules.volume
import qs.modules.vpn
import qs.modules.weather
import qs.modules.workspaces
import qs.theme

ShellRoot {
    id: shell

    property var idleInhibitorRef
    property var nightLightRef

    function closeOtherModals(exclude) {
        const modals = [launcherMenu, bluetoothMenu, networkMenu, volumeMenu, powerMenu, clockMenu, mediaPlayerModal, weatherModal, systemInfoModal, systemUpdatesModal, notificationsModal, clipboardModal, nightLightMenu];
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

    SystemInfoModal {
        id: systemInfoModal

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(systemInfoModal);

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

    ClipboardModal {
        id: clipboardModal

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(clipboardModal);

        }
    }

    NightLightModal {
        id: nightLightMenu

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(nightLightMenu);

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

                        SystemInfoButton {
                            id: systemButton

                            modal: systemInfoModal
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

                        NightLightButton {
                            id: nightLight

                            modal: nightLightMenu
                        }

                        NetworkSpeedButton {
                            modal: networkMenu
                        }

                        DevicesButton {
                            bluetoothModal: bluetoothMenu
                            volumeModal: volumeMenu
                        }

                        WeatherButton {
                            modal: weatherModal
                        }

                        ClipboardButton {
                            modal: clipboardModal
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

    NetworkSpeedModal {
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

    GlobalShortcut {
        name: "toggle-night-light"
        description: "Toggle night light"
        onPressed: {
            if (shell.nightLightRef)
                shell.nightLightRef.toggle();

        }
    }

    GlobalShortcut {
        name: "toggle-night-light-modal"
        description: "Toggle night light modal"
        onPressed: shell.toggleMenu(nightLightMenu)
    }

    GlobalShortcut {
        name: "toggle-clipboard"
        description: "Toggle clipboard history"
        onPressed: shell.toggleMenu(clipboardModal)
    }

    GlobalShortcut {
        name: "toggle-system"
        description: "Toggle system modal"
        onPressed: shell.toggleMenu(systemInfoModal)
    }

    GlobalShortcut {
        name: "toggle-system-updates"
        description: "Toggle system updates modal"
        onPressed: shell.toggleMenu(systemUpdatesModal)
    }

    GlobalShortcut {
        name: "toggle-media-player"
        description: "Toggle media player modal"
        onPressed: shell.toggleMenu(mediaPlayerModal)
    }

    GlobalShortcut {
        name: "toggle-clock"
        description: "Toggle clock modal"
        onPressed: shell.toggleMenu(clockMenu)
    }

}
