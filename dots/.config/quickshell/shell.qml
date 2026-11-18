import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Wayland
import qs.modules.appLauncher
import qs.modules.bluetooth
import qs.modules.clipboard
import qs.modules.clock
import qs.modules.compact
import qs.modules.devices
import qs.modules.idleInhibitor
import qs.modules.mediaPlayer
import qs.modules.networkSpeed
import qs.modules.nightLight
import qs.modules.notifications
import qs.modules.power
import qs.modules.recorder
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
        const modals = [launcherMenu, bluetoothMenu, networkMenu, volumeMenu, powerMenu, clockMenu, mediaPlayerModal, weatherModal, systemInfoModal, systemUpdatesModal, notificationsModal, clipboardModal, nightLightMenu, recorderModal, compactControlsModal];
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

    CompactControlsModal {
        id: compactControlsModal

        systemInfoModal: systemInfoModal
        systemUpdatesModal: systemUpdatesModal
        nightLightModal: nightLightMenu
        networkModal: networkMenu
        weatherModal: weatherModal
        recorderModal: recorderModal
        clipboardModal: clipboardModal
        targetScreen: null

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(compactControlsModal);

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
            readonly property real fontScale: Theme.fontScaleForScreen(modelData)
            implicitHeight: Theme.scaledSize(45, topBar.fontScale)
            anchors.top: true
            anchors.left: true
            anchors.right: true
            color: "transparent"

            readonly property bool compact: width < 1100

            Rectangle {
                anchors.top: parent.top
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: parent.bottom
                anchors.topMargin: Theme.scaledSize(5, topBar.fontScale)
                anchors.leftMargin: Theme.scaledSize(5, topBar.fontScale)
                anchors.rightMargin: Theme.scaledSize(5, topBar.fontScale)
                color: Theme.surfaceContainer
                radius: 5
                opacity: 0.9

                Item {
                    anchors.fill: parent
                    anchors.leftMargin: Theme.scaledSize(10, topBar.fontScale)
                    anchors.rightMargin: Theme.scaledSize(10, topBar.fontScale)

                    // Left side
                    RowLayout {
                        anchors.left: parent.left
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: Theme.scaledSize(8, topBar.fontScale)

                        AppLauncherButton {
                            modal: launcherMenu
                            fontScale: topBar.fontScale
                        }

                        WorkspacesPanel {
                            currentMonitor: topBar.modelData
                            fontScale: topBar.fontScale
                        }

                        SystemInfoButton {
                            id: systemButton

                            modal: systemInfoModal
                            targetScreen: topBar.modelData
                            fontScale: topBar.fontScale
                            Layout.leftMargin: Theme.scaledSize(8, topBar.fontScale)
                            visible: !topBar.compact
                        }

                        SystemUpdatesButton {
                            id: systemUpdatesButton

                            modal: systemUpdatesModal
                            targetScreen: topBar.modelData
                            fontScale: topBar.fontScale
                            Layout.leftMargin: Theme.scaledSize(5, topBar.fontScale)
                            Layout.rightMargin: Theme.scaledSize(5, topBar.fontScale)
                            visible: !topBar.compact
                        }

                    }

                    // Center (fixed)
                    MediaPlayerButton {
                        id: mediaPlayerButton

                        modal: mediaPlayerModal
                        fontScale: topBar.fontScale
                        anchors.centerIn: parent
                    }

                    // Right side
                    RowLayout {
                        anchors.right: parent.right
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: Theme.scaledSize(8, topBar.fontScale)

                        IdleInhibitorButton {
                            id: idleInhibitor

                            fontScale: topBar.fontScale
                            visible: !topBar.compact
                        }

                        NightLightButton {
                            id: nightLight

                            modal: nightLightMenu
                            fontScale: topBar.fontScale
                            visible: !topBar.compact
                        }

                        NetworkSpeedButton {
                            modal: networkMenu
                            fontScale: topBar.fontScale
                            visible: !topBar.compact
                        }

                        DevicesButton {
                            bluetoothModal: bluetoothMenu
                            volumeModal: volumeMenu
                            fontScale: topBar.fontScale
                        }

                        WeatherButton {
                            modal: weatherModal
                            targetScreen: topBar.modelData
                            fontScale: topBar.fontScale
                            visible: !topBar.compact
                        }

                        RecorderButton {
                            modal: recorderModal
                            targetScreen: topBar.modelData
                            fontScale: topBar.fontScale
                            visible: !topBar.compact
                        }

                        ClipboardButton {
                            modal: clipboardModal
                            fontScale: topBar.fontScale
                            visible: !topBar.compact
                        }

                        Rectangle {
                            visible: topBar.compact
                            implicitWidth: Theme.scaledSize(24, topBar.fontScale)
                            implicitHeight: Theme.scaledSize(24, topBar.fontScale)
                            radius: Theme.scaledSize(12, topBar.fontScale)
                            color: compactControlsMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                            Text {
                                anchors.centerIn: parent
                                text: "▦"
                                font.pixelSize: Theme.scaledFont(16, topBar.fontScale)
                                color: Theme.surfaceVariantText
                            }

                            MouseArea {
                                id: compactControlsMouse

                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: {
                                    compactControlsModal.targetScreen = topBar.modelData;
                                    compactControlsModal.fontScale = topBar.fontScale;
                                    shell.toggleMenu(compactControlsModal);
                                }
                            }
                        }

                        NotificationButton {
                            modal: notificationsModal
                            fontScale: topBar.fontScale
                        }

                        ClockButton {
                            modal: clockMenu
                            fontScale: topBar.fontScale
                        }

                        PowerButton {
                            modal: powerMenu
                            fontScale: topBar.fontScale
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

    RecorderModal {
        id: recorderModal

        onVisibleChanged: {
            if (visible)
                shell.closeOtherModals(recorderModal);

        }
    }

    VolumeModal {
        id: volumeMenu

        osd: volumeOSD
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

    GlobalShortcut {
        name: "toggle-recorder"
        description: "Toggle recorder modal"
        onPressed: shell.toggleMenu(recorderModal)
    }

    GlobalShortcut {
        name: "force-record"
        description: "Start or stop a recording"
        onPressed: GSRService.toggleShortcut()
    }

}
