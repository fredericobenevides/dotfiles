import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs.modules.systemUpdates
import qs.theme

PanelWindow {
    id: systemUpdatesModal

    property bool showRepo: false
    property bool showAur: false
    readonly property var visiblePackages: {
        let list = [];
        if (systemUpdatesModal.showRepo)
            for (let i = 0; i < SystemUpdatesService.repoPackages.length; i++) list.push({
            "type": "repo",
            "name": SystemUpdatesService.repoPackages[i]
        });

        if (systemUpdatesModal.showAur)
            for (let i = 0; i < SystemUpdatesService.aurPackages.length; i++) list.push({
            "type": "aur",
            "name": SystemUpdatesService.aurPackages[i]
        });

        return list;
    }
    readonly property bool visibleLoading: (systemUpdatesModal.showRepo && SystemUpdatesService.repoLoading) || (systemUpdatesModal.showAur && SystemUpdatesService.aurLoading)
    readonly property int listHeight: 260
    readonly property int packageBoxHeight: 16 + 4 + systemUpdatesModal.listHeight + 20

    function open() {
        visible = true;
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
        if (visible) {
            systemUpdatesModal.showRepo = true;
            systemUpdatesModal.showAur = true;
            SystemUpdatesService.loadRepoPackages();
            SystemUpdatesService.loadAurPackages();
            SystemUpdatesService.refresh();
            bg.forceActiveFocus();
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: systemUpdatesModal.closeModal()
    }

    Rectangle {
        id: bg

        width: 380
        implicitHeight: contentColumn.implicitHeight + 24
        anchors.top: parent.top
        anchors.topMargin: 6
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.horizontalCenterOffset: -520
        radius: 20
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: 1
        Keys.onPressed: (event) => {
            if (event.key === Qt.Key_Escape) {
                systemUpdatesModal.closeModal();
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
        }

        FontLoader {
            id: materialSymbols

            source: Qt.resolvedUrl("/usr/share/quickshell/dms/assets/fonts/material-design-icons/variablefont/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
        }

        ColumnLayout {
            id: contentColumn

            anchors.fill: parent
            anchors.margins: 16
            spacing: 12

            RowLayout {
                Layout.fillWidth: true
                spacing: 8

                Text {
                    text: "\uE923"
                    font.family: materialSymbols.name
                    font.pixelSize: 18
                    color: Theme.primary
                }

                Text {
                    Layout.fillWidth: true
                    text: "System Updates"
                    font.pixelSize: 16
                    font.bold: true
                    color: Theme.surfaceText
                }

                Rectangle {
                    Layout.preferredWidth: 24
                    Layout.preferredHeight: 24
                    radius: 12
                    color: closeMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "\uE5CD"
                        font.family: materialSymbols.name
                        font.pixelSize: 14
                        color: Theme.surfaceVariantText
                    }

                    MouseArea {
                        id: closeMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: systemUpdatesModal.closeModal()
                    }

                }

            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 8

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 26
                    radius: 13
                    color: Theme.primary

                    Row {
                        anchors.centerIn: parent
                        spacing: 5

                        Text {
                            text: "Repo:"
                            font.pixelSize: Theme.fontLabelMedium
                            font.bold: true
                            color: Theme.primaryText
                            anchors.verticalCenter: parent.verticalCenter
                        }

                        Text {
                            text: SystemUpdatesService.repoCount
                            font.pixelSize: Theme.fontLabelMedium
                            font.bold: true
                            color: Theme.primaryText
                            anchors.verticalCenter: parent.verticalCenter
                        }

                    }

                }

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 26
                    radius: 13
                    color: Theme.primary

                    Row {
                        anchors.centerIn: parent
                        spacing: 5

                        Text {
                            text: "AUR:"
                            font.pixelSize: Theme.fontLabelMedium
                            font.bold: true
                            color: Theme.primaryText
                            anchors.verticalCenter: parent.verticalCenter
                        }

                        Text {
                            text: SystemUpdatesService.aurCount
                            font.pixelSize: Theme.fontLabelMedium
                            font.bold: true
                            color: Theme.primaryText
                            anchors.verticalCenter: parent.verticalCenter
                        }

                    }

                }

            }

            Rectangle {
                Layout.fillWidth: true
                height: systemUpdatesModal.packageBoxHeight
                implicitHeight: systemUpdatesModal.packageBoxHeight
                visible: systemUpdatesModal.showRepo || systemUpdatesModal.showAur
                radius: 12
                color: Theme.surfaceContainerHigh
                clip: true

                Column {
                    id: packagesColumn

                    width: parent.width
                    anchors.top: parent.top
                    anchors.topMargin: 10
                    anchors.bottom: parent.bottom
                    anchors.bottomMargin: 10
                    spacing: 4

                    Text {
                        height: 16
                        verticalAlignment: Text.AlignVCenter
                        visible: systemUpdatesModal.visibleLoading
                        text: "Loading..."
                        font.pixelSize: Theme.fontLabelSmall
                        color: Theme.surfaceVariantText
                    }

                    ListView {
                        id: packageList

                        width: parent.width
                        height: systemUpdatesModal.listHeight
                        clip: true
                        interactive: systemUpdatesModal.visiblePackages.length > 10
                        model: systemUpdatesModal.visiblePackages

                        ScrollBar.vertical: ScrollBar {
                            policy: ScrollBar.AsNeeded
                            width: 14
                            minimumSize: 0.25

                            contentItem: Rectangle {
                                implicitWidth: 14
                                radius: 7
                                color: parent.pressed ? Theme.surfaceVariant : Theme.surfaceContainerHighest
                            }

                        }

                        delegate: Item {
                            readonly property bool repoHeader: modelData.type === "repo" && index === 0 && systemUpdatesModal.showRepo && SystemUpdatesService.repoPackages.length > 0
                            readonly property bool aurHeader: modelData.type === "aur" && index === (systemUpdatesModal.showRepo ? SystemUpdatesService.repoPackages.length : 0) && SystemUpdatesService.aurPackages.length > 0
                            readonly property bool isHeader: repoHeader || aurHeader

                            width: packageList.width
                            height: isHeader ? (aurHeader ? 46 : 38) : 22

                            RowLayout {
                                visible: parent.repoHeader
                                anchors.top: parent.top
                                anchors.left: parent.left
                                anchors.right: parent.right
                                height: 16
                                spacing: 8

                                Text {
                                    text: "Repo (" + SystemUpdatesService.repoPackages.length + ")"
                                    font.pixelSize: Theme.fontLabelSmall
                                    font.bold: true
                                    color: Theme.primary
                                }

                                Rectangle {
                                    Layout.fillWidth: true
                                    Layout.alignment: Qt.AlignVCenter
                                    height: 1
                                    color: Theme.surfaceContainerHighest
                                }

                            }

                            RowLayout {
                                visible: parent.aurHeader
                                anchors.top: parent.top
                                anchors.topMargin: 8
                                anchors.left: parent.left
                                anchors.right: parent.right
                                height: 16
                                spacing: 8

                                Text {
                                    text: "AUR (" + SystemUpdatesService.aurPackages.length + ")"
                                    font.pixelSize: Theme.fontLabelSmall
                                    font.bold: true
                                    color: Theme.primary
                                }

                                Rectangle {
                                    Layout.fillWidth: true
                                    Layout.alignment: Qt.AlignVCenter
                                    height: 1
                                    color: Theme.surfaceContainerHighest
                                }

                            }

                            Text {
                                anchors.left: parent.left
                                anchors.right: parent.right
                                anchors.bottom: parent.bottom
                                height: 22
                                verticalAlignment: Text.AlignVCenter
                                text: "• " + modelData.name
                                font.pixelSize: Theme.fontLabelSmall
                                color: modelData.type === "aur" ? Theme.surfaceText : Theme.surfaceVariantText
                                elide: Text.ElideRight
                            }

                        }

                    }

                }

            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 8

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 52
                    radius: 12
                    color: pacmanMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "Repo"
                        font.pixelSize: Theme.fontLabelMedium
                        font.bold: true
                        color: Theme.surfaceText
                    }

                    MouseArea {
                        id: pacmanMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            pacmanProc.running = true;
                            systemUpdatesModal.closeModal();
                        }
                    }

                }

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 52
                    radius: 12
                    color: yayMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "AUR"
                        font.pixelSize: Theme.fontLabelMedium
                        font.bold: true
                        color: Theme.surfaceText
                    }

                    MouseArea {
                        id: yayMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            yayProc.running = true;
                            systemUpdatesModal.closeModal();
                        }
                    }

                }

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 52
                    radius: 12
                    color: fullMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "Repo+AUR"
                        font.pixelSize: Theme.fontLabelMedium
                        font.bold: true
                        color: Theme.primary
                    }

                    MouseArea {
                        id: fullMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            fullProc.running = true;
                            systemUpdatesModal.closeModal();
                        }
                    }

                }

            }

        }

    }

    Process {
        id: pacmanProc

        command: ["kitty", "--title", "qs-kitty-update", "sh", "-c", "sudo pacman -Syu; echo ''; echo 'Press Enter to close...'; read"]
    }

    Process {
        id: yayProc

        command: ["kitty", "--title", "qs-kitty-update", "sh", "-c", "yay -Sua; echo ''; echo 'Press Enter to close...'; read"]
    }

    Process {
        id: fullProc

        command: ["kitty", "--title", "qs-kitty-update", "sh", "-c", "yay -Syu; echo ''; echo 'Press Enter to close...'; read"]
    }

}
