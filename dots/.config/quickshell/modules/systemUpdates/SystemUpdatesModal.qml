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
    property int selectedButton: 2
    readonly property int listHeight: 260
    readonly property int packageBoxHeight: 16 + systemUpdatesModal.listHeight + 20

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
            systemUpdatesModal.selectedButton = 2;
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

        width: 640
        implicitHeight: contentColumn.implicitHeight + 24
        anchors.top: parent.top
        anchors.topMargin: 6
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.horizontalCenterOffset: -390
        radius: 20
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: 1
        Keys.onPressed: (event) => {
            if (event.key === Qt.Key_Escape) {
                systemUpdatesModal.closeModal();
                event.accepted = true;
            } else if (event.key === Qt.Key_Left) {
                systemUpdatesModal.selectedButton = (systemUpdatesModal.selectedButton - 1 + 3) % 3;
                event.accepted = true;
            } else if (event.key === Qt.Key_Right) {
                systemUpdatesModal.selectedButton = (systemUpdatesModal.selectedButton + 1) % 3;
                event.accepted = true;
            } else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                if (systemUpdatesModal.selectedButton === 0)
                    pacmanProc.running = true;
                else if (systemUpdatesModal.selectedButton === 1)
                    yayProc.running = true;
                else
                    fullProc.running = true;
                systemUpdatesModal.closeModal();
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
        }

        FontLoader {
            id: materialSymbols

            source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
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

                Row {
                    anchors.fill: parent
                    anchors.topMargin: 10
                    anchors.bottomMargin: 10
                    anchors.leftMargin: 10
                    anchors.rightMargin: 10
                    spacing: 10

                    Column {
                        width: (parent.width - 10) / 2
                        height: parent.height
                        spacing: 4

                        RowLayout {
                            width: parent.width
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

                        Text {
                            visible: systemUpdatesModal.showRepo && SystemUpdatesService.repoLoading
                            text: "Loading..."
                            font.pixelSize: Theme.fontLabelSmall
                            color: Theme.surfaceVariantText
                        }

                        ListView {
                            width: parent.width
                            height: systemUpdatesModal.listHeight
                            clip: true
                            interactive: SystemUpdatesService.repoPackages.length > 10
                            model: systemUpdatesModal.showRepo ? SystemUpdatesService.repoPackages : []

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

                            delegate: Text {
                                width: ListView.view.width
                                height: 22
                                verticalAlignment: Text.AlignVCenter
                                text: "• " + modelData
                                font.pixelSize: Theme.fontLabelSmall
                                color: Theme.surfaceVariantText
                                elide: Text.ElideRight
                            }

                        }

                    }

                    Rectangle {
                        width: 1
                        height: parent.height - 20
                        anchors.verticalCenter: parent.verticalCenter
                        color: Theme.surfaceContainerHighest
                    }

                    Column {
                        width: (parent.width - 10) / 2
                        height: parent.height
                        spacing: 4

                        RowLayout {
                            width: parent.width
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
                            visible: systemUpdatesModal.showAur && SystemUpdatesService.aurLoading
                            text: "Loading..."
                            font.pixelSize: Theme.fontLabelSmall
                            color: Theme.surfaceVariantText
                        }

                        ListView {
                            width: parent.width
                            height: systemUpdatesModal.listHeight
                            clip: true
                            interactive: SystemUpdatesService.aurPackages.length > 10
                            model: systemUpdatesModal.showAur ? SystemUpdatesService.aurPackages : []

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

                            delegate: Text {
                                width: ListView.view.width
                                height: 22
                                verticalAlignment: Text.AlignVCenter
                                text: "• " + modelData
                                font.pixelSize: Theme.fontLabelSmall
                                color: Theme.surfaceText
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
                    color: (systemUpdatesModal.selectedButton === 0 || pacmanMouse.containsMouse) ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    border.color: systemUpdatesModal.selectedButton === 0 ? Theme.primary : "transparent"
                    border.width: systemUpdatesModal.selectedButton === 0 ? 1 : 0

                    Text {
                        anchors.centerIn: parent
                        text: "Repo"
                        font.pixelSize: Theme.fontLabelMedium
                        font.bold: true
                        color: systemUpdatesModal.selectedButton === 0 ? Theme.primary : Theme.surfaceText
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
                    color: (systemUpdatesModal.selectedButton === 1 || yayMouse.containsMouse) ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    border.color: systemUpdatesModal.selectedButton === 1 ? Theme.primary : "transparent"
                    border.width: systemUpdatesModal.selectedButton === 1 ? 1 : 0

                    Text {
                        anchors.centerIn: parent
                        text: "AUR"
                        font.pixelSize: Theme.fontLabelMedium
                        font.bold: true
                        color: systemUpdatesModal.selectedButton === 1 ? Theme.primary : Theme.surfaceText
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
                    color: (systemUpdatesModal.selectedButton === 2 || fullMouse.containsMouse) ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    border.color: systemUpdatesModal.selectedButton === 2 ? Theme.primary : "transparent"
                    border.width: systemUpdatesModal.selectedButton === 2 ? 1 : 0

                    Text {
                        anchors.centerIn: parent
                        text: "Repo+AUR"
                        font.pixelSize: Theme.fontLabelMedium
                        font.bold: true
                        color: systemUpdatesModal.selectedButton === 2 ? Theme.primary : Theme.surfaceText
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
