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

    property real fontScale: 1.0
    property var targetScreen: null
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

    function openAurPackage(pkgLine) {
        const name = pkgLine.split(/\s+/)[0];
        aurLinkProcess.command = ["xdg-open", "https://aur.archlinux.org/packages/" + name];
        aurLinkProcess.running = true;
    }

    focusable: true
    visible: false
    screen: targetScreen
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

        width: Theme.scaledSize(640, systemUpdatesModal.fontScale)
        implicitHeight: contentColumn.implicitHeight + Theme.scaledSize(24, systemUpdatesModal.fontScale)
        anchors.centerIn: parent
        radius: Theme.scaledSize(20, systemUpdatesModal.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: Theme.scaledSize(1, systemUpdatesModal.fontScale)
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
            anchors.margins: Theme.scaledSize(16, systemUpdatesModal.fontScale)
            spacing: Theme.scaledSize(12, systemUpdatesModal.fontScale)

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, systemUpdatesModal.fontScale)

                Text {
                    text: "\uE923"
                    font.family: materialSymbols.name
                    font.pixelSize: Theme.scaledFont(18, systemUpdatesModal.fontScale)
                    color: Theme.primary
                }

                Text {
                    Layout.fillWidth: true
                    text: "System Updates"
                    font.pixelSize: Theme.scaledFont(16, systemUpdatesModal.fontScale)
                    font.bold: true
                    color: Theme.surfaceText
                }

                Rectangle {
                    Layout.preferredWidth: Theme.scaledSize(24, systemUpdatesModal.fontScale)
                    Layout.preferredHeight: Theme.scaledSize(24, systemUpdatesModal.fontScale)
                    radius: Theme.scaledSize(12, systemUpdatesModal.fontScale)
                    color: closeMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "\uE5CD"
                        font.family: materialSymbols.name
                        font.pixelSize: Theme.scaledFont(14, systemUpdatesModal.fontScale)
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
                Layout.topMargin: Theme.scaledSize(8, systemUpdatesModal.fontScale)
                spacing: Theme.scaledSize(8, systemUpdatesModal.fontScale)

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: Theme.scaledSize(26, systemUpdatesModal.fontScale)
                    radius: Theme.scaledSize(13, systemUpdatesModal.fontScale)
                    color: Theme.primary

                    Row {
                        anchors.centerIn: parent
                        spacing: Theme.scaledSize(5, systemUpdatesModal.fontScale)

                        Text {
                            text: "Repo:"
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemUpdatesModal.fontScale)
                            font.bold: true
                            color: Theme.primaryText
                            anchors.verticalCenter: parent.verticalCenter
                        }

                        Text {
                            text: SystemUpdatesService.repoCount
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemUpdatesModal.fontScale)
                            font.bold: true
                            color: Theme.primaryText
                            anchors.verticalCenter: parent.verticalCenter
                        }

                    }

                }

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: Theme.scaledSize(26, systemUpdatesModal.fontScale)
                    radius: Theme.scaledSize(13, systemUpdatesModal.fontScale)
                    color: Theme.primary

                    Row {
                        anchors.centerIn: parent
                        spacing: Theme.scaledSize(5, systemUpdatesModal.fontScale)

                        Text {
                            text: "AUR:"
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemUpdatesModal.fontScale)
                            font.bold: true
                            color: Theme.primaryText
                            anchors.verticalCenter: parent.verticalCenter
                        }

                        Text {
                            text: SystemUpdatesService.aurCount
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemUpdatesModal.fontScale)
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
                radius: Theme.scaledSize(12, systemUpdatesModal.fontScale)
                color: Theme.surfaceContainerHigh
                clip: true

                Row {
                    anchors.fill: parent
                    anchors.topMargin: Theme.scaledSize(10, systemUpdatesModal.fontScale)
                    anchors.bottomMargin: Theme.scaledSize(10, systemUpdatesModal.fontScale)
                    anchors.leftMargin: Theme.scaledSize(10, systemUpdatesModal.fontScale)
                    anchors.rightMargin: Theme.scaledSize(10, systemUpdatesModal.fontScale)
                    spacing: Theme.scaledSize(10, systemUpdatesModal.fontScale)

                    Column {
                        width: (parent.width - 10) / 2
                        height: parent.height
                        spacing: Theme.scaledSize(4, systemUpdatesModal.fontScale)

                        RowLayout {
                            width: parent.width
                            height: Theme.scaledSize(16, systemUpdatesModal.fontScale)
                            spacing: Theme.scaledSize(8, systemUpdatesModal.fontScale)

                            Text {
                                text: "Repo (" + SystemUpdatesService.repoPackages.length + ")"
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemUpdatesModal.fontScale)
                                font.bold: true
                                color: Theme.primary
                            }

                            Rectangle {
                                Layout.fillWidth: true
                                Layout.alignment: Qt.AlignVCenter
                                height: Theme.scaledSize(1, systemUpdatesModal.fontScale)
                                color: Theme.surfaceContainerHighest
                            }

                        }

                        Text {
                            visible: systemUpdatesModal.showRepo && SystemUpdatesService.repoLoading
                            text: "Loading..."
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemUpdatesModal.fontScale)
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
                                width: Theme.scaledSize(14, systemUpdatesModal.fontScale)
                                minimumSize: 0.25

                                contentItem: Rectangle {
                                    implicitWidth: Theme.scaledSize(14, systemUpdatesModal.fontScale)
                                    radius: Theme.scaledSize(7, systemUpdatesModal.fontScale)
                                    color: parent.pressed ? Theme.surfaceVariant : Theme.surfaceContainerHighest
                                }

                            }

                            delegate: Text {
                                width: ListView.view.width
                                height: Theme.scaledSize(22, systemUpdatesModal.fontScale)
                                verticalAlignment: Text.AlignVCenter
                                text: "• " + modelData
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemUpdatesModal.fontScale)
                                color: Theme.surfaceVariantText
                                elide: Text.ElideRight
                            }

                        }

                    }

                    Rectangle {
                        width: Theme.scaledSize(1, systemUpdatesModal.fontScale)
                        height: parent.height - Theme.scaledSize(20, systemUpdatesModal.fontScale)
                        anchors.verticalCenter: parent.verticalCenter
                        color: Theme.surfaceContainerHighest
                    }

                    Column {
                        width: (parent.width - 10) / 2
                        height: parent.height
                        spacing: Theme.scaledSize(4, systemUpdatesModal.fontScale)

                        RowLayout {
                            width: parent.width
                            height: Theme.scaledSize(16, systemUpdatesModal.fontScale)
                            spacing: Theme.scaledSize(8, systemUpdatesModal.fontScale)

                            Text {
                                text: "AUR (" + SystemUpdatesService.aurPackages.length + ")"
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemUpdatesModal.fontScale)
                                font.bold: true
                                color: Theme.primary
                            }

                            Rectangle {
                                Layout.fillWidth: true
                                Layout.alignment: Qt.AlignVCenter
                                height: Theme.scaledSize(1, systemUpdatesModal.fontScale)
                                color: Theme.surfaceContainerHighest
                            }

                        }

                        Text {
                            visible: systemUpdatesModal.showAur && SystemUpdatesService.aurLoading
                            text: "Loading..."
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemUpdatesModal.fontScale)
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
                                width: Theme.scaledSize(14, systemUpdatesModal.fontScale)
                                minimumSize: 0.25

                                contentItem: Rectangle {
                                    implicitWidth: Theme.scaledSize(14, systemUpdatesModal.fontScale)
                                    radius: Theme.scaledSize(7, systemUpdatesModal.fontScale)
                                    color: parent.pressed ? Theme.surfaceVariant : Theme.surfaceContainerHighest
                                }

                            }

                            delegate: Item {
                                width: ListView.view.width
                                height: Theme.scaledSize(22, systemUpdatesModal.fontScale)

                                Rectangle {
                                    anchors.fill: parent
                                    radius: Theme.scaledSize(4, systemUpdatesModal.fontScale)
                                    color: aurMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent"
                                }

                                Text {
                                    anchors.left: parent.left
                                    anchors.right: parent.right
                                    anchors.leftMargin: Theme.scaledSize(4, systemUpdatesModal.fontScale)
                                    anchors.verticalCenter: parent.verticalCenter
                                    text: "• " + modelData
                                    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, systemUpdatesModal.fontScale)
                                    color: aurMouse.containsMouse ? Theme.primary : Theme.surfaceText
                                    elide: Text.ElideRight
                                }

                                MouseArea {
                                    id: aurMouse

                                    anchors.fill: parent
                                    hoverEnabled: true
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: systemUpdatesModal.openAurPackage(modelData)
                                }

                            }

                        }

                    }

                }

            }

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, systemUpdatesModal.fontScale)

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: Theme.scaledSize(52, systemUpdatesModal.fontScale)
                    radius: Theme.scaledSize(12, systemUpdatesModal.fontScale)
                    color: (systemUpdatesModal.selectedButton === 0 || pacmanMouse.containsMouse) ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    border.color: systemUpdatesModal.selectedButton === 0 ? Theme.primary : "transparent"
                    border.width: systemUpdatesModal.selectedButton === Theme.scaledSize(0, systemUpdatesModal.fontScale) ? Theme.scaledSize(1, systemUpdatesModal.fontScale) : Theme.scaledSize(0, systemUpdatesModal.fontScale)

                    Text {
                        anchors.centerIn: parent
                        text: "Repo"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemUpdatesModal.fontScale)
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
                    Layout.preferredHeight: Theme.scaledSize(52, systemUpdatesModal.fontScale)
                    radius: Theme.scaledSize(12, systemUpdatesModal.fontScale)
                    color: (systemUpdatesModal.selectedButton === 1 || yayMouse.containsMouse) ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    border.color: systemUpdatesModal.selectedButton === 1 ? Theme.primary : "transparent"
                    border.width: systemUpdatesModal.selectedButton === Theme.scaledSize(1, systemUpdatesModal.fontScale) ? Theme.scaledSize(1, systemUpdatesModal.fontScale) : Theme.scaledSize(0, systemUpdatesModal.fontScale)

                    Text {
                        anchors.centerIn: parent
                        text: "AUR"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemUpdatesModal.fontScale)
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
                    Layout.preferredHeight: Theme.scaledSize(52, systemUpdatesModal.fontScale)
                    radius: Theme.scaledSize(12, systemUpdatesModal.fontScale)
                    color: (systemUpdatesModal.selectedButton === 2 || fullMouse.containsMouse) ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    border.color: systemUpdatesModal.selectedButton === 2 ? Theme.primary : "transparent"
                    border.width: systemUpdatesModal.selectedButton === Theme.scaledSize(2, systemUpdatesModal.fontScale) ? Theme.scaledSize(1, systemUpdatesModal.fontScale) : Theme.scaledSize(0, systemUpdatesModal.fontScale)

                    Text {
                        anchors.centerIn: parent
                        text: "Repo+AUR"
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, systemUpdatesModal.fontScale)
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

    Process {
        id: aurLinkProcess

        command: ["xdg-open", ""]
    }

}
