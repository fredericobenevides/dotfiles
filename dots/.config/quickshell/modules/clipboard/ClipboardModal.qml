import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import qs.modules.clipboard
import qs.theme

PanelWindow {
    id: clipboardModal

    property real fontScale: 1.0
    property bool anyCardHovered: false

    function open() {
        visible = true;
        ClipboardService.refresh();
        searchField.text = "";
        bg.forceActiveFocus();
    }

    function closeModal() {
        visible = false;
        ClipboardService.reset();
    }

    focusable: true
    visible: false
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    color: "transparent"

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    MouseArea {
        anchors.fill: parent
        onClicked: clipboardModal.closeModal()
    }

    Rectangle {
        id: bg

        width: Theme.scaledSize(500, clipboardModal.fontScale)
        height: Theme.scaledSize(520, clipboardModal.fontScale)
        anchors.top: parent.top
        anchors.topMargin: Theme.scaledSize(6, clipboardModal.fontScale)
        anchors.right: parent.right
        anchors.rightMargin: Theme.scaledSize(6, clipboardModal.fontScale)
        radius: Theme.scaledSize(16, clipboardModal.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: Theme.scaledSize(1, clipboardModal.fontScale)
        Keys.onPressed: function(event) {
            if (event.key === Qt.Key_Escape) {
                clipboardModal.closeModal();
                event.accepted = true;
            } else if (event.key === Qt.Key_Down) {
                ClipboardService.selectNext();
                event.accepted = true;
            } else if (event.key === Qt.Key_Up) {
                if (ClipboardService.selectedIndex <= 0)
                    searchField.forceActiveFocus();
                else
                    ClipboardService.selectPrev();
                event.accepted = true;
            } else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                const list = ClipboardService.filteredEntries;
                if (list.length > 0 && ClipboardService.selectedIndex >= 0 && ClipboardService.selectedIndex < list.length) {
                    ClipboardService.copyEntry(list[ClipboardService.selectedIndex]);
                    clipboardModal.closeModal();
                }
                event.accepted = true;
            } else if (event.key === Qt.Key_Delete && (event.modifiers & Qt.ControlModifier)) {
                ClipboardService.clearAll();
                event.accepted = true;
            } else if (event.key === Qt.Key_Delete) {
                const list = ClipboardService.filteredEntries;
                if (list.length > 0 && ClipboardService.selectedIndex >= 0 && ClipboardService.selectedIndex < list.length)
                    ClipboardService.deleteEntry(list[ClipboardService.selectedIndex]);

                event.accepted = true;
            } else if (event.key === Qt.Key_Tab) {
                searchField.forceActiveFocus();
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
            hoverEnabled: true
            onExited: clipboardModal.anyCardHovered = false
        }

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: Theme.scaledSize(12, clipboardModal.fontScale)
            spacing: Theme.scaledSize(10, clipboardModal.fontScale)

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, clipboardModal.fontScale)

                Text {
                    text: "\uE14F"
                    font.family: materialSymbols.name
                    font.pixelSize: Theme.scaledFont(18, clipboardModal.fontScale)
                    color: Theme.primary
                }

                Text {
                    Layout.fillWidth: true
                    text: "Clipboard History (" + ClipboardService.filteredEntries.length + ")"
                    font.pixelSize: Theme.scaledFont(18, clipboardModal.fontScale)
                    font.bold: true
                    color: Theme.surfaceText
                }

                Rectangle {
                    Layout.preferredWidth: Theme.scaledSize(36, clipboardModal.fontScale)
                    Layout.preferredHeight: Theme.scaledSize(36, clipboardModal.fontScale)
                    Layout.alignment: Qt.AlignVCenter
                    radius: Theme.scaledSize(18, clipboardModal.fontScale)
                    color: clearArea.containsMouse ? Theme.error : "transparent"

                    Text {
                        anchors.centerIn: parent
                        text: "\uE16C"
                        font.family: materialSymbols.name
                        font.pixelSize: Theme.scaledFont(22, clipboardModal.fontScale)
                        color: clearArea.containsMouse ? Theme.surfaceText : Theme.surfaceVariantText
                    }

                    MouseArea {
                        id: clearArea

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: ClipboardService.clearAll()
                    }

                }

            }

            Rectangle {
                Layout.fillWidth: true
                Layout.topMargin: Theme.scaledSize(8, clipboardModal.fontScale)
                Layout.preferredHeight: Theme.scaledSize(36, clipboardModal.fontScale)
                radius: Theme.scaledSize(18, clipboardModal.fontScale)
                color: Theme.surfaceContainerHigh
                border.color: searchField.activeFocus ? Theme.primary : "transparent"
                border.width: Theme.scaledSize(1, clipboardModal.fontScale)

                RowLayout {
                    anchors.fill: parent
                    anchors.leftMargin: Theme.scaledSize(10, clipboardModal.fontScale)
                    anchors.rightMargin: Theme.scaledSize(10, clipboardModal.fontScale)
                    spacing: Theme.scaledSize(6, clipboardModal.fontScale)

                    Text {
                        text: "\uE8B6"
                        font.family: materialSymbols.name
                        font.pixelSize: Theme.scaledFont(14, clipboardModal.fontScale)
                        color: Theme.surfaceVariantText
                    }

                    Item {
                        Layout.fillWidth: true
                        Layout.preferredHeight: Theme.scaledSize(24, clipboardModal.fontScale)
                        Layout.alignment: Qt.AlignVCenter

                        Text {
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            text: "Search clipboard..."
                            font.pixelSize: Theme.scaledFont(14, clipboardModal.fontScale)
                            color: Theme.surfaceVariantText
                            visible: searchField.text === "" && !searchField.activeFocus
                        }

                        TextInput {
                            id: searchField

                            anchors.fill: parent
                            z: 1
                            clip: true
                            color: Theme.surfaceText
                            font.pixelSize: Theme.scaledFont(14, clipboardModal.fontScale)
                            selectByMouse: true
                            verticalAlignment: Text.AlignVCenter
                            onTextChanged: ClipboardService.searchText = text
                            Keys.onPressed: function(event) {
                                if (event.key === Qt.Key_Escape) {
                                    clipboardModal.closeModal();
                                    event.accepted = true;
                                } else if (event.key === Qt.Key_Down) {
                                    ClipboardService.selectNext();
                                    bg.forceActiveFocus();
                                    event.accepted = true;
                                } else if (event.key === Qt.Key_Up) {
                                    ClipboardService.selectPrev();
                                    bg.forceActiveFocus();
                                    event.accepted = true;
                                } else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                                    const list = ClipboardService.filteredEntries;
                                    if (list.length > 0 && ClipboardService.selectedIndex >= 0 && ClipboardService.selectedIndex < list.length) {
                                        ClipboardService.copyEntry(list[ClipboardService.selectedIndex]);
                                        clipboardModal.closeModal();
                                    }
                                    event.accepted = true;
                                } else if (event.key === Qt.Key_Delete && (event.modifiers & Qt.ControlModifier)) {
                                    ClipboardService.clearAll();
                                    event.accepted = true;
                                } else if (event.key === Qt.Key_Delete) {
                                    const list = ClipboardService.filteredEntries;
                                    if (list.length > 0 && ClipboardService.selectedIndex >= 0 && ClipboardService.selectedIndex < list.length)
                                        ClipboardService.deleteEntry(list[ClipboardService.selectedIndex]);

                                    event.accepted = true;
                                } else if (event.key === Qt.Key_Tab) {
                                    bg.forceActiveFocus();
                                    event.accepted = true;
                                }
                            }
                        }

                    }

                }

            }

            ListView {
                id: listView

                Layout.fillWidth: true
                Layout.fillHeight: true
                model: ClipboardService.filteredEntries
                spacing: Theme.scaledSize(8, clipboardModal.fontScale)
                clip: true
                currentIndex: ClipboardService.selectedIndex
                highlightFollowsCurrentItem: true
                highlightMoveDuration: 100

                Text {
                    anchors.centerIn: parent
                    text: ClipboardService.loading ? "Loading..." : "No clipboard entries"
                    font.pixelSize: Theme.scaledFont(14, clipboardModal.fontScale)
                    color: Theme.surfaceVariantText
                    visible: ClipboardService.filteredEntries.length === 0
                }

                ScrollBar.vertical: ScrollBar {
                    policy: ScrollBar.AsNeeded
                }

                delegate: Rectangle {
                    required property int index
                    required property var modelData
                    readonly property bool selected: ListView.isCurrentItem && !clipboardModal.anyCardHovered

                    width: listView.width
                    height: Theme.scaledSize(80, clipboardModal.fontScale)
                    radius: Theme.scaledSize(12, clipboardModal.fontScale)
                    color: (selected || cardHover.containsMouse) ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    border.color: (selected || cardHover.containsMouse) ? Theme.primary : "transparent"
                    border.width: (selected || cardHover.containsMouse) ? Theme.scaledSize(1, clipboardModal.fontScale) : Theme.scaledSize(0, clipboardModal.fontScale)

                    MouseArea {
                        id: cardHover

                        anchors.fill: parent
                        hoverEnabled: true
                        acceptedButtons: Qt.NoButton
                        onEntered: clipboardModal.anyCardHovered = true
                    }

                    Rectangle {
                        anchors.right: delBtnRect.left
                        anchors.verticalCenter: parent.verticalCenter
                        anchors.rightMargin: Theme.scaledSize(8, clipboardModal.fontScale)
                        width: Theme.scaledSize(26, clipboardModal.fontScale)
                        height: Theme.scaledSize(26, clipboardModal.fontScale)
                        radius: Theme.scaledSize(13, clipboardModal.fontScale)
                        color: copyBtnArea.containsMouse ? Theme.surfaceContainerHighest : "transparent"

                        Text {
                            anchors.centerIn: parent
                            text: "\uE14D"
                            font.family: materialSymbols.name
                            font.pixelSize: Theme.scaledFont(16, clipboardModal.fontScale)
                            color: copyBtnArea.containsMouse ? Theme.primary : Theme.surfaceVariantText
                        }

                        MouseArea {
                            id: copyBtnArea

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: {
                                ClipboardService.copyEntry(modelData);
                                clipboardModal.closeModal();
                            }
                        }

                    }

                    Rectangle {
                        id: delBtnRect

                        anchors.right: parent.right
                        anchors.verticalCenter: parent.verticalCenter
                        anchors.rightMargin: Theme.scaledSize(10, clipboardModal.fontScale)
                        width: Theme.scaledSize(26, clipboardModal.fontScale)
                        height: Theme.scaledSize(26, clipboardModal.fontScale)
                        radius: Theme.scaledSize(13, clipboardModal.fontScale)
                        color: delBtnArea.containsMouse ? Theme.surfaceContainerHighest : "transparent"

                        Text {
                            anchors.centerIn: parent
                            text: "\uE5CD"
                            font.family: materialSymbols.name
                            font.pixelSize: Theme.scaledFont(16, clipboardModal.fontScale)
                            color: delBtnArea.containsMouse ? Theme.primary : Theme.surfaceVariantText
                        }

                        MouseArea {
                            id: delBtnArea

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: ClipboardService.deleteEntry(modelData)
                        }

                    }

                    RowLayout {
                        anchors.left: parent.left
                        anchors.verticalCenter: parent.verticalCenter
                        anchors.leftMargin: Theme.scaledSize(10, clipboardModal.fontScale)
                        anchors.rightMargin: Theme.scaledSize(72, clipboardModal.fontScale)
                        spacing: Theme.scaledSize(10, clipboardModal.fontScale)

                        Rectangle {
                            Layout.preferredWidth: Theme.scaledSize(28, clipboardModal.fontScale)
                            Layout.preferredHeight: Theme.scaledSize(28, clipboardModal.fontScale)
                            Layout.alignment: Qt.AlignVCenter
                            radius: Theme.scaledSize(14, clipboardModal.fontScale)
                            color: Theme.primary

                            Text {
                                anchors.centerIn: parent
                                text: (index + 1).toString()
                                font.pixelSize: Theme.scaledFont(11, clipboardModal.fontScale)
                                font.bold: true
                                color: Theme.primaryText
                            }

                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            Layout.alignment: Qt.AlignVCenter
                            spacing: Theme.scaledSize(8, clipboardModal.fontScale)

                            Text {
                                Layout.fillWidth: true
                                text: modelData.type === "image" ? "Image" : (modelData.type === "long_text" ? "Long Text" : "Text")
                                font.pixelSize: Theme.scaledFont(10, clipboardModal.fontScale)
                                font.bold: true
                                color: Theme.primary
                                elide: Text.ElideRight
                            }

                            Text {
                                Layout.fillWidth: true
                                text: modelData.type === "image" ? modelData.preview : modelData.preview.substring(0, 120)
                                font.pixelSize: Theme.scaledFont(12, clipboardModal.fontScale)
                                color: Theme.surfaceText
                                elide: Text.ElideRight
                                maximumLineCount: 2
                                wrapMode: Text.WordWrap
                            }

                        }

                    }

                }

            }

        }

    }

}
