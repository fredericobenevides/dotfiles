import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import qs.modules.clipboard
import qs.theme

PanelWindow {
    id: clipboardModal

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

        width: 500
        height: 520
        anchors.top: parent.top
        anchors.topMargin: 6
        anchors.right: parent.right
        anchors.rightMargin: 6
        radius: 16
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: 1
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
            anchors.margins: 12
            spacing: 10

            RowLayout {
                Layout.fillWidth: true
                spacing: 8

                Text {
                    text: "\uE14F"
                    font.family: materialSymbols.name
                    font.pixelSize: 18
                    color: Theme.primary
                }

                Text {
                    Layout.fillWidth: true
                    text: "Clipboard History (" + ClipboardService.filteredEntries.length + ")"
                    font.pixelSize: 18
                    font.bold: true
                    color: Theme.surfaceText
                }

                Rectangle {
                    Layout.preferredWidth: 36
                    Layout.preferredHeight: 36
                    Layout.alignment: Qt.AlignVCenter
                    radius: 18
                    color: clearArea.containsMouse ? Theme.error : "transparent"

                    Text {
                        anchors.centerIn: parent
                        text: "\uE16C"
                        font.family: materialSymbols.name
                        font.pixelSize: 22
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
                Layout.topMargin: 8
                Layout.preferredHeight: 36
                radius: 18
                color: Theme.surfaceContainerHigh
                border.color: searchField.activeFocus ? Theme.primary : "transparent"
                border.width: 1

                RowLayout {
                    anchors.fill: parent
                    anchors.leftMargin: 10
                    anchors.rightMargin: 10
                    spacing: 6

                    Text {
                        text: "\uE8B6"
                        font.family: materialSymbols.name
                        font.pixelSize: 14
                        color: Theme.surfaceVariantText
                    }

                    Item {
                        Layout.fillWidth: true
                        Layout.preferredHeight: 24
                        Layout.alignment: Qt.AlignVCenter

                        Text {
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            text: "Search clipboard..."
                            font.pixelSize: 14
                            color: Theme.surfaceVariantText
                            visible: searchField.text === "" && !searchField.activeFocus
                        }

                        TextInput {
                            id: searchField

                            anchors.fill: parent
                            z: 1
                            clip: true
                            color: Theme.surfaceText
                            font.pixelSize: 14
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
                spacing: 8
                clip: true
                currentIndex: ClipboardService.selectedIndex
                highlightFollowsCurrentItem: true
                highlightMoveDuration: 100

                Text {
                    anchors.centerIn: parent
                    text: ClipboardService.loading ? "Loading..." : "No clipboard entries"
                    font.pixelSize: 14
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
                    height: 80
                    radius: 12
                    color: (selected || cardHover.containsMouse) ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    border.color: (selected || cardHover.containsMouse) ? Theme.primary : "transparent"
                    border.width: (selected || cardHover.containsMouse) ? 1 : 0

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
                        anchors.rightMargin: 8
                        width: 26
                        height: 26
                        radius: 13
                        color: copyBtnArea.containsMouse ? Theme.surfaceContainerHighest : "transparent"

                        Text {
                            anchors.centerIn: parent
                            text: "\uE14D"
                            font.family: materialSymbols.name
                            font.pixelSize: 16
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
                        anchors.rightMargin: 10
                        width: 26
                        height: 26
                        radius: 13
                        color: delBtnArea.containsMouse ? Theme.surfaceContainerHighest : "transparent"

                        Text {
                            anchors.centerIn: parent
                            text: "\uE5CD"
                            font.family: materialSymbols.name
                            font.pixelSize: 16
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
                        anchors.leftMargin: 10
                        anchors.rightMargin: 72
                        spacing: 10

                        Rectangle {
                            Layout.preferredWidth: 28
                            Layout.preferredHeight: 28
                            Layout.alignment: Qt.AlignVCenter
                            radius: 14
                            color: Theme.primary

                            Text {
                                anchors.centerIn: parent
                                text: (index + 1).toString()
                                font.pixelSize: 11
                                font.bold: true
                                color: Theme.primaryText
                            }

                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            Layout.alignment: Qt.AlignVCenter
                            spacing: 8

                            Text {
                                Layout.fillWidth: true
                                text: modelData.type === "image" ? "Image" : (modelData.type === "long_text" ? "Long Text" : "Text")
                                font.pixelSize: 10
                                font.bold: true
                                color: Theme.primary
                                elide: Text.ElideRight
                            }

                            Text {
                                Layout.fillWidth: true
                                text: modelData.type === "image" ? modelData.preview : modelData.preview.substring(0, 120)
                                font.pixelSize: 12
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
