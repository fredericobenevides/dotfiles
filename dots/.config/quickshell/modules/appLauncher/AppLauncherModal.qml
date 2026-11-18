import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import Quickshell.Widgets
import qs.theme

PanelWindow {
    id: launcherMenu

    property real fontScale: 1.0
    property string searchText: ""
    property bool anyCardHovered: false
    readonly property string launcherIcon: "󰀻"
    readonly property bool isMathExpression: {
        if (!searchText)
            return false;

        return evaluateMath(searchText) !== null;
    }
    readonly property string mathResult: {
        if (!isMathExpression)
            return "";

        return String(evaluateMath(searchText));
    }

    function evaluateMath(expr) {
        const clean = expr.replace(/\s/g, '');
        if (clean.length === 0)
            return null;

        if (!/^[0-9+\-*/().%^]+$/.test(clean))
            return null;

        const jsExpr = clean.replace(/\^/g, '**');
        try {
            const result = eval(jsExpr);
            if (typeof result !== 'number' || !isFinite(result))
                return null;

            return result;
        } catch (e) {
            return null;
        }
    }

    function copyCalcResult() {
        calcCopyProcess.command = ["wl-copy", "--", launcherMenu.mathResult];
        calcCopyProcess.running = true;
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
            searchInput.text = "";
            searchInput.forceActiveFocus();
            anyCardHovered = false;
        }
    }

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Process {
        id: calcCopyProcess

        command: ["wl-copy", ""]
    }

    ScriptModel {
        id: filteredAppsModel

        values: {
            const apps = DesktopEntries.applications.values;
            if (!launcherMenu.searchText)
                return apps;

            const filtered = apps.filter((app) => {
                const q = launcherMenu.searchText;
                const fields = [app.name, app.genericName, app.id, app.execString];
                for (let i = 0; i < fields.length; i++) {
                    if (fields[i] && fields[i].toLowerCase().includes(q))
                        return true;

                }
                const keywords = app.keywords;
                if (keywords) {
                    for (let i = 0; i < keywords.length; i++) {
                        if (keywords[i].toLowerCase().includes(q))
                            return true;

                    }
                }
                return false;
            });
            if (launcherMenu.isMathExpression) {
                const r = launcherMenu.mathResult;
                const expr = launcherMenu.searchText;
                return [{
                    "name": expr + " = " + r,
                    "icon": "accessories-calculator",
                    "genericName": "Calculator",
                    "id": "calculator-result",
                    "execString": "",
                    "keywords": [],
                    "execute": function() {
                        launcherMenu.copyCalcResult();
                        launcherMenu.visible = false;
                    }
                }].concat(filtered);
            }
            return filtered;
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: launcherMenu.visible = false
    }

    Rectangle {
        id: bg

        width: Theme.scaledSize(600, launcherMenu.fontScale)
        height: Theme.scaledSize(600, launcherMenu.fontScale)
        anchors.centerIn: parent
        color: Theme.surfaceContainer
        radius: Theme.scaledSize(16, launcherMenu.fontScale)
        Keys.onPressed: (event) => {
            if (event.key === Qt.Key_Escape) {
                searchInput.text = "";
                launcherMenu.visible = false;
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
            hoverEnabled: true
            onExited: launcherMenu.anyCardHovered = false
        }

        Rectangle {
            id: searchBar

            height: Theme.scaledSize(46, launcherMenu.fontScale)
            anchors.top: parent.top
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.margins: Theme.scaledSize(16, launcherMenu.fontScale)
            radius: Theme.scaledSize(10, launcherMenu.fontScale)
            color: Theme.surfaceContainerHigh
            border.color: Theme.primary
            border.width: Theme.scaledSize(2, launcherMenu.fontScale)

            RowLayout {
                anchors.fill: parent
                anchors.leftMargin: Theme.scaledSize(14, launcherMenu.fontScale)
                anchors.rightMargin: Theme.scaledSize(14, launcherMenu.fontScale)
                spacing: Theme.scaledSize(10, launcherMenu.fontScale)

                Text {
                    id: searchIcon

                    text: "󰍉"
                    font.pixelSize: Theme.scaledFont(18, launcherMenu.fontScale)
                    color: Theme.surfaceVariantText
                    Layout.alignment: Qt.AlignVCenter
                }

                TextInput {
                    id: searchInput

                    Layout.fillWidth: true
                    Layout.alignment: Qt.AlignVCenter
                    verticalAlignment: Text.AlignVCenter
                    font.pixelSize: Theme.scaledFont(Theme.fontLabelLarge, launcherMenu.fontScale)
                    color: Theme.surfaceText
                    focus: launcherMenu.visible
                    onTextChanged: {
                        launcherMenu.searchText = text.toLowerCase();
                        appsGrid.currentIndex = 0;
                    }
                    Keys.onPressed: (event) => {
                        if (event.key === Qt.Key_Escape) {
                            searchInput.text = "";
                            launcherMenu.visible = false;
                            event.accepted = true;
                        } else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                            const currentApp = filteredAppsModel.values[appsGrid.currentIndex];
                            if (currentApp) {
                                currentApp.execute();
                                launcherMenu.visible = false;
                            }
                            event.accepted = true;
                        } else if (event.key === Qt.Key_Down || event.key === Qt.Key_Tab) {
                            if (appsGrid.count > 0) {
                                appsGrid.forceActiveFocus();
                                event.accepted = true;
                            }
                        } else if (event.key === Qt.Key_Right && (searchInput.text.length === 0 || searchInput.cursorPosition === searchInput.text.length)) {
                            if (appsGrid.count > 0) {
                                appsGrid.forceActiveFocus();
                                appsGrid.currentIndex = Math.min(1, appsGrid.count - 1);
                                event.accepted = true;
                            }
                        } else if (event.key === Qt.Key_Left && (searchInput.text.length === 0 || searchInput.cursorPosition === 0)) {
                            if (appsGrid.count > 0) {
                                appsGrid.forceActiveFocus();
                                appsGrid.currentIndex = 0;
                                event.accepted = true;
                            }
                        }
                    }

                    Text {
                        text: "Search applications..."
                        color: Theme.surfaceVariantText
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelLarge, launcherMenu.fontScale)
                        anchors.verticalCenter: parent.verticalCenter
                        visible: !parent.text && !parent.activeFocus
                    }

                }

            }

        }

        GridView {
            id: appsGrid

            anchors.top: searchBar.bottom
            anchors.bottom: statusBar.top
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.topMargin: Theme.scaledSize(16, launcherMenu.fontScale)
            anchors.bottomMargin: Theme.scaledSize(8, launcherMenu.fontScale)
            anchors.leftMargin: Theme.scaledSize(16, launcherMenu.fontScale)
            anchors.rightMargin: Theme.scaledSize(16, launcherMenu.fontScale)
            clip: true
            cellWidth: (bg.width - 32) / 4
            cellHeight: 110
            model: filteredAppsModel
            keyNavigationEnabled: true
            highlightFollowsCurrentItem: true
            Keys.onPressed: (event) => {
                if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                    const currentApp = filteredAppsModel.values[currentIndex];
                    if (currentApp) {
                        currentApp.execute(); // Executa o aplicativo
                        launcherMenu.visible = false; // Fecha o menu
                    }
                    event.accepted = true;
                } else if (event.key === Qt.Key_Tab) {
                    searchInput.forceActiveFocus();
                    event.accepted = true;
                } else if (event.key === Qt.Key_Escape) {
                    searchInput.text = "";
                    launcherMenu.visible = false;
                    event.accepted = true;
                }
            }
            Keys.onUpPressed: (event) => {
                if (currentIndex < 4) {
                    searchInput.forceActiveFocus();
                    event.accepted = true;
                } else {
                    event.accepted = false;
                }
            }

            delegate: Rectangle {
                id: appDelegate

                readonly property bool isCurrent: GridView.isCurrentItem

                width: appsGrid.cellWidth
                height: appsGrid.cellHeight
                radius: Theme.scaledSize(12, launcherMenu.fontScale)
                color: (delegateMouse.containsMouse || (isCurrent && !launcherMenu.anyCardHovered)) ? Theme.surfaceContainerHighest : "transparent"
                border.color: (delegateMouse.containsMouse || (isCurrent && !launcherMenu.anyCardHovered)) ? Theme.primary : "transparent"
                border.width: (delegateMouse.containsMouse || (isCurrent && !launcherMenu.anyCardHovered)) ? Theme.scaledSize(1, launcherMenu.fontScale) : Theme.scaledSize(0, launcherMenu.fontScale)

                ColumnLayout {
                    anchors.centerIn: parent
                    spacing: Theme.scaledSize(8, launcherMenu.fontScale)

                    Item {
                        Layout.preferredWidth: Theme.scaledSize(44, launcherMenu.fontScale)
                        Layout.preferredHeight: Theme.scaledSize(44, launcherMenu.fontScale)
                        Layout.alignment: Qt.AlignHCenter

                        Text {
                            text: "\uE8F0"
                            font.family: materialSymbols.name
                            font.pixelSize: Theme.scaledFont(40, launcherMenu.fontScale)
                            color: Theme.primary
                            anchors.centerIn: parent
                            visible: modelData.id === "calculator-result"
                        }

                        IconImage {
                            anchors.fill: parent
                            source: Quickshell.iconPath(modelData.icon, "application-x-executable")
                            visible: modelData.id !== "calculator-result"
                        }

                    }

                    Text {
                        text: modelData.name
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, launcherMenu.fontScale)
                        color: Theme.surfaceText
                        Layout.alignment: Qt.AlignHCenter
                        horizontalAlignment: Text.AlignHCenter
                        elide: Text.ElideRight
                        Layout.maximumWidth: appsGrid.cellWidth - Theme.scaledSize(10, launcherMenu.fontScale)
                    }

                }

                MouseArea {
                    id: delegateMouse

                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onEntered: launcherMenu.anyCardHovered = true
                    onClicked: {
                        modelData.execute();
                        searchInput.text = "";
                        launcherMenu.visible = false;
                    }
                }

            }

        }

        Rectangle {
            id: statusBar

            height: Theme.scaledSize(38, launcherMenu.fontScale)
            anchors.bottom: parent.bottom
            anchors.left: parent.left
            anchors.right: parent.right
            color: Theme.surfaceContainerHigh

            RowLayout {
                anchors.fill: parent
                anchors.leftMargin: Theme.scaledSize(12, launcherMenu.fontScale)
                anchors.rightMargin: Theme.scaledSize(12, launcherMenu.fontScale)

                RowLayout {
                    spacing: Theme.scaledSize(8, launcherMenu.fontScale)
                    Layout.alignment: Qt.AlignVCenter

                    Text {
                        text: launcherMenu.launcherIcon
                        font.pixelSize: Theme.scaledFont(18, launcherMenu.fontScale)
                        color: Theme.surfaceVariantText
                        Layout.alignment: Qt.AlignVCenter
                    }

                    Text {
                        text: "Applications: " + appsGrid.count
                        color: Theme.surfaceVariantText
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, launcherMenu.fontScale)
                        Layout.alignment: Qt.AlignVCenter
                    }

                }

                Item {
                    Layout.fillWidth: true
                }

                RowLayout {
                    spacing: Theme.scaledSize(4, launcherMenu.fontScale)
                    Layout.alignment: Qt.AlignVCenter

                    Rectangle {
                        width: Theme.scaledSize(24, launcherMenu.fontScale)
                        height: Theme.scaledSize(18, launcherMenu.fontScale)
                        color: Theme.surfaceContainerHighest
                        radius: Theme.scaledSize(4, launcherMenu.fontScale)
                        border.color: Theme.surfaceVariantText
                        border.width: Theme.scaledSize(1, launcherMenu.fontScale)

                        Text {
                            text: "↑↓"
                            color: Theme.surfaceText
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, launcherMenu.fontScale)
                            font.bold: true
                            anchors.centerIn: parent
                        }

                    }

                    Text {
                        text: "Nav"
                        color: Theme.surfaceVariantText
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, launcherMenu.fontScale)
                    }

                }

                Item {
                    width: Theme.scaledSize(8, launcherMenu.fontScale)
                }

                RowLayout {
                    spacing: Theme.scaledSize(4, launcherMenu.fontScale)
                    Layout.alignment: Qt.AlignVCenter

                    Rectangle {
                        width: Theme.scaledSize(42, launcherMenu.fontScale)
                        height: Theme.scaledSize(18, launcherMenu.fontScale)
                        color: Theme.surfaceContainerHighest
                        radius: Theme.scaledSize(4, launcherMenu.fontScale)
                        border.color: Theme.surfaceVariantText
                        border.width: Theme.scaledSize(1, launcherMenu.fontScale)

                        Text {
                            text: "Enter"
                            color: Theme.surfaceText
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, launcherMenu.fontScale)
                            font.bold: true
                            anchors.centerIn: parent
                        }

                    }

                    Text {
                        text: "Execute"
                        color: Theme.surfaceVariantText
                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, launcherMenu.fontScale)
                    }

                }

            }

        }

    }

}
