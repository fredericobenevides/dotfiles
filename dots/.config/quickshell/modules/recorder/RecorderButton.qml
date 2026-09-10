import QtQuick
import QtQuick.Layouts
import qs.modules.recorder
import qs.theme

Item {
    id: root

    property bool hovered: false
    property var modal
    readonly property bool recording: GSRService.recording

    implicitWidth: bg.width
    implicitHeight: bg.height

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Rectangle {
        id: bg

        width: root.recording ? recRow.implicitWidth + 18 : 24
        height: 24
        radius: 7
        color: root.recording ? Theme.error : (root.hovered ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh)

        RowLayout {
            id: recRow

            anchors.centerIn: parent
            spacing: 5

            Text {
                text: "\uE04B"
                font.family: materialSymbols.name
                font.pixelSize: 15
                color: root.recording ? Theme.surfaceText : (root.hovered ? Theme.surfaceText : Theme.surfaceVariantText)
            }

            Rectangle {
                width: 6
                height: 6
                radius: 3
                color: Theme.surfaceText
                visible: root.recording
                opacity: recPulse.recToggled ? 1 : 0.3

                Behavior on opacity {
                    NumberAnimation {
                        duration: 400
                    }

                }

            }

            Text {
                text: GSRService.formatElapsed()
                visible: root.recording
                font.family: "JetBrainsMono Nerd Font"
                font.pixelSize: 11
                color: Theme.surfaceText
            }

        }

        MouseArea {
            anchors.fill: parent
            cursorShape: Qt.PointingHandCursor
            hoverEnabled: true
            onEntered: root.hovered = true
            onExited: root.hovered = false
            onClicked: {
                if (!root.modal)
                    return ;

                root.modal.visible = !root.modal.visible;
                if (root.modal.visible && root.modal.open)
                    root.modal.open();

            }
        }

        Behavior on width {
            NumberAnimation {
                duration: 140
            }

        }

        Behavior on color {
            ColorAnimation {
                duration: 180
            }

        }

    }

    Timer {
        id: recPulse

        property bool recToggled: false

        interval: 700
        repeat: true
        running: root.recording
        onTriggered: recToggled = !recToggled
    }

}
