import QtQuick
import qs.theme

Item {
    id: root

    property string text: ""
    property color color: Theme.surfaceText
    property real fontSize: 13
    property bool bold: false
    property real spacing: 32
    readonly property real loopWidth: label.implicitWidth / 2

    signal clicked()

    function startScroll() {
        if (label.implicitWidth <= root.width) {
            label.x = 0;
            return ;
        }
        scrollAnim.stop();
        scrollAnim.from = label.x;
        scrollAnim.to = -loopWidth;
        scrollAnim.start();
    }

    function wrap() {
        label.x = 0;
        root.startScroll();
    }

    implicitHeight: label.implicitHeight
    clip: true
    onTextChanged: {
        label.x = 0;
        scrollTimer.restart();
    }
    onWidthChanged: scrollTimer.restart()

    Text {
        id: label

        text: root.text ? root.text + (root.text ? "          " : "") + root.text : ""
        color: root.color
        font.pixelSize: root.fontSize
        font.weight: root.bold ? Font.Bold : Font.Normal
        verticalAlignment: Text.AlignVCenter
    }

    MouseArea {
        anchors.fill: parent
        preventStealing: true
        onClicked: root.clicked()
    }

    Timer {
        id: scrollTimer

        interval: 1800
        repeat: false
        onTriggered: {
            if (label.implicitWidth > root.width)
                root.startScroll();

        }
    }

    NumberAnimation {
        id: scrollAnim

        target: label
        property: "x"
        from: 0
        to: -root.loopWidth
        duration: Math.max(3000, root.loopWidth * 45)
        easing.type: Easing.Linear
        onFinished: root.wrap()
    }

}
