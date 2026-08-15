import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.modules.weather
import qs.theme

Item {
    id: root

    property var modal
    property bool hovered: false
    readonly property bool hasWeather: WeatherService.weatherUpdatedAt > 0 || WeatherService.available

    implicitWidth: contentRow.implicitWidth + 12
    implicitHeight: 24

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/quickshell/dms/assets/fonts/material-design-icons/variablefont/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Row {
        id: contentRow

        anchors.left: parent.left
        anchors.leftMargin: 6
        anchors.verticalCenter: parent.verticalCenter
        spacing: 6

        Text {
            text: hasWeather ? WeatherService.getWeatherIcon(WeatherService.wCode, WeatherService.isDay) : "\uE2BD"
            font.family: materialSymbols.name
            font.pixelSize: 16
            color: Theme.primary
            anchors.verticalCenter: parent.verticalCenter
        }

        Text {
            text: hasWeather ? (WeatherService.formatTemp(WeatherService.temp) || "--°C") : "--°C"
            font.pixelSize: Theme.fontLabelSmall
            font.bold: true
            color: root.hovered ? Theme.surfaceText : Theme.surfaceVariantText
            anchors.verticalCenter: parent.verticalCenter
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

}
