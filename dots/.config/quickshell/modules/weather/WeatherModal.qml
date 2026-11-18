import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import qs.modules.weather
import qs.theme

PanelWindow {
    id: weatherModal

    property real fontScale: 1.0
    property var targetScreen: null
    property bool showHourly: false

    focusable: true
    visible: false
    screen: targetScreen
    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true
    color: "transparent"
    onVisibleChanged: {
        if (visible)
            bg.forceActiveFocus();

    }

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    MouseArea {
        anchors.fill: parent
        onClicked: weatherModal.visible = false
    }

    Rectangle {
        id: bg

        width: Theme.scaledSize(720, weatherModal.fontScale)
        height: content.implicitHeight + Theme.scaledSize(48, weatherModal.fontScale)
        anchors.centerIn: parent
        radius: Theme.scaledSize(16, weatherModal.fontScale)
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: Theme.scaledSize(1, weatherModal.fontScale)

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: content

            anchors.fill: parent
            anchors.margins: Theme.scaledSize(12, weatherModal.fontScale)
            spacing: Theme.scaledSize(12, weatherModal.fontScale)

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, weatherModal.fontScale)

                Text {
                    text: "\uE2CA"
                    font.family: materialSymbols.name
                    font.pixelSize: Theme.scaledFont(18, weatherModal.fontScale)
                    color: Theme.primary
                }

                Text {
                    Layout.fillWidth: true
                    text: "Weather"
                    font.pixelSize: Theme.scaledFont(18, weatherModal.fontScale)
                    font.bold: true
                    color: Theme.surfaceText
                }

                Rectangle {
                    id: refreshButton

                    width: Theme.scaledSize(24, weatherModal.fontScale)
                    height: Theme.scaledSize(24, weatherModal.fontScale)
                    radius: Theme.scaledSize(12, weatherModal.fontScale)
                    color: refreshMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "\uE5D5"
                        font.family: materialSymbols.name
                        font.pixelSize: Theme.scaledFont(14, weatherModal.fontScale)
                        color: Theme.surfaceText
                    }

                    MouseArea {
                        id: refreshMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: WeatherService.refreshWeather()
                    }

                }

                Rectangle {
                    id: locationButton

                    width: Theme.scaledSize(24, weatherModal.fontScale)
                    height: Theme.scaledSize(24, weatherModal.fontScale)
                    radius: Theme.scaledSize(12, weatherModal.fontScale)
                    color: locationMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "\uE55C"
                        font.family: materialSymbols.name
                        font.pixelSize: Theme.scaledFont(14, weatherModal.fontScale)
                        color: Theme.surfaceText
                    }

                    MouseArea {
                        id: locationMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: WeatherService.refreshLocation()
                    }

                }

            }

            Rectangle {
                Layout.fillWidth: true
                radius: Theme.scaledSize(14, weatherModal.fontScale)
                color: Theme.surfaceContainerHigh
                border.color: Theme.surfaceContainerHighest
                border.width: Theme.scaledSize(1, weatherModal.fontScale)
                implicitHeight: heroContent.implicitHeight + Theme.scaledSize(24, weatherModal.fontScale)

                RowLayout {
                    id: heroContent

                    anchors.fill: parent
                    anchors.margins: Theme.scaledSize(20, weatherModal.fontScale)
                    spacing: Theme.scaledSize(24, weatherModal.fontScale)

                    ColumnLayout {
                        Layout.preferredWidth: Theme.scaledSize(260, weatherModal.fontScale)
                        Layout.alignment: Qt.AlignVCenter
                        spacing: Theme.scaledSize(4, weatherModal.fontScale)

                        RowLayout {
                            spacing: Theme.scaledSize(10, weatherModal.fontScale)

                            Text {
                                text: WeatherService.getWeatherIcon(WeatherService.wCode, WeatherService.isDay)
                                font.family: materialSymbols.name
                                font.pixelSize: Theme.scaledFont(48, weatherModal.fontScale)
                                color: Theme.primary
                            }

                            ColumnLayout {
                                Layout.fillWidth: true
                                Layout.alignment: Qt.AlignVCenter
                                spacing: Theme.scaledSize(2, weatherModal.fontScale)

                                Text {
                                    text: WeatherService.formatTemp(WeatherService.temp) || "--°C"
                                    font.pixelSize: Theme.scaledFont(40, weatherModal.fontScale)
                                    font.weight: Font.Light
                                    color: Theme.surfaceText
                                }

                                Text {
                                    text: WeatherService.getWeatherCondition(WeatherService.wCode)
                                    font.pixelSize: Theme.scaledFont(Theme.fontLabelMedium, weatherModal.fontScale)
                                    color: Theme.surfaceVariantText
                                }

                                Text {
                                    text: WeatherService.city
                                    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, weatherModal.fontScale)
                                    color: Theme.surfaceVariantText
                                }

                            }

                        }

                    }

                    Rectangle {
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        implicitHeight: Theme.scaledSize(150, weatherModal.fontScale)
                        radius: Theme.scaledSize(12, weatherModal.fontScale)
                        color: Theme.surfaceContainer
                        border.color: Theme.surfaceContainerHighest
                        border.width: Theme.scaledSize(1, weatherModal.fontScale)

                        GridLayout {
                            anchors.fill: parent
                            anchors.margins: Theme.scaledSize(16, weatherModal.fontScale)
                            columns: 3
                            columnSpacing: Theme.scaledSize(20, weatherModal.fontScale)
                            rowSpacing: Theme.scaledSize(12, weatherModal.fontScale)

                            Repeater {
                                model: [{
                                    "label": "Humidity",
                                    "value": WeatherService.formatPercent(WeatherService.humidity) || "--"
                                }, {
                                    "label": "Wind",
                                    "value": WeatherService.formatSpeed(WeatherService.wind) || "--"
                                }, {
                                    "label": "Pressure",
                                    "value": WeatherService.formatPressure(WeatherService.pressure) || "--"
                                }, {
                                    "label": "Cloud Cover",
                                    "value": WeatherService.formatPercent(WeatherService.cloudCover)
                                }, {
                                    "label": "Sunrise",
                                    "value": WeatherService.sunrise
                                }, {
                                    "label": "Sunset",
                                    "value": WeatherService.sunset
                                }]

                                delegate: ColumnLayout {
                                    Layout.fillWidth: true
                                    spacing: Theme.scaledSize(2, weatherModal.fontScale)

                                    Text {
                                        text: modelData.label
                                        font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, weatherModal.fontScale)
                                        color: Theme.surfaceVariantText
                                    }

                                    Text {
                                        text: modelData.value
                                        font.pixelSize: Theme.scaledFont(Theme.fontLabelLarge, weatherModal.fontScale)
                                        font.bold: true
                                        color: Theme.surfaceText
                                    }

                                }

                            }

                        }

                    }

                }

            }

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, weatherModal.fontScale)

                RowLayout {
                    Layout.fillWidth: true
                    spacing: Theme.scaledSize(4, weatherModal.fontScale)

                    Rectangle {
                        Layout.preferredWidth: Theme.scaledSize(64, weatherModal.fontScale)
                        Layout.preferredHeight: Theme.scaledSize(24, weatherModal.fontScale)
                        radius: Theme.scaledSize(12, weatherModal.fontScale)
                        color: !weatherModal.showHourly ? Theme.primary : Theme.surfaceContainerHigh

                        Text {
                            anchors.centerIn: parent
                            text: "Daily"
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, weatherModal.fontScale)
                            font.bold: true
                            color: !weatherModal.showHourly ? Theme.primaryText : Theme.surfaceVariantText
                        }

                        MouseArea {
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: weatherModal.showHourly = false
                        }

                    }

                    Rectangle {
                        Layout.preferredWidth: Theme.scaledSize(64, weatherModal.fontScale)
                        Layout.preferredHeight: Theme.scaledSize(24, weatherModal.fontScale)
                        radius: Theme.scaledSize(12, weatherModal.fontScale)
                        color: weatherModal.showHourly ? Theme.primary : Theme.surfaceContainerHigh

                        Text {
                            anchors.centerIn: parent
                            text: "Hourly"
                            font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, weatherModal.fontScale)
                            font.bold: true
                            color: weatherModal.showHourly ? Theme.primaryText : Theme.surfaceVariantText
                        }

                        MouseArea {
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: weatherModal.showHourly = true
                        }

                    }

                }

            }

            RowLayout {
                Layout.fillWidth: true
                spacing: Theme.scaledSize(8, weatherModal.fontScale)

                Repeater {
                    model: weatherModal.showHourly ? WeatherService.hourlyList : WeatherService.dailyForecast

                    delegate: Rectangle {
                        Layout.fillWidth: true
                        Layout.preferredHeight: Theme.scaledSize(150, weatherModal.fontScale)
                        radius: Theme.scaledSize(12, weatherModal.fontScale)
                        color: Theme.surfaceContainerHigh
                        border.color: Theme.surfaceContainerHighest
                        border.width: Theme.scaledSize(1, weatherModal.fontScale)

                        // Daily layout (centered column)
                        ColumnLayout {
                            anchors.fill: parent
                            anchors.margins: Theme.scaledSize(8, weatherModal.fontScale)
                            spacing: Theme.scaledSize(4, weatherModal.fontScale)
                            visible: !weatherModal.showHourly

                            Item {
                                Layout.fillHeight: true
                            }

                            Text {
                                Layout.alignment: Qt.AlignHCenter
                                text: WeatherService.formatDayName(modelData.date, index)
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, weatherModal.fontScale)
                                font.bold: true
                                color: Theme.surfaceText
                            }

                            Text {
                                Layout.alignment: Qt.AlignHCenter
                                text: WeatherService.getWeatherIcon(modelData.code, true)
                                font.family: materialSymbols.name
                                font.pixelSize: Theme.scaledFont(26, weatherModal.fontScale)
                                color: Theme.primary
                            }

                            Text {
                                Layout.alignment: Qt.AlignHCenter
                                text: WeatherService.formatTemp(modelData.tempMin) + " / " + WeatherService.formatTemp(modelData.tempMax)
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, weatherModal.fontScale)
                                color: Theme.surfaceVariantText
                            }

                            Text {
                                Layout.alignment: Qt.AlignHCenter
                                visible: modelData.precip > 0
                                text: WeatherService.formatPrecipitation(modelData.precip)
                                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, weatherModal.fontScale)
                                color: Theme.surfaceVariantText
                            }

                            Item {
                                Layout.fillHeight: true
                            }

                        }

                        // Hourly layout (icon left, data right)
                        RowLayout {
                            anchors.fill: parent
                            anchors.margins: Theme.scaledSize(10, weatherModal.fontScale)
                            spacing: Theme.scaledSize(10, weatherModal.fontScale)
                            visible: weatherModal.showHourly

                            ColumnLayout {
                                Layout.preferredWidth: Theme.scaledSize(60, weatherModal.fontScale)
                                Layout.alignment: Qt.AlignVCenter
                                spacing: Theme.scaledSize(2, weatherModal.fontScale)

                                Text {
                                    Layout.alignment: Qt.AlignHCenter
                                    text: WeatherService.formatHour(modelData.time)
                                    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, weatherModal.fontScale)
                                    font.bold: true
                                    color: Theme.surfaceText
                                }

                                Text {
                                    Layout.alignment: Qt.AlignHCenter
                                    text: WeatherService.getWeatherIcon(modelData.code, WeatherService.hourIsDay(modelData.time))
                                    font.family: materialSymbols.name
                                    font.pixelSize: Theme.scaledFont(28, weatherModal.fontScale)
                                    color: Theme.primary
                                }

                                Text {
                                    Layout.alignment: Qt.AlignHCenter
                                    text: WeatherService.formatTemp(modelData.temp) || "--"
                                    font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, weatherModal.fontScale)
                                    color: Theme.surfaceVariantText
                                }

                            }

                            GridLayout {
                                Layout.fillWidth: true
                                Layout.fillHeight: true
                                columns: 1
                                rowSpacing: Theme.scaledSize(4, weatherModal.fontScale)

                                Repeater {
                                    model: [{
                                        "label": "Humidity",
                                        "value": WeatherService.formatPercent(modelData.humidity)
                                    }, {
                                        "label": "Wind",
                                        "value": WeatherService.formatSpeed(modelData.wind)
                                    }, {
                                        "label": "Pressure",
                                        "value": WeatherService.formatPressure(modelData.pressure)
                                    }, {
                                        "label": "Cloud",
                                        "value": WeatherService.formatPercent(modelData.cloudCover)
                                    }]

                                    delegate: ColumnLayout {
                                        Layout.fillWidth: true
                                        spacing: Theme.scaledSize(0, weatherModal.fontScale)

                                        Text {
                                            text: modelData.label
                                            font.pixelSize: Theme.scaledFont(9, weatherModal.fontScale)
                                            color: Theme.surfaceVariantText
                                        }

                                        Text {
                                            text: modelData.value
                                            font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, weatherModal.fontScale)
                                            font.bold: true
                                            color: Theme.surfaceText
                                        }

                                    }

                                }

                            }

                        }

                    }

                }

            }

        }

        Rectangle {
            id: buttonTooltip

            function updatePosition() {
                const target = refreshMouse.containsMouse ? refreshButton : locationButton;
                const pos = target.mapToItem(buttonTooltip.parent, 0, target.height + 4);
                buttonTooltip.x = pos.x + (target.width - buttonTooltip.width) / 2;
                buttonTooltip.y = pos.y;
            }

            visible: refreshMouse.containsMouse || locationMouse.containsMouse
            width: tooltipLabel.implicitWidth + Theme.scaledSize(14, weatherModal.fontScale)
            height: tooltipLabel.implicitHeight + Theme.scaledSize(8, weatherModal.fontScale)
            radius: Theme.scaledSize(6, weatherModal.fontScale)
            color: Theme.surfaceContainerHighest
            border.color: Theme.outlineVariant
            border.width: Theme.scaledSize(1, weatherModal.fontScale)
            z: 100
            onVisibleChanged: positionTimer.restart()

            Timer {
                id: positionTimer

                interval: 0
                repeat: false
                onTriggered: buttonTooltip.updatePosition()
            }

            Text {
                id: tooltipLabel

                anchors.centerIn: parent
                text: refreshMouse.containsMouse ? "Refresh weather data" : "Re-detect location"
                font.pixelSize: Theme.scaledFont(Theme.fontLabelSmall, weatherModal.fontScale)
                color: Theme.surfaceText
            }

        }

    }

}
