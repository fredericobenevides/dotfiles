import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import qs.modules.weather
import qs.theme

PanelWindow {
    id: weatherModal

    property bool showHourly: false

    focusable: true

    visible: false
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

        source: Qt.resolvedUrl("/usr/share/quickshell/dms/assets/fonts/material-design-icons/variablefont/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    MouseArea {
        anchors.fill: parent
        onClicked: weatherModal.visible = false
    }

    Rectangle {
        id: bg

        width: 720
        height: content.implicitHeight + 48
        anchors.top: parent.top
        anchors.topMargin: 6
        anchors.horizontalCenter: parent.horizontalCenter
        radius: 16
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: 1

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: content

            anchors.fill: parent
            anchors.margins: 12
            spacing: 12

            RowLayout {
                Layout.fillWidth: true
                spacing: 8

                Text {
                    text: "\uE2CA"
                    font.family: materialSymbols.name
                    font.pixelSize: 18
                    color: Theme.primary
                }

                Text {
                    Layout.fillWidth: true
                    text: "Weather"
                    font.pixelSize: 18
                    font.bold: true
                    color: Theme.surfaceText
                }

                Rectangle {
                    id: refreshButton

                    width: 24
                    height: 24
                    radius: 12
                    color: refreshMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "\uE5D5"
                        font.family: materialSymbols.name
                        font.pixelSize: 14
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

                    width: 24
                    height: 24
                    radius: 12
                    color: locationMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "\uE55C"
                        font.family: materialSymbols.name
                        font.pixelSize: 14
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
                radius: 14
                color: Theme.surfaceContainerHigh
                border.color: Theme.surfaceContainerHighest
                border.width: 1
                implicitHeight: heroContent.implicitHeight + 24

                RowLayout {
                    id: heroContent

                    anchors.fill: parent
                    anchors.margins: 20
                    spacing: 24

                    ColumnLayout {
                        Layout.preferredWidth: 260
                        Layout.alignment: Qt.AlignVCenter
                        spacing: 4

                        RowLayout {
                            spacing: 10

                            Text {
                                text: WeatherService.getWeatherIcon(WeatherService.wCode, WeatherService.isDay)
                                font.family: materialSymbols.name
                                font.pixelSize: 48
                                color: Theme.primary
                            }

                            ColumnLayout {
                                Layout.fillWidth: true
                                Layout.alignment: Qt.AlignVCenter
                                spacing: 2

                                Text {
                                    text: WeatherService.formatTemp(WeatherService.temp) || "--°C"
                                    font.pixelSize: 40
                                    font.weight: Font.Light
                                    color: Theme.surfaceText
                                }

                                Text {
                                    text: WeatherService.getWeatherCondition(WeatherService.wCode)
                                    font.pixelSize: Theme.fontLabelMedium
                                    color: Theme.surfaceVariantText
                                }

                                Text {
                                    text: WeatherService.city
                                    font.pixelSize: Theme.fontLabelSmall
                                    color: Theme.surfaceVariantText
                                }

                            }

                        }

                    }

                    Rectangle {
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        implicitHeight: 150
                        radius: 12
                        color: Theme.surfaceContainer
                        border.color: Theme.surfaceContainerHighest
                        border.width: 1

                        GridLayout {
                            anchors.fill: parent
                            anchors.margins: 16
                            columns: 3
                            columnSpacing: 20
                            rowSpacing: 12

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
                                    spacing: 2

                                    Text {
                                        text: modelData.label
                                        font.pixelSize: Theme.fontLabelSmall
                                        color: Theme.surfaceVariantText
                                    }

                                    Text {
                                        text: modelData.value
                                        font.pixelSize: Theme.fontLabelLarge
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
                spacing: 8

                RowLayout {
                    Layout.fillWidth: true
                    spacing: 4

                    Rectangle {
                        Layout.preferredWidth: 64
                        Layout.preferredHeight: 24
                        radius: 12
                        color: !weatherModal.showHourly ? Theme.primary : Theme.surfaceContainerHigh

                        Text {
                            anchors.centerIn: parent
                            text: "Daily"
                            font.pixelSize: Theme.fontLabelSmall
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
                        Layout.preferredWidth: 64
                        Layout.preferredHeight: 24
                        radius: 12
                        color: weatherModal.showHourly ? Theme.primary : Theme.surfaceContainerHigh

                        Text {
                            anchors.centerIn: parent
                            text: "Hourly"
                            font.pixelSize: Theme.fontLabelSmall
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
                spacing: 8

                Repeater {
                    model: weatherModal.showHourly ? WeatherService.hourlyList : WeatherService.dailyForecast

                    delegate: Rectangle {
                        Layout.fillWidth: true
                        Layout.preferredHeight: 150
                        radius: 12
                        color: Theme.surfaceContainerHigh
                        border.color: Theme.surfaceContainerHighest
                        border.width: 1

                        ColumnLayout {
                            anchors.fill: parent
                            anchors.margins: 8
                            spacing: 4

                            Item {
                                Layout.fillHeight: true
                            }

                            Text {
                                Layout.alignment: Qt.AlignHCenter
                                text: weatherModal.showHourly ? WeatherService.formatHour(modelData.time) : WeatherService.formatDayName(modelData.date, index)
                                font.pixelSize: Theme.fontLabelSmall
                                font.bold: true
                                color: Theme.surfaceText
                            }

                            Text {
                                Layout.alignment: Qt.AlignHCenter
                                text: weatherModal.showHourly ? WeatherService.getWeatherIcon(modelData.code, WeatherService.hourIsDay(modelData.time)) : WeatherService.getWeatherIcon(modelData.code, true)
                                font.family: materialSymbols.name
                                font.pixelSize: 26
                                color: Theme.primary
                            }

                            Text {
                                Layout.alignment: Qt.AlignHCenter
                                text: weatherModal.showHourly ? (WeatherService.formatTemp(modelData.temp) || "--") : WeatherService.formatTemp(modelData.tempMin) + " / " + WeatherService.formatTemp(modelData.tempMax)
                                font.pixelSize: Theme.fontLabelSmall
                                color: Theme.surfaceVariantText
                            }

                            Text {
                                Layout.alignment: Qt.AlignHCenter
                                visible: !weatherModal.showHourly && modelData.precip > 0
                                text: WeatherService.formatPrecipitation(modelData.precip)
                                font.pixelSize: Theme.fontLabelSmall
                                color: Theme.surfaceVariantText
                            }

                            Item {
                                Layout.fillHeight: true
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
            width: tooltipLabel.implicitWidth + 14
            height: tooltipLabel.implicitHeight + 8
            radius: 6
            color: Theme.surfaceContainerHighest
            border.color: Theme.outlineVariant
            border.width: 1
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
                font.pixelSize: Theme.fontLabelSmall
                color: Theme.surfaceText
            }

        }

    }

}
