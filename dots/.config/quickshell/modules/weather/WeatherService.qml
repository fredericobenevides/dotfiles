pragma Singleton
pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    readonly property string cacheDir: Quickshell.env("HOME") + "/.cache/quickshell"
    readonly property string cachePath: cacheDir + "/weather.json"
    readonly property string locationCachePath: cacheDir + "/weather-location.json"
    readonly property int locationTtlMs: 24 * 60 * 60 * 1000
    readonly property int weatherTtlMs: 15 * 60 * 1000

    property bool loading: true
    property bool available: false
    property string city: ""
    property string country: ""
    property real latitude: NaN
    property real longitude: NaN
    property double locationUpdatedAt: 0
    property double weatherUpdatedAt: 0
    property var weather: ({})

    property var weatherIcons: ({
        "0": "clear_day",
        "1": "clear_day",
        "2": "partly_cloudy_day",
        "3": "cloud",
        "45": "foggy",
        "48": "foggy",
        "51": "rainy",
        "53": "rainy",
        "55": "rainy",
        "56": "rainy",
        "57": "rainy",
        "61": "rainy",
        "63": "rainy",
        "65": "rainy",
        "66": "rainy",
        "67": "rainy",
        "71": "cloudy_snowing",
        "73": "cloudy_snowing",
        "75": "snowing_heavy",
        "77": "cloudy_snowing",
        "80": "rainy",
        "81": "rainy",
        "82": "rainy",
        "85": "cloudy_snowing",
        "86": "snowing_heavy",
        "95": "thunderstorm",
        "96": "thunderstorm",
        "99": "thunderstorm"
    })

    property var nightWeatherIcons: ({
        "0": "clear_night",
        "1": "clear_night",
        "2": "partly_cloudy_night",
        "3": "cloud",
        "45": "foggy",
        "48": "foggy",
        "51": "rainy",
        "53": "rainy",
        "55": "rainy",
        "56": "rainy",
        "57": "rainy",
        "61": "rainy",
        "63": "rainy",
        "65": "rainy",
        "66": "rainy",
        "67": "rainy",
        "71": "cloudy_snowing",
        "73": "cloudy_snowing",
        "75": "snowing_heavy",
        "77": "cloudy_snowing",
        "80": "rainy",
        "81": "rainy",
        "82": "rainy",
        "85": "cloudy_snowing",
        "86": "snowing_heavy",
        "95": "thunderstorm",
        "96": "thunderstorm",
        "99": "thunderstorm"
    })

    property var dailyForecast: []
    property var hourlyList: []
    property string sunrise: "--"
    property string sunset: "--"
    property int temp: 0
    property int humidity: 0
    property string wind: ""
    property int pressure: 0
    property int cloudCover: 0
    property int wCode: 0
    property bool isDay: true
    property bool _bootstrapStarted: false
    property int _cacheSettled: 0

    function handleCacheSettled() {
        _cacheSettled++;
        if (_cacheSettled < 2)
            return;
        if (hasValidCoords())
            refreshWeather();
        else
            bootstrapLocation();
    }

    Timer {
        id: weatherRefreshTimer

        interval: weatherTtlMs
        repeat: true
        running: false
        onTriggered: root.refreshWeather()
    }

    function bootstrapLocation() {
        if (_bootstrapStarted)
            return;
        _bootstrapStarted = true;
        locationProcess.command = ["curl", "-sS", "--fail", "--connect-timeout", "3", "--max-time", "6", "--compressed", "https://ipwho.is/?output=json"];
        locationProcess.running = true;
    }

    function refreshLocation() {
        _bootstrapStarted = false;
        locationProcess.command = ["curl", "-sS", "--fail", "--connect-timeout", "3", "--max-time", "6", "--compressed", "https://ipwho.is/?output=json"];
        locationProcess.running = true;
    }

    Component.onCompleted: {
        Quickshell.execDetached(["mkdir", "-p", cacheDir]);
    }

    function getWeatherIcon(code, day = true) {
        const map = day ? weatherIcons : nightWeatherIcons;
        return map[String(code)] || "cloud";
    }

    function getWeatherCondition(code) {
        const conditions = {
            "0": "Clear Sky",
            "1": "Clear Sky",
            "2": "Partly Cloudy",
            "3": "Overcast",
            "45": "Fog",
            "48": "Fog",
            "51": "Drizzle",
            "53": "Drizzle",
            "55": "Drizzle",
            "56": "Freezing Drizzle",
            "57": "Freezing Drizzle",
            "61": "Light Rain",
            "63": "Rain",
            "65": "Heavy Rain",
            "66": "Light Rain",
            "67": "Heavy Rain",
            "71": "Light Snow",
            "73": "Snow",
            "75": "Heavy Snow",
            "77": "Snow",
            "80": "Light Rain",
            "81": "Rain",
            "82": "Heavy Rain",
            "85": "Light Snow Showers",
            "86": "Heavy Snow Showers",
            "95": "Thunderstorm",
            "96": "Thunderstorm with Hail",
            "99": "Thunderstorm with Hail"
        }
        return conditions[String(code)] || "Unknown"
    }

    function formatTemp(celsius, includeUnits = true) {
        if (celsius == null)
            return null
        const value = Math.round(celsius)
        return includeUnits ? value + "°C" : value
    }

    function formatSpeed(kmh, includeUnits = true) {
        if (kmh == null)
            return null
        return includeUnits ? Math.round(kmh) + " km/h" : Math.round(kmh)
    }

    function formatPressure(hpa, includeUnits = true) {
        if (hpa == null)
            return null
        return includeUnits ? Math.round(hpa) + " hPa" : Math.round(hpa)
    }

    function formatPrecipitation(mm, includeUnits = true) {
        if (mm == null || mm === 0)
            return "0 mm"
        return includeUnits ? mm.toFixed(1) + " mm" : mm.toFixed(1)
    }

    function formatPercent(percent, includeUnits = true) {
        if (percent == null)
            return null
        return includeUnits ? Math.round(percent) + "%" : Math.round(percent)
    }

    function formatTime(isoString) {
        if (!isoString)
            return "--"
        try {
            return new Date(isoString).toLocaleTimeString(Qt.locale(), "HH:mm")
        } catch (e) {
            return "--"
        }
    }

    function formatDayName(isoDate, index) {
        if (index === 0)
            return "Today";
        if (index === 1)
            return "Tomorrow";

        try {
            return new Date(isoDate).toLocaleDateString(Qt.locale("en_US"), "ddd");
        } catch (e) {
            return "Day";
        }
    }

    function formatHour(isoString) {
        if (!isoString)
            return "--"
        try {
            return new Date(isoString).toLocaleTimeString(Qt.locale("en_US"), "HH:mm");
        } catch (e) {
            return "--"
        }
    }

    function hourIsDay(isoString) {
        try {
            const hour = new Date(isoString).getHours();
            return hour >= 6 && hour < 18;
        } catch (e) {
            return isDay;
        }
    }

    function hasValidCoords() {
        return latitude == latitude && longitude == longitude;
    }

    function isCacheFresh(updatedAt, ttlMs) {
        return updatedAt > 0 && (Date.now() - updatedAt) < ttlMs;
    }

    function hasFreshLocation() {
        return hasValidCoords() && isCacheFresh(locationUpdatedAt, locationTtlMs);
    }

    function hasFreshWeather() {
        return hasValidCoords() && isCacheFresh(weatherUpdatedAt, weatherTtlMs);
    }

    function loadLocationCache(text) {
        if (!text || text.trim() === "")
            return;

        try {
            const data = JSON.parse(text);
            if (data.latitude == null || data.longitude == null)
                return;

            root.latitude = data.latitude;
            root.longitude = data.longitude;
            root.city = data.city || root.city;
            root.country = data.country || root.country;
            root.locationUpdatedAt = data.updatedAt || 0;
        } catch (e) {
        }
    }

    function loadWeatherCache(text) {
        if (!text || text.trim() === "")
            return;

        try {
            const data = JSON.parse(text);
            if (!data.weather)
                return;

            if (!hasValidCoords() && data.latitude != null && data.longitude != null) {
                root.latitude = data.latitude;
                root.longitude = data.longitude;
            }

            root.city = data.city || root.city;
            root.country = data.country || root.country;
            root.locationUpdatedAt = data.locationUpdatedAt || root.locationUpdatedAt;
            root.weatherUpdatedAt = data.weatherUpdatedAt || 0;
            applyWeatherData(data.weather, true);
        } catch (e) {
        }
    }

    function saveLocationCache() {
        if (!hasValidCoords())
            return;

        locationCacheFileView.setText(JSON.stringify({
            "latitude": latitude,
            "longitude": longitude,
            "city": city,
            "country": country,
            "updatedAt": locationUpdatedAt
        }, null, 2));
    }

    function saveWeatherCache(data) {
        if (!hasValidCoords() || !data)
            return;

        weatherCacheFileView.setText(JSON.stringify({
            "latitude": latitude,
            "longitude": longitude,
            "city": city,
            "country": country,
            "locationUpdatedAt": locationUpdatedAt,
            "weatherUpdatedAt": weatherUpdatedAt,
            "weather": data
        }, null, 2));
    }

    function applyWeatherData(data, fromCache = false) {
        const current = data.current || {};
        const daily = data.daily || {};
        const hourly = data.hourly || {};
        root.weather = data;
        root.temp = current.temperature_2m ?? 0;
        root.humidity = current.relative_humidity_2m ?? 0;
        root.wind = current.wind_speed_10m ?? "";
        root.pressure = current.surface_pressure ?? 0;
        root.wCode = current.weather_code ?? 0;
        root.isDay = current.is_day === 1;
        root.sunrise = daily.sunrise?.[0] ? root.formatTime(daily.sunrise[0]) : "--";
        root.sunset = daily.sunset?.[0] ? root.formatTime(daily.sunset[0]) : "--";
        const days = daily.time || [];
        const dailyList = [];
        for (let i = 0; i < days.length; i++) {
            dailyList.push({
                "date": days[i],
                "code": daily.weather_code?.[i] ?? 0,
                "tempMax": daily.temperature_2m_max?.[i] ?? 0,
                "tempMin": daily.temperature_2m_min?.[i] ?? 0,
                "precip": daily.precipitation_sum?.[i] ?? 0
            });
        }
        root.dailyForecast = dailyList;

        const hours = hourly.time || [];
        const hourList = [];
        const now = new Date();
        now.setMinutes(0, 0, 0);
        let startIndex = 0;
        for (let i = 0; i < hours.length; i++) {
            try {
                if (new Date(hours[i]).getTime() >= now.getTime()) {
                    startIndex = i;
                    break;
                }
            } catch (e) {
            }
        }
        root.cloudCover = hourly.cloud_cover?.[startIndex] ?? 0;
        for (let i = startIndex; i < Math.min(hours.length, startIndex + 5); i++) {
            hourList.push({
                "time": hours[i],
                "code": hourly.weather_code?.[i] ?? 0,
                "temp": hourly.temperature_2m?.[i] ?? 0,
                "humidity": hourly.relative_humidity_2m?.[i] ?? 0,
                "wind": hourly.wind_speed_10m?.[i] ?? 0,
                "pressure": hourly.surface_pressure?.[i] ?? 0,
                "cloudCover": hourly.cloud_cover?.[i] ?? 0,
                "precip": hourly.precipitation?.[i] ?? 0
            });
        }
        root.hourlyList = hourList;
        root.available = true;

        if (!fromCache) {
            weatherUpdatedAt = Date.now();
            saveWeatherCache(data);
        }

        root.loading = false;
        weatherRefreshTimer.restart();
    }

    function refresh(forceLocation = false) {
        loading = true;

        if (hasFreshWeather() && !forceLocation) {
            applyWeatherData(weather, true);
            loading = false;
            return;
        }

        if (hasValidCoords() && !forceLocation) {
            fetchWeather(latitude, longitude, city, country);
            return;
        }

        bootstrapLocation();
    }

    function refreshWeather() {
        if (hasValidCoords()) {
            fetchWeather(latitude, longitude, city, country);
            return;
        }

        refresh();
    }

    function fetchWeather(lat, lon, city, country) {
        const url = "https://api.open-meteo.com/v1/forecast?latitude=" + lat + "&longitude=" + lon + "&current=temperature_2m,relative_humidity_2m,is_day,weather_code,surface_pressure,wind_speed_10m&daily=sunrise,sunset,temperature_2m_max,temperature_2m_min,weather_code,precipitation_sum&hourly=temperature_2m,weather_code,wind_speed_10m,relative_humidity_2m,surface_pressure,visibility,cloud_cover&timezone=auto&forecast_days=7"
        root.latitude = lat
        root.longitude = lon
        root.city = city || "Local Weather"
        root.country = country || ""
        locationUpdatedAt = Date.now()
        saveLocationCache()
        weatherProcess.command = ["curl", "-sS", "--fail", "--connect-timeout", "3", "--max-time", "6", "--compressed", url]
        weatherProcess.running = true
    }

    FileView {
        id: locationCacheFileView
        path: locationCachePath
        atomicWrites: true
        watchChanges: true
        onLoaded: {
            loadLocationCache(text());
            root.handleCacheSettled();
        }
        onLoadFailed: error => {
            root.handleCacheSettled();
        }
    }

    FileView {
        id: weatherCacheFileView
        path: cachePath
        atomicWrites: true
        watchChanges: true
        onLoaded: {
            loadWeatherCache(text());
            root.handleCacheSettled();
        }
        onLoadFailed: error => {
            root.handleCacheSettled();
        }
    }

    Process {
        id: locationProcess

        stdout: StdioCollector {
            onStreamFinished: {
                const raw = text.trim()
                if (!raw || raw[0] !== "{") {
                    root.loading = false
                    root.available = false
                    return
                }

                try {
                    const data = JSON.parse(raw)
                    if ((data.success !== true && data.status !== "success") || data.latitude == null || data.longitude == null) {
                        root.available = false
                        root.loading = false
                        return
                    }

                    root.latitude = data.latitude
                    root.longitude = data.longitude
                    root.city = data.city || "Local Weather"
                    root.country = data.country || ""
                    root.locationUpdatedAt = Date.now()
                    saveLocationCache()
                    root.fetchWeather(data.latitude, data.longitude, data.city, data.country)
                } catch (e) {
                    root.available = false
                    root.loading = false
                }
            }
        }

        onExited: exitCode => {
            if (exitCode !== 0) {
                root.loading = false
                root.available = false
            }
        }
    }

    Process {
        id: weatherProcess

        stdout: StdioCollector {
            onStreamFinished: {
                const raw = text.trim()
                if (!raw || raw[0] !== "{") {
                    root.loading = false
                    root.available = false
                    return
                }

                try {
                    const data = JSON.parse(raw)
                    applyWeatherData(data)
                } catch (e) {
                    root.available = false
                }

                root.loading = false
            }
        }

        onExited: exitCode => {
            if (exitCode !== 0) {
                root.loading = false
                root.available = false
            }
        }
    }
}
