import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs.modules.vpn
import qs.theme

PanelWindow {
    id: networkMenu

    property string activeType: ""
    property string selectedTab: "ethernet"
    readonly property string fontFamily: "JetBrainsMono Nerd Font"
    property var ethernetDevices: []
    property string activeEthernetDevice: ""
    property var wifiAccumulated: []
    property int wifiScanAttempts: 0
    readonly property int wifiMaxScanAttempts: 5
    property string activeWifiDevice: ""
    property string activeWifiSsid: ""
    property var ambiguousSsid: ({
    })
    property var savedNetworks: []
    property bool wifiScanning: false
    property string passwordTarget: ""
    property string passwordInput: ""
    property var infoTarget: ({
    })
    property bool showInfoModal: false
    property var infoModalRows: []
    property var networkInfo: ({
    })
    property var infoRows: []
    property var pendingInfoData: null
    property var typeBuffer: []
    property var deviceBuffer: []
    property var infoBuffer: []
    property var wifiBuffer: []
    property var savedBuffer: []
    property var routeTable: []
    property string defaultRouteDevice: ""
    property bool wifiRadioEnabled: true
    property var savedAutoconnect: ({
    })
    property bool showPassword: false
    property bool disconnecting: false
    property string disconnectingWifiSsid: ""
    property string disconnectingDev: ""
    property bool connecting: false
    property string connectingSsid: ""
    property string connectingDev: ""
    property var routeBuffer: []
    property var radioBuffer: []

    function refresh() {
        wifiAccumulated = [];
        wifiScanAttempts = 0;
        routeBuffer = [];
        routeProcess.command = ["sh", "-c", "ip route show default"];
        routeProcess.running = true;
        radioBuffer = [];
        radioProcess.command = ["nmcli", "-t", "radio"];
        radioProcess.running = true;
        typeBuffer = [];
        typeProcess.command = ["nmcli", "-t", "-f", "TYPE", "connection", "show", "--active"];
        typeProcess.running = true;
        refreshWifi();
    }

    function mergeWifiNetworks(newList) {
        const map = {
        };
        for (let i = 0; i < wifiAccumulated.length; i++) {
            const n = wifiAccumulated[i];
            const key = n.ssid + "|" + (n.band || "2.4GHz");
            map[key] = n;
        }
        for (let i = 0; i < newList.length; i++) {
            const n = newList[i];
            const key = n.ssid + "|" + (n.band || "2.4GHz");
            const existing = map[key];
            if (!existing)
                map[key] = n;
            else if (n.active && !existing.active)
                map[key] = n;
            else if (n.signal > existing.signal)
                map[key] = n;
        }
        const merged = [];
        for (const key in map) merged.push(map[key])
        merged.sort((a, b) => {
            if (a.active !== b.active)
                return a.active ? -1 : 1;

            if (a.saved !== b.saved)
                return a.saved ? -1 : 1;

            return (b.signal || 0) - (a.signal || 0);
        });
        return merged;
    }

    function buildAmbiguousSsid() {
        const counts = {
        };
        for (let i = 0; i < wifiAccumulated.length; i++) {
            const ssid = wifiAccumulated[i].ssid;
            counts[ssid] = (counts[ssid] || 0) + 1;
        }
        const amb = {
        };
        for (const key in counts) if (counts[key] > 1) {
            amb[key] = true;
        }
        ambiguousSsid = amb;
    }

    function refreshWifi() {
        wifiScanning = true;
        if (wifiListProcess.running)
            return ;

        wifiBuffer = [];
        wifiListProcess.command = ["sh", "-c", "nmcli -t -f SSID,SIGNAL,SECURITY,ACTIVE,FREQ,RATE device wifi list --rescan yes || nmcli -t -f SSID,SIGNAL,SECURITY,ACTIVE,FREQ,RATE device wifi list --rescan no"];
        wifiListProcess.running = true;
        savedProcess.running = true;
    }

    function refreshDevices() {
        deviceBuffer = [];
        deviceProcess.command = ["nmcli", "-t", "-f", "DEVICE,TYPE,STATE,CONNECTION", "device", "status"];
        deviceProcess.running = true;
    }

    function refreshInfoForActive() {
        if (selectedTab === "wifi" && activeWifiDevice) {
            refreshInfo(activeWifiDevice);
        } else if (selectedTab === "ethernet" && activeEthernetDevice) {
            refreshInfo(activeEthernetDevice);
        } else if (activeEthernetDevice) {
            refreshInfo(activeEthernetDevice);
        } else if (activeWifiDevice) {
            refreshInfo(activeWifiDevice);
        } else {
            networkInfo = {
            };
            infoRows = [];
        }
    }

    function connectWifi(ssid) {
        if (wifiListProcess.running)
            wifiListProcess.running = false;

        connecting = true;
        connectingSsid = ssid;
        connectingDev = "";
        transitionTimeout.restart();
        if (savedNetworks.indexOf(ssid) !== -1)
            wifiConnectProcess.command = ["nmcli", "connection", "up", ssid];
        else
            wifiConnectProcess.command = ["nmcli", "device", "wifi", "connect", ssid];
        wifiConnectProcess.running = true;
    }

    function forgetNetwork(name) {
        forgetProcess.command = ["nmcli", "connection", "delete", name];
        forgetProcess.running = true;
    }

    function connectWifiWithPassword(ssid, password) {
        passwordTarget = "";
        passwordInput = "";
        if (wifiListProcess.running)
            wifiListProcess.running = false;

        connecting = true;
        connectingSsid = ssid;
        connectingDev = "";
        transitionTimeout.restart();
        wifiConnectProcess.command = ["nmcli", "device", "wifi", "connect", ssid, "password", password];
        wifiConnectProcess.running = true;
    }

    function setAutoconnect(name, enabled) {
        autoconnectProcess.command = ["nmcli", "connection", "modify", name, "connection.autoconnect", enabled ? "yes" : "no"];
        autoconnectProcess.running = true;
    }

    function toggleWifiRadio() {
        radioToggleProcess.command = ["nmcli", "radio", "wifi", wifiRadioEnabled ? "off" : "on"];
        radioToggleProcess.running = true;
    }

    function refreshSaved() {
        savedBuffer = [];
        savedProcess.running = true;
    }

    function buildSectionedRows(map) {
        function pushSection(title, defs) {
            const values = [];
            for (let i = 0; i < defs.length; i++) {
                const value = map[defs[i][0]];
                if (value === undefined || value === "--" || value === "")
                    continue;

                if (Array.isArray(value)) {
                    for (let j = 0; j < value.length; j++) {
                        values.push({
                            "label": defs[i][1],
                            "value": value[j],
                            "indent": true
                        });
                    }
                } else {
                    values.push({
                        "label": defs[i][1],
                        "value": value,
                        "indent": true
                    });
                }
            }
            if (values.length > 0) {
                rows.push({
                    "section": title
                });
                for (let i = 0; i < values.length; i++) {
                    rows.push(values[i]);
                }
            }
        }

        const rows = [];
        const general = [["GENERAL.DEVICE", "Interface"], ["GENERAL.DRIVER", "Driver"], ["GENERAL.HWADDR", "MAC Addr"], ["GENERAL.SPEED", "Speed"]];
        const ip4 = [["IP4.ADDRESS", "IPv4 address"], ["IP4.GATEWAY", "Gateway"], ["IP4.DNS", "DNS"]];
        const ip6 = [["IP6.ADDRESS", "IPv6 address"], ["IP6.GATEWAY", "Gateway"], ["IP6.DNS", "DNS"]];
        for (let i = 0; i < general.length; i++) {
            const value = map[general[i][0]];
            if (value === undefined || value === "--" || value === "")
                continue;

            if (Array.isArray(value)) {
                for (let j = 0; j < value.length; j++) {
                    rows.push({
                        "label": general[i][1],
                        "value": value[j]
                    });
                }
            } else {
                rows.push({
                    "label": general[i][1],
                    "value": value
                });
            }
        }
        pushSection("IPv4", ip4);
        pushSection("IPv6", ip6);
        return rows;
    }

    function buildInfoModalRows(data) {
        if (data && data.active)
            return buildSectionedRows(networkInfo);

        const rows = [];
        if (!data)
            return rows;

        if (data.kind === "wifi") {
            rows.push({
                "label": "Network",
                "value": data.ssid
            });
            rows.push({
                "label": "Signal",
                "value": data.signal + "%"
            });
            rows.push({
                "label": "Security",
                "value": data.security || "Open"
            });
            rows.push({
                "label": "Saved",
                "value": data.saved ? "Yes" : "No"
            });
            rows.push({
                "label": "Connected",
                "value": data.active ? "Yes" : "No"
            });
        } else if (data.kind === "ethernet") {
            rows.push({
                "label": "Device",
                "value": data.device
            });
            rows.push({
                "label": "State",
                "value": data.state
            });
            if (data.connection)
                rows.push({
                "label": "Connection",
                "value": data.connection
            });

        }
        return rows;
    }

    function openInfo(data) {
        passwordTarget = "";
        infoTarget = data;
        pendingInfoData = null;
        if (data && data.active) {
            const device = data.kind === "wifi" ? activeWifiDevice : activeEthernetDevice;
            if (device) {
                pendingInfoData = data;
                infoModalRows = Object.keys(networkInfo).length > 0 ? buildSectionedRows(networkInfo) : [];
                showInfoModal = true;
                refreshInfo(device);
                return ;
            }
        }
        infoModalRows = buildInfoModalRows(data);
        showInfoModal = true;
    }

    function refreshInfo(device) {
        infoBuffer = [];
        infoProcess.command = ["sh", "-c", "nmcli -t -f GENERAL.DEVICE,GENERAL.DRIVER,GENERAL.HWADDR,GENERAL.CONNECTION,IP4.ADDRESS,IP4.GATEWAY,IP4.DNS,IP6.ADDRESS,IP6.GATEWAY,IP6.DNS device show " + device + "; SPEED=$(cat /sys/class/net/" + device + "/speed 2>/dev/null); [ -n \"$SPEED\" ] && echo \"GENERAL.SPEED:${SPEED} Mb/s\""];
        infoProcess.running = true;
    }

    function connectDevice(device) {
        connecting = true;
        connectingDev = device;
        connectingSsid = "";
        transitionTimeout.restart();
        connectProcess.command = ["nmcli", "device", "connect", device];
        connectProcess.running = true;
    }

    function disconnectDevice(device) {
        disconnecting = true;
        disconnectingWifiSsid = device === activeWifiDevice ? activeWifiSsid : "";
        disconnectingDev = device === activeEthernetDevice ? device : "";
        transitionTimeout.restart();
        disconnectProcess.command = ["nmcli", "device", "disconnect", device];
        disconnectProcess.running = true;
    }

    function buildInfoRows(map) {
        const rows = [];
        const defs = [["GENERAL.DEVICE", "Device"], ["GENERAL.CONNECTION", "Connection"], ["IP4.ADDRESS", "IPv4"], ["IP4.GATEWAY", "Gateway"], ["IP4.DNS", "DNS"], ["IP6.ADDRESS", "IPv6"], ["IP6.GATEWAY", "Gateway IPv6"]];
        for (let i = 0; i < defs.length; i++) {
            const key = defs[i][0];
            const label = defs[i][1];
            const value = map[key];
            if (value && value !== "--" && value !== "")
                rows.push({
                "label": label,
                "value": value
            });

        }
        return rows;
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
            bg.forceActiveFocus();
            refresh();
        } else {
            wifiRescanTimer.stop();
        }
    }

    FontLoader {
        id: materialSymbols

        source: Qt.resolvedUrl("/usr/share/fonts/TTF/MaterialSymbolsRounded[FILL,GRAD,opsz,wght].ttf")
    }

    Process {
        id: typeProcess

        onExited: {
            activeType = typeBuffer.indexOf("ethernet") !== -1 ? "ethernet" : "wifi";
            if (activeType === "")
                activeType = "ethernet";

            selectedTab = activeType;
            refreshDevices();
        }

        stdout: SplitParser {
            onRead: (data) => {
                if (data === "802-3-ethernet")
                    typeBuffer.push("ethernet");
                else if (data === "802-11-wireless")
                    typeBuffer.push("wifi");
            }
        }

    }

    Process {
        id: deviceProcess

        onExited: {
            const ethList = [];
            let ethActive = "";
            let wifiActive = "";
            let wifiActiveSsid = "";
            for (let i = 0; i < deviceBuffer.length; i++) {
                const parts = deviceBuffer[i].split(":");
                if (parts.length < 3 || parts[0] === "DEVICE")
                    continue;

                if (parts[1] === "ethernet") {
                    ethList.push({
                        "device": parts[0],
                        "state": parts[2],
                        "connection": parts[3] || ""
                    });
                    if (parts[2] === "connected")
                        ethActive = parts[0];

                } else if (parts[1] === "wifi" && parts[2] === "connected") {
                    wifiActive = parts[0];
                    wifiActiveSsid = parts[3] || "";
                }
            }
            ethernetDevices = ethList;
            activeEthernetDevice = ethActive;
            activeWifiDevice = wifiActive;
            activeWifiSsid = wifiActiveSsid;
            if (disconnectingWifiSsid !== "" && activeWifiSsid === "") {
                disconnecting = false;
                disconnectingWifiSsid = "";
            }
            if (disconnectingDev !== "" && activeEthernetDevice !== disconnectingDev) {
                disconnecting = false;
                disconnectingDev = "";
            }
            if (connectingSsid !== "" && activeWifiSsid !== "") {
                connecting = false;
                connectingSsid = "";
                connectingDev = "";
            }
            if (connectingDev !== "" && activeEthernetDevice === connectingDev) {
                connecting = false;
                connectingSsid = "";
                connectingDev = "";
            }
            refreshInfoForActive();
        }

        stdout: SplitParser {
            onRead: (data) => {
                deviceBuffer.push(data);
            }
        }

    }

    Process {
        id: infoProcess

        onExited: {
            const map = {
            };
            for (let i = 0; i < infoBuffer.length; i++) {
                const line = infoBuffer[i];
                const idx = line.indexOf(":");
                if (idx === -1)
                    continue;

                let key = line.substring(0, idx);
                const value = line.substring(idx + 1);
                const bracket = key.indexOf("[");
                if (bracket !== -1)
                    key = key.substring(0, bracket);

                if (key in map) {
                    if (Array.isArray(map[key]))
                        map[key].push(value);
                    else
                        map[key] = [map[key], value];
                } else {
                    map[key] = value;
                }
            }
            networkInfo = map;
            infoRows = buildInfoRows(map);
            if (pendingInfoData && showInfoModal) {
                infoModalRows = buildSectionedRows(networkInfo);
                pendingInfoData = null;
            }
        }

        stdout: SplitParser {
            onRead: (data) => {
                infoBuffer.push(data);
            }
        }

    }

    Process {
        id: connectProcess

        onExited: {
            refreshDevices();
            refreshWifi();
        }

        stderr: SplitParser {
            onRead: (data) => {
                console.log("connect stderr:", data);
            }
        }

    }

    Process {
        id: disconnectProcess

        onExited: {
            refreshDevices();
            refreshWifi();
        }

        stderr: SplitParser {
            onRead: (data) => {
                console.log("disconnect stderr:", data);
            }
        }

    }

    Process {
        id: forgetProcess

        onExited: {
            refreshWifi();
        }
    }

    Timer {
        id: wifiRescanTimer

        interval: 3500
        onTriggered: {
            if (visible && selectedTab === "wifi")
                refreshWifi();

        }
    }

    Timer {
        id: routeRefreshTimer

        interval: 5000
        repeat: true
        running: visible
        onTriggered: {
            routeBuffer = [];
            routeProcess.running = true;
            refreshDevices();
        }
    }

    Timer {
        id: transitionTimeout

        interval: 8000
        onTriggered: {
            connecting = false;
            connectingSsid = "";
            connectingDev = "";
            disconnecting = false;
            disconnectingWifiSsid = "";
            disconnectingDev = "";
        }
    }

    Process {
        id: wifiListProcess

        onExited: {
            wifiScanning = false;
            const list = [];
            const seen = {
            };
            for (let i = 0; i < wifiBuffer.length; i++) {
                const parts = wifiBuffer[i].split(":");
                if (parts.length < 3 || parts[0] === "")
                    continue;

                const ssid = parts[0];
                const signal = parseInt(parts[1]) || 0;
                const security = parts[2] || "";
                const active = parts[3] === "yes";
                const freq = parts[4] || "";
                const rate = parts[5] || "";
                const freqMhz = parseFloat(freq) || 0;
                const band = freqMhz >= 5000 ? "5GHz" : "2.4GHz";
                const key = ssid + "|" + band;
                if (seen[key] !== undefined) {
                    const existing = list[seen[key]];
                    if (signal > existing.signal) {
                        existing.signal = signal;
                        existing.security = security;
                        existing.active = active;
                        existing.freq = freq;
                        existing.rate = rate;
                    }
                    continue;
                }
                seen[key] = list.length;
                list.push({
                    "ssid": ssid,
                    "signal": signal,
                    "security": security,
                    "active": active,
                    "freq": freq,
                    "rate": rate,
                    "band": band,
                    "saved": savedNetworks.indexOf(ssid) !== -1
                });
            }
            wifiAccumulated = mergeWifiNetworks(list);
            buildAmbiguousSsid();
            wifiScanAttempts += 1;
            if (wifiScanAttempts < wifiMaxScanAttempts && visible && selectedTab === "wifi")
                wifiRescanTimer.start();

        }

        stdout: SplitParser {
            onRead: (data) => {
                wifiBuffer.push(data);
            }
        }

    }

    Process {
        id: savedProcess

        command: ["nmcli", "-t", "-f", "NAME,TYPE,AUTOCONNECT", "connection", "show"]
        onExited: {
            const saved = [];
            const autoconnect = {
            };
            for (let i = 0; i < savedBuffer.length; i++) {
                const parts = savedBuffer[i].split(":");
                if (parts.length >= 3 && parts[1] === "802-11-wireless") {
                    saved.push(parts[0]);
                    autoconnect[parts[0]] = parts[2] === "yes";
                }
            }
            savedNetworks = saved;
            savedAutoconnect = autoconnect;
            savedBuffer = [];
        }

        stdout: SplitParser {
            onRead: (data) => {
                savedBuffer.push(data);
            }
        }

    }

    Process {
        id: autoconnectProcess

        onExited: {
            refreshSaved();
        }
    }

    Process {
        id: routeProcess

        onExited: {
            const table = [];
            for (let i = 0; i < routeBuffer.length; i++) {
                const parts = routeBuffer[i].split(/\s+/);
                if (parts.length === 0 || parts[0] !== "default")
                    continue;

                let device = "";
                let metric = 0;
                for (let j = 1; j < parts.length; j++) {
                    if (parts[j] === "dev" && j + 1 < parts.length)
                        device = parts[++j];
                    else if (parts[j] === "metric" && j + 1 < parts.length)
                        metric = parseInt(parts[++j]) || 0;
                }
                if (device)
                    table.push({
                    "device": device,
                    "metric": metric
                });

            }
            routeTable = table;
            let best = "";
            let bestMetric = Number.MAX_SAFE_INTEGER;
            for (let i = 0; i < table.length; i++) {
                if (table[i].metric < bestMetric) {
                    bestMetric = table[i].metric;
                    best = table[i].device;
                }
            }
            defaultRouteDevice = best;
        }

        stdout: SplitParser {
            onRead: (data) => {
                routeBuffer.push(data);
            }
        }

    }

    Process {
        id: radioProcess

        onExited: {
            const parts = radioBuffer.length > 0 ? radioBuffer[0].split(":") : [];
            wifiRadioEnabled = parts.length >= 2 ? parts[1] === "enabled" : true;
        }

        stdout: SplitParser {
            onRead: (data) => {
                radioBuffer.push(data);
            }
        }

    }

    Process {
        id: radioToggleProcess

        onExited: {
            radioBuffer = [];
            radioProcess.running = true;
            if (wifiListProcess.running)
                wifiListProcess.running = false;

            wifiAccumulated = [];
            wifiScanAttempts = 0;
            refreshWifi();
        }
    }

    Process {
        id: wifiConnectProcess

        onExited: {
            refreshWifi();
            refreshDevices();
        }

        stderr: SplitParser {
            onRead: (data) => {
                console.log("wifi connect stderr:", data);
            }
        }

    }

    MouseArea {
        anchors.fill: parent
        onClicked: networkMenu.visible = false
    }

    Rectangle {
        id: bg

        width: 590
        height: content.implicitHeight + 24
        anchors.top: parent.top
        anchors.topMargin: 6
        anchors.right: parent.right
        anchors.rightMargin: 174
        radius: 16
        color: Theme.surfaceContainer
        border.color: Theme.surfaceContainerHighest
        border.width: 1
        Keys.onPressed: (event) => {
            if (event.key === Qt.Key_Escape) {
                networkMenu.visible = false;
                event.accepted = true;
            }
        }

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: content

            anchors.fill: parent
            anchors.margins: 12
            spacing: 10

            RowLayout {
                Layout.fillWidth: true

                Text {
                    text: "\uF1EB"
                    font.family: networkMenu.fontFamily
                    font.pixelSize: 18
                    color: Theme.surfaceVariantText
                }

                Text {
                    Layout.fillWidth: true
                    text: "Network"
                    font.pixelSize: 18
                    font.bold: true
                    color: Theme.surfaceText
                }

            }

            RowLayout {
                Layout.fillWidth: true
                Layout.topMargin: 8
                spacing: 8

                Rectangle {
                    id: ethTab

                    Layout.fillWidth: true
                    Layout.preferredWidth: 1
                    height: 34
                    radius: 7
                    color: selectedTab === "ethernet" ? Theme.primary : ethTabMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "Ethernet"
                        font.pixelSize: 16
                        font.bold: true
                        color: selectedTab === "ethernet" ? Theme.primaryText : Theme.surfaceText
                    }

                    MouseArea {
                        id: ethTabMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            selectedTab = "ethernet";
                            refreshInfoForActive();
                        }
                    }

                }

                Rectangle {
                    id: wifiTab

                    Layout.fillWidth: true
                    Layout.preferredWidth: 1
                    height: 34
                    radius: 7
                    color: selectedTab === "wifi" ? Theme.primary : wifiTabMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "Wifi"
                        font.pixelSize: 16
                        font.bold: true
                        color: selectedTab === "wifi" ? Theme.primaryText : Theme.surfaceText
                    }

                    MouseArea {
                        id: wifiTabMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            selectedTab = "wifi";
                            refreshWifi();
                            refreshInfoForActive();
                        }
                    }

                }

                Rectangle {
                    id: vpnTab

                    Layout.fillWidth: true
                    Layout.preferredWidth: 1
                    height: 34
                    radius: 7
                    color: selectedTab === "vpn" ? Theme.primary : vpnTabMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "VPN"
                        font.pixelSize: 16
                        font.bold: true
                        color: selectedTab === "vpn" ? Theme.primaryText : Theme.surfaceText
                    }

                    MouseArea {
                        id: vpnTabMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            selectedTab = "vpn";
                        }
                    }

                }

            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 10
                visible: selectedTab === "ethernet"

                ColumnLayout {
                    Layout.fillWidth: true
                    spacing: 4

                    Repeater {
                        model: networkMenu.ethernetDevices

                        delegate: Rectangle {
                            required property var modelData
                            readonly property bool isCurrent: modelData.device === networkMenu.activeEthernetDevice
                            readonly property bool isDefaultRoute: isCurrent && modelData.device === networkMenu.defaultRouteDevice
                            readonly property bool isFallback: isCurrent && networkMenu.defaultRouteDevice !== "" && modelData.device !== networkMenu.defaultRouteDevice

                            Layout.fillWidth: true
                            height: 56
                            radius: 7
                            color: ethItemMouse.containsMouse || isCurrent ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                            border.color: isDefaultRoute ? Theme.primary : (isFallback ? Theme.fallback : "transparent")
                            border.width: isCurrent ? 1 : 0

                            MouseArea {
                                id: ethItemMouse

                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: {
                                    connectDevice(modelData.device);
                                }
                            }

                            RowLayout {
                                anchors.fill: parent
                                anchors.leftMargin: 12
                                anchors.rightMargin: 8
                                spacing: 8

                                Text {
                                    text: "\uEB2F"
                                    font.family: materialSymbols.name
                                    font.pixelSize: 12
                                    color: isFallback ? Theme.fallback : Theme.surfaceVariantText
                                }

                                ColumnLayout {
                                    Layout.fillWidth: true
                                    spacing: 2

                                    Text {
                                        Layout.fillWidth: true
                                        text: modelData.device
                                        elide: Text.ElideRight
                                        font.pixelSize: Theme.fontLabelMedium
                                        font.bold: true
                                        color: isFallback ? Theme.fallback : Theme.surfaceText
                                    }

                                    RowLayout {
                                        Layout.fillWidth: true
                                        spacing: 6

                                        Rectangle {
                                            Layout.alignment: Qt.AlignVCenter
                                            width: 6
                                            height: 6
                                            radius: 3
                                            color: Theme.success
                                            visible: VpnService.connected && isCurrent && modelData.device === networkMenu.defaultRouteDevice
                                        }

                                        Text {
                                            text: networkMenu.connectingDev === modelData.device ? "Connecting..." : networkMenu.disconnectingDev === modelData.device ? "Disconnecting..." : isCurrent ? "Connected" : "Disconnected"
                                            font.pixelSize: Theme.fontLabelSmall
                                            color: networkMenu.connectingDev === modelData.device ? "#f9e2af" : networkMenu.disconnectingDev === modelData.device ? "#f9e2af" : isDefaultRoute ? Theme.primary : (isFallback ? Theme.fallback : Theme.surfaceVariantText)
                                        }

                                        Text {
                                            text: {
                                                const t = networkMenu.routeTable;
                                                for (let i = 0; i < t.length; i++) {
                                                    if (t[i].device === modelData.device)
                                                        return ((modelData.device === networkMenu.defaultRouteDevice && VpnService.connected) ? "WARP · " : "") + (modelData.device === networkMenu.defaultRouteDevice ? "Default route" : "Fallback") + " · " + t[i].metric;

                                                }
                                                return "";
                                            }
                                            visible: {
                                                if (!isCurrent)
                                                    return false;

                                                const t = networkMenu.routeTable;
                                                for (let i = 0; i < t.length; i++) {
                                                    if (t[i].device === modelData.device)
                                                        return true;

                                                }
                                                return false;
                                            }
                                            font.pixelSize: Theme.fontLabelSmall
                                            color: networkMenu.defaultRouteDevice === modelData.device && VpnService.connected ? Theme.success : modelData.device === networkMenu.defaultRouteDevice ? Theme.primary : (isCurrent ? Theme.fallback : Theme.surfaceVariantText)
                                        }

                                    }

                                }

                                Item {
                                    Layout.preferredWidth: 30
                                    Layout.preferredHeight: 56

                                    Rectangle {
                                        anchors.fill: parent
                                        radius: 7
                                        color: ethMenuMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent"
                                    }

                                    Text {
                                        anchors.centerIn: parent
                                        text: "..."
                                        font.pixelSize: Theme.fontLabelLarge
                                        font.bold: true
                                        color: ethMenuMouse.containsMouse ? Theme.primary : Theme.surfaceVariantText
                                    }

                                    MouseArea {
                                        id: ethMenuMouse

                                        anchors.fill: parent
                                        hoverEnabled: true
                                        cursorShape: Qt.PointingHandCursor
                                        onClicked: {
                                            if (rowMenu.opened && rowMenu.anchorItem === ethMenuMouse.parent) {
                                                rowMenu.close();
                                                return ;
                                            }
                                            const items = [];
                                            if (isCurrent)
                                                items.push({
                                                "label": "Disconnect",
                                                "action": function() {
                                                    networkMenu.disconnectDevice(modelData.device);
                                                }
                                            });
                                            else
                                                items.push({
                                                "label": "Connect",
                                                "action": function() {
                                                    connectDevice(modelData.device);
                                                }
                                            });
                                            items.push({
                                                "label": "Network Info",
                                                "action": function() {
                                                    networkMenu.openInfo({
                                                        "kind": "ethernet",
                                                        "device": modelData.device,
                                                        "state": modelData.state,
                                                        "connection": modelData.connection,
                                                        "active": modelData.device === networkMenu.activeEthernetDevice
                                                    });
                                                }
                                            });
                                            rowMenu.items = items;
                                            rowMenu.showFor(ethMenuMouse.parent);
                                        }
                                    }

                                }

                            }

                        }

                    }

                }

            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 10
                visible: selectedTab === "wifi"

                Rectangle {
                    Layout.fillWidth: true
                    height: 56
                    radius: 7
                    color: wifiRadioCardMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    border.color: networkMenu.wifiRadioEnabled ? Theme.primary : "transparent"
                    border.width: networkMenu.wifiRadioEnabled ? 1 : 0

                    MouseArea {
                        id: wifiRadioCardMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: networkMenu.toggleWifiRadio()
                    }

                    RowLayout {
                        anchors.fill: parent
                        anchors.leftMargin: 12
                        anchors.rightMargin: 8
                        spacing: 8

                        Text {
                            text: networkMenu.wifiRadioEnabled ? "\uE63E" : "\uE648"
                            font.family: materialSymbols.name
                            font.pixelSize: 16
                            color: networkMenu.wifiRadioEnabled ? Theme.primary : Theme.surfaceVariantText
                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: 2

                            Text {
                                Layout.fillWidth: true
                                text: "WiFi enabled"
                                elide: Text.ElideRight
                                font.pixelSize: Theme.fontLabelMedium
                                font.bold: true
                                color: Theme.surfaceText
                            }

                            Text {
                                Layout.fillWidth: true
                                text: networkMenu.wifiRadioEnabled ? "On" : "Off"
                                font.pixelSize: Theme.fontLabelSmall
                                color: networkMenu.wifiRadioEnabled ? Theme.primary : Theme.surfaceVariantText
                            }

                        }

                        Item {
                            Layout.fillWidth: true
                        }

                        Rectangle {
                            Layout.preferredWidth: 34
                            Layout.preferredHeight: 20
                            Layout.alignment: Qt.AlignVCenter
                            radius: 10
                            color: networkMenu.wifiRadioEnabled ? Theme.primary : Theme.surfaceContainerHigh
                            border.color: networkMenu.wifiRadioEnabled ? "transparent" : Theme.outlineVariant
                            border.width: 1

                            Rectangle {
                                width: 16
                                height: 16
                                radius: 8
                                x: networkMenu.wifiRadioEnabled ? parent.width - width - 2 : 2
                                anchors.verticalCenter: parent.verticalCenter
                                color: networkMenu.wifiRadioEnabled ? Theme.primaryText : Theme.surfaceVariantText

                                Behavior on x {
                                    NumberAnimation {
                                        duration: 160
                                        easing.type: Easing.OutCubic
                                    }

                                }

                            }

                            MouseArea {
                                anchors.fill: parent
                                cursorShape: Qt.PointingHandCursor
                                onClicked: networkMenu.toggleWifiRadio()
                            }

                        }

                    }

                }

                ListView {
                    id: wifiList

                    Layout.fillWidth: true
                    height: 300
                    clip: true
                    spacing: 4
                    boundsBehavior: Flickable.StopAtBounds
                    visible: networkMenu.wifiAccumulated.length > 0
                    model: networkMenu.wifiAccumulated

                    ScrollBar.vertical: ScrollBar {
                        policy: ScrollBar.AsNeeded
                        width: 14
                        minimumSize: 0.25

                        contentItem: Rectangle {
                            implicitWidth: 14
                            radius: 7
                            color: parent.pressed ? Theme.surfaceVariant : Theme.surfaceContainerHighest
                        }

                    }

                    delegate: Rectangle {
                        required property var modelData
                        readonly property bool isActive: modelData.active || (networkMenu.activeWifiSsid !== "" && modelData.ssid === networkMenu.activeWifiSsid && !networkMenu.ambiguousSsid[modelData.ssid])
                        readonly property bool isDefaultRouteConnected: isActive && networkMenu.activeWifiDevice !== "" && networkMenu.activeWifiDevice === networkMenu.defaultRouteDevice
                        readonly property bool isFallbackConnected: isActive && networkMenu.defaultRouteDevice !== "" && networkMenu.activeWifiDevice !== networkMenu.defaultRouteDevice

                        width: wifiList.width
                        height: 56
                        radius: 7
                        color: wifiItemMouse.containsMouse || isActive ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                        border.color: isDefaultRouteConnected ? Theme.primary : (isFallbackConnected ? Theme.fallback : "transparent")
                        border.width: isActive ? 1 : 0

                        MouseArea {
                            id: wifiItemMouse

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: {
                                if (modelData.saved) {
                                    networkMenu.connectWifi(modelData.ssid);
                                } else {
                                    networkMenu.passwordTarget = modelData.ssid;
                                    networkMenu.passwordInput = "";
                                    passwordField.forceActiveFocus();
                                }
                            }
                        }

                        RowLayout {
                            anchors.fill: parent
                            anchors.leftMargin: 12
                            anchors.rightMargin: 8
                            spacing: 8

                            Text {
                                text: {
                                    const s = modelData.signal;
                                    if (isActive || s >= 50)
                                        return "\uE63E";

                                    if (s >= 25)
                                        return "\uE4D9";

                                    return "\uE4CA";
                                }
                                font.family: materialSymbols.name
                                font.pixelSize: 16
                                color: isDefaultRouteConnected ? Theme.primary : (isFallbackConnected ? Theme.fallback : Theme.surfaceVariantText)
                            }

                            ColumnLayout {
                                Layout.fillWidth: true
                                spacing: 2

                                Item {
                                    Layout.fillWidth: true
                                    Layout.preferredHeight: wifiSsidText.implicitHeight

                                    Text {
                                        id: wifiSsidText

                                        anchors.left: parent.left
                                        anchors.verticalCenter: parent.verticalCenter
                                        text: modelData.ssid
                                        font.pixelSize: Theme.fontLabelMedium
                                        font.bold: true
                                        color: Theme.surfaceText
                                    }

                                    Text {
                                        id: wifiBandTag

                                        anchors.left: wifiSsidText.right
                                        anchors.leftMargin: 6
                                        anchors.verticalCenter: parent.verticalCenter
                                        text: "(" + (modelData.rate ? modelData.band + " · " + modelData.rate.replace("Mbit/s", "Mb/s") : modelData.band) + ")"
                                        font.pixelSize: Theme.fontLabelSmall
                                        font.bold: true
                                        color: isFallbackConnected ? Theme.fallback : (modelData.band === "5GHz" ? Theme.primary : Theme.surfaceVariantText)
                                    }

                                }

                                RowLayout {
                                    Layout.fillWidth: true
                                    spacing: 6

                                    Rectangle {
                                        Layout.alignment: Qt.AlignVCenter
                                        width: 6
                                        height: 6
                                        radius: 3
                                        color: Theme.success
                                        visible: VpnService.connected && isActive && networkMenu.activeWifiDevice === networkMenu.defaultRouteDevice
                                    }

                                    Text {
                                        text: networkMenu.connectingSsid === modelData.ssid ? "Connecting..." : networkMenu.disconnectingWifiSsid === modelData.ssid ? "Disconnecting..." : isActive ? "Connected" : (modelData.security !== "" ? "Secure" : "Open")
                                        font.pixelSize: Theme.fontLabelSmall
                                        color: networkMenu.connectingSsid === modelData.ssid ? "#f9e2af" : networkMenu.disconnectingWifiSsid === modelData.ssid ? "#f9e2af" : isDefaultRouteConnected ? Theme.primary : (isFallbackConnected ? Theme.fallback : Theme.surfaceVariantText)
                                    }

                                    Text {
                                        text: "•"
                                        font.pixelSize: Theme.fontLabelSmall
                                        color: isFallbackConnected ? Theme.fallback : Theme.surfaceVariantText
                                        visible: modelData.saved
                                    }

                                    Text {
                                        text: "Saved"
                                        font.pixelSize: Theme.fontLabelSmall
                                        color: isFallbackConnected ? Theme.fallback : Theme.primary
                                        visible: modelData.saved
                                    }

                                    Text {
                                        text: "•"
                                        font.pixelSize: Theme.fontLabelSmall
                                        color: isFallbackConnected ? Theme.fallback : Theme.surfaceVariantText
                                    }

                                    Text {
                                        text: modelData.signal + "%"
                                        font.pixelSize: Theme.fontLabelSmall
                                        color: isDefaultRouteConnected ? Theme.primary : (isFallbackConnected ? Theme.fallback : Theme.surfaceVariantText)
                                    }

                                    Text {
                                        text: "•"
                                        font.pixelSize: Theme.fontLabelSmall
                                        color: isFallbackConnected ? Theme.fallback : Theme.surfaceVariantText
                                        visible: {
                                            if (!isActive)
                                                return false;

                                            const t = networkMenu.routeTable;
                                            for (let i = 0; i < t.length; i++) {
                                                if (t[i].device === networkMenu.activeWifiDevice)
                                                    return true;

                                            }
                                            return false;
                                        }
                                    }

                                    Text {
                                        text: {
                                            const t = networkMenu.routeTable;
                                            for (let i = 0; i < t.length; i++) {
                                                if (t[i].device === networkMenu.activeWifiDevice)
                                                    return ((networkMenu.activeWifiDevice === networkMenu.defaultRouteDevice && VpnService.connected) ? "WARP · " : "") + (networkMenu.activeWifiDevice === networkMenu.defaultRouteDevice ? "Default route" : "Fallback") + " · " + t[i].metric;

                                            }
                                            return "";
                                        }
                                        visible: isActive
                                        font.pixelSize: Theme.fontLabelSmall
                                        color: networkMenu.activeWifiDevice === networkMenu.defaultRouteDevice && VpnService.connected ? Theme.success : networkMenu.activeWifiDevice === networkMenu.defaultRouteDevice ? Theme.primary : (isFallbackConnected ? Theme.fallback : Theme.surfaceVariantText)
                                    }

                                }

                            }

                            Item {
                                Layout.preferredWidth: 30
                                Layout.preferredHeight: 56

                                Rectangle {
                                    anchors.fill: parent
                                    radius: 7
                                    color: wifiMenuMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent"
                                }

                                Text {
                                    anchors.centerIn: parent
                                    text: "..."
                                    font.pixelSize: Theme.fontLabelLarge
                                    font.bold: true
                                    color: isFallbackConnected ? Theme.fallback : (wifiMenuMouse.containsMouse ? Theme.primary : Theme.surfaceVariantText)
                                }

                                MouseArea {
                                    id: wifiMenuMouse

                                    anchors.fill: parent
                                    hoverEnabled: true
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: {
                                        if (rowMenu.opened && rowMenu.anchorItem === wifiMenuMouse.parent) {
                                            rowMenu.close();
                                            return ;
                                        }
                                        const items = [];
                                        if (isActive) {
                                            items.push({
                                                "label": "Disconnect",
                                                "action": function() {
                                                    if (networkMenu.activeWifiDevice)
                                                        networkMenu.disconnectDevice(networkMenu.activeWifiDevice);

                                                }
                                            });
                                            items.push({
                                                "label": "Forget Network",
                                                "action": function() {
                                                    networkMenu.forgetNetwork(modelData.ssid);
                                                }
                                            });
                                            items.push({
                                                "label": (networkMenu.savedAutoconnect[modelData.ssid] === true ? "Autoconnect: On" : "Autoconnect: Off"),
                                                "action": function() {
                                                    networkMenu.setAutoconnect(modelData.ssid, networkMenu.savedAutoconnect[modelData.ssid] !== true);
                                                }
                                            });
                                        } else {
                                            items.push({
                                                "label": "Connect",
                                                "action": function() {
                                                    if (modelData.saved) {
                                                        networkMenu.connectWifi(modelData.ssid);
                                                    } else {
                                                        networkMenu.passwordTarget = modelData.ssid;
                                                        networkMenu.passwordInput = "";
                                                        passwordField.forceActiveFocus();
                                                    }
                                                }
                                            });
                                            if (modelData.saved)
                                                items.push({
                                                "label": (networkMenu.savedAutoconnect[modelData.ssid] === true ? "Autoconnect: On" : "Autoconnect: Off"),
                                                "action": function() {
                                                    networkMenu.setAutoconnect(modelData.ssid, networkMenu.savedAutoconnect[modelData.ssid] !== true);
                                                }
                                            });

                                        }
                                        items.push({
                                            "label": "Network Info",
                                            "action": function() {
                                                networkMenu.openInfo({
                                                    "kind": "wifi",
                                                    "ssid": modelData.ssid,
                                                    "signal": modelData.signal,
                                                    "security": modelData.security,
                                                    "saved": modelData.saved,
                                                    "active": modelData.active
                                                });
                                            }
                                        });
                                        rowMenu.items = items;
                                        rowMenu.showFor(wifiMenuMouse.parent);
                                    }
                                }

                            }

                        }

                    }

                }

                Item {
                    Layout.fillWidth: true
                    height: 300
                    visible: networkMenu.wifiAccumulated.length === 0

                    RowLayout {
                        anchors.centerIn: parent
                        spacing: 10
                        visible: networkMenu.wifiScanning && networkMenu.wifiRadioEnabled

                        BusyIndicator {
                            Layout.preferredWidth: 26
                            Layout.preferredHeight: 26
                            running: networkMenu.wifiScanning
                            palette.text: Theme.primary
                        }

                        Text {
                            text: "Searching for networks..."
                            font.pixelSize: Theme.fontLabelMedium
                            color: Theme.surfaceVariantText
                        }

                    }

                    Text {
                        anchors.centerIn: parent
                        text: "No networks found"
                        font.pixelSize: Theme.fontLabelMedium
                        color: Theme.surfaceVariantText
                        visible: !networkMenu.wifiScanning && networkMenu.wifiRadioEnabled
                    }

                    Text {
                        anchors.centerIn: parent
                        text: "WiFi disabled"
                        font.pixelSize: Theme.fontLabelMedium
                        color: Theme.surfaceVariantText
                        visible: !networkMenu.wifiRadioEnabled
                    }

                }

            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 10
                visible: selectedTab === "vpn"

                Rectangle {
                    Layout.fillWidth: true
                    height: 56
                    radius: 7
                    color: vpnCardMouse.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh
                    border.color: VpnService.connected ? Theme.success : "transparent"
                    border.width: VpnService.connected ? 1 : 0

                    MouseArea {
                        id: vpnCardMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: VpnService.toggle()
                    }

                    RowLayout {
                        anchors.fill: parent
                        anchors.leftMargin: 12
                        anchors.rightMargin: 8
                        spacing: 8

                        Text {
                            text: "\uF3E7"
                            font.family: networkMenu.fontFamily
                            font.pixelSize: 16
                            color: VpnService.connected ? Theme.success : (VpnService.connecting ? "#f9e2af" : Theme.surfaceVariantText)
                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: 2

                            Text {
                                Layout.fillWidth: true
                                text: "Cloudflare WARP"
                                elide: Text.ElideRight
                                font.pixelSize: Theme.fontLabelMedium
                                font.bold: true
                                color: Theme.surfaceText
                            }

                            Text {
                                Layout.fillWidth: true
                                text: {
                                    if (VpnService.connecting)
                                        return "Connecting...";

                                    return VpnService.connected ? "Connected" : "Disconnected";
                                }
                                font.pixelSize: Theme.fontLabelSmall
                                color: VpnService.connected ? Theme.success : (VpnService.connecting ? "#f9e2af" : Theme.surfaceVariantText)
                            }

                        }

                        Item {
                            Layout.fillWidth: true
                        }

                        Rectangle {
                            Layout.preferredWidth: 34
                            Layout.preferredHeight: 20
                            Layout.alignment: Qt.AlignVCenter
                            radius: 10
                            color: VpnService.connected ? Theme.success : Theme.surfaceContainerHigh
                            border.color: VpnService.connected ? "transparent" : Theme.outlineVariant
                            border.width: 1

                            Rectangle {
                                width: 16
                                height: 16
                                radius: 8
                                x: VpnService.connected ? parent.width - width - 2 : 2
                                anchors.verticalCenter: parent.verticalCenter
                                color: VpnService.connected ? Theme.primaryText : Theme.surfaceVariantText

                                Behavior on x {
                                    NumberAnimation {
                                        duration: 160
                                        easing.type: Easing.OutCubic
                                    }

                                }

                            }

                            MouseArea {
                                anchors.fill: parent
                                cursorShape: Qt.PointingHandCursor
                                onClicked: VpnService.toggle()
                            }

                        }

                    }

                }

            }

            Rectangle {
                id: passwordOverlay

                Layout.fillWidth: true
                Layout.fillHeight: true
                visible: networkMenu.passwordTarget !== ""
                color: "#99000000"
                z: 100
                onVisibleChanged: {
                    if (visible) {
                        networkMenu.showPassword = false;
                        passwordField.forceActiveFocus();
                    }
                }

                MouseArea {
                    anchors.fill: parent
                }

                Rectangle {
                    anchors.centerIn: parent
                    width: 420
                    height: passCol.implicitHeight + 32
                    radius: 12
                    color: Theme.surfaceContainer
                    border.color: Theme.surfaceContainerHighest
                    border.width: 1

                    ColumnLayout {
                        id: passCol

                        anchors.fill: parent
                        anchors.margins: 16
                        spacing: 12

                        Text {
                            Layout.fillWidth: true
                            text: "Connect to " + networkMenu.passwordTarget
                            elide: Text.ElideRight
                            font.pixelSize: Theme.fontLabelLarge
                            font.bold: true
                            color: Theme.surfaceText
                        }

                        Rectangle {
                            Layout.fillWidth: true
                            height: 40
                            radius: 7
                            color: Theme.surfaceContainerHigh
                            border.color: passwordField.activeFocus ? Theme.primary : "transparent"
                            border.width: 1

                            TextInput {
                                id: passwordField

                                anchors.fill: parent
                                anchors.leftMargin: 12
                                anchors.rightMargin: 40
                                verticalAlignment: TextInput.AlignVCenter
                                font.pixelSize: Theme.fontLabelLarge
                                color: Theme.surfaceText
                                echoMode: networkMenu.showPassword ? TextInput.Normal : TextInput.Password
                                text: networkMenu.passwordInput
                                onTextChanged: networkMenu.passwordInput = text
                                Keys.onPressed: (event) => {
                                    if (event.key === Qt.Key_Escape) {
                                        networkMenu.passwordTarget = "";
                                        event.accepted = true;
                                    } else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                                        networkMenu.connectWifiWithPassword(networkMenu.passwordTarget, networkMenu.passwordInput);
                                        event.accepted = true;
                                    }
                                }
                            }

                            Rectangle {
                                width: 36
                                height: 38
                                anchors.right: parent.right
                                anchors.verticalCenter: parent.verticalCenter
                                radius: 7
                                color: passEyeMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent"

                                Text {
                                    anchors.centerIn: parent
                                    text: networkMenu.showPassword ? "\uE8F4" : "\uE8F5"
                                    font.family: materialSymbols.name
                                    font.pixelSize: 16
                                    color: passEyeMouse.containsMouse ? Theme.primary : Theme.surfaceVariantText
                                }

                                MouseArea {
                                    id: passEyeMouse

                                    anchors.fill: parent
                                    hoverEnabled: true
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: networkMenu.showPassword = !networkMenu.showPassword
                                }

                            }

                        }

                        RowLayout {
                            Layout.fillWidth: true
                            spacing: 8

                            Item {
                                Layout.fillWidth: true
                            }

                            Rectangle {
                                width: 120
                                height: 38
                                radius: 7
                                color: cancelBtn.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                                Text {
                                    anchors.centerIn: parent
                                    text: "Cancelar"
                                    font.pixelSize: Theme.fontLabelMedium
                                    font.bold: true
                                    color: Theme.surfaceText
                                }

                                MouseArea {
                                    id: cancelBtn

                                    anchors.fill: parent
                                    hoverEnabled: true
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: networkMenu.passwordTarget = ""
                                }

                            }

                            Rectangle {
                                width: 120
                                height: 38
                                radius: 7
                                color: connectBtn.containsMouse ? Theme.primary : Theme.surfaceContainerHighest

                                Text {
                                    anchors.centerIn: parent
                                    text: "Connect"
                                    font.pixelSize: Theme.fontLabelMedium
                                    font.bold: true
                                    color: connectBtn.containsMouse ? Theme.primaryText : Theme.surfaceText
                                }

                                MouseArea {
                                    id: connectBtn

                                    anchors.fill: parent
                                    hoverEnabled: true
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: networkMenu.connectWifiWithPassword(networkMenu.passwordTarget, networkMenu.passwordInput)
                                }

                            }

                        }

                    }

                }

            }

        }

    }

    Rectangle {
        id: infoOverlay

        anchors.fill: parent
        visible: networkMenu.showInfoModal
        color: "#99000000"
        z: 200
        Keys.onPressed: (event) => {
            if (event.key === Qt.Key_Escape) {
                networkMenu.showInfoModal = false;
                event.accepted = true;
            }
        }
        onVisibleChanged: {
            if (visible)
                forceActiveFocus();

        }

        MouseArea {
            anchors.fill: parent
            onClicked: networkMenu.showInfoModal = false
        }

        Rectangle {
            anchors.centerIn: parent
            width: 590
            height: modalCol.implicitHeight + 32
            radius: 12
            color: Theme.surfaceContainer
            border.color: Theme.surfaceContainerHighest
            border.width: 1

            ColumnLayout {
                id: modalCol

                anchors.fill: parent
                anchors.margins: 16
                spacing: 12

                RowLayout {
                    Layout.fillWidth: true
                    spacing: 8

                    Text {
                        text: "\uE88E"
                        font.family: materialSymbols.name
                        font.pixelSize: 18
                        color: Theme.primary
                    }

                    Text {
                        Layout.fillWidth: true
                        text: "Network Information"
                        font.pixelSize: 18
                        font.bold: true
                        color: Theme.surfaceText
                    }

                }

                Rectangle {
                    Layout.fillWidth: true
                    implicitHeight: infoRowsCol.implicitHeight + 12
                    radius: 7
                    color: Theme.surfaceContainerHigh

                    ColumnLayout {
                        id: infoRowsCol

                        anchors.fill: parent
                        anchors.margins: 10
                        spacing: 6

                        Repeater {
                            model: networkMenu.infoModalRows

                            delegate: ColumnLayout {
                                required property int index
                                required property var modelData

                                Layout.fillWidth: true
                                spacing: 6

                                Text {
                                    Layout.fillWidth: true
                                    Layout.topMargin: modelData.section !== undefined && index > 0 ? 10 : 0
                                    Layout.bottomMargin: modelData.section !== undefined ? 6 : 0
                                    visible: modelData.section !== undefined
                                    text: modelData.section
                                    font.pixelSize: Theme.fontLabelMedium
                                    font.bold: true
                                    color: Theme.primary
                                }

                                RowLayout {
                                    Layout.fillWidth: true
                                    Layout.leftMargin: modelData.indent ? 12 : 0
                                    visible: modelData.section === undefined
                                    spacing: 12

                                    Text {
                                        Layout.fillWidth: false
                                        text: modelData.label
                                        font.pixelSize: Theme.fontLabelLarge
                                        color: Theme.surfaceVariantText
                                    }

                                    Text {
                                        Layout.fillWidth: true
                                        text: modelData.value
                                        elide: Text.ElideRight
                                        horizontalAlignment: Text.AlignRight
                                        font.pixelSize: Theme.fontLabelLarge
                                        color: Theme.surfaceText
                                    }

                                }

                            }

                        }

                    }

                }

                Rectangle {
                    Layout.alignment: Qt.AlignRight
                    width: 120
                    height: 38
                    radius: 7
                    color: closeBtn.containsMouse ? Theme.surfaceContainerHighest : Theme.surfaceContainerHigh

                    Text {
                        anchors.centerIn: parent
                        text: "Fechar"
                        font.pixelSize: Theme.fontLabelMedium
                        font.bold: true
                        color: Theme.surfaceText
                    }

                    MouseArea {
                        id: closeBtn

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: networkMenu.showInfoModal = false
                    }

                }

            }

        }

    }

    Popup {
        id: rowMenu

        property var items: []
        property var anchorItem: null

        function showFor(anchor) {
            anchorItem = anchor;
            let target = networkMenu.contentItem;
            if (!target) {
                let cur = anchor;
                while (cur) {
                    const w = cur.Window ? cur.Window.window : null;
                    if (w && w.contentItem) {
                        target = w.contentItem;
                        break;
                    }
                    cur = cur.parent;
                }
            }
            if (!target) {
                open();
                return ;
            }
            const p = anchor.mapToItem(target, 0, 0);
            x = Math.max(2, Math.min(target.width - width - 2, p.x - width - 8));
            y = Math.max(2, Math.min(target.height - height - 2, p.y + anchor.height / 2 - height / 2));
            open();
        }

        width: 180
        leftPadding: 4
        rightPadding: 4
        topPadding: 4
        bottomPadding: 4
        modal: true
        dim: false
        closePolicy: Popup.CloseOnPressOutside | Popup.CloseOnEscape

        background: Rectangle {
            color: Theme.surfaceContainerHigh
            radius: 10
            border.color: Theme.outlineVariant
            border.width: 1
        }

        contentItem: ColumnLayout {
            spacing: 2

            Repeater {
                model: rowMenu.items

                delegate: Rectangle {
                    required property var modelData

                    Layout.fillWidth: true
                    Layout.preferredHeight: 32
                    radius: 6
                    color: rowMenuItemMouse.containsMouse ? Theme.surfaceContainerHighest : "transparent"

                    Text {
                        anchors.left: parent.left
                        anchors.leftMargin: 12
                        anchors.verticalCenter: parent.verticalCenter
                        text: modelData.label
                        font.pixelSize: Theme.fontLabelMedium
                        color: rowMenuItemMouse.containsMouse ? Theme.primary : Theme.surfaceText
                    }

                    MouseArea {
                        id: rowMenuItemMouse

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            rowMenu.close();
                            modelData.action();
                        }
                    }

                }

            }

        }

    }

}
