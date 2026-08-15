pragma Singleton
pragma ComponentBehavior: Bound

import QtQuick
import QtQml
import Quickshell
import Quickshell.Services.Mpris

Singleton {
    id: root

    readonly property list<MprisPlayer> availablePlayers: Mpris.players.values
    property MprisPlayer activePlayer: null
    property string stableTitle: ""
    property string stableArtist: ""
    property string stableArtUrl: ""
    readonly property string positionText: activePlayer && activePlayer.positionSupported ? formatTime(activePlayer.position) : "00:00"
    readonly property string lengthText: activePlayer && activePlayer.lengthSupported ? formatTime(activePlayer.length) : "00:00"

    function normalize(value) {
        return (value || "").toString().toLowerCase();
    }

    function formatTime(seconds) {
        seconds = Math.max(0, Math.floor(seconds || 0));
        const m = Math.floor(seconds / 60);
        const s = seconds % 60;
        return m.toString().padStart(2, "0") + ":" + s.toString().padStart(2, "0");
    }

    function isIdle(player) {
        return player && player.playbackState === MprisPlaybackState.Stopped && !player.trackTitle && !player.trackArtist;
    }

    function isFirefoxYoutubeHoverPreview(player) {
        if (!player)
            return false;

        const id = normalize(player.identity);
        if (!id.includes("firefox"))
            return false;

        const url = (player.metadata && player.metadata["xesam:url"] ? player.metadata["xesam:url"] : "").toString();
        return /^https?:\/\/(www\.)?youtube\.com\/?($|\?|#)/i.test(url);
    }

    function getArtworkUrl(player) {
        if (!player)
            return "";

        if (player.trackArtUrl)
            return player.trackArtUrl;

        if (player.metadata && player.metadata["mpris:artUrl"])
            return player.metadata["mpris:artUrl"].toString();

        const url = player.metadata && player.metadata["xesam:url"] ? player.metadata["xesam:url"].toString() : "";
        if (!url)
            return "";

        if (url.includes("youtube.com") || url.includes("youtu.be")) {
            const regExp = /^.*(youtu.be\/|v\/|u\/\w\/|embed\/|watch\?v=|\&v=)([^#\&\?]*).*/;
            const match = url.match(regExp);
            if (match && match[2].length === 11)
                return "https://img.youtube.com/vi/" + match[2] + "/hqdefault.jpg";
        }

        return "";
    }

    function _syncStableMeta() {
        const p = activePlayer;
        if (!p) {
            stableTitle = "";
            stableArtist = "";
            stableArtUrl = "";
            return;
        }

        if (!isFirefoxYoutubeHoverPreview(p)) {
            if (p.trackTitle)
                stableTitle = p.trackTitle;
            if (p.trackArtist)
                stableArtist = p.trackArtist;
        }

        stableArtUrl = getArtworkUrl(p);
    }

    function _resolveActivePlayer() {
        const players = availablePlayers.filter(p => p && !isIdle(p));
        const playing = players.find(p => p.isPlaying);

        if (playing) {
            activePlayer = playing;
            return;
        }

        if (activePlayer && players.indexOf(activePlayer) >= 0)
            return;

        activePlayer = players.find(p => p.canControl) ?? players[0] ?? null;
    }

    function previousOrRewind() {
        if (!activePlayer)
            return;

        if (activePlayer.position > 8 && activePlayer.canSeek)
            activePlayer.position = 0.1;
        else if (activePlayer.canGoPrevious)
            activePlayer.previous();
    }

    function next() {
        const player = activePlayer;
        if (player?.canGoNext)
            player.next();
    }

    function seekToFraction(fraction) {
        const player = activePlayer;
        if (!player || !player.positionSupported || !player.lengthSupported || player.length <= 0)
            return;

        player.position = Math.max(0.1, Math.min(player.length * fraction, player.length * 0.99));
    }

    onAvailablePlayersChanged: _resolveActivePlayer()
    onActivePlayerChanged: _syncStableMeta()

    Connections {
        target: Mpris.players
        function onValuesChanged() {
            root._resolveActivePlayer();
        }
    }

    Connections {
        target: root.activePlayer
        ignoreUnknownSignals: true
        function onTrackTitleChanged() {
            root._syncStableMeta();
        }
        function onTrackArtistChanged() {
            root._syncStableMeta();
        }
        function onTrackArtUrlChanged() {
            root._syncStableMeta();
        }
        function onMetadataChanged() {
            root._syncStableMeta();
        }
        function onPlaybackStateChanged() {
            root._resolveActivePlayer();
            root._syncStableMeta();
        }
    }

    Instantiator {
        model: root.availablePlayers
        delegate: Connections {
            required property MprisPlayer modelData
            target: modelData
            ignoreUnknownSignals: true

            function onIsPlayingChanged() {
                if (modelData.isPlaying)
                    root._resolveActivePlayer();
            }

            function onTrackTitleChanged() {
                root._syncStableMeta();
            }

            function onTrackArtistChanged() {
                root._syncStableMeta();
            }

            function onTrackArtUrlChanged() {
                root._syncStableMeta();
            }

            function onMetadataChanged() {
                root._syncStableMeta();
            }

            function onPlaybackStateChanged() {
                root._resolveActivePlayer();
                root._syncStableMeta();
            }
        }
    }
}
