import QtQuick
pragma Singleton

QtObject {
    readonly property string largeFontMonitorName: "DP-3"
    readonly property real defaultFontScale: 1.0
    readonly property real largeFontScale: 1.25
    readonly property int fontLabelSmall: 10
    readonly property int fontLabelMedium: 12
    readonly property int fontLabelLarge: 14

    function fontScaleForScreen(screen) {
        return screen && screen.name === largeFontMonitorName ? largeFontScale : defaultFontScale;
    }

    function scaledFont(size, scale) {
        return Math.max(1, scaledSize(size, scale));
    }

    function scaledSize(size, scale) {
        return Math.max(0, Math.round(size * scale));
    }

    readonly property color outlineVariant: "#45475a"
    readonly property color primary: "#cba6f7"
    readonly property color primaryText: "#181825"
    readonly property color surfaceContainer: "#1e1e2e"
    readonly property color surfaceContainerHigh: "#313244"
    readonly property color surfaceContainerHighest: "#45475a"
    readonly property color surfaceText: "#cdd6f4"
    readonly property color surfaceVariant: "#45475a"
    readonly property color surfaceVariantText: "#a6adc8"
    readonly property color error: "#f38ba8"
    readonly property color attention: "#b4befe"
    readonly property color success: "#a6e3a1"
    readonly property color fallback: "#89b4fa"
}
