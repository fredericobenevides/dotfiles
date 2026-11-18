-- Monitors
-- See https://wiki.hypr.land/Configuring/Basics/Monitors/

hl.monitor({
    output   = "HDMI-A-1",
    mode     = "preferred",
    position = "0x0",
    scale    = 1,
    transform = 0,
})

hl.monitor({
    output   = "DP-3",
    mode     = "2560x1440@240",
    position = "0x1080",
    scale    = 1,
    transform = 0,
})

hl.monitor({
    output   = "DP-2",
    mode     = "1920x1080@165",
    position = "-1080x300",
    scale    = 1,
    transform = 3,
})

hl.monitor({
    output   = "DP-1",
    mode     = "1920x1080@165",
    position = "2560x1080",
    scale    = 1,
    transform = 1,
})