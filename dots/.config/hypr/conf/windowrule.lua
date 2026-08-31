-- Window and workspace rules
-- See https://wiki.hypr.land/Configuring/Basics/Window-Rules/
-- See https://wiki.hypr.land/Configuring/Basics/Workspace-Rules/

-- Suppress maximize events for all windows
hl.window_rule({
    name  = "suppress-maximize-events",
    match = { class = ".*" },
    suppress_event = "maximize",
})

hl.window_rule({
    -- Fix some dragging issues with XWayland
    name  = "fix-xwayland-drags",
    match = {
        class      = "^$",
        title      = "^$",
        xwayland   = true,
        float      = true,
        fullscreen = false,
        pin        = false,
    },

    no_focus = true,
})

-- Opacity rules
hl.window_rule({ name = "kitty-opacity",       match = { class = "^kitty" },              opacity = "0.80 0.80" })
hl.window_rule({ name = "code-opacity",         match = { class = "^code" },               opacity = "0.95 0.95" })
hl.window_rule({ name = "jetbrains-opacity",    match = { class = "^jetbrains-idea" },     opacity = "0.95 0.95" })
hl.window_rule({ name = "spotify-opacity",      match = { class = "^spotify" },            opacity = "0.95 0.95" })
hl.window_rule({ name = "vivaldi-opacity",      match = { class = "^vivaldi-stable" },     opacity = "1.0 1.0" })

-- Float/center rules for dialogs
hl.window_rule({ name = "open-file-dialog",     match = { title = "^Open File" },     float = true, center = true })
hl.window_rule({ name = "open-folder-dialog",   match = { title = "^Open Folder" },   float = true, center = true })
hl.window_rule({ name = "save-as-dialog",       match = { title = "^Save As" },       float = true, center = true })
hl.window_rule({ name = "pavucontrol",          match = { class = "org.pulseaudio.pavucontrol" }, float = true, center = true, size = { "monitor_w * 0.45", "monitor_h * 0.85" } })
hl.window_rule({ name = "vivaldi-settings",     match = { title = "^Vivaldi Settings" },         float = true, center = true, size = { "monitor_w * 0.45", "monitor_h * 0.85" } })

-- Quickshell update terminal
hl.window_rule({ name = "qs-kitty-update",      match = { title = "^qs-kitty-update" }, float = true, center = true, size = { "monitor_w * 0.60", "monitor_h * 0.80" } })

-- Workspace assignments
hl.window_rule({ name = "code-workspace",       match = { class = "^code" },           workspace = "3" })
hl.window_rule({ name = "spotify-workspace",    match = { class = "^spotify" },        workspace = "6" })
hl.window_rule({ name = "steam-workspace",      match = { class = "steam" },           workspace = "6 silent" })
hl.window_rule({ name = "steam-app-workspace",  match = { class = "^steam_app_" },     workspace = "6 silent" })

-- Steam rules
hl.window_rule({ name = "steam-opacity",        match = { class = "steam" },           opacity = "0.80 0.80" })
hl.window_rule({ name = "steam-friends",        match = { class = "steam", title = "^Friends List" },     float = true, center = true, size = { "monitor_w * 0.30", "monitor_h * 0.65" } })
hl.window_rule({ name = "steam-settings",       match = { class = "steam", title = "^Steam Settings" },   float = true, center = true, size = { "monitor_w * 0.45", "monitor_h * 0.85" } })

-- Steam game rules
hl.window_rule({ name = "steam-app-immediate",  match = { class = "^steam_app_" }, immediate = true })
hl.window_rule({ name = "steam-app-no-anim",    match = { class = "^steam_app_" }, no_anim = true })
hl.window_rule({ name = "steam-app-no-blur",    match = { class = "^steam_app_" }, no_blur = true })
hl.window_rule({ name = "steam-app-no-shadow",  match = { class = "^steam_app_" }, no_shadow = true })
hl.window_rule({ name = "steam-app-rounding",   match = { class = "^steam_app_" }, rounding = 0 })
hl.window_rule({ name = "steam-app-fullscreen", match = { class = "^steam_app_" }, fullscreen = true })
