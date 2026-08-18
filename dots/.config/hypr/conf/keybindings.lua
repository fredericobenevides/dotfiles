-- Keybindings
-- See https://wiki.hypr.land/Configuring/Basics/Binds/

local mainMod = "SUPER"

local terminal    = "kitty"
local browser     = "vivaldi"
local fileManager = "thunar"
local colorPicker = "hyprpicker"
local screenshot   = "grim"
local screenshotArea = screenshot .. ' -g "$(slurp -d)"'

-- Applications
hl.bind(mainMod .. " + Return",        hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + b",             hl.dsp.exec_cmd(browser))
hl.bind(mainMod .. " + E",             hl.dsp.exec_cmd(fileManager))
hl.bind(mainMod .. " + SPACE",         hl.dsp.global("quickshell:toggle-launcher"))
hl.bind(mainMod .. " + C",             hl.dsp.exec_cmd(colorPicker))

-- Windows
hl.bind(mainMod .. " + Q",             hl.dsp.window.close())
hl.bind(mainMod .. " + SHIFT + Q",     hl.dsp.exec_cmd("hyprctl activewindow | grep pid | tr -d 'pid:' | xargs kill"))
hl.bind(mainMod .. " + F",             hl.dsp.window.fullscreen({ mode = "maximized", action = "toggle" }))
hl.bind(mainMod .. " + SHIFT + F",     hl.dsp.window.fullscreen({ mode = "fullscreen", action = "toggle" }))
hl.bind(mainMod .. " + SHIFT + E",     hl.dsp.exit())
hl.bind(mainMod .. " + T",             hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + SHIFT + T",     hl.dsp.exec_cmd("hyprctl dispatch workspaceopt allfloat"))
hl.bind(mainMod .. " + ALT + left",    hl.dsp.window.swap({ direction = "l" }))
hl.bind(mainMod .. " + ALT + right",   hl.dsp.window.swap({ direction = "r" }))
hl.bind(mainMod .. " + ALT + up",      hl.dsp.window.swap({ direction = "u" }))
hl.bind(mainMod .. " + ALT + down",    hl.dsp.window.swap({ direction = "d" }))
hl.bind("ALT + Tab",                   hl.dsp.window.cycle_next(),            { repeating = true })
hl.bind("ALT + Tab",                   hl.dsp.window.alter_zorder({ mode = "top" }), { repeating = true })
hl.bind(mainMod .. " + P",             hl.dsp.window.pseudo())
hl.bind(mainMod .. " + G",             hl.dsp.group.toggle())

-- Windows movement
hl.bind(mainMod .. " + left",          hl.dsp.focus({ direction = "l" }))
hl.bind(mainMod .. " + right",         hl.dsp.focus({ direction = "r" }))
hl.bind(mainMod .. " + up",            hl.dsp.focus({ direction = "u" }))
hl.bind(mainMod .. " + down",          hl.dsp.focus({ direction = "d" }))
hl.bind(mainMod .. " + mouse:272",     hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273",     hl.dsp.window.resize(), { mouse = true })
hl.bind(mainMod .. " + SHIFT + right", hl.dsp.exec_cmd("hyprctl dispatch resizeactivewidth 100"))
hl.bind(mainMod .. " + SHIFT + left",  hl.dsp.exec_cmd("hyprctl dispatch resizeactivewidth -100"))
hl.bind(mainMod .. " + SHIFT + down",  hl.dsp.exec_cmd("hyprctl dispatch resizeactiveheight 100"))
hl.bind(mainMod .. " + SHIFT + up",    hl.dsp.exec_cmd("hyprctl dispatch resizeactiveheight -100"))

-- Special workspaces
hl.bind(mainMod .. " + S",             hl.dsp.workspace.toggle_special("magic"))
hl.bind(mainMod .. " + SHIFT + S",     hl.dsp.window.move({ workspace = "special:magic" }))
hl.bind(mainMod .. " + D",             hl.dsp.workspace.toggle_special("dev"))
hl.bind(mainMod .. " + SHIFT + D",     hl.dsp.window.move({ workspace = "special:dev" }))

-- Switch workspaces
for i = 1, 9 do
    local key = tostring(i)
    hl.bind(mainMod .. " + " .. key,   hl.dsp.focus({ workspace = i }))
end
hl.bind(mainMod .. " + 0",             hl.dsp.focus({ workspace = 10 }))
hl.bind(mainMod .. " + CTRL + up",     hl.dsp.focus({ workspace = "+1" }))
hl.bind(mainMod .. " + CTRL + down",   hl.dsp.focus({ workspace = "-1" }))
hl.bind(mainMod .. " + mouse_down",    hl.dsp.focus({ workspace = "+1" }))
hl.bind(mainMod .. " + mouse_up",      hl.dsp.focus({ workspace = "-1" }))

-- Move to workspace
for i = 1, 9 do
    local key = tostring(i)
    hl.bind(mainMod .. " + SHIFT + " .. key,  hl.dsp.window.move({ workspace = i }))
end
hl.bind(mainMod .. " + SHIFT + 0",            hl.dsp.window.move({ workspace = 10 }))
hl.bind(mainMod .. " + SHIFT + bracketleft",  hl.dsp.window.move({ workspace = "-1" }), { desc = "Move to Prev WS" })
hl.bind(mainMod .. " + SHIFT + bracketright", hl.dsp.window.move({ workspace = "+1" }), { desc = "Move to Next WS" })

-- Move to workspace (silent)
for i = 1, 9 do
    local key = tostring(i)
    hl.bind(mainMod .. " + CTRL + " .. key,   hl.dsp.window.move({ workspace = i, follow = false }))
end
hl.bind(mainMod .. " + CTRL + 0",             hl.dsp.window.move({ workspace = 10, follow = false }))
hl.bind(mainMod .. " + CTRL + bracketleft",   hl.dsp.window.move({ workspace = "-1", follow = false }), { desc = "Move to Prev WS" })
hl.bind(mainMod .. " + CTRL + bracketright",  hl.dsp.window.move({ workspace = "+1", follow = false }), { desc = "Move to Next WS" })

-- Group navigation
hl.bind(mainMod .. " + SHIFT + right",        hl.dsp.group.next())
hl.bind(mainMod .. " + SHIFT + left",         hl.dsp.group.prev())

-- Actions
hl.bind(mainMod .. " + CTRL + R",             hl.dsp.exec_cmd("hyprctl reload"))
hl.bind(mainMod .. " + CTRL + L",             hl.dsp.exec_cmd("pidof hyprlock || hyprlock"))
hl.bind(mainMod .. " + PRINT",                hl.dsp.exec_cmd(screenshot))
hl.bind(mainMod .. " + SHIFT + PRINT",        hl.dsp.exec_cmd(screenshotArea))

-- Laptop multimedia keys
hl.bind("XF86AudioRaiseVolume",               hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume",               hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"),  { locked = true, repeating = true })
hl.bind("XF86AudioMute",                      hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),  { locked = true, repeating = true })
hl.bind("XF86AudioMicMute",                   hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp",                hl.dsp.exec_cmd("brightnessctl s 10%+"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown",              hl.dsp.exec_cmd("brightnessctl s 10%-"), { locked = true, repeating = true })

-- Playerctl
hl.bind("XF86AudioNext",  hl.dsp.exec_cmd("playerctl next"),        { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"),  { locked = true })
hl.bind("XF86AudioPlay",  hl.dsp.exec_cmd("playerctl play-pause"),  { locked = true })
hl.bind("XF86AudioPrev",  hl.dsp.exec_cmd("playerctl previous"),    { locked = true })

-- Cursor Zoom
hl.bind(mainMod .. " + SHIFT + mouse_down", hl.dsp.exec_cmd("hyprctl eval \"hl.config({ cursor = { zoom_factor = hl.get_config('cursor.zoom_factor') - 0.7 } })\""), { desc = "Zoom In" })
hl.bind(mainMod .. " + SHIFT + mouse_up",   hl.dsp.exec_cmd("hyprctl eval \"hl.config({ cursor = { zoom_factor = hl.get_config('cursor.zoom_factor') + 0.7 } })\""), { desc = "Zoom Out" })
hl.bind(mainMod .. " + SHIFT + Z",          hl.dsp.exec_cmd("hyprctl eval \"hl.config({ cursor = { zoom_factor = 1 } })\""), { desc = "Zoom Reset" })

-- Swaync
hl.bind(mainMod .. " + N", hl.dsp.exec_cmd("swaync-client -t -sw"), { desc = "Notification Center" })
