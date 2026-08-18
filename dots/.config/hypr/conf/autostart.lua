-- Autostart
-- See https://wiki.hypr.land/Configuring/Basics/Autostart/

hl.on("hyprland.start", function()
    hl.exec_cmd("hypridle")
    hl.exec_cmd("fcitx5 -d")
    hl.exec_cmd("hyprctl setcursor Bibata-Modern-Ice 24")

    hl.exec_cmd("awww-daemon")
    hl.exec_cmd('sh -c \'for i in $(seq 1 20); do awww query >/dev/null 2>&1 && break; sleep 0.25; done; awww img "$(find "$wallpaper_path" -type f | shuf -n 1)"\'')
        
    hl.exec_cmd("qs -c ~/.config/quickshell")
end)
