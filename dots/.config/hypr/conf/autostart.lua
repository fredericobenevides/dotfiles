-- Autostart
-- See https://wiki.hypr.land/Configuring/Basics/Autostart/

hl.on("hyprland.start", function()
    hl.exec_cmd("hypridle")
    hl.exec_cmd("fcitx5 -d")
    hl.exec_cmd("hyprctl setcursor Bibata-Modern-Ice 24")

    hl.exec_cmd("awww-daemon")
    hl.exec_cmd('sh -c \'for i in $(seq 1 20); do awww query >/dev/null 2>&1 && break; sleep 0.25; done; fallback="$(find "$wallpaper_path" -type f | shuf -n 1)"; for o in $(hyprctl monitors | awk "/^Monitor /{print \\$2}"); do w=$(hyprctl monitors | awk -v m="$o" "\\$1==\\"Monitor\\" && \\$2==m {f=1; next} f && /@/{split(\\$1,a,\\"x\\"); print a[1]; exit}"); img="$fallback"; if [ "$w" = "2560" ]; then hw="$(find "$wallpaper_path" -type f -name \\"*2560*\\" | shuf -n 1)"; [ -n "$hw" ] && img="$hw"; fi; awww img -o "$o" "$img"; done\'')
        
    hl.exec_cmd("wl-paste --type text/plain --watch cliphist store")
    hl.exec_cmd("qs -c ~/.config/quickshell")
end)
