# 1. ttf-jetbrains-mono-nerd: Main font (Letters + Icons)
# 2. noto-fonts-emoji: Color emojis
# 3. ttf-nerd-fonts-symbols-common: Pure glyphs for system fallback
# 4. woff2-font-awesome: The new standard for Font Awesome icons on Arch
FONT_PKGS := ttf-jetbrains-mono-nerd noto-fonts-emoji ttf-nerd-fonts-symbols-common woff2-font-awesome

nerd-fonts-all:
	@echo "#### [Fonts] Installing official Arch packages (including woff2 transition)"
	sudo pacman -S --needed --noconfirm $(FONT_PKGS)
	@echo "-- Rebuilding font cache..."
	fc-cache -f
	@echo "-- Done! Your Waybar and Emacs icons are ready."