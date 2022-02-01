BG=$(find "$HOME/Documents/backgrounds" -type f | sort -R | head -n 1)
feh --bg-fill "$BG"
