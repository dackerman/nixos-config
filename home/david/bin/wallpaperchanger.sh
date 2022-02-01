interval="${1:-300}" # 5 minute default
while true; do
    $HOME/bin/setrandomwallpaper.sh
    sleep "$interval"
done
