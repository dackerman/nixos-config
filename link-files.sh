set -e

platform=$1

if [ -z "$1" ]
then
    echo "Specify a platform, either desktop or laptop"
    exit 1;
fi

function timestamp() {
    date +%s
}

function link_file() {
    echo ""
    filepath=$1
    if [ -z "$2" ]
    then
        prefix="`pwd`"
    else
        prefix="`pwd`/$2"
    fi
    if [[ "`readlink -f /$filepath`" == "$prefix/$filepath" ]]
    then
        echo "ALREADY LINKED: $filepath (skipping)"
        return
    fi
    if [ -e "/$filepath" ]
    then
        newfilepath="/$filepath.$(timestamp).backup"
        echo "Moving existing file to $newfilepath"
        sudo mv "/$filepath" "$newfilepath"
    fi
    echo "linking /$filepath to $prefix/$filepath"
    sudo ln -s "$prefix/$filepath" "/$filepath"
}

link_file "etc/nixos/configuration.nix"
link_file "etc/nixos/grub-bg.png"
link_file "home/david/.xmonad/xmonad.hs"
link_file "home/david/.xmobarrc"
link_file "home/david/.config"

link_file "etc/nixos/hardware-configuration.nix" "$platform"
link_file "etc/nixos/display-config.nix" "$platform"
link_file "etc/nixos/host-info.nix" "$platform"

echo "done."
