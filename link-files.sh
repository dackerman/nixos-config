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
    if [[ "`readlink -f $filepath`" == "$prefix$filepath" ]]
    then
        echo "ALREADY LINKED: $filepath (skipping)"
        return
    fi
    if [ -e "$filepath" ]
    then
        newfilepath="$filepath.$(timestamp).backup"
        echo "Moving existing file to $newfilepath"
        sudo mv "$filepath" "$newfilepath"
    fi
    mkdir -p $(dirname "$filepath")
    echo "linking $filepath to $prefix$filepath"
    sudo ln -s "$prefix$filepath" "$filepath"
}

# Link all shared files to home
find home -type f | sed 's/^/\//' | while read -r line; do link_file "$line"; done

# Link all files within platform
find "$platform" -type f | cut -d'/' -f2- | sed 's/^/\//' | while read -r line; do link_file "$line" "$platform"; done

echo "done."
