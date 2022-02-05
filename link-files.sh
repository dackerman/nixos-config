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

# takes the given file in this git repository and and makes a symlink
# to it from the corresponding system directory. For example, a file
# in this repository under ./etc gets symlinked to /etc/that-file in
# the system itself. Uses `sudo` if necessary.
#
# If the optional platform_folder arg is provided, it prefixes the
# file in the git repository with that value. For example, if "laptop"
# is provided, a file in this repository under ./laptop/etc gets
# symlinked to /etc as well.
#
# arguments: file_to_link [platform_folder]
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

# Link all shared files in these top-level directories
for dir in etc home
do
    find "$dir" -type f | sed 's/^/\//' | while read -r line; do link_file "$line"; done
done

# Link all platform-specific files
find "$platform" -type f | cut -d'/' -f2- | sed 's/^/\//' | while read -r line; do link_file "$line" "$platform"; done

echo "done."
