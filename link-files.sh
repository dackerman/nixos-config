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

# List of explicit directories to link as a whole
explicit_dirs=("/home/david/.config/fish")

# Symlink a directory as a whole
function link_dir() {
    dirpath=$1
    if [ -e "$dirpath" ] || [ -L "$dirpath" ]; then
        newdirpath="$dirpath.$(timestamp).backup"
        echo "Moving existing directory to $newdirpath"
        sudo mv "$dirpath" "$newdirpath"
    fi
    mkdir -p $(dirname "$dirpath")
    echo "linking $dirpath to `pwd`$dirpath"
    sudo ln -s "`pwd`$dirpath" "$dirpath"
}

# Link explicit directories first
for dir in "${explicit_dirs[@]}"; do
    link_dir "$dir"
done

# Helper: returns 0 if $1 starts with $2
startswith() {
    case "$1" in
        $2*) return 0 ;;
        *) return 1 ;;
    esac
}

# Link all shared files in these top-level directories, skipping files inside explicit_dirs
for dir in etc home
do
    find "$dir" -type f | sed 's/^/\//' | while read -r line; do
        skip=false
        for exdir in "${explicit_dirs[@]}"; do
            if startswith "$line" "$exdir/"; then
                skip=true
                break
            fi
        done
        if ! $skip; then
            link_file "$line"
        fi
    done
done

# Link all platform-specific files, skipping files inside explicit_dirs
find "$platform" -type f | cut -d'/' -f2- | sed 's/^/\//' | while read -r line; do
    skip=false
    for exdir in "${explicit_dirs[@]}"; do
        if startswith "$line" "$exdir/"; then
            skip=true
            break
        fi
    done
    if ! $skip; then
        link_file "$line" "$platform"
    fi
done

echo "done."
