#!/usr/bin/env bash

# Download latest Cursor AppImage directly to ~/bin
echo "Getting download URL from Cursor API..."
DOWNLOAD_URL=$(curl -s "https://www.cursor.com/api/download?platform=linux-x64&releaseTrack=stable" | jq -r '.downloadUrl')

if [ -z "$DOWNLOAD_URL" ] || [ "$DOWNLOAD_URL" = "null" ]; then
    echo "Error: Could not retrieve download URL"
    exit 1
fi

echo "Downloading Cursor AppImage from: $DOWNLOAD_URL"
curl -L "$DOWNLOAD_URL" -o ~/bin/cursor.AppImage

# Make it executable
chmod +x ~/bin/cursor.AppImage

# Create wrapper script
echo "Creating wrapper script..."
cat > ~/bin/cursor << 'EOF'
#!/usr/bin/env bash
exec ~/bin/cursor.AppImage "$@"
EOF
chmod +x ~/bin/cursor

echo "✓ Cursor installed successfully!"
echo "✓ It will auto-update when you launch it"
echo ""
echo "To use Cursor, run: cursor"