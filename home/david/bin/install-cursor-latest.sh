#!/usr/bin/env bash

# Download latest Cursor AppImage directly to ~/bin
echo "Downloading latest Cursor AppImage..."
curl -L https://downloader.cursor.sh/linux/appImage/x64 -o ~/bin/cursor.AppImage

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