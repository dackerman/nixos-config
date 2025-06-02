#!/usr/bin/env bash

echo "Updating Cursor..."
nix-env -u code-cursor

if [ $? -eq 0 ]; then
    echo "✓ Cursor updated successfully"
    cursor --version
else
    echo "✗ Failed to update Cursor"
    exit 1
fi