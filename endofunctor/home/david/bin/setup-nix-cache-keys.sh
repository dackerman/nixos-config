#!/usr/bin/env bash
set -e

echo "Setting up Nix binary cache signing keys..."

# Generate the key pair
sudo nix-store --generate-binary-cache-key endofunctor-cache /var/cache-priv-key.pem /var/cache-pub-key.pem

# Set proper permissions
sudo chmod 600 /var/cache-priv-key.pem
sudo chmod 644 /var/cache-pub-key.pem

echo "Keys generated successfully!"
echo
echo "Public key:"
cat /var/cache-pub-key.pem
echo
echo "Add this to your other machines' configuration:"
echo "  trusted-public-keys = [ \"$(cat /var/cache-pub-key.pem)\" ];"