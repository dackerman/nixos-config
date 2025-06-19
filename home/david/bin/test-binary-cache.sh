#!/usr/bin/env bash

echo "Testing binary cache connection to endofunctor..."

# Check if we can reach the cache
if curl -s -o /dev/null -w "%{http_code}" http://endofunctor:5000/nix-cache-info | grep -q "200"; then
    echo "✓ Cache server is reachable"
    echo
    echo "Cache info:"
    curl -s http://endofunctor:5000/nix-cache-info
    echo
else
    echo "✗ Cannot reach cache server at http://endofunctor:5000"
    echo "  Make sure endofunctor has been rebuilt with nix-serve enabled"
    exit 1
fi

# Test if substitution works
echo "Testing substitution (this will show what would be downloaded):"
nix-store -r --dry-run $(nix-instantiate '<nixpkgs>' -A hello) 2>&1 | grep -E "(downloading|will be fetched)"

echo
echo "Current substituters:"
nix show-config | grep substituters