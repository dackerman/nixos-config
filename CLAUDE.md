# NixOS Configuration Project Guide

## Build & System Commands
- Update NixOS: `sudo nixos-rebuild switch`
- Link files to system: `./link-files.sh <platform>` (platform = laptop|desktop|endofunctor)
- Git shorthand: `gs` for git status

## Project Structure
- `/etc/nixos/configuration.nix`: Main shared NixOS config
- `/etc/nixos/machine-config.nix`: Platform-specific config (symlinked)
- Platform directories: `laptop/`, `desktop/`, `endofunctor/`

## Code Style Guidelines
- NixOS config: Follow the functional programming style of Nix
- Indent with 2 spaces
- Use clear variable names that indicate purpose
- Group related configuration settings together
- Comment complex sections or non-obvious choices
- Prefer explicit imports over wildcard imports
- Keep formatting consistent with existing files
- Organize environment packages by category with comments

## Version Control
- Keep commits focused on single features or fixes
- Test configuration changes before committing
- Use clear, descriptive commit messages