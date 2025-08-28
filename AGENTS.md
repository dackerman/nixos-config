AGENTS Guide for nixos-config

1) Build/lint/test
- Build/apply system: sudo -A nixos-rebuild switch
- Dry-run build: sudo -A nixos-rebuild test
- Evaluate config: nix eval --impure --expr 'import <nixpkgs/nixos> {}'
- Check Nix formatting: nix fmt .
- Lint Nix: nix flake check (if flake present; otherwise skip)
- XMonad compile: xmonad --recompile
- Single test: no test suite; test by switching with --upgrade or using nixos-rebuild build then boot

2) Repo workflows
- Files here are symlinked into /etc/nixos and $HOME via ./link-files.sh [platform]. Only run when adding new files. After edits, just rebuild.
- Never run privileged commands automatically; always use sudo -A for GUI askpass.
- Do not change git config; commit focused changes with clear messages.

3) Code style (Nix, Haskell, Shell)
- Nix: functional style, 2-space indent, explicit imports, group related options, comment non-obvious choices, organize environment.systemPackages by category.
- Haskell (XMonad): keep pure config in SharedConfig; name bindings in lowerCamelCase; import qualified modules; avoid partial functions; pattern-match exhaustively.
- Shell: shebang #!/usr/bin/env bash; set -euo pipefail in new scripts; quote variables; prefer arrays; handle errors with clear messages.
- Naming: descriptive, kebab-case for file names, lowerCamelCase for Haskell, snake_case for bash vars if needed.
- Errors: fail fast on invalid paths; prefer explicit options over magic defaults; add comments for hardware-specific tweaks.

4) Tools and shortcuts
- Git helpers in home/david/.config/fish/functions (gs for status, gl for log, etc.).
- Build host selector via platform dirs: laptop/, homoiconicity/, endofunctor/; main shared config in etc/nixos/configuration.nix.

5) AI assistant notes
- No Cursor or Copilot rules present.
- When suggesting sudo commands, present them for the user to run manually.
