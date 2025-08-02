# Fish shell configuration

# Manage SSH keys with keychain
if status is-interactive
    keychain --eval --quiet id_ed25519 | source
end

# GPG agent setup
set -gx GPG_TTY (tty)
if status is-interactive
    gpg-connect-agent updatestartuptty /bye >/dev/null
end

# Set environment variables
set -gx CHROME_EXECUTABLE (which google-chrome-stable)
set -gx OPENAI_API_KEY (cat /home/david/openai-api-key.txt)
set -gx CLAUDE_API_KEY (cat /home/david/claude-api-key.txt)
set -gx ANTHROPIC_API_KEY "$CLAUDE_API_KEY"
set -gx GEMINI_API_KEY (cat /home/david/gemini-api-key.txt)
set -gx SUDO_ASKPASS (nix-instantiate --eval -E '(import <nixpkgs> {}).x11_ssh_askpass.outPath' | tr -d '"')/libexec/x11-ssh-askpass

fish_add_path /home/david/.npm-store/bin
fish_add_path /home/david/.local/bin
fish_add_path /home/david/bin

# Aliases for convenience (fish uses abbreviations instead)
abbr gs 'git status'

# Auto-start tmux (skip for SSH sessions)
# if not set -q SSH_CLIENT; and not set -q SSH_TTY
#     ~/bin/auto-tmux
# end