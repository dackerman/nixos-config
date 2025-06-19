# Fish shell configuration

# Add ~/bin to PATH
fish_add_path /home/david/bin

# Manage SSH keys with keychain
if status is-interactive
    eval (keychain --eval --agents ssh --quiet id_ed25519)
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

# Add npm-store to PATH
fish_add_path /home/david/.npm-store/bin
fish_add_path /home/david/.local/bin

# Aliases for convenience (fish uses abbreviations instead)
abbr gs 'git status'
abbr s 'kitten ssh'
