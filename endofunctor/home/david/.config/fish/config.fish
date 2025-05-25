# Fish shell configuration

# Add ~/bin to PATH
fish_add_path /home/david/bin

# Set environment variables
set -gx CHROME_EXECUTABLE (which google-chrome-stable)

# Aliases for convenience (fish uses abbreviations instead)
abbr gs 'git status'

# Auto-start tmux
~/bin/auto-tmux