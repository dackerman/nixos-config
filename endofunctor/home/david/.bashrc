#  Manage SSH keys
eval $(keychain --eval --quiet id_ed25519)

export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye >/dev/null

export CLAUDE_API_KEY=$(cat ~/claude-api-key.txt)
export ANTHROPIC_API_KEY="$CLAUDE_API_KEY"
export OPENAI_API_KEY=$(cat ~/openai-api-key.txt)
export GEMINI_API_KEY=$(cat ~/gemini-api-key.txt)

alias gs='git status'

export PATH="/home/david/.npm-store/bin:$PATH"


aishell() {
  local prompt="$*"
  if [ -z "$prompt" ]; then
    echo "Usage: aishell [description of command you need]"
    return 1
  fi

  # Run the Node.js script with sourced flag
  AISHELL_SOURCED=true /home/david/code/aishell/dist/index.js "$prompt"

  # Get the last command from file
  if [ -f ~/.aishell_last_command ]; then
    local cmd=$(cat ~/.aishell_last_command)
    rm ~/.aishell_last_command
    # Add to history and pre-fill command line
    history -s "$cmd"
    # Return the command but don't execute it
    echo -n "$cmd"
  fi
}

export PATH="$PATH:/home/david/.local/bin"
