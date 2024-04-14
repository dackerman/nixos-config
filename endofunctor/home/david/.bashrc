#  Manage SSH keys
eval $(keychain --eval --quiet id_ed25519)

export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye >/dev/null
