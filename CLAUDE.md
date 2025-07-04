# NixOS Configuration Project Guide

## Build & System Commands
- Update NixOS: `sudo nixos-rebuild switch`
- Link files to system: `./link-files.sh [platform]` (defaults to hostname, or specify: laptop|homoiconicity|endofunctor)
- Git shorthand: `gs` for git status
- **Important Note**: Don't actually try to run sudo nixos-rebuild switch. Always prompt the user to do this (and any other sudo commands) themselves.
- When running a sudo command, always use -A to invoke the GUI askpass

## Project Structure
- `/etc/nixos/configuration.nix`: Main shared NixOS config (symlinked from this directory)
- `/etc/nixos/machine-config.nix`: Platform-specific config (symlinked)
- Platform directories: `laptop/`, `homoiconicity/`, `endofunctor/`
- **Important**: NixOS configuration files are symlinked from this directory to `/etc/nixos/`, so edits here update the system configuration directly

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

## Symlink Management
- `./link-files.sh` only needs to be run if _new_ files are added that need to be symlinked to a given computer. Once they are symlinked, they can be edited in this repository and they'll be automatically updated in the proper location.

## Technology Stack & Locations

### System & Boot Management
- **NixOS**: Core OS (`/etc/nixos/configuration.nix`, machine-specific configs)
- **Systemd-boot**: UEFI bootloader across all platforms
- **LUKS encryption**: Full disk encryption on endofunctor
- **Nix package manager**: With binary cache support

### Window Management & Desktop
- **XMonad**: Tiling window manager (Haskell)
  - Config: `{platform}/home/david/.xmonad/xmonad.hs`
  - Shared lib: `/home/david/.xmonad/lib/SharedConfig.hs`
- **XMobar**: Status bar (`{platform}/home/david/.xmobarrc`)
- **GNOME**: Secondary desktop environment
- **LightDM**: Display manager
- **Stalonetray**: System tray (`{platform}/home/david/.stalonetrayrc`)

### Shell & Terminal
- **Fish**: Primary shell (`{platform}/home/david/.config/fish/config.fish`)
- **Bash**: Secondary shell with completion
- **Terminator**: Terminal with gruvbox theme (`/home/david/.config/terminator/config`)
- **Kitty**: Modern terminal with OSC 52 clipboard
- **Tmux**: Terminal multiplexer (`/endofunctor/home/david/.tmux.conf`)

### Development Tools & Languages
- **Emacs**: Primary editor with daemon mode
- **VS Code**: Secondary editor
- **Code-Cursor**: AI-powered editor (user-managed)
- **Git**: Version control with GitHub CLI (gh)

#### Programming Languages
- **Node.js & Yarn**: JavaScript development
- **Go**: With gopls language server
- **Java (JDK)**: For Java development
- **Clojure ecosystem**: SBCL, Leiningen, Neil, Babashka, clj-kondo, clojure-lsp
- **Haskell**: For XMonad configuration

### AI & Development Services
- **API Integration**: OpenAI, Claude (Anthropic), Gemini
- **Ollama**: Local AI with CUDA acceleration
- **Language servers**: gopls, clojure-lsp

### Networking & Remote Access
- **NetworkManager**: Network configuration
- **Tailscale**: VPN mesh networking
- **SSH**: Remote access with X11 forwarding, keychain management
- **Mosh**: Mobile shell for unstable connections

### Audio & Video
- **PipeWire**: Modern audio server with PulseAudio compatibility
- **Pasystray & Pavucontrol**: Audio controls
- **VLC**: Video player with ffmpeg patches
- **OBS Studio**: Screen recording
- **FFmpeg**: Video processing

### Applications & Productivity
- **Browsers**: Chrome, Firefox, Brave
- **Communication**: Signal Desktop, Zoom
- **Graphics**: GIMP, Krita, Inkscape
- **File Management**: Nautilus (GNOME Files)
- **Documentation**: Zeal offline docs, MultiMarkdown
- **Database**: DBeaver SQL client
- **IDE**: IntelliJ IDEA Community

### System Utilities
- **feh**: Image viewer and wallpaper setter
- **dmenu**: Application launcher
- **scrot**: Screenshot utility
- **xclip**: Clipboard utility
- **fzf**: Fuzzy finder
- **htop & btop**: System monitors
- **jq**: JSON processor
- **tree**: Directory tree viewer

### Security & Encryption
- **GnuPG**: Encryption with pinentry-curses
- **VeraCrypt**: Encrypted drives and files
- **Lightlocker**: Screen locking

### Notifications & System Tray
- **Dunst**: Desktop notifications
- **TWMN**: Alternative notification daemon (`{platform}/home/david/.config/twmn/twmn.conf`)

### Platform-Specific Hardware

#### Desktop (homoiconicity)
- **NVIDIA drivers**: GPU with performance optimizations
- **Bluetooth**: With blueman manager
- **Multiple ethernet**: Network interfaces

#### Laptop (decomplected)
- **Framework laptop**: Custom kernel (5.15) for wireless
- **Libinput**: Touchpad configuration
- **Light**: Backlight control with actkbd bindings
- **Autorandr**: Multi-monitor management
- **Remote building**: Uses endofunctor as build machine

#### Endofunctor
- **Nix-serve**: Binary cache server (port 5000)
- **NVIDIA**: Desktop GPU with display optimizations
- **Dual monitors**: Multi-display setup
- **Cron**: Scheduled tasks for Fastmail integration

### Custom Scripts & Automation
Located in `/home/david/bin/`:
- **auto-tmux**: Automatic tmux session management
- **wallpaperchanger.sh**: Dynamic wallpaper rotation
- **shot**: Screenshot utility
- **sync-mail & archive-tagged-mail**: Email management
- **install-cursor-latest.sh**: Cursor editor updates

### Configuration Management
- **Symlink system**: `link-files.sh` manages dotfile linking
- **Platform-specific**: Separate laptop/desktop/endofunctor directories
- **Shared configs**: Common dotfiles in `/home/david/`

### Services & Daemons
- **Docker**: Containerization with overlay2 storage
- **OpenSSH**: Remote access server
- **CUPS**: Printing with Avahi network discovery
- **Emacs daemon**: Background editor service

### File Systems & Storage
- **NTFS**: Cross-OS data partition mounting
- **rclone**: Google Drive synchronization

### Fonts
- **Monoid**: Programming font
- **Hack**: Terminal and system font
- **Fira Code Nerd Font**: With programming ligatures