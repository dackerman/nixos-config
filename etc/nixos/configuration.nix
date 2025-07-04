{ config, pkgs, lib, ... }:

  let no-segfault-vlc = pkgs.vlc.override {
        ffmpeg = pkgs.ffmpeg.overrideAttrs (self: prev: {
          patches = prev.patches ++ [
            (pkgs.fetchpatch {
              url = "https://git.ffmpeg.org/gitweb/ffmpeg.git/patch/e9c93009fc34ca9dfcf0c6f2ed90ef1df298abf7";
              hash = "sha256-aE9WN7a2INbss7oRys+AC9d9+yBzlJdeBRcwSDpG0Qw=";
            })
          ];
        });
      };
  in {
  imports =
    [
      /etc/nixos/hardware-configuration.nix
      /etc/nixos/machine-config.nix
    ];

  nix.package = pkgs.nixVersions.latest;

  nixpkgs.config = {
    allowUnfree = true;
  };

  # Enable nix-ld to run dynamically linked executables
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    stdenv.cc.cc
    zlib
    openssl
    fuse
    dbus

    # Essential Electron/Cursor dependencies
    alsa-lib
    at-spi2-atk
    cairo
    cups
    expat
    fontconfig
    freetype
    glib
    gtk3
    libdrm
    libgbm
    libGL
    libxkbcommon
    nspr
    nss
    pango
    xorg.libX11
    xorg.libXcomposite
    xorg.libXdamage
    xorg.libXext
    xorg.libXfixes
    xorg.libXrandr
    xorg.libxcb
  ];

  time.timeZone = "America/New_York";

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22    # SSH
      3000  # dev http server
      3001
      5173  # AI family planner app
      8065  # Mattermost
      9630  # websocket
      # 19000 # expo.dev metro port
      11434 # ollama
      8080  # weaviate
      4173  # weaviate-gui frontend
      3000  # weaviate-gui backend
    ];
  };

  networking.useDHCP = false;

  # Disable NetworkManager-wait-online because it doesn't restart properly
  # after a nixos-rebuild. Seems like some sort of a bug. See report here:
  # https://github.com/NixOS/nixpkgs/issues/180175
  systemd.services.NetworkManager-wait-online.enable = lib.mkForce false;
  systemd.services.systemd-networkd-wait-online.enable = lib.mkForce false;

  programs.bash = {
    completion.enable = true;
    # interactiveShellInit = "source ~/bash-scripts/z.sh";
  };

  environment.variables = {
    EDITOR = "emacsclient -c";
  };

  programs.fish.enable = true;

  environment.systemPackages = with pkgs; [
    # System tools
    dmenu                     # open applications
    terminator                # terminal emulator
    kitty                     # terminal emulator with OSC 52 support
    xmobar                    # top bar for xmonad
    nautilus                  # file finder
    pv                        # terminal-based progress viewer
    feh                       # simple X-server image viewer
    dunst                      # X-server notifications
    libnotify
    stalonetray               # system tray for xmobar
    pasystray                 # system tray icon for pulseaudio
    pavucontrol               # tweaking pulseaudio settings
    alsa-utils                # provides amixer for volume control
    pulseaudio                # provides pactl for pipewire compatibility
    scrot                     # take screenshots
    xclip                     # send to clipboard from terminal
    veracrypt                 # encrypted drives and files
    killall
    gnupg                     # encryption program
    pinentry-curses           # pinentry needed for gnupg
    espeak                    # text-to-speech from the command line
    jq
    multimarkdown             # view markdown from emacs
    unzip
    protontricks              # for running windows games through steam
    keychain                  # for managing SSH keys https://wiki.archlinux.org/title/SSH_keys#SSH_agents
    x11_ssh_askpass           # GUI password prompt for sudo
    htop
    btop
    lightlocker
    rlwrap
    gh
    google-cloud-sdk
    mosh                      # mobile shell for remote connections

    # Programming and editing
    emacs
    # code-cursor  # Moved to user profile for independent updates
    git
    tree
    nodejs
    yarn
    sbcl
    clojure
    neil                      # clojure build/dep tool
    leiningen
    clj-kondo                 # clojure linting support in editor
    zeal                      # offline developer documentation
    clojure-lsp
    babashka
    vscode
    nodePackages.prettier
    fzf
    go
    gopls                     # language server for Go
    tmux                      # terminal multiplexer
    zellij                    # tmux-like app

    # Applications
    signal-desktop            # chat application
    google-chrome
    firefox
    brave
    gimp
    krita
    inkscape
    vlc # no-segfault-vlc
    obs-studio                # screen recording
    ffmpeg                    # convert mp4 to gif
    yt-dlp
    isync                     # sync email
    notmuch                   # process email
    rclone                    # Google Drive syncing utility
    jdk                       # Running/compiling Java programs
    jetbrains.idea-community
    # blender
    # obsidian                  # Second brain
    dbeaver-bin                 # SQL client
    zoom-us
    (ollama.override { acceleration = "cuda"; })
  ];

  services.tailscale.enable = true;
  networking.firewall.checkReversePath = "loose";

  programs.steam.enable = true;

  services.pcscd.enable = true;

  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-curses;
    enableSSHSupport = true;
    settings = {
      default-cache-ttl = 31536000;
      max-cache-ttl = 31536000;
    };
  };

  fonts.packages = with pkgs; [
    monoid
    hack-font
    nerd-fonts.fira-code
  ];

  services.emacs.enable = true;

  services.openssh = {
    enable = true;
    settings = {
      X11Forwarding = true;
      PermitRootLogin = "no";
      PasswordAuthentication = true;
      PubkeyAuthentication = true;
    };
  };

  services.printing = {
    enable = true;
    # drivers = [
    #   pkgs.gutenprint
    #   # pkgs.brgenml1lpr
    #   # pkgs.brgenml1cupswrapper
    #   pkgs.mfcj470dw-cupswrapper
    # ];
  };
  # networking printing
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  # hardware.opengl.driSupport32Bit = true;

  services.pulseaudio = {
    enable = false;
  };

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  services.displayManager.defaultSession = "none+xmonad";

  services.xserver = {
    enable = true;
    xkb.layout = "us";

    desktopManager = {
      gnome.enable = true; # Gnome Desktop

      xterm.enable = false;
    };

    displayManager = {

      lightdm = {
        enable = true;
      };

      sessionCommands = ''
        # xset -dpms  # Disable Energy Star, as we are going to suspend anyway and it may hide "success" on that
        # xset s blank # `noblank` may be useful for debugging
        # xset s 300 # seconds
        # ${pkgs.lightlocker}/bin/light-locker --idle-hint &

        source ~/.profile
        stalonetray &
        pasystray &
        /home/david/bin/wallpaperchanger.sh 300 & # change wallpaper every 5 min
      '';
    };

    windowManager = {
      xmonad = {
       enable = true;
       enableContribAndExtras = true;
       extraPackages = haskellPackages: [
         haskellPackages.data-default
       ];
      };
    };
  };

  systemd.targets.hybrid-sleep.enable = true;
  services.logind.extraConfig = ''
    IdleAction=ignore
    IdleActionSec=1800s
  '';

  # Notification service
  # systemd.user.services.twmnd = {
  #   enable = true;
  #   description = "Desktop notification program twmn";
  #   wantedBy = ["default.target"];
  #   after = ["network.target"];
  #   serviceConfig = {
  #     Type = "simple";
  #     ExecStart = "/run/current-system/sw/bin/twmnd";
  #     Restart = "always";
  #   };
  # };

  virtualisation.docker = {
    enable = true;
    storageDriver = "overlay2";
  };

  users.users.david = {
    isNormalUser = true;
    description = "David Ackerman";
    shell = pkgs.fish;
    extraGroups = [ "networkmanager" "wheel" "audio" "docker" ];
    # openssh.authorizedKeys.keyFiles = [
    #   "/home/david/.ssh/id_rsa.pub"
    #   "/home/david/.ssh/laptop_rsa.pub"
    # ];
  };

  # system.autoUpgrade.enable = true;
}
