{ config, pkgs, ... }:

{
  imports =
    [
      /etc/nixos/hardware-configuration.nix
      /etc/nixos/machine-config.nix
    ];

  # nix = {
  #   buildCores = 0;
  #   trustedBinaryCaches = [
  #     "https://cache.nixos.org/"
  #     # "https://ryantrinkle.com:5443"
  #     "http://hydra.nixos.org"
  #   ];
  # };

  nixpkgs.config = {
    allowUnfree = true;
    # pulseaudio = true;

    # packageOverrides = pkgs: {
    #   git = pkgs.git.override { guiSupport = true; };
    # };
  };

  time.timeZone = "America/New_York";

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      3000 # dev http server
      9630 # websocket
      19000 # expo.dev metro port
    ];
  };

  networking.useDHCP = false;

  programs.bash = {
    enableCompletion = true;
    # interactiveShellInit = "source ~/bash-scripts/z.sh";
  };

  environment.variables = {
    EDITOR = "emacsclient -c";
  };

  environment.systemPackages = with pkgs; [
    # System tools
    dmenu                     # open applications
    terminator                # terminal emulator
    xmobar                    # top bar for xmonad
    gnome.nautilus            # file finder
    pv                        # terminal-based progress viewer
    feh                       # simple X-server image viewer
    twmn                      # X-server notifications
    stalonetray               # system tray for xmobar
    pasystray                 # system tray icon for pulseaudio
    pavucontrol               # tweaking pulseaudio settings
    pamixer                   # CLI tool for managing pulseaudio
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
    htop
    btop

    # Programming and editing
    emacs
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

    # Applications
    signal-desktop            # chat application
    google-chrome
    firefox
    gimp
    krita
    inkscape
    vlc
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
    dbeaver                   # SQL client
  ];

  services.autorandr = {
    enable = true;
  };


  services.tailscale.enable = true;
  networking.firewall.checkReversePath = "loose";

  programs.steam.enable = true;

  services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "curses";
    enableSSHSupport = true;
  };

  fonts.packages = with pkgs; [
    monoid
    hack-font
  ];

  services.emacs.enable = true;

  services.openssh = {
    enable = true;
    settings.X11Forwarding = true;
  };

  services.printing = {
    enable = true;
    drivers = [
      # pkgs.gutenprint
      # pkgs.brgenml1lpr
      # pkgs.brgenml1cupswrapper
      pkgs.mfcj470dw-cupswrapper
    ];
  };
  # networking printing
  services.avahi = {
    enable = true;
    nssmdns = true;
  };

  # hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio = {
    enable = true;
  };

  services.xserver = {
    enable = true;
    layout = "us";

    desktopManager = {
      gnome.enable = true; # Gnome Desktop

      xterm.enable = false;
    };

    displayManager = {
      defaultSession = "none+xmonad";

      lightdm = {
        enable = true;
      };

      sessionCommands = ''
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

  # Notification service
  systemd.user.services.twmnd = {
    enable = true;
    description = "Desktop notification program twmn";
    wantedBy = ["default.target"];
    after = ["network.target"];
    serviceConfig = {
      Type = "simple";
      ExecStart = "/run/current-system/sw/bin/twmnd";
      Restart = "always";
    };
  };

  virtualisation.docker = {
    enable = true;
    storageDriver = "devicemapper";
  };

  users.users.david = {
    isNormalUser = true;
    description = "David Ackerman";
    extraGroups = [ "networkmanager" "wheel" "audio" ];
    # openssh.authorizedKeys.keyFiles = [
    #   "/home/david/.ssh/id_rsa.pub"
    #   "/home/david/.ssh/laptop_rsa.pub"
    # ];
  };

  # system.autoUpgrade.enable = true;
}
