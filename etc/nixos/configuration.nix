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

    # packageOverrides = pkgs: {
    #   git = pkgs.git.override { guiSupport = true; };
    # };
  };

  time.timeZone = "US/Pacific";

  networking.firewall = {
    allowedTCPPorts = [ ];
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

    # Programming and editing
    emacs
    git
    tree
    nodejs
    yarn
    clojure
    android-studio

    # Applications
    signal-desktop            # chat application
    google-chrome
    firefox
    gimp
    vlc
    obs-studio                # screen recording
    ffmpeg                    # convert mp4 to gif
    youtube-dl
    isync                     # sync email
    notmuch                   # process email
    rclone                    # Google Drive syncing utility
    adoptopenjdk-jre-bin      # Running Java programs
    blender
    obsidian                  # Second brain
  ];

  programs.steam.enable = true;

  services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "curses";
    enableSSHSupport = true;
  };

  fonts.fonts = with pkgs; [
    monoid
    hack-font
  ];

  services.emacs.enable = true;

  services.openssh.enable = true;

  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint ];
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

  # virtualisation.docker = {
  #   enable = true;
  #   storageDriver = "devicemapper";
  # };

  users.users.david = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" ];
    # openssh.authorizedKeys.keyFiles = [
    #   "/home/david/.ssh/id_rsa.pub"
    #   "/home/david/.ssh/laptop_rsa.pub"
    # ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
