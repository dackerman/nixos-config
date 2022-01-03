{ config, pkgs, ... }:

{
  imports =
    [
      /etc/nixos/hardware-configuration.nix
      /etc/nixos/host-info.nix
      /etc/nixos/display-config.nix
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

  programs.bash = {
    enableCompletion = true;
    # interactiveShellInit = "source ~/bash-scripts/z.sh";
  };

  environment.systemPackages = with pkgs; [
    # Programming and editing
    emacs
    git
    wget
    tree
    
    # Browsers
    google-chrome
    firefox

    # System tools
    dmenu                     # open applications
    terminator                # terminal emulator
    xmobar                    # top bar for xmonad
    gnome.nautilus            # file finder
    pv                        # terminal-based progress viewer
    feh                       # simple X-server image viewer

    # Applications
    signal-desktop            # chat application
  ];

  fonts.fonts = with pkgs; [
    monoid
    hack-font
  ];

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
      # autoLogin.user = "david";
      # autoLogin.enable = true;
      defaultSession = "none+xmonad";

      # session = [
      #   { manage = "desktop"; name = "gnome"; }
      #   { manage = "window"; name = "xmonad"; }
      # ];
      
      lightdm = {
        enable = true;
      };
      
      sessionCommands = ''
      /home/david/bin/setrandomwallpaper.sh
      '';
    };

    windowManager = {
      xmonad = {
       enable = true;
       enableContribAndExtras = true;
       config = pkgs.lib.readFile /home/david/.xmonad/xmonad.hs;
      };
    };
  };

  # virtualisation.docker = {
  #   enable = true;
  #   storageDriver = "devicemapper";
  # };

  users.users.david = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
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
