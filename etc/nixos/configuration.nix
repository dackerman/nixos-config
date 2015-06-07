{ config, pkgs, ... }:

{
  imports =
    [
      /etc/nixos/hardware-configuration.nix
      /etc/nixos/host-info.nix
      /etc/nixos/display-config.nix
    ];
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
    splashImage = "/etc/nixos/grub-bg.png";
  };

  nix = {
    buildCores = 0;
    trustedBinaryCaches = [
      "https://cache.nixos.org/"
      "https://ryantrinkle.com:5443"
    ];
  };

  nixpkgs.config = {
    allowUnfree = true;

    chromium = {
      enablePepperFlash = true;
      enablePepperPDF = true;
    };
    firefox = {
      enableAdobeFlash = true;
    };
    packageOverrides = pkgs: {
      git = pkgs.git.override { guiSupport = true; };
    };
  };

  time.timeZone = "US/Pacific";

  environment.systemPackages = with pkgs; [
    # Emacs
    emacs
    emacs24Packages.dash
    emacs24Packages.haskellMode
    emacs24Packages.magit

    # Haskell
    (pkgs.haskellngPackages.ghcWithPackages (p: with p; [
      xmonad
      xmonad-contrib
      xmonad-extras
      xmobar
      cabal-install
    ]))

    # Node
    nodejs
    nodePackages.grunt-cli
    nodePackages.bower
    nodePackages.npm2nix

    # Misc
    xscreensaver
    git
    dmenu2
    terminator
    wget
    vim
    gimp
    rdesktop
    zlib
    # wineUnstable
    wine # both of these require gratuitous compilation
    dropbox
    galculator
    vpnc

    # Clojure
    jre

    # Audio
    audacity
    lsof
    pavucontrol
    lame

    # Browsers
    chromium
    firefox

    #clang
    #llvm
    #ncurses
    #haskellPackages.ghc
    #haskellPackages.cabalInstall
  ];

  services.openssh.enable = true;

  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint ];
  };

  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio = {
    enable = true;
  };

  services.xserver = {
    enable = true;
    layout = "us";

    desktopManager = {
      default = "none";
      xterm.enable = false;
    };

    displayManager = {
      slim = {
        enable = true;
        defaultUser = "david";
        autoLogin = true;
      };
      sessionCommands = ''
      /home/david/bin/setrandomwallpaper.sh
      '';
    };

    windowManager = {
      xmonad = {
       enable = true;
       enableContribAndExtras = true;
       extraPackages = self: [ self.xmonadContrib ];
      };
      default = "xmonad";
    };
  };

  users.extraUsers.david = {
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    uid = 1000;
    openssh.authorizedKeys.keyFiles = [
      "/home/david/.ssh/id_rsa.pub"
      "/home/david/.ssh/laptop_rsa.pub"
    ];
  };
}
