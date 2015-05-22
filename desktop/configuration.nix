# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
    splashImage = "/etc/nixos/grub-bg.png";
  };

  networking.hostName = "immutable-grape"; # Define your hostname.
  networking.hostId = "04f2fa20";

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
    videoDrivers = [ "nvidia" ];

    xrandrHeads = [ "DVI-D-0" "DVI-I-1" ];

    desktopManager = {
      default = "none";
      xterm.enable = false;
    };

    displayManager = {
      slim = {
        enable = true;
        #theme = pkgs.fetchurl {
        #  url    = "https://github.com/jagajaga/nixos-slim-theme/archive/Final.tar.gz";
        #  sha256 = "4cab5987a7f1ad3cc463780d9f1ee3fbf43603105e6a6e538e4c2147bde3ee6b";
        #};
        defaultUser = "david";
        autoLogin = true;
      };
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
  };
}
