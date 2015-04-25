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
  };

  
  time.timeZone = "US/Pacific";

  environment.systemPackages = with pkgs; [
    # Emacs
    emacs
    emacs24Packages.dash
    emacs24Packages.haskellMode

    # Haskell
    haskellPackages.xmonad
    haskellPackages.xmobar
    haskellPackages.elmReactor
    haskellPackages.elmRepl
    haskellPackages.elmServer
    haskellPackages.elmGet

    # Node
    nodejs
    nodePackages.grunt-cli
    nodePackages.bower
    
    # Misc
    python27Full
    git
    dmenu2
    terminator
    chromium
    firefox
    wget
    vim
    gimp
    rdesktop
    wine
    dropbox
  ];

  services.openssh.enable = true;

  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint ];
  };

  hardware.opengl.driSupport32Bit = true;

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
      lightdm = {
        enable = true;
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
