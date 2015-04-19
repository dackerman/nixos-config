# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sdb";

  networking.hostName = "immutable-oranges"; # Define your hostname.
  networking.hostId = "1a9f99d1";

  nixpkgs.config = {
    allowUnfree = true;
    
    chromium = {
      enablePepperFlash = true;
      enablePepperPDF = true;
    };
  };
  
  time.timeZone = "US/Pacific";

  environment.systemPackages = with pkgs; [
    python27Full
    git
    dmenu2
    terminator
    chromium
    firefox
    wget
    vim
    emacs
    emacs24Packages.dash
    emacs24Packages.haskellMode
    haskellPackages.xmonad
    haskellPackages.xmobar
    nodePackages.grunt-cli
    nodejs
  ];

  services.openssh.enable = true;

  services.printing.enable = true;

  hardware.opengl.driSupport32Bit = true;

  services.xserver = {
    enable = true;
    layout = "us";
    videoDrivers = [ "nvidia" ];

    xrandrHeads = [ "DVI-I-1" "DVI-D-0" ];

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
