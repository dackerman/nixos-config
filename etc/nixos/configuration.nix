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
      "http://hydra.nixos.org"
    ];
  };

  nixpkgs.config = {
    allowUnfree = true;

    packageOverrides = pkgs: {
      git = pkgs.git.override { guiSupport = true; };
    };
  };

  time.timeZone = "US/Pacific";

  networking.firewall = {
    allowedTCPPorts = [ 3000 24800 ];
  };

  programs.bash = {
    enableCompletion = true;
    interactiveShellInit = "source ~/bash-scripts/z.sh";
  };

  environment.systemPackages = with pkgs; [
    # Emacs
    emacs

    git
    dmenu2
    terminator
    wget
    dropbox

    haskellPackages.xmobar
  ];

  fonts.fonts = [ pkgs.terminus_font ];

  services.openssh.enable = true;

  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint ];
  };

  services.synergy.server = {
    enable = true;
    configFile = "/home/david/.synergy.conf";
    autoStart = true;
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
      };
      default = "xmonad";
    };
  };

  virtualisation.docker = {
    enable = true;
    storageDriver = "devicemapper";
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
