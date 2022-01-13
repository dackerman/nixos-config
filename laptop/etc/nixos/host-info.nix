{ config, pkgs, ... }:

{
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Framework laptop needs a custom kernel to avoid
  # bugs in wireless networking
  # boot.kernelPackages = pkgs.linuxPackages_5_14;
  boot.kernelPackages = pkgs.linuxPackages_5_15;
  boot.kernelParams = [ "net.ifnames=0" ];

  services.blueman.enable = true;

  networking = {
    hostName = "decomplected";
    hostId = "1a9f99d0";
    usePredictableInterfaceNames = false;
    networkmanager.enable = true;
    extraHosts = ''
      192.168.1.1 router.asus.com
    '';
    interfaces.wlan0.useDHCP = true;
  };

  # Possible way to get bluetooth to work
  # Pulled from https://community.frame.work/t/using-the-ax210-with-linux-on-the-framework-laptop/1844/47
  # 
  # boot.kernelPackages = pkgs.linuxPackages_5_13;
  # services.blueman.enable = true;
  # security.rtkit.enable = true;
  # services.pipewire = {
  #   enable = true;
  #   alsa.enable = true;
  #   pulse.enable = true;
  #   jack.enable = true;
  #   config.pipewire = {
  #     "context.properties" = {
  #       "link.max-buffers" = 16;  # version < 3 clients can't handle more than this
  #       "log.level" = 2;          # https://docs.pipewire.org/#Logging
  #     };
  #   };
  # };
}
