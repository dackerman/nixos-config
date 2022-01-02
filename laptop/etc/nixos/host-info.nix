{ config, pkgs, ... }:

{
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Framework laptop needs a custom kernel to avoid
  # bugs in wireless networking
  boot.kernelPackages = pkgs.linuxPackages_5_14;
  boot.kernelParams = [ "net.ifnames=0" ];

  networking = {
    hostName = "decomplected";
    hostId = "1a9f99d0";
    wireless.enable = true;
    usePredictableInterfaceNames = false;
    networkmanager.enable = true;
    extraHosts = ''
      192.168.1.1 router.asus.com
    '';
  };
  interfaces.wlan0.useDHCP = true;
}
