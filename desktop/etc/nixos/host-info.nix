{ config, pkgs, ... }:

{
  networking = {
    hostName = "homoiconicity";
    hostId = "04f2fa20";


    interfaces.enp0s31f6.useDHCP = true;
    interfaces.enp7s0.useDHCP = true;
  };

  # Boot into UEFI mode
  boot.loader.grub.enable = false;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.systemd-boot.enable = true;

  boot.supportedFilesystems = ["ntfs"];

  fileSystems."/mnt/cross-os-data" =
    { device = "/dev/disk/by-label/CrossOSData";
      fsType = "ntfs";
      options = ["rw" "uid=1000"];
    };
}
