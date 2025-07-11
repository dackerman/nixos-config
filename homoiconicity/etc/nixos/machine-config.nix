{ config, pkgs, ... }:

{
  # Boot into UEFI mode
  boot.loader.grub.enable = false;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.systemd-boot.enable = true;

  boot.supportedFilesystems = ["ntfs"];

  # fileSystems."/mnt/cross-os-data" =
  #   { device = "/dev/disk/by-label/CrossOSData";
  #     fsType = "ntfs";
  #     options = ["rw" "uid=1000"];
  #   };

  # Use rclone to sync notes with GoogleDrive
  # systemd.user.services.syncnotes = {
  #   enable = true;
  #   description = "sync ~/notes to Google Drive";
  #   wantedBy = ["default.target"];
  #   after = ["network.target"];
  #   serviceConfig = {
  #     Type = "notify";
  #     ExecStart = "/run/current-system/sw/bin/bash -c 'PATH=/run/wrappers/bin:$PATH exec /run/current-system/sw/bin/rclone mount drive:notes /home/david/notes -vv --config=/home/david/.config/rclone/rclone.conf --cache-dir=/home/david/.cache/rclone'";
  #     Restart = "always";
  #   };
  # };

  services.xserver = {
    dpi = 96;
    videoDrivers = [ "nvidia" ];
  };

  hardware.nvidia.open = false;

  networking = {
    hostName = "homoiconicity";
    hostId = "04f2fa20";

    interfaces.enp0s31f6.useDHCP = true;
    interfaces.enp7s0.useDHCP = true;
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # try to fix bluetooth issue
  # see https://nixos.wiki/wiki/Bluetooth
  hardware.enableAllFirmware = true;

  # Remote builder configuration
  nix.buildMachines = [{
    hostName = "endofunctor";
    system = "x86_64-linux";
    maxJobs = 4;
    speedFactor = 2;
    supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
    mandatoryFeatures = [ ];
  }];
  nix.distributedBuilds = true;

  # Nix substituters configuration
  nix.settings = {
    substituters = [
      "https://cache.nixos.org/"
      "http://endofunctor:5000"
    ];
    trusted-substituters = [
      "http://endofunctor:5000"
    ];
    trusted-public-keys = [ "endofunctor-cache:+rvLp8nQBCqsu1a/9eMlNfPhrELEhezXWF/UrdGoy5g=" ];
  };

  # Fix HiDPI settings for Gnome
  environment.variables = {
    GDK_SCALE = "1";
    GDK_DPI_SCALE = "1";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=1";
  };
  system.stateVersion = "22.05";
}
