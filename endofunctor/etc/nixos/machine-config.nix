{ config, pkgs, ... }:

{

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-669b6436-9348-4a77-830f-2229f783b4fe".device = "/dev/disk/by-uuid/669b6436-9348-4a77-830f-2229f783b4fe";
  boot.initrd.luks.devices."luks-669b6436-9348-4a77-830f-2229f783b4fe".keyFile = "/crypto_keyfile.bin";

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";

  #fileSystems."/mnt/cross-os-data" =
  #  { device = "/dev/disk/by-label/CrossOSData";
  #    fsType = "ntfs";
  #    options = ["rw" "uid=1000"];
  #  };

  services.xserver = {
    xkbVariant = "";
    dpi = 96;
    videoDrivers = [ "nvidia" ];
  };

  networking = {
    hostName = "endofunctor";
    hostId = "d79f7d3b";
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # try to fix bluetooth issue
  # see https://nixos.wiki/wiki/Bluetooth
  hardware.enableAllFirmware = true;

  # Fix HiDPI settings for Gnome
  environment.variables = {
    GDK_SCALE = "1";
    GDK_DPI_SCALE = "1";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=1";
  };
  
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
