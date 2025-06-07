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

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";

  fileSystems."/mnt/cross-os-data" =
    { device = "/dev/disk/by-label/CrossOSData";
      fsType = "ntfs";
      options = ["rw" "uid=1000"];
    };

  services.xserver = {
    xkb.variant = "";
    dpi = 96;
    videoDrivers = [ "nvidia" ];

    screenSection = ''
      Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
      Option         "AllowIndirectGLXProtocol" "off"
      Option         "TripleBuffer" "on"
    '';
  };

  services.cron = {
    enable = true;
    systemCronJobs = [
      "*/30 * * * *    david    /home/david/code/fastmail-share-fix/cron.sh"
    ];
  };

  # Try to fix issue of ethernet crapping out overnight. Hypothesis
  # is that power saving mode is causing it to crash.
  # See https://www.reddit.com/r/buildapc/comments/xypn1m/network_card_intel_ethernet_controller_i225v_igc/
  boot.kernelParams = [
    "pcie_port_pm=off"
    "pcie_aspm.policy=performance"
    "nvidia-drm.modeset=1"
    "nvidia-drm.fbdev=1"
  ];

  # NVIDIA module options to fix HDMI FRL issues
  boot.extraModprobeConfig = ''
    options nvidia NVreg_EnableHDMIFRL=0
    # Workaround for blank screen with movable cursor on resume
    options nvidia_modeset vblank_sem_control=0
  '';


  hardware.graphics.enable = true;

  hardware.nvidia = {
    modesetting.enable = true;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    
    # Disable power management to avoid black screen on resume
    # per https://nixos.wiki/wiki/Nvidia
    powerManagement.enable = false;
  };

  networking = {
    hostName = "endofunctor";
    hostId = "d79f7d3b";
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Binary cache server
  services.nix-serve = {
    enable = true;
    port = 5000;
    secretKeyFile = "/var/cache-priv-key.pem";
  };

  # Open firewall for nix-serve
  networking.firewall.allowedTCPPorts = [ 5000 ];

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
