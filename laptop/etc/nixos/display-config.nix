{ config, pkgs, ... }:

{
  services.xserver.synaptics = {
    enable = true;
    vertTwoFingerScroll = true;
    tapButtons = false;
  };
  networking.wireless.enable = true;

}
