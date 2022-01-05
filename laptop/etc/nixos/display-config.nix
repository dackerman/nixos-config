{ config, pkgs, ... }:

{
  services.xserver.libinput = {
    enable = true;
    touchpad.disableWhileTyping = true;
    touchpad.tapping = false;
    touchpad.naturalScrolling = false;
  };
  
  hardware.bluetooth.enable = true;
}
