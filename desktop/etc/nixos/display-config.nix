{ config, pkgs, ... }:

{
  services.xserver = {
    videoDrivers = [ "nvidia" ];

    xrandrHeads = [ "DVI-D-0" "DVI-I-1" ];
  };
}
