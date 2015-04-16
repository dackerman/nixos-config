Nixos Config
------------


Configuration files for the Nix operating system.  These files define all of the settings you would want when you set up your linux machine, but instead of dozens of config files and various commands needing to be run, it's all here in one configuration file.  NixOS uses the Nix package manager, which means that everything that is installed is put into a folder versioned by the contents so that you can trivially roll back to previous versions if anything goes wrong doing an upgrade or changing your settings.

After changing your configuration, simply run "sudo nixos-rebuild switch" to install or uninstall as necessary.  See the nixos website at http://nixos.org for more information.
