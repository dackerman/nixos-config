NixOS Config
------------

## Overview

These are the [NixOS](https://nixos.org/) config and other dotfiles I run
on both my homoiconicity and [Framework](https://frame.work/) laptop.

This repository is set up such that there's one collection of files
that are shared across machines as-is, and a collection that are
specific to each machine. For example, my keyboard shortcut prefix is
slightly different on my laptop and homoiconicity, since I use a [kinesis
keyboard](https://kinesis-ergo.com/shop/advantage2/) on my homoiconicity.

The way this works is that everything under `./etc/**` in this
repository will get symlinked on the machine at `/etc/**`, `./home/**`
to `/home/**`, etc. Then, `laptop` and `homoiconicity` are for
platform-specific files - so if I'm running on homoiconicity, it would link
`./homoiconicity/etc/**` to `/etc/**` and so on.

### Most important files

My NixOS and XMonad files are below - the way they generally work is
both the shared and platform-specific files get symlinked into the
same directory so they can reference each other.

For the NixOS config, my main one is shared and it imports
`machine-config.nix` which may be symlinked to the laptop or homoiconicity
version.
* main shared config is at [etc/nixos/configuration.nix](./etc/nixos/configuration.nix)
* homoiconicity nixos module [homoiconicity/etc/nixos/machine-config.nix](./homoiconicity/etc/nixos/machine-config.nix)
* laptop nixos module [laptop/etc/nixos/machine-config.nix](./laptop/etc/nixos/machine-config.nix)

For my XMonad config, I actually define the main method in my
platform-specific file, and then import the `./lib/SharedConfig` file
in each of them. This way I have full control over what parts of the
shared config I pull in for i.e. keyboard shortcuts.
* shared XMonad config [home/david/.xmonad/lib/SharedConfig.hs](./home/david/.xmonad/lib/SharedConfig.hs)
* homoiconicity XMonad main [homoiconicity/home/david/.xmonad/xmonad.hs](./homoiconicity/home/david/.xmonad/xmonad.hs)

## Usage

I assume you already have [NixOS
installed](https://nixos.org/download.html#nix-install-linux) to begin
with.

### Linking files

    ./link_files [platform]

The first time you pull this repository, you want to symlink all of
your dotfiles to point to files in this repository. You do that by
running the above command where `platform` can be `laptop` or
`homoiconicity`. If you don't specify a platform, it will use your
hostname by default. This may ask for your sudo password to link paths that are
owned by root (usually paths in `/etc/nixos`).


### Updating nixos

    sudo nixos-rebuild switch

After changing your configuration, run the above command to apply all
the changes to your system to match
`/etc/nixos/configuration.nix`. After doing this, you may want to
reboot your computer so that various systemd services are started
properly.
