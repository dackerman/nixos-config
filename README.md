NixOS Config
------------

## Overview

These are the [NixOS](https://nixos.org/) config and other dotfiles I run
on both my desktop and [Framework](https://frame.work/) laptop.

This repository is set up such that there's one collection of files
that are shared across machines as-is, and a collection that are
specific to each machine. For example, my keyboard shortcut prefix is
slightly different on my laptop and desktop, since I use a [kinesis
keyboard](https://kinesis-ergo.com/shop/advantage2/) on my desktop.

## Usage

I assume you already have [NixOS
installed](https://nixos.org/download.html#nix-install-linux) to begin
with.

### Linking files

The first time you pull this repository, you want to symlink all of
your dotfiles to point to files in this repository. You do that by
running `./link_files <platform>` where `platform` can be `laptop` or
`desktop`. This may ask for your sudo password to link paths that are
owned by root (usually paths in `/etc/nixos`).


### Updating nixos

After changing your configuration, run `sudo nixos-rebuild switch`
which will apply all the changes to your system to match
`/etc/nixos/configuration.nix`. After doing this, you may want to
reboot your computer so that various systemd services are started
properly.
