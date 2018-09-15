{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.callPackage ./scythe.nix {}
