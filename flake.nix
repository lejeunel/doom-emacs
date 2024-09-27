{ stdenv, lib, pkgs, fetchFromGitHub }:

with lib;

let
in
  stdenv.mkDerivation rec {
    name = "doomemacs";

    src = fetchFromGitHub {
      owner = "doomemacs";
      repo = "doomemacs";
    };

    installPhase = ''
      ln -s $src/* ~/.config/emacs
    '';
}}
