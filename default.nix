with import <nixpkgs> {};
{ stdenv, racket }:
stdenv.mkDerivation {
  name = "typewriter";
  builder = ./builder.sh;
  system = builtins.currentSystem;
  inherit racket;
}
