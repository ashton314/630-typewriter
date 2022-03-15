with import <nixpkgs> {};
{ senv ? stdenv, rkt ? racket }:
senv.mkDerivation {
  name = "typewriter";
  builder = ./builder.sh;
  system = builtins.currentSystem;
  inherit stdenv;
  inherit rkt;
}
