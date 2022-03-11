# { pkgs ? import <nixpkgs> {} }:

# derivation {
#   name = "typewriter";
#   builder = "${bash}/bin/bash";
#   args = [ ./builder.sh ];
#   system = builtins.currentSystem;
#   inherit racket;
#   inherit raco;
# }

with import <nixpkgs> {};

{ stdenv, racket }:

stdenv.mkDerivation {
  name = "typewriter";
  # builder = "${bash}/bin/bash";
  # args = [ ./builder.sh ];
  builder = ./builder.sh;
  system = builtins.currentSystem;
  inherit racket;
}
