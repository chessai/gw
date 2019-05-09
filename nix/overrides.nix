{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  gw = (
    with rec {
      gwSource = pkgs.lib.cleanSource ../.;
      gwBasic  = self.callCabal2nix "gw" gwSource { };
    };
    overrideCabal gwBasic (old: {
    })
  );
}
