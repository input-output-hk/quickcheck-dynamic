{ inputs, system }:

import inputs.nixpkgs {
  inherit system;
  config = inputs.haskell-nix.config;
  overlays = [
    inputs.haskell-nix.overlay
  ];
}
