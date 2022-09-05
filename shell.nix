{
  # Used in CI to have a smaller closure
  withoutDevTools ? false
}:
let
  project = import ./default.nix ;

  inherit (project) pkgs compiler;

  libs = [ pkgs.git
           pkgs.zlib
         ] ;

  buildTools = {
    cabal = "latest";
  };

  devTools = if withoutDevTools then { } else {
    hlint = "latest";
    haskell-language-server = "latest";
    fourmolu = "latest";
  };

in project.shellFor {
  tools = buildTools // devTools;
  buildInputs = libs ;
}
