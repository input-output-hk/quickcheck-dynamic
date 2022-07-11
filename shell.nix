{
  # Used in CI to have a smaller closure
  withoutDevTools ? false
}:
let
  project = import ./default.nix ;

  buildTools = {
    cabal = "latest";
  };

  devTools = if withoutDevTools then { } else {
    hlint = "latest";
    haskell-language-server = "latest";
    fourmolu = "latest";
  };

  shell = project.shellFor {
    tools =   buildTools // devTools;
  };

in shell { }
