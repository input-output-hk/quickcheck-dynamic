{
  description = "QuickCheck Dynamic";

  inputs = {
    iogx.url = "github:zeme-wana/iogx";
  };

  outputs = inputs:
    inputs.iogx.mkFlake {
      inherit inputs;
      debug = true;
      repoRoot = ./.;
      flakeOutputsPrefix = "";
      systems = [ "x86_64-darwin" "x86_64-linux" ];
      haskellCompilers = [ "ghc8107" ];
      defaultHaskellCompiler = "ghc8107";
      haskellCrossSystem = null;
      haskellProjectFile = ./nix/haskell-project.nix;
      perSystemOutputsFile = null;
      shellName = "quickcheck-dynamic";
      shellPrompt = "\n\\[\\033[1;31m\\][quickcheck-dynamic]\\$\\[\\033[0m\\] ";
      shellWelcomeMessage = "🤟 \\033[1;31mWelcome to quickcheck-dynamic\\033[0m 🤟";
      shellModuleFile = null;
      includeHydraJobs = true;
      excludeProfiledHaskellFromHydraJobs = true;
      blacklistedHydraJobs = [ ];
      enableHydraPreCommitCheck = true;
      includeReadTheDocsSite = false;
      readTheDocsSiteDir = null;
      readTheDocsHaddockPrologue = "";
      readTheDocsExtraHaddockPackages = null;
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    accept-flake-config = true;
    allow-import-from-derivation = true;
  };
}
