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
      haskellCompilers = [ "ghc927" ];
      defaultHaskellCompiler = "ghc927";
      haskellCrossSystem = null;
      haskellProjectFile = ./nix/haskell-project.nix;
      perSystemOutputsFile = null;
      shellPrompt = "\n\\[\\033[1;31m\\][quickcheck-dynamic]\\$\\[\\033[0m\\] ";
      shellWelcomeMessage = "🤟 \\033[1;31mWelcome to quickcheck-dynamic\\033[0m 🤟";
      shellModuleFile = null;
      includeHydraJobs = true;
      excludeProfiledHaskellFromHydraJobs = true;
      blacklistedHydraJobs = [ ];
      enableHydraPreCommitCheck = true;
      readTheDocsSiteDir = null;
      readTheDocsHaddockPrologue = "";
      readTheDocsExtraHaddockPackages = null;
      preCommitCheckHooks = { stylish-haskell.enable = false; }; # We use fourmolu
    };

  nixConfig = {
    extra-substituters = [
      "https://iohk.cachix.org"
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    accept-flake-config = true;
    allow-import-from-derivation = true;
  };
}
