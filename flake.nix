{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
      haskellNix,
    }:
    let
      ghc-version = "ghc9102";
      overlays = [
        haskellNix.overlay
        (
          final: _prev:
          let
            # haskell.nixのtoolsで参照されるhaskell-language-server。
            tool-haskell-language-server =
              final.haskell-nix.tool ghc-version "haskell-language-server"
                "latest";
          in
          {
            # nixpkgsで普通にインストールされるfourmoluはhaskell-language-serverのものと違うので上書きして合わせる。
            inherit (tool-haskell-language-server.project.hsPkgs.fourmolu.components.exes) fourmolu;
          }
        )
        (final: prev: {
          network-transport-kaisui = final.haskell-nix.cabalProject' {
            src = ./network-transport-kaisui;
            compiler-nix-name = ghc-version;
            shell = {
              tools = {
                cabal = "latest";
                cabal-gild = "latest";
                haskell-language-server = "latest";
              };
              buildInputs = with prev; [
                fourmolu
                (writeScriptBin "haskell-language-server-wrapper" ''
                  #!${stdenv.shell}
                  exec haskell-language-server "$@"
                '')
              ];
            };
          };
        })
      ];
    in
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        treefmtEval = treefmt-nix.lib.evalModule pkgs (_: {
          projectRootFile = "flake.nix";
          programs = {
            cabal-gild.enable = true;
            deadnix.enable = true;
            hlint.enable = true;
            nixfmt.enable = true;
            prettier.enable = true;
            shellcheck.enable = true;
            shfmt.enable = true;
            statix.enable = true;

            fourmolu = {
              enable = true;
              package = pkgs.fourmolu;
            };
          };
        });
        flake = pkgs.network-transport-kaisui.flake { };
      in
      (builtins.removeAttrs flake [
        "ciJobs"
        "devShell"
      ])
      // {
        apps = pkgs.lib.mapAttrs (
          name: app:
          app
          // {
            meta = {
              description = "Kaisui distributed process application: ${name}";
              mainProgram = builtins.baseNameOf app.program;
            };
          }
        ) flake.apps;
        checks = flake.checks // {
          formatting = treefmtEval.config.build.check self;
        };
        formatter = treefmtEval.config.build.wrapper;
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org/"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://kaisui.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "kaisui.cachix.org-1:4r6+ZZtsY0YS5PkH2k9JorlwjC00vLiUzHNMxbeqBwM="
    ];
    allow-import-from-derivation = true;
  };
}
