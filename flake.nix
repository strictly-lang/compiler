{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, naersk }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system};
            packageName = "strictly";
            rustPkgs = import nixpkgs {
              inherit system;
              overlays = [ (import rust-overlay) ];
            };
            rustWithWasmTarget = rustPkgs.rust-bin.nightly.latest.default.override {
              extensions = [ "rust-src" ];
              targets = [ "wasm32-wasi" ];
            };
            naerskLib = pkgs.callPackage naersk {};
            naerskLibWasm = pkgs.callPackage naersk {
              rustc = rustWithWasmTarget;
            };
            app = naerskLibWasm.buildPackage {
              name = "strictly";
              src = ./.;
              cargoBuildOptions = x: x ++ [ "-p" "strictly" ];
              copyLibs = true;
              CARGO_BUILD_TARGET = "wasm32-wasi";
            };
        in {
          packages.${packageName} = app;
          defaultPackage = self.packages.${system}.${packageName};

          devShell = pkgs.mkShell {
            buildInputs = [
                pkgs.nodejs
                pkgs.wasmtime
                rustWithWasmTarget
                pkgs.rust-analyzer
                pkgs.wasm-tools
              ];
          };
        }
      );
}

