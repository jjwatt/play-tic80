{
  description = "A multiplatform TIC-80 environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nixgl.url = "github:nix-community/nixGL";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      nixgl,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = if system == "x86_64-linux" then [ nixgl.overlay ] else [ ];
        pkgs = import nixpkgs { inherit system overlays; };

        # Override the existing tic-80 package to inject the Pro CMake flag
        tic-80-pro = pkgs.tic-80.overrideAttrs (oldAttrs: {
          cmakeFlags = (oldAttrs.cmakeFlags or [ ]) ++ [ "-DBUILD_PRO=ON" ];
        });

        tic-80-wrapped = if system == "x86_64-linux" then
          pkgs.writeShellScriptBin "tic80" ''
            if [ -e /etc/NIXOS ]; then
                exec ${tic-80-pro}/bin/tic80 "$@"
            else
                # Non-NixOS x86_64 Linux
                exec ${pkgs.nixgl.nixGLMesa}/bin/nixGLMesa ${tic-80-pro}/bin/tic80 "$@"
            fi
            ''
        else
          tic-80-pro;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            luaPackages.fennel
            fennel-ls
            fnlfmt
            luaPackages.lua
            tic-80-wrapped
          ];
        };
      }
    );
}
