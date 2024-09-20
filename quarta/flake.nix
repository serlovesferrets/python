{
  description = "Umanet python environment";

  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      mapSystem = f: nixpkgs.lib.genAttrs systems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = mapSystem ({ pkgs }: {
        default = with pkgs;
          let
            projectPkgs = [ python312 ];
            devPkgs = [ pyright ruff ];
          in
          mkShell {
            buildInputs = projectPkgs ++ devPkgs;
          };
      });
    };
}
