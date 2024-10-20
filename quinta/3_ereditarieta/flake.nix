{
  description = "Ereditarietà";

  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      mapSystem = f: nixpkgs.lib.genAttrs systems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = mapSystem ({ pkgs }: {
            default = pkgs.mkShell {
                buildInputs = with pkgs; [ cabal-install ];
            };
          });
      # packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
      # packages.x86_64-linux.default = self.packages.x86_64-linux.hello;
    };
}
