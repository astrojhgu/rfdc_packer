{
  description = "Bluespec development shell using nixos-25.05";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  #nixConfig = {
  #  substituters = [
  #    "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store/"
  #    "https://cache.nixos.org/"
  #  ];
  #  trusted-public-keys = [
  #  ];
  #};

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          name = "bluespec-shell";
          buildInputs = with pkgs; [
            bluespec
            verilator  # 可选：如果你想用 Verilog backend 仿真
            gtkwave    # 可选：波形查看工具
            yosys
            yosys-bluespec
            yosys-synlig
            netlistsvg
	    iverilog
          ];

          shellHook = ''
            echo "Bluespec development shell (nixos-25.05)"
            echo "bsc version: $(bsc -v)"
          '';
        };
      });
}
