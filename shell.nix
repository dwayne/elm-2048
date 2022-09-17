{
  pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/454887a35de6317a30be284e8adc2d2f6d8a07c4.tar.gz") {}
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.caddy
    pkgs.elmPackages.elm
    pkgs.nodePackages.sass
  ];

  shellHook =
    ''
    export project=${builtins.toString ./.}
    export experiments="$project/experiments"
    export prototype="$project/prototype"
    '';
}
