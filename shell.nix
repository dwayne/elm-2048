{
  pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/8de22cec9b51d6fdce55480688c67dbe6f33dd34.tar.gz") {}
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.caddy
    pkgs.elmPackages.elm
    pkgs.nodePackages.sass
    pkgs.nodePackages.uglify-js
  ];

  shellHook =
    ''
    export project=${builtins.toString ./.}
    export build="$project/.build"
    export experiments="$project/experiments"
    export prototype="$project/prototype"

    export PATH="$project/bin:$PATH"
    '';
}
