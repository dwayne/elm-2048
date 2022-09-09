{
  pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/b42e50fe36242b1b205a7d501b7911d698218086.tar.gz") {}
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.caddy
    pkgs.nodejs-16_x
  ];
}
