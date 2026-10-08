{ pkgs }:

let
  # NB: fetch a versioned CDN path, not /redoc/latest/..., so the bundle is
  # reproducible. The hash below matches Redoc 2.5.2 (also what static/redoc.standalone.js contains).
  redoc = pkgs.stdenv.mkDerivation {
    pname = "redoc-standalone";
    version = "2.5.2";

    src = pkgs.fetchurl {
      url = "https://cdn.redoc.ly/redoc/v2.5.2/bundles/redoc.standalone.js";
      sha256 = "sha256-a+ta6iO3HQkSLuNd6PMm44rsBQ53Tzv7ofNOXDZgiYY=";
    };

    dontUnpack = true;

    installPhase = ''
      mkdir -p $out
      cp $src $out/redoc.standalone.js
    '';
  };

  icono = pkgs.stdenv.mkDerivation {
    pname = "icono";
    version = "1.3.0";

    src = pkgs.fetchurl {
      url = "https://cdnjs.cloudflare.com/ajax/libs/icono/1.3.0/icono.min.css";
      sha256 = "sha256-Dyy0zD6bW58Q4LIezBxY4nGzwBoQlgbBHKVOEwg5l1U=";
    };

    dontUnpack = true;

    installPhase = ''
      mkdir -p $out
      cp $src $out/icono.min.css
    '';
  };

  milligram = pkgs.stdenv.mkDerivation {
    pname = "milligram";
    version = "1.4.1";

    src = pkgs.fetchurl {
      url = "https://cdnjs.cloudflare.com/ajax/libs/milligram/1.4.1/milligram.min.css";
      sha256 = "sha256-baSxKEISHdSAWiipPkWRuquIMjgNIR//a++CyhnQdIM=";
    };

    dontUnpack = true;

    installPhase = ''
      mkdir -p $out
      cp $src $out/milligram.min.css
    '';
  };

in
{
  inherit redoc icono milligram;
  packages = {
    inherit redoc icono milligram;
  };
}
