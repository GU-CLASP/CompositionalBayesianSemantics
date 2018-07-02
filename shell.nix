let nixpkgs_source = (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-17.09.tar.gz);
in {pkgs ? import nixpkgs_source {
    inherit system;
  }, system ? builtins.currentSystem, nodejs ? pkgs."nodejs-4_x"}:

let
  nodeEnv = import ../webppl/node-env.nix {
    inherit (pkgs) stdenv python2 utillinux runCommand writeTextFile;
    inherit nodejs;
  };
  nodePkgs = import ../webppl/node-packages.nix {
    inherit (pkgs) fetchurl fetchgit;
    inherit nodeEnv;
  };
  # webppl-viz = nodePkgs."webppl-viz-git://github.com/probmods/webppl-viz.git".override (oldAttrs: {
  #   buildInputs = oldAttrs.buildInputs ++ [ nodePkgs.node-gyp nodePkgs.gyp nodePkgs.node-gyp-build pkgs.pkgconfig pkgs.cairo pkgs.libjpeg pkgs.giflib  ];
  # });

   # nixpkgs_source = /local_dir; # for local directory
   # nixpkgs_source = nixpkgs.fetchFromGitHub { # for safety of checking the hash
   #    owner = "jyp";
   #    repo = "nixpkgs";
   #    rev = "e32d48eb4d2a4c2d668cc8b1ca77bb03c2510b74";
   #    sha256 = "02dwn1k1yxc8d2fviynalb43csggd0v3lmsaaammswkqcyb7m836";
   #  };
   # nixpkgs_source = ~/repo/nixpkgs;

  hp = pkgs.haskellPackages.override{
      overrides = self: super: {
        pretty-compact = self.callPackage ./pretty-compact.nix {};
        # typedflow = self.callPackage ./typedflow.nix {};
        };};
  ghc = hp.ghcWithPackages (ps: with ps; ([ cabal-install statistics ]));

in pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [ nodePkgs.webppl nodePkgs.npm nodejs nodePkgs.node-gyp ghc pkgs.gnuplot ];
  shellHook = ''
    export LANG=en_US.UTF-8
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}

