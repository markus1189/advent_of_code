{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  range = lib.range 0 2;
  compileHaskell = day: inputFile:
    pkgs.runCommand "aoc-builder" { } ''
      mkdir -p $out/bin
      ${myGhc}/bin/ghc --make -O3 -rtsopts -eventlog -threaded -o "$out/bin/d${day}-compiled" ${inputFile}
    '';
  updateScript = pkgs.writeShellScriptBin "update" ''
    set -e

    DAY="$(${pkgs.coreutils}/bin/date +%d)"
    DAY_NO_ZERO="$(${pkgs.coreutils}/bin/date +%-d)"
    echo "Updating for day $DAY_NO_ZERO"
    cp -n -v day00.hs "day''${DAY}.hs"
    sed -i -e "s/^\([[:space:]]*\)range = lib.range 0 [[:digit:]]\+/\1range = lib.range 0 ''${DAY_NO_ZERO}/" shell.nix

    echo Done
  '';
  myHaskellPackages = ps:
    with ps; [
      array
      adjunctions
      comonad
      data-fix
      fgl
      fourmolu
      grid
      hashtables
      haskell-language-server
      linear
      lens
      monad-extras
      monad-loops
      parsec
      pointedlist
      recursion-schemes
      split
      tasty
      tasty-hspec
      text
      unordered-containers
      stm
    ];
  myGhc = haskellPackages.ghcWithHoogle myHaskellPackages;
  compileDay = n:
    let n' = if n < 10 then "0${toString n}" else toString n;
    in compileHaskell n' (./day + "${n'}.hs");
  mkDay = n:
    let n' = if n < 10 then "0${toString n}" else toString n;
    in writeScriptBin "d${n'}" ''
      ${myGhc}/bin/runhaskell --ghc-arg=-Wall day${n'}.hs
    '';
in mkShell {
  buildInputs = [ myGhc updateScript ] ++ map mkDay range
    ++ map compileDay range;
}
