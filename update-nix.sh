#! /usr/bin/env nix-shell
#! nix-shell -i sh -p cabal2nix

mydir=`dirname $0`

cabal2nix ${mydir} > ${mydir}/default.nix

