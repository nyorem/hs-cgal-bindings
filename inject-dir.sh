#! /usr/bin/env bash

CABAL_FILE="hs-cgal-bindings.cabal"
sed -e "s?CGAL_DIR?$CGAL_DIR?g" $CABAL_FILE".in" > $CABAL_FILE

