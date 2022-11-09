#!/bin/bash

cabal run foxdec-exe -- ./config/config_verbose.dhall examples/grader grader
cp -r examples/grader /artifacts/
