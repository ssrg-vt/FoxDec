#!/bin/bash
# Usage:
#
# ./run_foxdec
#
# This will provide you with information on command-line parameters 
time cabal run foxdec-exe -- "$@"
