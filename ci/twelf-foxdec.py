#!/usr/bin/env python3

import argparse
import os
import subprocess
import sys

## =============================================================================
## == CONSTANTS ================================================================
## =============================================================================

DISASM_PROG    = "foxdec-xed"
CFI_PATCH_PROG = "foxdec-cfi-patch"
FOXDEC_PROG    = "foxdec"

## =============================================================================
## == PATHS ====================================================================
## =============================================================================

def xedfile_path(name):
    return f"{name}.xed"

def cfifile_path(name):
    return f"{name}.cfi.txt"

## =============================================================================
## == SUBPROCESSES =============================================================
## =============================================================================

def run_foxdec(binary, config, directory):
    cmd = [FOXDEC_PROG, '-c', config, '-i', 'ELLF', '-d', directory, '-n', binary, '--Gellf']
    with open('foxdec.log', 'w') as f:
        f.write(f"RUNNING: {' '.join(cmd)}\n\n")
        subprocess.run(cmd, stdout=f)



## =============================================================================
## == CHECK HEALTH =============================================================
## =============================================================================

def check_health():
    print(f"{sys.argv[0]} Health Check... ", end="")

    progs = [DISASM_PROG, CFI_PATCH_PROG, FOXDEC_PROG]
    for prog in progs:
        cmd = [prog, '--help']
        subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=True)

    print("[OK]")

## =============================================================================
## == MAIN =====================================================================
## =============================================================================

def main():
    parser = argparse.ArgumentParser(description="Front-End for the FoxDec commands")
    parser.add_argument("--checkhealth", action="store_true", help="Check if the foxdec commands can be invoked")
    parser.add_argument("--binary", help="The binary to disassemble")
    parser.add_argument("--config", help="The FoxDec .dhall config")
    parser.add_argument("--directory", help="The directory to run FoxDec in")
    args = parser.parse_args()

    if args.checkhealth:
        check_health()
        return

    if not args.binary or not args.config or not args.directory:
        print("All arguments (--binary, --config, --directory) must be passed", sys.stderr)
        sys.exit(1)

    run_foxdec(args.binary, args.config, args.directory)



if __name__ == '__main__':
    main()

