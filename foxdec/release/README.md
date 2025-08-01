# FoxDec

As example, we run FoxDec on the given binary `./examples/wc_small/wc`.

## Step 1: run XED

	./run_xed.sh ./examples/wc_small/wc

The parameter is the binary that is to be disassembled.
This should create the file `./examples/wc_small/wc.xed`.

## Step 2: run FoxDec

	./run_foxdec.sh ./examples/wc_small/ -n wc -i BINARY --Gmetrics
	
The first parameter is the directory in which a file with the given name is present. In the example, file `examples/wc_small/wc` should exist. The following command-line options are available (pick at least one `--G` option):


| Option | Meaning | Info |
|---|---|---|
|  `-n FILENAME` |   Name of the binary. | Obligatory |
| `-i INPUTTYPE` |   Either the string `BINARY` or the string `L0`. | Obligatory |
| `-v` | Verbose mode | Optional |
| `--GL0` | Generate a .L0 file (only if INPUTTYPE==BINARY). |
| `--Gcallgraph` | Generate an annotated call graph. | |
| `--GNASM` | Generate NASM | |
| `--Gfuncs` | Generate per function a control flow graph (CFG) and information. ||
| `--Gmetrics` | Generate metrics in .metrics.txt file. ||
