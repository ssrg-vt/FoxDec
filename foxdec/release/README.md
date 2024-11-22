# ICSE'25 "Formally Verified Binary-level Pointer Analysis"
###Reproduction package (anonymized)
 


**Requirements**: Docker and an x86-64 machine or M1/M2/M3 chip.

**Quick start**:
To run the pointer analysis on all binaries mentioned in the paper and reproduce the results, simply run: 

	./docker-run.sh
	
This will populate the subdirectory `./artifacts/`. Specifically, it will generate files containing metrics that contain the data used in the paper. 

For example, the file `./artifacts/wc/wc.metrics.txt` will contain the metrics for the `wc` binary.


**More usage information:** All binaries on which pointer analysis is run, are included within file `ballpark.zip` in the subdirectory `./examples/`. This package is intended for push-button reproduction of the results. To apply the pointer analysis to other binaries, please contact the first author of the paper. Basically, one needs to add the binary and a `.entry` file into the `examples` directory of `ballpark.zip` and then modify `Dockerfile` (see line 28 of that file). A non-anonymized version will be made publicly available as well, that allows more convenience.

**Isabelle/HOL:** IN directory `isabelle` all proofs can be found. The files can be loaded in Isabelle version 2021-1.
