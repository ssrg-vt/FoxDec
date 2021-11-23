The FoxDec (for **Fo**rmal **x**86-64 **Dec**ompilation) project is investigating [decompilation][decompile] of x86-64 binaries into C code that is _sound_ as well as fully _recompilable_. Soundness ensures that the decompiled C code is functionally equivalent to the input binary. Recompilability ensures that the decompiled C code can be successfully compiled to generate an executable binary. 

FoxDec is currently actively being developped. In its current stage, it does disassembly, control flow reconstruction and function boundary detection.
Work-in-progress is variable analysis, decompilation to C, data flow analysis, and much more.


<span style="font-size: 150%; color:darkblue">NEWS</span>

* Our [decompilation-to-C paper][sefm20-paper] has received the Best Paper award at [SEFM 2020][sefm20]!
* Our [decompilation-to-C paper][sefm20-paper] has been accepted at [SEFM 2020][sefm20]!

## Table of Contents
1. [Introduction](#intro)
2. [How to build](#build)
3. [How to use](#usage)
4. [Documentation](#docs)
5. [Papers](#papers)
6. [Contact](#contact)

## Introduction <a name="intro"></a>
**Formally verified decompilation.**
Decompilation to a high-level language involves multiple phases. At a high-level, the phases usually include disassembly that lifts assembly code from binary, control flow graph (CFG) recovery that extracts program CFG from assembly, extraction of high-level program constructs (e.g., statements, variables, references) from assembly, and type assignment. FoxDec is investigating techniques for the decompilation phases that are formally verified. 

FoxDec's decompilation phases include disassembly; CFG recovery; extraction of an abstract code that models a program as a CFG of basic blocks; converting basic blocks into sequential code that models the program's corresponding state changes over memory, registers, and flags; variable analysis that maps memory regions to variables and references; and type analysis that assigns types. Converting basic blocks into sequential code that captures program state changes requires a formal model of the underlying machine (i.e., formal semantics of x86-64 instructions). The project leverages the [Chum project][chum]'s formal x86-64 machine model for this purpose. 

Central to formally verified decompilation is the notion of _sound decompilation_. FoxDec defines soundness for each of these decompilation phases (sound disassembly is explored in a different project) and formally verifies them: algorithms for each phase are formalized in the [Isabelle/HOL theorem prover][isabelle] and proven correct. 

**Use cases.**
Sound, recompilable decompilation to C has a variety of use cases. For example, patching a binary to fix errors or potential security exploits is highly complex in settings where source code or third-party libraries are no longer available or build processes or tools have become outdated. Patching at the C-level is relatively easier and a compelling alternative when the decompiled C code is formally proven to be functionally equivalent to the binary. 

Other use cases include binary analysis, binary porting (as an alternative to [software emulation][qemu]), and binary optimization, each of which can now be performed at the C level (e.g., using off-the-shelf C code analysis and optimization tools). 





## How to build <a name="build"></a>
The GitHub page is [here][git].

1. Install [Graphviz](https://graphviz.org) and make sure `dot` is accessible by updating the `PATH` environment variable.
2. Install [Stack](https://docs.haskellstack.org/en/stable/README/), the build tool used for developping FoxDec. 
3. Go to directory `./foxdec/`.
3. Run `stack build --haddock --haddock-arguments --odir=docs/haddock`.
This builds the application and generates documentation using [Haddock](https://haskell-haddock.readthedocs.io/en/latest/index.html).
4. Run `stack install`. This copies executables to accessible locations.
5. Run `foxdec-exe 1 examples/du du`. This runs FoxDec on the `du` example.

We use some tools that are assumed to be standard available and accessible (i.e., added to the `PATH` environment variable). For Linux, these are `readelf` and `objdump` (latter is optional but convenient).
For Mac, these are `otool` and `nm`.

## How to use <a name="usage"></a>
<p style="text-align: center;">
  <img src="./foxdec/docs/overview/overview.png" alt="FoxDec Overview" width="90%"/>
</p>

1. Extract information from a binary.
2. Run FoxDec to generate a `.report` file.
3. Access/browse the verification results.

<span style="font-size: 150%">1. Extracting information from a binary<a name="usage1"></a></span>

Before FoxDec can be applied to a binary, several files need to be generated. 
We provide scripts `dump_elf.sh` and `dump_macho.sh` that generate these files automatically for respectively files in the ELF (Linux) and MACH-O (MacOS) format.

    ./dump_elf.sh BINARY NAME
    
Here, `BINARY` is the name of the binary and `NAME` is a short working title (without file extensions or paths). For example:

	mkdir du
	cd du
    ./dump_elf.sh /usr/bin/du du

This generates a new directory `du` and generates the following files:

- **`NAME.dump`**: We use `readelf` to get an overview of all segments/sections in the binary. For each relevant section, a hexdump is appended to **`NAME.dump`**.
- **`NAME.section`**: A plain-text file containing an overview of all relevant segments/sections.
- **`NAME.symbols`**: A plain-text file containing an overview of all external function symbols.
- **`NAME.entry`**: A plain-text file containing the entry points of the binary (one if executable, multiple if library).
- **`NAME.objdump`** (optional): For debugging purposes, we find it convenient to have the `objdump` available. This file is not used by FoxDec itself.

<span style="font-size: 150%">2. Running FoxDec</span>

    foxdec-exe PDF_YES_NO DIRNAME NAME
    
Here, `DIRNAME` is the name of the directory that contains the files generated by [step 1](#usage1). `NAME` is the same short working title as used during that step. Parameter `PDF_YES_NO` must be equal to either `1` or `0`, depending on whether FoxDec should use Graphviz to generate PDFs. For large examples, we recommend `0` as `dot` will simply get stuck. Example usage:

    foxdec-exe 1 examples/du du

<span style="font-size: 150%">3. Access/browse verification results</span>

After running FoxDec, a couple of files are generated. Some files provide plain-text information derived from the binary. The `.report` file, specifically, stores the verification results in a non-plain-text binary file that can be accessed with the API provided [here][reportinterface].

The following files are generated:

- **`NAME.report`**: A non-plain-text file containing verification results (see [here][reportinterface]).
- **`NAME.calls`**: A plain-text overview of all function entries in the binary and a Bool indicating whether the function is terminal or not.
- **`NAME_calls.dot`**: A Graphviz `.dot` file containing the call graph, annotated with verification conditions necessary to ensure \"normal\" behavior (e.g., no stack overflows, calling convention adherence).
- **`NAME.indirections`**: A plain-text overview of all resolved indirections.
- **`ENTRY/NAME.dot`**: For each function entry `ENTRY` a control flow graph (CFG).



## Documentation<a name="docs"></a>
Source code documentation can be found [here](https://ssrg-vt.github.io/FoxDec/foxdec/docs/haddock/index.html).




## Papers<a name="papers"></a>
* [_Sound C Code Decompilation for a Subset of x86-64 Binaries_][sefm20-paper],
Freek Verbeek, Pierre Olivier, and Binoy Ravindran, 18th International Conference on Software Engineering and Formal Methods (SEFM 2020), September 14-18, 2020, Amsterdam, The Netherlands.


## Contacts<a name="contact"></a>
* [Binoy Ravindran][binoy], Virginia Tech, <binoy@vt.edu>
* [Freek Verbeek][freek], Virginia Tech, <freek@vt.edu>


---

FoxDec is an open-source project from the Systems Software Research Group
([SSRG][ssrg]) at [Virginia Tech][vt]. It is supported by the Office of Naval
Research ([ONR][onr]) under grant N00014-17-1-2297.




[1]: https://www.ssrg.ece.vt.edu/papers/memocode19.pdf
[2]: https://www.ssrg.ece.vt.edu/papers/cpp2019.pdf
[symbexec]: https://www.ssrg.ece.vt.edu/papers/spisa19.pdf
[3]: https://github.com/ssrg-vt/Chum-src/tree/master/cpp19_artifact
[memocode19]: https://memocode.github.io/2019/
[spisa19]: https://www.cl.cam.ac.uk/~jrh13/spisa19.html
[cpp19]: https://popl19.sigplan.org/track/CPP-2019
[cpp19artifact]: https://filebox.ece.vt.edu/~iroessle/cpp_2019.zip
[ssrg]: https://www.ssrg.ece.vt.edu/
[vt]: https://vt.edu/
[onr]: https://www.onr.navy.mil/
[navsea]: https://www.navsea.navy.mil/
[neec]: https://www.navsea.navy.mil/Home/Warfare-Centers/Partnerships/NEEC/
[freek]: http://www.cs.ru.nl/~freekver/
[binoy]: https://ece.vt.edu/people/profile/ravindran
[strata]: https://dl.acm.org/doi/10.1145/2980983.2908121
[isabelle]: https://isabelle.in.tum.de/
[sefm20]: https://event.cwi.nl/sefm2020/
[chum]: https://ssrg-vt.github.io/Chum/
[decompile]: https://en.wikipedia.org/wiki/Decompiler
[qemu]: https://www.qemu.org/
[sefm20-paper]: https://www.ssrg.ece.vt.edu/papers/sefm20.pdf
[sefm20-artifacts]: https://doi.org/10.5281/zenodo.3952034
[reportinterface]: https://ssrg-vt.github.io/FoxDec/foxdec/docs/haddock/VerificationReportInterface.html
[git]: https://github.com/ssrg-vt/FoxDec
