The FoxDec (for **Fo**rmal **x**86-64 **Dec**ompilation) project is investigating [decompilation][decompile] of x86-64 binaries into C code that is _sound_ as well as fully _recompilable_. Soundness ensures that the decompiled C code is functionally equivalent to the input binary. Recompilability ensures that the decompiled C code can be successfully compiled to generate an executable binary. 

FoxDec is currently actively being developed. In its current stage, it does disassembly, control flow reconstruction and function boundary detection.
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

***NOTE:*** the instructions for Apple Macbooks with the ***ARM M1 chip*** can be found [here](foxdec/docs/ARM64.md).

1. Install [Graphviz](https://graphviz.org) and make sure `dot` is accessible by updating the `PATH` environment variable.
2. Install [Stack](https://docs.haskellstack.org/en/stable/README/), the build tool used for developping FoxDec. 
3. Update the `PATH` variable: `PATH=$PATH:$HOME/.local/bin`
4. Install [Capstone 4.0.1][capstone] ([git](https://github.com/capstone-engine/capstone/tree/4.0.1)), by downloading it and running ```./make.sh``` and then ```sudo ./make.sh install```.<br> **IMPORTANT:** it must specifically be version 4.0.1, do not install Capstone using <tt>apt-get</tt> or <tt>brew</tt> as that will install a newer version.
5. Clone into the git ```git clone git@github.com:ssrg-vt/FoxDec.git```.
6. Go to directory `./foxdec/`.
7. Run ```stack build --haddock --haddock-arguments --odir=docs/haddock```.
This builds the application and generates documentation using [Haddock](https://haskell-haddock.readthedocs.io/en/latest/index.html).
8. Run ```stack install```. This copies executables to accessible locations.
9. Run ```foxdec-exe ./config/config.dhall examples/du du```. This runs FoxDec on the `du` example.

We use some tools that are assumed to be standard available and accessible (i.e., added to the `PATH` environment variable). For Linux, these are `readelf` and `objdump` (latter is optional but convenient).
For Mac, these are `otool` and `nm`.

***Building (and running) with profiling***
*(only relevant for developpers):*

    stack build --profile
    time stack exec --profile -- foxdec-exe ./config/config.dhall examples/tar tar +RTS -p
    less foxdec-exe-.prof

## How to use <a name="usage"></a>
These are instructions for a quickstart on ELF files. For more information, we refer to the [user manual](./foxdec/docs/manual/foxdec_manual.pdf).

<p style="text-align: center;">
  <img style="border: 3px solid #555;" src="./foxdec/docs/overview/overview.png" alt="FoxDec Overview" width="90%"/>
</p>

1. Run FoxDec on the binary to generate a `.report` file.
2. Use the `.report` file to generate JSON output, or to export to Isabelle/HOL.

<span style="font-size: 150%">1. Run FoxDec on the binary<a name="usage1"></a></span>

For ELF files, the binary can be read by FoxDec directly. Copy-paste the executable to a new directory in the `examples` subdir.

	mkdir examples/du
	cp /usr/bin/du examples/du/

Then, run FoxDec as follows:

    foxdec-exe CONFIG DIRNAME NAME
    
Here, `CONFIG` is the name of a config file (default: `./config/config.dhall`). `DIRNAME` is the name of the directory that contains the binary. `NAME` is the name of the binary.  Example usage:

    foxdec-exe ./config/config.dhall examples/du/ du



The following files are generated:

- **`NAME.report`**: This file stores the verification results in a non-plain-text binary file that can be exported to JSON.
- **`NAME_calls.dot`**: A Graphviz `.dot` file containing the call graph, annotated with verification conditions necessary to ensure \"normal\" behavior (e.g., no stack overflows, calling convention adherence).
- **`ENTRY/NAME.dot`**: For each function entry `ENTRY` a control flow graph (CFG).


<span style="font-size: 150%">2. Use the `.report` file to generate JSON output<a name="usage1"></a></span>

From the same directory as where `foxdec-exe` was ran, run:

    foxdec-json-exe DIRNAME NAME {True,False}

Here `DIRNAME` and `NAME` refer to the same parameters as [step 1](#usage1).

    foxdec-json-exe examples/du/ du True > examples/du/du.output
    foxdec-json-exe examples/du/ du False > examples/du/du.json

The first reads in the `.report` file and generates output in a humandly readable plain-text file. It contains disassembled instructions, control flow, function boundaries, symbolic function summaries (pre- and postconditions), invariants, and all pointer domains of all memory writes.

The second contains the exact same information in JSON format.  The exact JSON taxonomy used to generate the JSON can be found [here][taxonomy].	


## Documentation<a name="docs"></a>
For instructions on how to apply to MachO binaries and how to generate Isabelle/HOL code, we ask that you contact us. 
The user manual for FoxDec version 0.1 can be found [here](./foxdec/docs/manual/foxdec_manual.pdf).
Source code documentation can be found [here](https://ssrg-vt.github.io/FoxDec/foxdec/docs/haddock/index.html).




## Papers<a name="papers"></a>
* [Formally Verified Lifting of C-compiled x86-64 Binaries][pldi22-paper],
Freek Verbeek, Joshua A. Bockenek, Zhoulai Fu and Binoy Ravindran, 43rd ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI 2022), June 13-17, 2022, San Diego, USA.
* [_Sound C Code Decompilation for a Subset of x86-64 Binaries_][sefm20-paper],
Freek Verbeek, Pierre Olivier, and Binoy Ravindran, 18th International Conference on Software Engineering and Formal Methods (SEFM 2020), September 14-18, 2020, Amsterdam, The Netherlands.


## Contacts<a name="contact"></a>
* [Binoy Ravindran][binoy], Virginia Tech, <binoy@vt.edu>
* [Freek Verbeek][freek], Virginia Tech, <freek@vt.edu>


---

FoxDec is an open-source project from the Systems Software Research Group
([SSRG][ssrg]) at [Virginia Tech][vt]. It is supported by the Defense Advanced Research Projects Agency (DARPA) under contract N6600121C4028, and by the Office of Naval
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
[capstone]: https://github.com/aquynh/capstone/archive/4.0.1.zip
[taxonomy]: https://ssrg-vt.github.io/FoxDec/foxdec/docs/haddock/Data-JSON_Taxonomy.html
[pldi22-paper]: https://dl.acm.org/doi/10.1145/3519939.3523702
