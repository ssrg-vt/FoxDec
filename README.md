The FoxDec (for **Fo**rmal **x**86-64 **Dec**ompilation) project is investigating [decompilation][decompile] of x86-64 binaries into C code that is _sound_ as well as fully _recompilable_. Soundness ensures that the decompiled C code is functionally equivalent to the input binary. Recompilability ensures that the decompiled C code can be successfully compiled to generate an executable binary. 

FoxDec is currently actively being developed. In its current stage, it does disassembly, control flow reconstruction and function boundary detection.
Work-in-progress is variable analysis, decompilation to C, data flow analysis, and much more.


<span style="font-size: 150%; color:darkblue">NEWS</span>

* Our [paper][icse25-paper] on binary-level pointer analysis has been accepted at [ICSE'25][icse25]!
* Our [paper][ccs24-paper] on verified lifting and symbolization has been accepted at [CCS'24][ccs24]!
* Our [paper][pldi22-paper] on verified lifting and verifying memory safety properties has been accepted at [PLDI'22][pldi22]!
* Our [decompilation-to-C paper][sefm20-paper] has received the Best Paper award at [SEFM 2020][sefm20]!

## Table of Contents
1. [Introduction](#intro)
2. [How to build](#build)
3. [How to use](#usage)
3. [How to recompile](#recompile)
5. [Papers](#papers)
6. [Contact](#contact)

## Introduction <a name="intro"></a>
**Formally verified decompilation.**
Decompilation to a high-level language involves multiple phases. At a high-level, the phases usually include disassembly that lifts assembly code from binary, control flow graph (CFG) recovery that extracts program CFG from assembly, extraction of high-level program constructs (e.g., statements, variables, references) from assembly, and type assignment. FoxDec is investigating techniques for the formally verified decompilation phases. 

FoxDec's decompilation phases include disassembly; CFG recovery; extraction of an abstract code that models a program as a CFG of basic blocks; converting basic blocks into sequential code that models the program's corresponding state changes over memory, registers, and flags; variable analysis that maps memory regions to variables and references; and type analysis that assigns types. Converting basic blocks into sequential code that captures program state changes requires a formal model of the underlying machine (i.e., formal semantics of x86-64 instructions). 

Central to formally verified decompilation is the notion of _sound decompilation_. FoxDec defines soundness for each of these decompilation phases (sound disassembly is explored in a different project) and formally verifies them: algorithms for each phase are formalized in the [Isabelle/HOL theorem prover][isabelle] and proven correct. 

**Use cases.**
Sound, recompilable decompilation to C has a variety of use cases. For example, patching a binary to fix errors or potential security exploits is highly complex in settings where source code or third-party libraries are no longer available or build processes or tools have become outdated. Patching at the C-level is relatively easier and a compelling alternative when the decompiled C code is formally proven to be functionally equivalent to the binary. 

Other use cases include binary analysis, binary porting (as an alternative to [software emulation][qemu]), and binary optimization, each of which can now be performed at the C level (e.g., using off-the-shelf C code analysis and optimization tools). 





## How to build <a name="build"></a>
Download FoxDec [here](https://github.com/ssrg-vt/FoxDec/blob/0abd8c85cda0ccf3da9ef6683acb77a99c04b8b6/foxdec/release/FoxDec.zip?raw=true). This will use [Docker](https://www.docker.com) to build and run FoxDec. Section [How to use](#usage) contains further instructions. The GitHub page is [here][git]. 

***NOTE:*** instructions for building without Docker can be found [here](foxdec/docs/build.md) *(only relevant for developpers)*.




## How to use <a name="usage"></a>
These are instructions for a quickstart on the already supplied binary `wc`.

1. Move the binary of interest to `./binary/`. The binary `wc` has already been supplied as running example.
2. Run FoxDec on the binary `./foxdec.sh wc`
3. Directory `./artifacts/` is now populated with information.

The following files are generated:

- **`$NAME.json`** and **`$NAME.json.txt`**: Contain disassembled instructions, control flow recovery, function boundaries, invariants, pointer analysis results. The two files contain the exact same information, one in JSON format and the other in humanly readable format. 
- **`$NAME.metrics.txt`**: a log containing metrics such as running time, number of covered instructions, accuracy of pointer analysis, etc. The two files contain the exact same information, one in JSON format and the other in humanly readable format.
- **`$NAME.callgraph.dot`**: A Graphviz `.dot` file containing the ACG (Annotated Call Graph), annotated with verification conditions necessary to ensure \"normal\" behavior (e.g., no stack overflows, calling convention adherence).
- **`nasm/$NAME.asm`**: recompilable NASM code.
- **`functions/$ENTRY/$NAME.dot`**: For each function entry **`$ENTRY`** a control flow graph (CFG) and a log containing a pre- and a postcondition.



**NOTE:** it is possible to manually add entry points, by adding a plain-text file `./binary/$NAME.entry` where each line is an entry address in hexadecimal notation.


<!---
## Documentation<a name="docs"></a>
For instructions on how to apply to MachO binaries and how to generate Isabelle/HOL code, we ask that you contact us. 
The user manual for FoxDec version 0.4 can be found [here](./foxdec/docs/manual/foxdec_manual.pdf).
Source code documentation can be found [here](https://ssrg-vt.github.io/FoxDec/foxdec/docs/haddock/index.html).
-->

## How to recompile <a name="recompile"></a>

The following instructions recompile the generated NASM code back into a binary.

    nasm -felf64 -g -F dwarf -o $NAME.o $NAME.asm
    gcc -c __gmon_start__.c -o __gmon_start__.o
    gcc -g -m64 -nostartfiles -fgnu-tm -o a.out $NAME.o __gmon_start__.o

For the last compilation step, one must manually supply additional libraries as needed.


## Papers<a name="papers"></a>
* [Verifiably Correct Lifting of Position-Independent x86-64 Binaries to Symbolized Assembly][ccs24-paper], Freek Verbeek, Nico Naus, Binoy Ravindran. Proceedings of the 2024 ACM SIGSAC Conference on Computer and Communications Security (CCS ’24).
* [Formally Verified Lifting of C-compiled x86-64 Binaries][pldi22-paper],
Freek Verbeek, Joshua A. Bockenek, Zhoulai Fu and Binoy Ravindran, 43rd ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI 2022), June 13-17, 2022, San Diego, USA.
* [_Sound C Code Decompilation for a Subset of x86-64 Binaries_][sefm20-paper],
Freek Verbeek, Pierre Olivier, and Binoy Ravindran, 18th International Conference on Software Engineering and Formal Methods (SEFM 2020), September 14-18, 2020, Amsterdam, The Netherlands.


## Contacts<a name="contact"></a>
* [Binoy Ravindran][binoy], Virginia Tech, <binoy@vt.edu>
* [Freek Verbeek][freek], Virginia Tech, <freek@vt.edu>


---


FoxDec is an open-source project from the Systems Software Research Group ([SSRG][ssrg]) at [Virginia Tech][vt].
It is supported by the Defense Advanced Research Projects Agency (DARPA) and Naval Information Warfare Center Pacific (NIWC Pacific) under Contract No. N66001-21-C-4028, DARPA under Agreement No. HR.00112090028, DARPA and the Army Contracting Command Aberdeen Proving Grounds (ACC-APG) under Prime Contract No. W912CG23C0024, DARPA under Prime Contract No. HR001124C0492, by the US Office of Naval Research (ONR) under grant N00014-17-1-2297, and by the US Naval Surface Warfare Center Dahlgren Division/Naval Engineering Education Consortium (NEEC) under grants N00174-20-1-0009 and N00174-16-C-0018.



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
[ccs24-paper]: https://dl.acm.org/doi/abs/10.1145/3658644.3690244
[pldi22]: https://pldi22.sigplan.org/
[ccs24]: https://www.sigsac.org/ccs/CCS2024/
[icse25]: https://conf.researchr.org/home/icse-2025
[icse25-paper]: https://www.computer.org/csdl/proceedings-article/icse/2025/056900a767/251mHBq8DZu



