# News
* Our decompilation-to-C paper has been accepted at [SEFM 2020][sefm20]!

# Overview
The FoxDec (for **Fo**rmal **x**86-64 **Dec**ompilation) project is investigating [decompilation][decompile] of x86-64 binaries into C code that is _sound_ as well as fully _recompilable_. Soundness ensures that the decompiled C code is functionally equivalent to the input binary. Recompilability ensures that the decompiled C code can be successfully compiled to generate an executable binary for x86-64 architectures. 

## Formally verified decompilation

Decompilation to a high-level language involves multiple phases. At a high-level, the phases usually include disassembly that lifts assembly code from binary, control flow graph (CFG) recovery that extracts program CFG from assembly, extraction of high-level program constructs (e.g., statements, variables, references) from assembly, and type assignment. FoxDec is investigating techniques for the decompilation phases that are formally verified. 

FoxDec's decompilation phases include disassembly; CFG recovery; extraction of an abstract code that models a program as a CFG of basic blocks; converting basic blocks into sequential code that models the program's corresponding state changes over memory, registers, and flags; variable analysis that maps memory regions to variables and references; and type analysis that assigns types. Converting basic blocks into sequential code of program state changes requires a formal model of the underlying machine (i.e., formal semantics of x86-64 instructions). The project leverages the [Chum project][chum]'s formal x86-64 machine model for this purpose. 

Central to formally verified decompilation is the notion of _sound decompilation_. FoxDec defines soundness for each of these decompilation phases (sound disassembly is explored in a different project) and formally verifies them: algorithms for each phase are formalized in the [Isabelle/HOL theorem prover][isabelle] and proven correct. 

## Use cases

Sound, recompilable decompilation to C has a variety of use cases. For example, patching a binary to fix errors or potential security exploits is highly complex in settings where source code or third-party libraries are no longer available or build processes or tools have become outdated. Patching at the C-level is relatively easier and compelling when the decompiled C code is formally proven to be functionally equivalent to the binary. 

Other use cases include binary analysis, binary porting (as an alternative to [software emulation][qemu]), and binary optimization, each of which can now be performed at the C level (e.g., using off-the-shelf C code analysis and optimization tools). 


# Papers
* _Sound C Code Decompilation for a Subset of x86-64 Binaries_,
Freek Verbeek, Pierre Olivier, and Binoy Ravindran, 18th International Conference on Software Engineering and Formal Methods (SEFM 2020), September 14-18, 2020, Amsterdam, The Netherlands.

# Code and proofs
* Code and proofs of our SEFM 2020 paper will be available soon. 

# Contacts
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

