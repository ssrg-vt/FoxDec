## How to build FoxDec (for developpers)<a name="build"></a>

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


### With profiling enabled:
    stack build --profile
    time stack exec --profile -- foxdec-exe ./config/config.dhall examples/tar tar +RTS -p
    less foxdec-exe.prof

## M1 ARM64 architectures


Regrettably, the Haskell environment is not yet up-to-date wrt. ARM64 architectures such as the M1 chip in new MacBooks. We cannot use our preferred build-tool `Stack`, but have a setup working using `cabal`. A drawback of this approach is that it may interfere with an existing installation of the Haskell environment. Also, the installation is *way* more involved. Specifically, it requires an edit in a `.h` file of MacOs' XCode Command Line Tools, which is unstable and comes with risk. As soon as `Stack` supports ARM64, we will revert to that approach. If there are any issues, do not hesitate to contact us.

The GitHub page is [here][git].

0. Install the Xcode Command Line tools:

        xcode-select --install

   Confirm the pop-up message. There is no need to install XCode in its entirety.
1. Install [Graphviz](https://graphviz.org) and make sure `dot` is accessible by updating the `PATH` environment variable.
2. Install `ghc` and `cabal` using [ghcup](https://www.haskell.org/ghcup/). The `curl`-command they provide interactively asks you which things to install; there is no need to install `stack` as it it is ARM64 incompatible. We have installed `ghc 8.10.7`.
3. Install `LLVM` (version between 9 and 13, we have installed version 13.0.1) using [homebrew][homebrew]:

        brew install llvm

4. Update the `PATH` variable: `export PATH="/opt/homebrew/opt/llvm/bin:$PATH"`
5. Install [Capstone 4.0.1][capstone] ([git](https://github.com/capstone-engine/capstone/tree/4.0.1)), by downloading it and running ```./make.sh``` and then ```sudo ./make.sh install```.<br> **IMPORTANT:** it must specifically be version 4.0.1, do not install Capstone using <tt>apt-get</tt> or <tt>brew</tt> as that will install a newer version.
6. Open, using **sudo**, the file `/Library/Developer/CommandLineTools/SDKs/MacOSX12.1.sdk/usr/include/mach/arm/_structs.h`
   Insert the following as line 443 
	
	    typedef unsigned __uint128_t __attribute__ ((mode (TI)));

   This will define the `__uint128_t` type: TI is Tetra-integer, which is 4 times the width of `int`.
5. Clone into the git ```git clone git@github.com:ssrg-vt/FoxDec.git```.
6. Go to directory `./foxdec/`.
7. Run

        cabal build
               
   This builds the application.
7. Run

        cabal haddock --haddock-option "--odir=docs/haddock"
       
   This generates documentation.
8. Run

        cabal install
       
   This copies executables to `~/.cabal/bin/`. Be sure that this directory is in your `PATH`.
9. Run ```foxdec-exe 1 examples/du du```. This runs FoxDec on the `du` example.

[capstone]: https://github.com/aquynh/capstone/archive/4.0.1.zip
[git]: https://github.com/ssrg-vt/FoxDec
[homebrew]: https://brew.sh/index_nl
