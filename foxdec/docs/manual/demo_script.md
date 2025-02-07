## DEMO SCRIPT:

## Lifting and modifying `wc_small`

Run FoxDec to lift binary to L0:

    stack exec foxdec-exe -- -c ./config/config.dhall -d examples/wc_small/ -n wc -v -i BINARY --GL0

Run FoxDec to obtain metrics from L0:

###     stack exec foxdec-exe -- -c ./config/config.dhall -d examples/wc_small/ -n wc -v -i L0 --Gmetrics

Run FoxDec to obtain Hoare Graph from L0:

    stack exec foxdec-exe -- -c ./config/config.dhall -d examples/wc_small/ -n wc -v -i L0 --Gjson

Run FoxDec to obtain NASM from L0:

    stack exec foxdec-exe -- -c ./config/config.dhall -d examples/wc_small/ -n wc -v -i L0 --GNASM


In new terminal in `./scripts/`, run:

    ./docker-run.sh ../examples/wc_small/ wc
    
Within Docker, compile NASM and link with gcc:

	cd nasm
	foxdec_nasm
	foxdec_gcc
	
In an editor, open `wc.asm`. In the main function, at the start, insert:

	PUSH RDI
	PUSH RSI
	CALL myPatch wrt ..plt
	POP RSI
	POP RDI
	
and add to the beginning of the file:

	extern myPatch

Recompiling should fail:

	foxdec_nasm
	foxdec_gcc

Compile `myPatch.c`:

	gcc -c -o myPatch.o myPatch.c

Now compile and link with `myPatch.o`:

	gcc -g -m64 -nostartfiles -fgnu-tm -o a.out wc.o __gmon_start__.o myPatch.o
	
As another example of a patch, in `wc.asm`, remove the code under label _init so that it just executes RET, and recompile.


## Lifting and modifying `wget`

First, apply FoxDec as shown above for `wc`. Make sure that in the config file, the option `safe_labels` is set to `True`.

Then, show that compilation fails because of external libraries:

	foxdec_nasm // Takes long, don't do in live demo
	foxdec_gcc  // Fails right away

In Docker, run:

	apt-get install gnutls-bin nettle-dev
	ln -s /usr/lib/x86_64-linux-gnu/libgnutls.so.30 /usr/lib/x86_64-linux-gnu/libgnutls.so
	ln -s /usr/lib/x86_64-linux-gnu/libidn2.so.0.3.6 /usr/lib/x86_64-linux-gnu/libidn2.so
	gcc -g -m64 -nostartfiles -fgnu-tm -o a.out $BINARY.o __gmon_start__.o -lnettle -lidn2 -lgnutls

This will recompile `wget`.

Show the patch in `myPatch.c`.

To compile the patch:

	gcc -c -I./lib -I./src/ myPatch.c
	gcc -g -m64 -nostartfiles -fgnu-tm -o wget_ wget_.o __gmon_start__.o -lnettle -lidn2 -lgnutls myPatch.o
	
To show patch works:

	./wget_ http://neverssl.com/index.html
	
	
	
	
	
	



