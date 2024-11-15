#!/bin/bash

# Define the location of the PIN tool and the output directory for logs
PIN_TOOL="pin -t /binary/obj-intel64/memory_write_logger.so"
EXAMPLE_DIR="/binary/FoxDec/foxdec/examples"

# Run the PIN tool on each binary
# $PIN_TOOL -o $EXAMPLE_DIR/du/du.pin_log.txt -- $EXAMPLE_DIR/du/du $EXAMPLE_DIR/du
# $PIN_TOOL -o $EXAMPLE_DIR/gzip/gzip.pin_log.txt -- $EXAMPLE_DIR/gzip/gzip -k $EXAMPLE_DIR/gzip/sample.txt
# $PIN_TOOL -o $EXAMPLE_DIR/host/host.pin_log.txt -- $EXAMPLE_DIR/host/host google.com
# $PIN_TOOL -o $EXAMPLE_DIR/sha512sum/sha512sum.pin_log.txt -- $EXAMPLE_DIR/sha512sum/sha512sum $EXAMPLE_DIR/sha512sum/sample.txt
# $PIN_TOOL -o $EXAMPLE_DIR/sort/sort.pin_log.txt -- $EXAMPLE_DIR/sort/sort $EXAMPLE_DIR/sort/sample.txt
# $PIN_TOOL -o $EXAMPLE_DIR/sqlite3/sqlite3.pin_log.txt -- $EXAMPLE_DIR/sqlite3/sqlite3 $EXAMPLE_DIR/sqlite3/sample.db < $EXAMPLE_DIR/sqlite3/commands.sql
$PIN_TOOL -o $EXAMPLE_DIR/ssh/ssh.pin_log.txt -- $EXAMPLE_DIR/ssh/ssh -T git@github.com
# $PIN_TOOL -o $EXAMPLE_DIR/tar/tar.pin_log.txt -- $EXAMPLE_DIR/tar/tar -cvf $EXAMPLE_DIR/tar/archive.tar $EXAMPLE_DIR/tar
# $PIN_TOOL -o $EXAMPLE_DIR/vi/vim.pin_log.txt -- $EXAMPLE_DIR/vi/vim +':$ | normal oNew line of text' +':wq' $EXAMPLE_DIR/vi/sample.txt
# $PIN_TOOL -o $EXAMPLE_DIR/wc/wc.pin_log.txt -- $EXAMPLE_DIR/wc/wc $EXAMPLE_DIR/wc/sample.txt
# $PIN_TOOL -o $EXAMPLE_DIR/wc_small/wc_small.pin_log.txt -- $EXAMPLE_DIR/wc_small/wc $EXAMPLE_DIR/wc_small/sample.txt
# $PIN_TOOL -o $EXAMPLE_DIR/wget2/wget2.pin_log.txt -- $EXAMPLE_DIR/wget2/wget2 https://example.com/file.zip
# $PIN_TOOL -o $EXAMPLE_DIR/zip/zip.pin_log.txt -- $EXAMPLE_DIR/zip/zip $EXAMPLE_DIR/zip/archive.zip $EXAMPLE_DIR/zip/sample.txt
# $PIN_TOOL -o $EXAMPLE_DIR/xxd/xxd.pin_log.txt -- $EXAMPLE_DIR/xxd/xxd $EXAMPLE_DIR/xxd/xxd > $EXAMPLE_DIR/xxd/xxd_dump.txt

#SPEC benchmarks
# $PIN_TOOL -o $EXAMPLE_DIR/spec/401.bzip2/bzip2.pin_log.txt -- $EXAMPLE_DIR/spec/401.bzip2/bzip2 $EXAMPLE_DIR/spec/401.bzip2/sample.txt


echo "All tests have been run and logs are saved in corresponding directories inside $EXAMPLE_DIR."