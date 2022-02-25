extern _BZ2_bzDecompress
extern _BZ2_bzDecompressEnd
extern _BZ2_bzDecompressInit
extern ___error
extern ___memcpy_chk
extern ___memmove_chk
extern ___stack_chk_fail
extern ___strlcpy_chk
extern __exit
extern _basename
extern _calloc
extern _close
extern _compat_mode
extern _crc32
extern _ctime
extern _deflate
extern _deflateEnd
extern _deflateInit2_
extern _dup
extern _errx
extern _exit
extern _fchflags
extern _fchmod
extern _fchown
extern _fclose
extern _fcopyfile
extern _fdopen
extern _ferror
extern _fflush
extern _fgetattrlist
extern _fgetc
extern _fgets
extern _fprintf
extern _fputc
extern _fread
extern _free
extern _fsetattrlist
extern _fstat$INODE64
extern _fts_close$INODE64
extern _fts_open$INODE64
extern _fts_read$INODE64
extern _funopen
extern _futimes
extern _fwrite
extern _getenv
extern _getopt_long
extern _getprogname
extern _gettimeofday
extern _inflate
extern _inflateEnd
extern _inflateInit2_
extern _isatty
extern _lseek
extern _lstat$INODE64
extern _lzma_code
extern _lzma_end
extern _lzma_stream_decoder
extern _malloc
extern _memcpy
extern _open
extern _pread
extern _printf
extern _puts
extern _read
extern _signal
extern _snprintf
extern _stat$INODE64
extern _strcmp
extern _strcpy
extern _strerror
extern _strlen
extern _strndup
extern _strrchr
extern _time
extern _umask
extern _unlink
extern _vwarn
extern _vwarnx
extern _warn
extern _write
extern ___stack_chk_guard
extern ___stderrp
extern ___stdinp
extern ___stdoutp
extern _optarg
extern _optind
extern dyld_stub_binder


section .text

default rel

; ---------------------
; Function: 0x10000221c
; ---------------------
; Entry 10000221c; block 0; address 10000221c
L10000221c_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R14
    PUSH RBX
    SUB RSP, 208
    MOV R14D, EDI
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -24], RAX
    TEST RSI, RSI
    JE L10000221c_2
    JMP L10000221c_1     ; inserted

; Entry 10000221c; block 1; address 100002240
L10000221c_1:
    MOV RBX, RSI
    MOV RCX, qword [RSI + 8]
    CMP RCX, 18446744073709551614
    JE L10000221c_4
    JMP L10000221c_3     ; inserted

; Entry 10000221c; block 2; address 100002261
L10000221c_2:
    XOR ESI, ESI
    JMP L10000221c_9     ; inserted

; Entry 10000221c; block 3; address 10000224d
L10000221c_3:
    CMP RCX, 18446744073709551615
    JNE L10000221c_6
    JMP L10000221c_5     ; inserted

; Entry 10000221c; block 4; address 100002270
L10000221c_4:
    MOV RDX, qword [RBX + 24]
    MOV SIL, 1
    XOR EAX, EAX
    CMP RDX, 18446744073709551614
    JNE L10000221c_8
    JMP L10000221c_11     ; inserted

; Entry 10000221c; block 5; address 100002253
L10000221c_5:
    MOV RDX, qword [RBX + 24]
    XOR ESI, ESI
    CMP RDX, 18446744073709551615
    JNE L10000221c_8
    JMP L10000221c_7     ; inserted

; Entry 10000221c; block 6; address 100002284
L10000221c_6:
    CMP RCX, 999999999
    JA L10000221c_14
    JMP L10000221c_13     ; inserted

; Entry 10000221c; block 7; address 10000225f
L10000221c_7:
    JMP L10000221c_9

; Entry 10000221c; block 8; address 100002297
L10000221c_8:
    LEA RAX, [RDX + -1000000000]
    CMP RAX, 18446744072709551613
    JBE L10000221c_14
    JMP L10000221c_15     ; inserted

; Entry 10000221c; block 9; address 100002263
L10000221c_9:
    MOV EDI, R14D
    CALL _futimes
    JMP L10000221c_10     ; inserted

; Entry 10000221c; block 10; address 10000226b
L10000221c_10:
    JMP L10000221c_12

; Entry 10000221c; block 11; address 10000227f
L10000221c_11:
    JMP L10000221c_12

; Entry 10000221c; block 12; address 100002398
L10000221c_12:
    MOV RCX, qword [rel ___stack_chk_guard]
    MOV RCX, qword [RCX]
    CMP RCX, qword [RBP + -24]
    JNE L10000221c_28
    JMP L10000221c_27     ; inserted

; Entry 10000221c; block 13; address 100002291
L10000221c_13:
    MOV RDX, qword [RBX + 24]
    XOR ESI, ESI
    JMP L10000221c_8     ; inserted

; Entry 10000221c; block 14; address 100002388
L10000221c_14:
    CALL ___error
    JMP L10000221c_26     ; inserted

; Entry 10000221c; block 15; address 1000022aa
L10000221c_15:
    MOV RAX, qword [RBX]
    MOV qword [RBP + -64], RAX
    MOVSXD RAX, ECX
    IMUL RAX, RAX, 274877907
    MOV RDI, RAX
    SHR RDI, 63
    SAR RAX, 38
    ADD EAX, EDI
    MOV dword [RBP + -56], EAX
    MOV RAX, qword [RBX + 16]
    MOV qword [RBP + -48], RAX
    MOVSXD RAX, EDX
    IMUL RAX, RAX, 274877907
    MOV RDI, RAX
    SHR RDI, 63
    SAR RAX, 38
    ADD EAX, EDI
    MOV dword [RBP + -40], EAX
    CMP RDX, 18446744073709551614
    SETNE AL
    NOT SIL
    TEST SIL, AL
    JNE L10000221c_17
    JMP L10000221c_16     ; inserted

; Entry 10000221c; block 16; address 100002300
L10000221c_16:
    LEA RSI, [RBP + -224]
    MOV EDI, R14D
    CALL _fstat$INODE64
    JMP L10000221c_18     ; inserted

; Entry 10000221c; block 17; address 1000023b4
L10000221c_17:
    CMP RCX, 18446744073709551615
    JE L10000221c_30
    JMP L10000221c_29     ; inserted

; Entry 10000221c; block 18; address 10000230f
L10000221c_18:
    CMP EAX, 18446744073709551615
    JE L10000221c_20
    JMP L10000221c_19     ; inserted

; Entry 10000221c; block 19; address 100002314
L10000221c_19:
    MOV RCX, qword [RBX + 8]
    CMP RCX, 18446744073709551614
    JNE L10000221c_22
    JMP L10000221c_21     ; inserted

; Entry 10000221c; block 20; address 100002393
L10000221c_20:
    MOV EAX, 4294967295
    JMP L10000221c_12     ; inserted

; Entry 10000221c; block 21; address 10000231e
L10000221c_21:
    MOV RAX, qword [RBP + -192]
    MOV qword [RBP + -64], RAX
    MOV RAX, 2361183241434822607
    IMUL qword [RBP + -184]
    MOV RAX, RDX
    SHR RAX, 63
    SHR RDX, 7
    ADD EDX, EAX
    MOV dword [RBP + -56], EDX
    JMP L10000221c_22     ; inserted

; Entry 10000221c; block 22; address 10000234a
L10000221c_22:
    MOV RDX, qword [RBX + 24]
    CMP RDX, 18446744073709551614
    JNE L10000221c_17
    JMP L10000221c_23     ; inserted

; Entry 10000221c; block 23; address 100002354
L10000221c_23:
    MOV RAX, qword [RBP + -176]
    MOV qword [RBP + -48], RAX
    MOV RAX, 2361183241434822607
    IMUL qword [RBP + -168]
    MOV RAX, RDX
    SHR RAX, 63
    SHR RDX, 7
    ADD EDX, EAX
    MOV dword [RBP + -40], EDX
    CMP RCX, 18446744073709551615
    JNE L10000221c_25
    JMP L10000221c_24     ; inserted

; Entry 10000221c; block 24; address 100002386
L10000221c_24:
    JMP L10000221c_30

; Entry 10000221c; block 25; address 1000023c0
L10000221c_25:
    LEA RSI, [RBP + -64]
    JMP L10000221c_9

; Entry 10000221c; block 26; address 10000238d
L10000221c_26:
    MOV dword [RAX], 22
    JMP L10000221c_20     ; inserted

; Entry 10000221c; block 27; address 1000023a8
L10000221c_27:
    ADD RSP, 208
    POP RBX
    POP R14
    POP RBP
    RET 

; Entry 10000221c; block 28; address 100002407
L10000221c_28:
    CALL ___stack_chk_fail

; Entry 10000221c; block 29; address 1000023ba
L10000221c_29:
    CMP RDX, 18446744073709551615
    JE L10000221c_30
    JMP L10000221c_25     ; inserted

; Entry 10000221c; block 30; address 1000023c9
L10000221c_30:
    LEA RDI, [RBP + -80]
    XOR ESI, ESI
    CALL _gettimeofday
    JMP L10000221c_31     ; inserted

; Entry 10000221c; block 31; address 1000023d4
L10000221c_31:
    CMP EAX, 18446744073709551615
    JE L10000221c_20
    JMP L10000221c_32     ; inserted

; Entry 10000221c; block 32; address 1000023d9
L10000221c_32:
    CMP qword [RBX + 8], 18446744073709551615
    JNE L10000221c_34
    JMP L10000221c_33     ; inserted

; Entry 10000221c; block 33; address 1000023e0
L10000221c_33:
    MOVUPS XMM0, oword [RBP + -80]
    MOVAPS oword [RBP + -64], XMM0
    JMP L10000221c_34     ; inserted

; Entry 10000221c; block 34; address 1000023e8
L10000221c_34:
    LEA RSI, [RBP + -64]
    CMP qword [RBX + 24], 18446744073709551615
    JNE L10000221c_9
    JMP L10000221c_35     ; inserted

; Entry 10000221c; block 35; address 1000023f7
L10000221c_35:
    LEA RAX, [RBP + -48]
    MOVUPS XMM0, oword [RBP + -80]
    MOVUPS oword [RAX], XMM0
    JMP L10000221c_9



; ---------------------
; Function: 0x10000240c
; ---------------------
; Entry 10000240c; block 0; address 10000240c
L10000240c_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 184
    MOV R12, RSI
    MOV R15D, EDI
    CALL _getprogname
    JMP L10000240c_1     ; inserted

; Entry 10000240c; block 1; address 10000242b
L10000240c_1:
    MOV qword [RBP + -56], RAX
    LEA RDI, [rel L__TEXT___cstring_0]
    CALL _getenv
    JMP L10000240c_2     ; inserted

; Entry 10000240c; block 2; address 10000243b
L10000240c_2:
    TEST RAX, RAX
    JE L10000240c_4
    JMP L10000240c_3     ; inserted

; Entry 10000240c; block 3; address 100002444
L10000240c_3:
    MOV RBX, RAX
    XOR EDX, EDX
    JMP L10000240c_8     ; inserted

; Entry 10000240c; block 4; address 100002552
L10000240c_4:
    LEA RSI, [rel L__TEXT___text + 2336]
    MOV EDI, 2
    CALL _signal
    JMP L10000240c_36     ; inserted

; Entry 10000240c; block 5; address 100002450
L10000240c_5:
    CMP CL, 32
    JNE L10000240c_7
    JMP L10000240c_6     ; inserted

; Entry 10000240c; block 6; address 100002455
L10000240c_6:
    INC RAX
    JMP L10000240c_8

; Entry 10000240c; block 7; address 10000245a
L10000240c_7:
    TEST CL, CL
    JE L10000240c_10
    JMP L10000240c_9     ; inserted

; Entry 10000240c; block 8; address 100002449
L10000240c_8:
    MOV CL, byte [RAX]
    CMP CL, 9
    JE L10000240c_6
    JMP L10000240c_5     ; inserted

; Entry 10000240c; block 9; address 10000245e
L10000240c_9:
    INC EDX
    JMP L10000240c_15     ; inserted

; Entry 10000240c; block 10; address 100002476
L10000240c_10:
    TEST EDX, EDX
    JE L10000240c_4
    JMP L10000240c_14     ; inserted

; Entry 10000240c; block 11; address 100002465
L10000240c_11:
    CMP CL, 32
    JE L10000240c_8
    JMP L10000240c_12     ; inserted

; Entry 10000240c; block 12; address 10000246a
L10000240c_12:
    TEST CL, CL
    JE L10000240c_14
    JMP L10000240c_13     ; inserted

; Entry 10000240c; block 13; address 10000246e
L10000240c_13:
    MOV CL, byte [RAX + 1]
    INC RAX
    JMP L10000240c_15

; Entry 10000240c; block 14; address 10000247e
L10000240c_14:
    ADD EDX, R15D
    MOV dword [RBP + -48], EDX
    MOVSXD RAX, EDX
    LEA RDI, [RAX * 8 * 8]
    CALL _malloc
    JMP L10000240c_16     ; inserted

; Entry 10000240c; block 15; address 100002460
L10000240c_15:
    CMP CL, 9
    JE L10000240c_8
    JMP L10000240c_11     ; inserted

; Entry 10000240c; block 16; address 100002494
L10000240c_16:
    TEST RAX, RAX
    JE L10000240c_18
    JMP L10000240c_17     ; inserted

; Entry 10000240c; block 17; address 10000249d
L10000240c_17:
    MOV R13, RAX
    MOV RAX, qword [R12]
    MOV qword [R13], RAX
    MOV R15D, 1
    JMP L10000240c_31     ; inserted

; Entry 10000240c; block 18; address 100002a78
L10000240c_18:
    LEA RDI, [rel L__TEXT___cstring_332]
    JMP L10000240c_152     ; inserted

; Entry 10000240c; block 19; address 1000024c5
L10000240c_19:
    CMP CL, 32
    JNE L10000240c_21
    JMP L10000240c_20     ; inserted

; Entry 10000240c; block 20; address 1000024ca
L10000240c_20:
    INC RDI
    INC RAX
    INC RBX
    JMP L10000240c_22

; Entry 10000240c; block 21; address 1000024d5
L10000240c_21:
    TEST CL, CL
    JE L10000240c_24
    JMP L10000240c_23     ; inserted

; Entry 10000240c; block 22; address 1000024be
L10000240c_22:
    MOV CL, byte [RDI]
    CMP CL, 9
    JE L10000240c_20
    JMP L10000240c_19     ; inserted

; Entry 10000240c; block 23; address 1000024d9
L10000240c_23:
    LEA R15, [R14 + 1]
    MOV qword [R13 + R14 * 8], RDI
    XOR ESI, ESI
    JMP L10000240c_28     ; inserted

; Entry 10000240c; block 24; address 10000251d
L10000240c_24:
    MOV RAX, qword [R12 + 8]
    TEST RAX, RAX
    JE L10000240c_34
    JMP L10000240c_33     ; inserted

; Entry 10000240c; block 25; address 1000024eb
L10000240c_25:
    CMP AL, 32
    JE L10000240c_26
    JMP L10000240c_27     ; inserted

; Entry 10000240c; block 26; address 1000024fb
L10000240c_26:
    CALL _strndup
    JMP L10000240c_30     ; inserted

; Entry 10000240c; block 27; address 1000024ef
L10000240c_27:
    INC RBX
    INC RSI
    TEST AL, AL
    JNE L10000240c_28
    JMP L10000240c_29     ; inserted

; Entry 10000240c; block 28; address 1000024e4
L10000240c_28:
    MOV AL, byte [RBX + -1]
    CMP AL, 9
    JE L10000240c_26
    JMP L10000240c_25     ; inserted

; Entry 10000240c; block 29; address 1000024f9
L10000240c_29:
    MOV R14, R15
    MOV R15D, dword [RBP + -48]
    JMP L10000240c_24     ; inserted

; Entry 10000240c; block 30; address 100002500
L10000240c_30:
    MOV qword [R13 + R14 * 8], RAX
    TEST RAX, RAX
    JNE L10000240c_31
    JMP L10000240c_32     ; inserted

; Entry 10000240c; block 31; address 1000024ae
L10000240c_31:
    MOV RAX, RBX
    MOV R14, R15
    INC RBX
    MOV RDI, RAX
    MOV R15D, dword [RBP + -48]
    JMP L10000240c_22     ; inserted

; Entry 10000240c; block 32; address 10000250a
L10000240c_32:
    LEA RDI, [rel L__TEXT___cstring_339]
    JMP L10000240c_152

; Entry 10000240c; block 33; address 100002527
L10000240c_33:
    MOVSXD R14, R14D
    ADD R12, 16
    JMP L10000240c_35     ; inserted

; Entry 10000240c; block 34; address 100002543
L10000240c_34:
    MOVSXD RAX, R14D
    MOV qword [R13 + RAX * 8], 0
    MOV R12, R13
    JMP L10000240c_4     ; inserted

; Entry 10000240c; block 35; address 10000252e
L10000240c_35:
    MOV qword [R13 + R14 * 8], RAX
    INC R14
    MOV RAX, qword [R12]
    ADD R12, 8
    TEST RAX, RAX
    JNE L10000240c_35
    JMP L10000240c_34     ; inserted

; Entry 10000240c; block 36; address 100002563
L10000240c_36:
    LEA RSI, [rel L__TEXT___cstring_5]
    MOV RDI, qword [RBP + -56]
    CALL _strcmp
    JMP L10000240c_37     ; inserted

; Entry 10000240c; block 37; address 100002573
L10000240c_37:
    TEST EAX, EAX
    JE L10000240c_39
    JMP L10000240c_38     ; inserted

; Entry 10000240c; block 38; address 100002577
L10000240c_38:
    LEA RSI, [rel L__TEXT___cstring_12]
    MOV RDI, qword [RBP + -56]
    CALL _strcmp
    JMP L10000240c_40     ; inserted

; Entry 10000240c; block 39; address 1000025a6
L10000240c_39:
    MOV byte [rel L__DATA___bss + 0], 1
    JMP L10000240c_44     ; inserted

; Entry 10000240c; block 40; address 100002587
L10000240c_40:
    TEST EAX, EAX
    JE L10000240c_42
    JMP L10000240c_41     ; inserted

; Entry 10000240c; block 41; address 10000258b
L10000240c_41:
    LEA RSI, [rel L__TEXT___cstring_17]
    MOV RDI, qword [RBP + -56]
    CALL _strcmp
    JMP L10000240c_43     ; inserted

; Entry 10000240c; block 42; address 10000259f
L10000240c_42:
    MOV byte [rel L__DATA___bss + 4], 1
    JMP L10000240c_39     ; inserted

; Entry 10000240c; block 43; address 10000259b
L10000240c_43:
    TEST EAX, EAX
    JNE L10000240c_44
    JMP L10000240c_42     ; inserted

; Entry 10000240c; block 44; address 1000025ad
L10000240c_44:
    LEA RSI, [rel L__TEXT___cstring_12]
    MOV RDI, qword [RBP + -56]
    CALL _strcmp
    JMP L10000240c_45     ; inserted

; Entry 10000240c; block 45; address 1000025bd
L10000240c_45:
    TEST EAX, EAX
    JNE L10000240c_47
    JMP L10000240c_46     ; inserted

; Entry 10000240c; block 46; address 1000025c1
L10000240c_46:
    MOV byte [rel L__DATA___bss + 5], 1
    JMP L10000240c_47     ; inserted

; Entry 10000240c; block 47; address 1000025c8
L10000240c_47:
    LEA R13, [rel L__TEXT___cstring_23]
    LEA RBX, [rel L__DATA___const + 0]
    LEA R14, [rel L__TEXT___text + 2220]
    MOV dword [RBP + -48], R15D
    JMP L10000240c_55     ; inserted

; Entry 10000240c; block 48; address 1000025f5
L10000240c_48:
    CMP EAX, 82
    JG L10000240c_50
    JMP L10000240c_49     ; inserted

; Entry 10000240c; block 49; address 1000025fa
L10000240c_49:
    CMP EAX, 75
    JG L10000240c_52
    JMP L10000240c_51     ; inserted

; Entry 10000240c; block 50; address 100002616
L10000240c_50:
    LEA ECX, [RAX + -97]
    CMP ECX, 21
    JA L10000240c_57
    JMP L10000240c_56     ; inserted

; Entry 10000240c; block 51; address 1000025ff
L10000240c_51:
    LEA ECX, [RAX + -49]
    CMP ECX, 9
    JAE L10000240c_54
    JMP L10000240c_53     ; inserted

; Entry 10000240c; block 52; address 100002645
L10000240c_52:
    CMP EAX, 78
    JNE L10000240c_71
    JMP L10000240c_70     ; inserted

; Entry 10000240c; block 53; address 10000260b
L10000240c_53:
    ADD EAX, 18446744073709551568
    MOV dword [rel L__DATA___data + 0], EAX
    JMP L10000240c_55

; Entry 10000240c; block 54; address 100002754
L10000240c_54:
    CMP EAX, 18446744073709551615
    JNE L10000240c_68
    JMP L10000240c_79     ; inserted

; Entry 10000240c; block 55; address 1000025e1
L10000240c_55:
    MOV EDI, R15D
    MOV RSI, R12
    MOV RDX, R13
    MOV RCX, RBX
    XOR R8D, R8D
    CALL _getopt_long
    JMP L10000240c_48     ; inserted

; Entry 10000240c; block 56; address 10000261e
L10000240c_56:
    MOVSXD RAX, dword [R14 + RCX * 4]
    ADD RAX, R14
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100002625 + 8*ECX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 100002627,100002793,1000026a0,1000026ac,1000026b8,1000026c4,1000026d0,1000026e3,1000026f6,100002702,10000270e

; Entry 10000240c; block 57; address 10000265e
L10000240c_57:
    CMP EAX, 83
    JNE L10000240c_73
    JMP L10000240c_72     ; inserted

; Entry 10000240c; block 58; address 100002627
L10000240c_58:
    MOV RAX, qword [rel ___stderrp]
    MOV RDI, qword [RAX]
    LEA RSI, [rel L__TEXT___cstring_50]
    MOV RDX, qword [RBP + -56]
    XOR EAX, EAX
    CALL _fprintf
    JMP L10000240c_69     ; inserted

; Entry 10000240c; block 59; address 1000026a0
L10000240c_59:
    MOV byte [rel L__DATA___bss + 4], 1
    JMP L10000240c_55

; Entry 10000240c; block 60; address 1000026ac
L10000240c_60:
    MOV byte [rel L__DATA___bss + 0], 1
    JMP L10000240c_55

; Entry 10000240c; block 61; address 1000026b8
L10000240c_61:
    MOV byte [rel L__DATA___bss + 12], 1
    JMP L10000240c_55

; Entry 10000240c; block 62; address 1000026c4
L10000240c_62:
    MOV byte [rel L__DATA___bss + 16], 1
    JMP L10000240c_55

; Entry 10000240c; block 63; address 1000026d0
L10000240c_63:
    MOV AL, 1
    MOV byte [rel L__DATA___bss + 8], AL
    MOV byte [rel L__DATA___bss + 0], AL
    JMP L10000240c_55

; Entry 10000240c; block 64; address 1000026e3
L10000240c_64:
    MOV byte [rel L__DATA___bss + 20], 1
    MOV byte [rel L__DATA___bss + 24], 0
    JMP L10000240c_55

; Entry 10000240c; block 65; address 1000026f6
L10000240c_65:
    MOV byte [rel L__DATA___bss + 28], 1
    JMP L10000240c_55

; Entry 10000240c; block 66; address 100002702
L10000240c_66:
    MOV byte [rel L__DATA___bss + 32], 1
    JMP L10000240c_55

; Entry 10000240c; block 67; address 10000270e
L10000240c_67:
    MOV AL, 1
    MOV byte [rel L__DATA___bss + 4], AL
    MOV byte [rel L__DATA___bss + 36], AL
    MOV byte [rel L__DATA___bss + 0], AL
    JMP L10000240c_55

; Entry 10000240c; block 68; address 100002793
L10000240c_68:
    CALL L100002bc9_0

; Entry 10000240c; block 69; address 100002643
L10000240c_69:
    JMP L10000240c_55

; Entry 10000240c; block 70; address 10000264e
L10000240c_70:
    MOV byte [rel L__DATA___bss + 20], 0
    MOV byte [rel L__DATA___bss + 24], 1
    JMP L10000240c_55

; Entry 10000240c; block 71; address 10000277f
L10000240c_71:
    CMP EAX, 76
    JNE L10000240c_68
    JMP L10000240c_85     ; inserted

; Entry 10000240c; block 72; address 100002667
L10000240c_72:
    MOV RAX, qword [rel _optarg]
    MOV R15, qword [RAX]
    MOV RDI, R15
    CALL _strlen
    JMP L10000240c_74     ; inserted

; Entry 10000240c; block 73; address 100002789
L10000240c_73:
    CMP EAX, 86
    JNE L10000240c_68
    JMP L10000240c_86     ; inserted

; Entry 10000240c; block 74; address 100002679
L10000240c_74:
    TEST EAX, EAX
    JE L10000240c_76
    JMP L10000240c_75     ; inserted

; Entry 10000240c; block 75; address 100002681
L10000240c_75:
    CMP EAX, 31
    JGE L10000240c_78
    JMP L10000240c_77     ; inserted

; Entry 10000240c; block 76; address 100002733
L10000240c_76:
    LEA RAX, [rel L__TEXT___cstring_126]
    MOV qword [rel L__DATA___data + 328], RAX
    MOV dword [rel L__DATA___data + 336], 0
    MOV R15D, dword [RBP + -48]
    JMP L10000240c_55

; Entry 10000240c; block 77; address 10000268a
L10000240c_77:
    MOV qword [rel L__DATA___data + 16], R15
    MOV dword [rel L__DATA___data + 24], EAX
    MOV R15D, dword [RBP + -48]
    JMP L10000240c_55

; Entry 10000240c; block 78; address 100002798
L10000240c_78:
    LEA RSI, [rel L__TEXT___cstring_93]
    MOV EDI, 1
    MOV RDX, R15
    XOR EAX, EAX
    CALL _errx

; Entry 10000240c; block 79; address 100002759
L10000240c_79:
    MOV RAX, qword [rel _optind]
    MOVSXD RAX, dword [RAX]
    SUB R15D, EAX
    JNE L10000240c_81
    JMP L10000240c_80     ; inserted

; Entry 10000240c; block 80; address 100002768
L10000240c_80:
    CMP byte [rel L__DATA___bss + 0], 1
    JNE L10000240c_83
    JMP L10000240c_82     ; inserted

; Entry 10000240c; block 81; address 1000027ae
L10000240c_81:
    LEA R12, [R12 + RAX * 8]
    MOV RBX, qword [R12]
    MOV R14D, R15D
    JMP L10000240c_122     ; inserted

; Entry 10000240c; block 82; address 100002775
L10000240c_82:
    CALL L100002c11_0
    JMP L10000240c_84     ; inserted

; Entry 10000240c; block 83; address 100002a86
L10000240c_83:
    CALL L100002ea0_0
    JMP L10000240c_153     ; inserted

; Entry 10000240c; block 84; address 10000277a
L10000240c_84:
    JMP L10000240c_153

; Entry 10000240c; block 85; address 100002784
L10000240c_85:
    CALL L100002b85_0

; Entry 10000240c; block 86; address 10000278e
L10000240c_86:
    CALL L100002b5b_0

; Entry 10000240c; block 87; address 1000027be
L10000240c_87:
    CMP byte [RBX + 1], 0
    JE L10000240c_89
    JMP L10000240c_88     ; inserted

; Entry 10000240c; block 88; address 1000027c8
L10000240c_88:
    CMP byte [rel L__DATA___bss + 5], 1
    MOV qword [RBP + -48], R12
    MOV qword [RBP + -56], RBX
    JNE L10000240c_91
    JMP L10000240c_90     ; inserted

; Entry 10000240c; block 89; address 100002a0f
L10000240c_89:
    CMP byte [rel L__DATA___bss + 0], 1
    JNE L10000240c_145
    JMP L10000240c_144     ; inserted

; Entry 10000240c; block 90; address 1000027d9
L10000240c_90:
    LEA RDI, [rel L__TEXT___cstring_1803]
    LEA RSI, [rel L__TEXT___cstring_1812]
    CALL _compat_mode
    JMP L10000240c_92     ; inserted

; Entry 10000240c; block 91; address 10000284d
L10000240c_91:
    XOR R13D, R13D
    MOV R12, RBX
    JMP L10000240c_101     ; inserted

; Entry 10000240c; block 92; address 1000027ec
L10000240c_92:
    TEST AL, AL
    JE L10000240c_91
    JMP L10000240c_93     ; inserted

; Entry 10000240c; block 93; address 1000027f0
L10000240c_93:
    MOV RDI, RBX
    MOV ESI, 46
    CALL _strrchr
    JMP L10000240c_94     ; inserted

; Entry 10000240c; block 94; address 1000027fd
L10000240c_94:
    TEST RAX, RAX
    JE L10000240c_96
    JMP L10000240c_95     ; inserted

; Entry 10000240c; block 95; address 100002802
L10000240c_95:
    MOV RDI, RAX
    LEA RSI, [rel L__TEXT___cstring_325]
    CALL _strcmp
    JMP L10000240c_97     ; inserted

; Entry 10000240c; block 96; address 100002815
L10000240c_96:
    MOV RDI, RBX
    CALL _strlen
    JMP L10000240c_98     ; inserted

; Entry 10000240c; block 97; address 100002811
L10000240c_97:
    TEST EAX, EAX
    JE L10000240c_91
    JMP L10000240c_96     ; inserted

; Entry 10000240c; block 98; address 10000281d
L10000240c_98:
    MOV R12, RAX
    LEA RDI, [RAX + 3]
    CALL _malloc
    JMP L10000240c_99     ; inserted

; Entry 10000240c; block 99; address 100002829
L10000240c_99:
    MOV R13, RAX
    MOV RDI, RAX
    MOV RSI, RBX
    MOV RDX, R12
    CALL _memcpy
    JMP L10000240c_100     ; inserted

; Entry 10000240c; block 100; address 10000283a
L10000240c_100:
    MOV word [R13 + R12], 23086
    MOV byte [R13 + R12 + 2], 0
    MOV R12, R13
    JMP L10000240c_101

; Entry 10000240c; block 101; address 100002853
L10000240c_101:
    MOV RDI, R12
    LEA RSI, [RBP + -224]
    CALL _stat$INODE64
    JMP L10000240c_102     ; inserted

; Entry 10000240c; block 102; address 100002862
L10000240c_102:
    TEST EAX, EAX
    JNE L10000240c_104
    JMP L10000240c_103     ; inserted

; Entry 10000240c; block 103; address 100002866
L10000240c_103:
    CMP byte [rel L__DATA___bss + 12], 0
    JNE L10000240c_106
    JMP L10000240c_105     ; inserted

; Entry 10000240c; block 104; address 100002897
L10000240c_104:
    TEST R13, R13
    JNE L10000240c_110
    JMP L10000240c_109     ; inserted

; Entry 10000240c; block 105; address 100002873
L10000240c_105:
    TEST byte [rel L__DATA___bss + 4], 1
    JNE L10000240c_106
    JMP L10000240c_107     ; inserted

; Entry 10000240c; block 106; address 100002947
L10000240c_106:
    MOVZX EAX, word [RBP + -220]
    AND EAX, 61440
    CMP EAX, 32768
    JE L10000240c_125
    JMP L10000240c_124     ; inserted

; Entry 10000240c; block 107; address 100002880
L10000240c_107:
    MOV RDI, R12
    LEA RSI, [RBP + -224]
    CALL _lstat$INODE64
    JMP L10000240c_108     ; inserted

; Entry 10000240c; block 108; address 10000288f
L10000240c_108:
    TEST EAX, EAX
    JE L10000240c_106
    JMP L10000240c_104     ; inserted

; Entry 10000240c; block 109; address 10000289c
L10000240c_109:
    TEST byte [rel L__DATA___bss + 0], 1
    JE L10000240c_110
    JMP L10000240c_111     ; inserted

; Entry 10000240c; block 110; address 10000290a
L10000240c_110:
    LEA RDI, [rel L__TEXT___cstring_1821]
    MOV RSI, qword [RBP + -56]
    MOV RDX, R12
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000240c_119     ; inserted

; Entry 10000240c; block 111; address 1000028a5
L10000240c_111:
    CALL ___error
    JMP L10000240c_112     ; inserted

; Entry 10000240c; block 112; address 1000028aa
L10000240c_112:
    CMP dword [RAX], 2
    JNE L10000240c_110
    JMP L10000240c_113     ; inserted

; Entry 10000240c; block 113; address 1000028af
L10000240c_113:
    MOV RDI, R12
    CALL _strlen
    JMP L10000240c_114     ; inserted

; Entry 10000240c; block 114; address 1000028b7
L10000240c_114:
    MOV R15, RAX
    MOVSXD RBX, dword [rel L__DATA___data + 24]
    LEA RDI, [RAX + RBX]
    INC RDI
    CALL _malloc
    JMP L10000240c_115     ; inserted

; Entry 10000240c; block 115; address 1000028cd
L10000240c_115:
    TEST RAX, RAX
    JE L10000240c_18
    JMP L10000240c_116     ; inserted

; Entry 10000240c; block 116; address 1000028d6
L10000240c_116:
    MOV R13, RAX
    MOV RDI, RAX
    MOV RSI, R12
    MOV RDX, R15
    CALL _memcpy
    JMP L10000240c_117     ; inserted

; Entry 10000240c; block 117; address 1000028e7
L10000240c_117:
    MOV RDI, R13
    ADD RDI, R15
    MOV RSI, qword [rel L__DATA___data + 16]
    INC RBX
    MOV RDX, RBX
    CALL _memcpy
    JMP L10000240c_118     ; inserted

; Entry 10000240c; block 118; address 1000028ff
L10000240c_118:
    MOV R12, R13
    MOV R15D, R14D
    JMP L10000240c_101

; Entry 10000240c; block 119; address 10000291f
L10000240c_119:
    MOV R12, qword [RBP + -48]
    JMP L10000240c_143     ; inserted

; Entry 10000240c; block 120; address 100002928
L10000240c_120:
    MOV RDI, R13
    CALL _free
    JMP L10000240c_121     ; inserted

; Entry 10000240c; block 121; address 100002930
L10000240c_121:
    MOV RBX, qword [R12 + 8]
    ADD R12, 8
    TEST RBX, RBX
    JNE L10000240c_122
    JMP L10000240c_123     ; inserted

; Entry 10000240c; block 122; address 1000027b9
L10000240c_122:
    CMP byte [RBX], 45
    JNE L10000240c_88
    JMP L10000240c_87     ; inserted

; Entry 10000240c; block 123; address 100002942
L10000240c_123:
    CMP R15D, 2
    JL L10000240c_153
    JMP L10000240c_154     ; inserted

; Entry 10000240c; block 124; address 10000295e
L10000240c_124:
    MOVZX EAX, AX
    CMP EAX, 16384
    JNE L10000240c_127
    JMP L10000240c_126     ; inserted

; Entry 10000240c; block 125; address 100002a2c
L10000240c_125:
    MOV RDI, R12
    LEA RSI, [RBP + -224]
    CALL L10000549a_0
    JMP L10000240c_148     ; inserted

; Entry 10000240c; block 126; address 10000296c
L10000240c_126:
    CMP byte [rel L__DATA___bss + 32], 1
    JNE L10000240c_129
    JMP L10000240c_128     ; inserted

; Entry 10000240c; block 127; address 100002a40
L10000240c_127:
    LEA RDI, [rel L__TEXT___cstring_1859]
    JMP L10000240c_149

; Entry 10000240c; block 128; address 100002979
L10000240c_128:
    MOV qword [RBP + -80], R12
    MOV qword [RBP + -72], 0
    LEA RDI, [RBP + -80]
    MOV ESI, 20
    XOR EDX, EDX
    CALL _fts_open$INODE64
    JMP L10000240c_130     ; inserted

; Entry 10000240c; block 129; address 100002a49
L10000240c_129:
    LEA RDI, [rel L__TEXT___cstring_1841]
    JMP L10000240c_149     ; inserted

; Entry 10000240c; block 130; address 100002995
L10000240c_130:
    TEST RAX, RAX
    JE L10000240c_132
    JMP L10000240c_131     ; inserted

; Entry 10000240c; block 131; address 10000299e
L10000240c_131:
    MOV R15, RAX
    MOV RDI, RAX
    CALL _fts_read$INODE64
    JMP L10000240c_133     ; inserted

; Entry 10000240c; block 132; address 100002a5f
L10000240c_132:
    LEA RDI, [rel L__TEXT___cstring_1884]
    MOV RSI, R12
    XOR EAX, EAX
    CALL _warn
    JMP L10000240c_151     ; inserted

; Entry 10000240c; block 133; address 1000029a9
L10000240c_133:
    TEST RAX, RAX
    MOV R12, qword [RBP + -48]
    JE L10000240c_135
    JMP L10000240c_134     ; inserted

; Entry 10000240c; block 134; address 1000029b2
L10000240c_134:
    MOV ECX, dword [RAX + 88]
    ADD ECX, 18446744073709551612
    CMP CX, 6
    JA L10000240c_137
    JMP L10000240c_136     ; inserted

; Entry 10000240c; block 135; address 1000029ff
L10000240c_135:
    MOV RDI, R15
    CALL _fts_close$INODE64
    JMP L10000240c_142     ; inserted

; Entry 10000240c; block 136; address 1000029be
L10000240c_136:
    MOVZX ECX, CX
    LEA RDX, [rel L__TEXT___text + 2308]
    MOVSXD RCX, dword [RDX + RCX * 4]
    ADD RCX, RDX
    MOV RCX, QWORD PTR [L_JUMP_TABLE_1000029cf + 8*CX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RCX; TARGETS: 1000029d1,1000029f2,1000029e5

; Entry 10000240c; block 137; address 1000029f2
L10000240c_137:
    MOV RDI, R15
    CALL _fts_read$INODE64
    JMP L10000240c_141     ; inserted

; Entry 10000240c; block 138; address 1000029d1
L10000240c_138:
    MOV RSI, qword [RAX + 48]
    LEA RDI, [rel L__TEXT___cstring_1566]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000240c_140     ; inserted

; Entry 10000240c; block 139; address 1000029e5
L10000240c_139:
    MOV RDI, qword [RAX + 48]
    MOV RSI, qword [RAX + 96]
    CALL L10000549a_0
    JMP L10000240c_137     ; inserted

; Entry 10000240c; block 140; address 1000029e3
L10000240c_140:
    JMP L10000240c_137

; Entry 10000240c; block 141; address 1000029fa
L10000240c_141:
    TEST RAX, RAX
    JNE L10000240c_134
    JMP L10000240c_135     ; inserted

; Entry 10000240c; block 142; address 100002a07
L10000240c_142:
    MOV R15D, R14D
    JMP L10000240c_143

; Entry 10000240c; block 143; address 100002923
L10000240c_143:
    TEST R13, R13
    JE L10000240c_121
    JMP L10000240c_120     ; inserted

; Entry 10000240c; block 144; address 100002a18
L10000240c_144:
    CALL L100002c11_0
    JMP L10000240c_146     ; inserted

; Entry 10000240c; block 145; address 100002a22
L10000240c_145:
    CALL L100002ea0_0
    JMP L10000240c_147     ; inserted

; Entry 10000240c; block 146; address 100002a1d
L10000240c_146:
    JMP L10000240c_121

; Entry 10000240c; block 147; address 100002a27
L10000240c_147:
    JMP L10000240c_121

; Entry 10000240c; block 148; address 100002a3b
L10000240c_148:
    JMP L10000240c_119

; Entry 10000240c; block 149; address 100002a50
L10000240c_149:
    MOV RSI, R12
    XOR EAX, EAX
    CALL L100003193_0
    JMP L10000240c_150     ; inserted

; Entry 10000240c; block 150; address 100002a5a
L10000240c_150:
    JMP L10000240c_119

; Entry 10000240c; block 151; address 100002a70
L10000240c_151:
    MOV R15D, R14D
    JMP L10000240c_119

; Entry 10000240c; block 152; address 100002a7f
L10000240c_152:
    XOR EAX, EAX
    CALL L100003102_0

; Entry 10000240c; block 153; address 100002a8b
L10000240c_153:
    MOV EDI, dword [rel L__DATA___bss + 44]
    CALL _exit

; Entry 10000240c; block 154; address 100002a9c
L10000240c_154:
    TEST byte [rel L__DATA___bss + 8], 1
    JE L10000240c_153
    JMP L10000240c_155     ; inserted

; Entry 10000240c; block 155; address 100002aa5
L10000240c_155:
    TEST byte [rel L__DATA___bss + 28], 1
    JNE L10000240c_153
    JMP L10000240c_156     ; inserted

; Entry 10000240c; block 156; address 100002aae
L10000240c_156:
    LEA RDX, [rel L__TEXT___cstring_127]
    MOV EDI, 4294967295
    XOR ESI, ESI
    XOR ECX, ECX
    CALL L100002f77_0
    JMP L10000240c_157     ; inserted

; Entry 10000240c; block 157; address 100002ac3
L10000240c_157:
    JMP L10000240c_153



; ---------------------
; Function: 0x100002b5b
; ---------------------
; Entry 100002b5b; block 0; address 100002b5b
L100002b5b_0:
    PUSH RBP
    MOV RBP, RSP
    MOV RAX, qword [rel ___stderrp]
    MOV RDI, qword [RAX]
    LEA RSI, [rel L__TEXT___cstring_2941]
    LEA RDX, [rel L__TEXT___const + 176]
    XOR EAX, EAX
    CALL _fprintf
    JMP L100002b5b_1     ; inserted

; Entry 100002b5b; block 1; address 100002b7e
L100002b5b_1:
    XOR EDI, EDI
    CALL _exit



; ---------------------
; Function: 0x100002b85
; ---------------------
; Entry 100002b85; block 0; address 100002b85
L100002b85_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH RBX
    PUSH RAX
    MOV RBX, qword [rel ___stderrp]
    MOV RDI, qword [RBX]
    LEA RSI, [rel L__TEXT___cstring_3954]
    LEA RDX, [rel L__TEXT___const + 176]
    XOR EAX, EAX
    CALL _fprintf
    JMP L100002b85_1     ; inserted

; Entry 100002b85; block 1; address 100002baa
L100002b85_1:
    MOV RDI, qword [RBX]
    LEA RSI, [rel L__TEXT___cstring_2941]
    LEA RDX, [rel L__TEXT___const + 208]
    XOR EAX, EAX
    CALL _fprintf
    JMP L100002b85_2     ; inserted

; Entry 100002b85; block 2; address 100002bc2
L100002b85_2:
    XOR EDI, EDI
    CALL _exit



; ---------------------
; Function: 0x100002bc9
; ---------------------
; Entry 100002bc9; block 0; address 100002bc9
L100002bc9_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH RBX
    PUSH RAX
    MOV RBX, qword [rel ___stderrp]
    MOV RDI, qword [RBX]
    LEA RSI, [rel L__TEXT___cstring_2941]
    LEA RDX, [rel L__TEXT___const + 176]
    XOR EAX, EAX
    CALL _fprintf
    JMP L100002bc9_1     ; inserted

; Entry 100002bc9; block 1; address 100002bee
L100002bc9_1:
    MOV RBX, qword [RBX]
    CALL _getprogname
    JMP L100002bc9_2     ; inserted

; Entry 100002bc9; block 2; address 100002bf6
L100002bc9_2:
    LEA RSI, [rel L__TEXT___cstring_2945]
    MOV RDI, RBX
    MOV RDX, RAX
    XOR EAX, EAX
    CALL _fprintf
    JMP L100002bc9_3     ; inserted

; Entry 100002bc9; block 3; address 100002c0a
L100002bc9_3:
    XOR EDI, EDI
    CALL _exit



; ---------------------
; Function: 0x100002c11
; ---------------------
; Entry 100002c11; block 0; address 100002c11
L100002c11_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R14
    PUSH RBX
    SUB RSP, 160
    MOV AL, byte [rel L__DATA___bss + 8]
    CMP byte [rel L__DATA___bss + 12], 0
    JNE L100002c11_2
    JMP L100002c11_1     ; inserted

; Entry 100002c11; block 1; address 100002c2e
L100002c11_1:
    TEST AL, 1
    JNE L100002c11_2
    JMP L100002c11_3     ; inserted

; Entry 100002c11; block 2; address 100002c50
L100002c11_2:
    TEST AL, 1
    JE L100002c11_9
    JMP L100002c11_8     ; inserted

; Entry 100002c11; block 3; address 100002c32
L100002c11_3:
    XOR EDI, EDI
    CALL _isatty
    JMP L100002c11_4     ; inserted

; Entry 100002c11; block 4; address 100002c39
L100002c11_4:
    TEST EAX, EAX
    JE L100002c11_6
    JMP L100002c11_5     ; inserted

; Entry 100002c11; block 5; address 100002c3d
L100002c11_5:
    LEA RDI, [rel L__TEXT___cstring_347]
    JMP L100002c11_34     ; inserted

; Entry 100002c11; block 6; address 100002c7e
L100002c11_6:
    CMP byte [rel L__DATA___bss + 8], 0
    JNE L100002c11_8
    JMP L100002c11_9     ; inserted

; Entry 100002c11; block 7; address 100002c4b
L100002c11_7:
    JMP L100002c11_14

; Entry 100002c11; block 8; address 100002c54
L100002c11_8:
    LEA RSI, [RBP + -168]
    XOR EDI, EDI
    CALL _fstat$INODE64
    JMP L100002c11_10     ; inserted

; Entry 100002c11; block 9; address 100002c87
L100002c11_9:
    MOV R14D, 4
    LEA RBX, [RBP + -20]
    JMP L100002c11_20     ; inserted

; Entry 100002c11; block 10; address 100002c62
L100002c11_10:
    TEST EAX, EAX
    JS L100002c11_12
    JMP L100002c11_11     ; inserted

; Entry 100002c11; block 11; address 100002c66
L100002c11_11:
    MOV RCX, qword [RBP + -120]
    MOV RSI, qword [RBP + -72]
    LEA RDX, [rel L__TEXT___cstring_136]
    XOR EDI, EDI
    CALL L100002f77_0
    JMP L100002c11_13     ; inserted

; Entry 100002c11; block 12; address 100002cd1
L100002c11_12:
    LEA RDI, [rel L__TEXT___cstring_388]
    JMP L100002c11_25     ; inserted

; Entry 100002c11; block 13; address 100002c7c
L100002c11_13:
    JMP L100002c11_14

; Entry 100002c11; block 14; address 100002cdf
L100002c11_14:
    ADD RSP, 160
    POP RBX
    POP R14
    POP RBP
    RET 

; Entry 100002c11; block 15; address 100002c9e
L100002c11_15:
    CMP RAX, 18446744073709551615
    JE L100002c11_17
    JMP L100002c11_16     ; inserted

; Entry 100002c11; block 16; address 100002ca4
L100002c11_16:
    TEST RAX, RAX
    JE L100002c11_19
    JMP L100002c11_18     ; inserted

; Entry 100002c11; block 17; address 100002cc8
L100002c11_17:
    LEA RDI, [rel L__TEXT___cstring_394]
    JMP L100002c11_25

; Entry 100002c11; block 18; address 100002ca9
L100002c11_18:
    ADD RBX, RAX
    SUB R14, RAX
    JNE L100002c11_20
    JMP L100002c11_21     ; inserted

; Entry 100002c11; block 19; address 100002cb4
L100002c11_19:
    MOV EAX, 4
    SUB RAX, R14
    CMP RAX, 4
    JE L100002c11_23
    JMP L100002c11_22     ; inserted

; Entry 100002c11; block 20; address 100002c91
L100002c11_20:
    XOR EDI, EDI
    MOV RSI, RBX
    MOV RDX, R14
    CALL _read
    JMP L100002c11_15     ; inserted

; Entry 100002c11; block 21; address 100002cb1
L100002c11_21:
    XOR R14D, R14D
    JMP L100002c11_19     ; inserted

; Entry 100002c11; block 22; address 100002cc2
L100002c11_22:
    CMP RAX, 18446744073709551615
    JNE L100002c11_24
    JMP L100002c11_17     ; inserted

; Entry 100002c11; block 23; address 100002ceb
L100002c11_23:
    LEA RDI, [RBP + -20]
    CALL L100003329_0
    JMP L100002c11_26     ; inserted

; Entry 100002c11; block 24; address 100002d33
L100002c11_24:
    LEA RDI, [rel L__TEXT___cstring_411]
    JMP L100002c11_34

; Entry 100002c11; block 25; address 100002cd8
L100002c11_25:
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L100002c11_14     ; inserted

; Entry 100002c11; block 26; address 100002cf4
L100002c11_26:
    CMP EAX, 4
    JA L100002c11_28
    JMP L100002c11_27     ; inserted

; Entry 100002c11; block 27; address 100002cf9
L100002c11_27:
    MOV EAX, EAX
    LEA RCX, [rel L__TEXT___text + 3184]
    MOVSXD RAX, dword [RCX + RAX * 4]
    ADD RAX, RCX
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100002d09 + 8*EAX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 100002d0b,100002d68,100002d86,100002dc8

; Entry 100002c11; block 28; address 100002d3f
L100002c11_28:
    CMP byte [rel L__DATA___bss + 12], 0
    JE L100002c11_36
    JMP L100002c11_35     ; inserted

; Entry 100002c11; block 29; address 100002d0b
L100002c11_29:
    LEA R9, [rel L__TEXT___cstring_470]
    LEA RDX, [RBP + -20]
    LEA R8, [RBP + -168]
    MOV ECX, 4
    XOR EDI, EDI
    MOV ESI, 1
    CALL L1000034a3_0
    JMP L100002c11_33     ; inserted

; Entry 100002c11; block 30; address 100002d68
L100002c11_30:
    LEA RDX, [RBP + -20]
    LEA R8, [RBP + -168]
    MOV ECX, 4
    XOR EDI, EDI
    MOV ESI, 1
    CALL L100003c24_0
    JMP L100002c11_39     ; inserted

; Entry 100002c11; block 31; address 100002d86
L100002c11_31:
    XOR EDI, EDI
    CALL L100003f14_0
    JMP L100002c11_40     ; inserted

; Entry 100002c11; block 32; address 100002dc8
L100002c11_32:
    LEA RDX, [RBP + -20]
    LEA R8, [RBP + -168]
    MOV ECX, 4
    XOR EDI, EDI
    MOV ESI, 1
    CALL L10000408c_0
    JMP L100002c11_45     ; inserted

; Entry 100002c11; block 33; address 100002d2e
L100002c11_33:
    JMP L100002c11_38

; Entry 100002c11; block 34; address 100002c44
L100002c11_34:
    XOR EAX, EAX
    CALL L100003193_0
    JMP L100002c11_7     ; inserted

; Entry 100002c11; block 35; address 100002d4c
L100002c11_35:
    LEA RDI, [RBP + -20]
    LEA RDX, [RBP + -168]
    MOV ESI, 4
    XOR ECX, ECX
    CALL L100003399_0
    JMP L100002c11_37     ; inserted

; Entry 100002c11; block 36; address 100002e71
L100002c11_36:
    LEA RDI, [rel L__TEXT___cstring_443]
    JMP L100002c11_34

; Entry 100002c11; block 37; address 100002d63
L100002c11_37:
    JMP L100002c11_38

; Entry 100002c11; block 38; address 100002e02
L100002c11_38:
    MOV RBX, RAX
    JMP L100002c11_46     ; inserted

; Entry 100002c11; block 39; address 100002d84
L100002c11_39:
    JMP L100002c11_38

; Entry 100002c11; block 40; address 100002d8d
L100002c11_40:
    TEST RAX, RAX
    JE L100002c11_42
    JMP L100002c11_41     ; inserted

; Entry 100002c11; block 41; address 100002d96
L100002c11_41:
    MOV R14, RAX
    MOV RAX, qword [rel ___stdoutp]
    MOV RSI, qword [RAX]
    LEA RDX, [RBP + -20]
    LEA R8, [RBP + -168]
    MOV ECX, 4
    MOV RDI, R14
    CALL L100003fa4_0
    JMP L100002c11_43     ; inserted

; Entry 100002c11; block 42; address 100002e7d
L100002c11_42:
    LEA RDI, [rel L__TEXT___cstring_478]
    JMP L100002c11_34

; Entry 100002c11; block 43; address 100002dbb
L100002c11_43:
    MOV RBX, RAX
    MOV RDI, R14
    CALL _fclose
    JMP L100002c11_44     ; inserted

; Entry 100002c11; block 44; address 100002dc6
L100002c11_44:
    JMP L100002c11_46

; Entry 100002c11; block 45; address 100002de4
L100002c11_45:
    JMP L100002c11_38

; Entry 100002c11; block 46; address 100002e05
L100002c11_46:
    MOV DL, byte [rel L__DATA___bss + 40]
    MOV AL, byte [rel L__DATA___bss + 36]
    CMP RBX, 18446744073709551615
    JE L100002c11_48
    JMP L100002c11_47     ; inserted

; Entry 100002c11; block 47; address 100002e17
L100002c11_47:
    TEST DL, 1
    JE L100002c11_48
    JMP L100002c11_49     ; inserted

; Entry 100002c11; block 48; address 100002e45
L100002c11_48:
    TEST DL, 1
    JE L100002c11_14
    JMP L100002c11_53     ; inserted

; Entry 100002c11; block 49; address 100002e1c
L100002c11_49:
    TEST AL, 1
    JNE L100002c11_48
    JMP L100002c11_50     ; inserted

; Entry 100002c11; block 50; address 100002e20
L100002c11_50:
    MOV RCX, qword [RBP + -168]
    CMP RCX, 18446744073709551615
    JE L100002c11_48
    JMP L100002c11_51     ; inserted

; Entry 100002c11; block 51; address 100002e2d
L100002c11_51:
    XOR EDI, EDI
    XOR ESI, ESI
    MOV RDX, RBX
    CALL L100004844_0
    JMP L100002c11_52     ; inserted

; Entry 100002c11; block 52; address 100002e39
L100002c11_52:
    MOV DL, byte [rel L__DATA___bss + 40]
    MOV AL, byte [rel L__DATA___bss + 36]
    JMP L100002c11_48     ; inserted

; Entry 100002c11; block 53; address 100002e4e
L100002c11_53:
    TEST AL, 1
    JE L100002c11_14
    JMP L100002c11_54     ; inserted

; Entry 100002c11; block 54; address 100002e56
L100002c11_54:
    XOR ESI, ESI
    CMP RBX, 18446744073709551615
    SETNE SIL
    LEA RDI, [rel L__TEXT___cstring_470]
    CALL L1000048ed_0
    JMP L100002c11_55     ; inserted

; Entry 100002c11; block 55; address 100002e6c
L100002c11_55:
    JMP L100002c11_14



; ---------------------
; Function: 0x100002ea0
; ---------------------
; Entry 100002ea0; block 0; address 100002ea0
L100002ea0_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 160
    CMP byte [rel L__DATA___bss + 12], 0
    JNE L100002ea0_2
    JMP L100002ea0_1     ; inserted

; Entry 100002ea0; block 1; address 100002eb4
L100002ea0_1:
    MOV EDI, 1
    CALL _isatty
    JMP L100002ea0_3     ; inserted

; Entry 100002ea0; block 2; address 100002ed5
L100002ea0_2:
    LEA RSI, [RBP + -152]
    XOR EDI, EDI
    CALL _fstat$INODE64
    JMP L100002ea0_6     ; inserted

; Entry 100002ea0; block 3; address 100002ebe
L100002ea0_3:
    TEST EAX, EAX
    JE L100002ea0_2
    JMP L100002ea0_4     ; inserted

; Entry 100002ea0; block 4; address 100002ec2
L100002ea0_4:
    LEA RDI, [rel L__TEXT___cstring_1637]
    XOR EAX, EAX
    CALL L100003193_0
    JMP L100002ea0_5     ; inserted

; Entry 100002ea0; block 5; address 100002ed0
L100002ea0_5:
    JMP L100002ea0_16

; Entry 100002ea0; block 6; address 100002ee3
L100002ea0_6:
    TEST EAX, EAX
    JS L100002ea0_8
    JMP L100002ea0_7     ; inserted

; Entry 100002ea0; block 7; address 100002ee7
L100002ea0_7:
    MOVZX EAX, word [RBP + -148]
    AND EAX, 61440
    CMP EAX, 32768
    JNE L100002ea0_10
    JMP L100002ea0_9     ; inserted

; Entry 100002ea0; block 8; address 100002f00
L100002ea0_8:
    LEA RDI, [rel L__TEXT___cstring_1679]
    JMP L100002ea0_21

; Entry 100002ea0; block 9; address 100002efa
L100002ea0_9:
    MOV R8, qword [RBP + -104]
    JMP L100002ea0_12

; Entry 100002ea0; block 10; address 100002f09
L100002ea0_10:
    XOR EDI, EDI
    CALL _time
    JMP L100002ea0_11     ; inserted

; Entry 100002ea0; block 11; address 100002f10
L100002ea0_11:
    MOV R8, RAX
    CMP RAX, 18446744073709551615
    JE L100002ea0_13
    JMP L100002ea0_12     ; inserted

; Entry 100002ea0; block 12; address 100002f19
L100002ea0_12:
    LEA RCX, [rel L__TEXT___cstring_126]
    LEA RDX, [RBP + -8]
    XOR EDI, EDI
    MOV ESI, 1
    CALL L100005087_0
    JMP L100002ea0_14     ; inserted

; Entry 100002ea0; block 13; address 100002f60
L100002ea0_13:
    LEA RDI, [rel L__TEXT___cstring_1696]
    JMP L100002ea0_21     ; inserted

; Entry 100002ea0; block 14; address 100002f30
L100002ea0_14:
    CMP RAX, 18446744073709551615
    JE L100002ea0_16
    JMP L100002ea0_15     ; inserted

; Entry 100002ea0; block 15; address 100002f36
L100002ea0_15:
    TEST byte [rel L__DATA___bss + 40], 1
    JE L100002ea0_16
    JMP L100002ea0_17     ; inserted

; Entry 100002ea0; block 16; address 100002f6e
L100002ea0_16:
    ADD RSP, 160
    POP RBP
    RET 

; Entry 100002ea0; block 17; address 100002f3f
L100002ea0_17:
    TEST byte [rel L__DATA___bss + 36], 1
    JNE L100002ea0_16
    JMP L100002ea0_18     ; inserted

; Entry 100002ea0; block 18; address 100002f48
L100002ea0_18:
    MOV RCX, qword [RBP + -8]
    CMP RCX, 18446744073709551615
    JE L100002ea0_16
    JMP L100002ea0_19     ; inserted

; Entry 100002ea0; block 19; address 100002f52
L100002ea0_19:
    XOR EDI, EDI
    XOR ESI, ESI
    MOV RDX, RAX
    CALL L100004844_0
    JMP L100002ea0_20     ; inserted

; Entry 100002ea0; block 20; address 100002f5e
L100002ea0_20:
    JMP L100002ea0_16

; Entry 100002ea0; block 21; address 100002f67
L100002ea0_21:
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L100002ea0_16     ; inserted



; ---------------------
; Function: 0x100002f77
; ---------------------
; Entry 100002f77; block 0; address 100002f77
L100002f77_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R12
    PUSH RBX
    SUB RSP, 32
    MOV R14, RDX
    MOV R15, RSI
    MOV EBX, EDI
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -40], RAX
    MOV qword [RBP + -56], RCX
    CMP byte [rel L__DATA___bss + 104], 0
    JNE L100002f77_2
    JMP L100002f77_1     ; inserted

; Entry 100002f77; block 1; address 100002fa9
L100002f77_1:
    CMP byte [rel L__DATA___bss + 40], 1
    JNE L100002f77_4
    JMP L100002f77_3     ; inserted

; Entry 100002f77; block 2; address 100002fd5
L100002f77_2:
    MOV byte [rel L__DATA___bss + 104], 1
    CMP EBX, 18446744073709551615
    JE L100002f77_7
    JMP L100002f77_6     ; inserted

; Entry 100002f77; block 3; address 100002fb2
L100002f77_3:
    LEA RDI, [rel L__TEXT___cstring_2816]
    XOR EAX, EAX
    CALL _printf
    JMP L100002f77_4     ; inserted

; Entry 100002f77; block 4; address 100002fc0
L100002f77_4:
    CMP byte [rel L__DATA___bss + 28], 0
    JNE L100002f77_2
    JMP L100002f77_5     ; inserted

; Entry 100002f77; block 5; address 100002fc9
L100002f77_5:
    LEA RDI, [rel L__TEXT___cstring_3991]
    CALL _puts
    JMP L100002f77_2     ; inserted

; Entry 100002f77; block 6; address 100002fe1
L100002f77_6:
    MOV EDI, EBX
    MOV RSI, 18446744073709551608
    MOV EDX, 2
    CALL _lseek
    JMP L100002f77_8     ; inserted

; Entry 100002f77; block 7; address 100003028
L100002f77_7:
    MOV RBX, qword [rel L__DATA___bss + 112]
    MOV R15, qword [rel L__DATA___bss + 120]
    CMP byte [rel L__DATA___bss + 40], 1
    JNE L100002f77_18
    JMP L100002f77_17     ; inserted

; Entry 100002f77; block 8; address 100002ff4
L100002f77_8:
    CMP RAX, 18446744073709551615
    JE L100002f77_10
    JMP L100002f77_9     ; inserted

; Entry 100002f77; block 9; address 100002ffa
L100002f77_9:
    LEA RSI, [RBP + -48]
    MOV EDX, 8
    MOV EDI, EBX
    CALL _read
    JMP L100002f77_11     ; inserted

; Entry 100002f77; block 10; address 10000304f
L100002f77_10:
    XOR EBX, EBX
    JMP L100002f77_20

; Entry 100002f77; block 11; address 10000300a
L100002f77_11:
    CMP RAX, 8
    JE L100002f77_13
    JMP L100002f77_12     ; inserted

; Entry 100002f77; block 12; address 100003010
L100002f77_12:
    LEA RDI, [rel L__TEXT___cstring_2845]
    XOR EBX, EBX
    CMP RAX, 18446744073709551615
    JNE L100002f77_15
    JMP L100002f77_14     ; inserted

; Entry 100002f77; block 13; address 100003053
L100002f77_13:
    MOV R12D, dword [RBP + -48]
    MOV EBX, dword [RBP + -44]
    JMP L100002f77_21

; Entry 100002f77; block 14; address 10000301f
L100002f77_14:
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L100002f77_16     ; inserted

; Entry 100002f77; block 15; address 10000305c
L100002f77_15:
    XOR EAX, EAX
    CALL L100003193_0
    JMP L100002f77_20     ; inserted

; Entry 100002f77; block 16; address 100003026
L100002f77_16:
    JMP L100002f77_20

; Entry 100002f77; block 17; address 10000303f
L100002f77_17:
    LEA RDI, [rel L__TEXT___cstring_2871]
    XOR EAX, EAX
    CALL _printf
    JMP L100002f77_19     ; inserted

; Entry 100002f77; block 18; address 100003098
L100002f77_18:
    ADD qword [rel L__DATA___bss + 112], RBX
    ADD qword [rel L__DATA___bss + 120], R15
    LEA RDI, [rel L__TEXT___cstring_2921]
    MOV RSI, R15
    MOV RDX, RBX
    XOR EAX, EAX
    CALL _printf
    JMP L100002f77_24     ; inserted

; Entry 100002f77; block 19; address 10000304d
L100002f77_19:
    JMP L100002f77_18

; Entry 100002f77; block 20; address 100003063
L100002f77_20:
    XOR R12D, R12D
    JMP L100002f77_21     ; inserted

; Entry 100002f77; block 21; address 100003066
L100002f77_21:
    CMP byte [rel L__DATA___bss + 40], 0
    JE L100002f77_18
    JMP L100002f77_22     ; inserted

; Entry 100002f77; block 22; address 10000306f
L100002f77_22:
    LEA RDI, [RBP + -56]
    CALL _ctime
    JMP L100002f77_23     ; inserted

; Entry 100002f77; block 23; address 100003078
L100002f77_23:
    MOV byte [RAX + 16], 0
    LEA RCX, [RAX + 4]
    LEA RDI, [rel L__TEXT___cstring_2900]
    LEA RSI, [rel L__TEXT___cstring_2915]
    MOV EDX, R12D
    XOR EAX, EAX
    CALL _printf
    JMP L100002f77_18     ; inserted

; Entry 100002f77; block 24; address 1000030ba
L100002f77_24:
    MOV RAX, qword [rel ___stdoutp]
    MOV RDX, qword [RAX]
    MOV RDI, RBX
    MOV RSI, R15
    CALL L100004fb3_0
    JMP L100002f77_25     ; inserted

; Entry 100002f77; block 25; address 1000030cf
L100002f77_25:
    LEA RDI, [rel L__TEXT___cstring_2936]
    MOV RSI, R14
    XOR EAX, EAX
    CALL _printf
    JMP L100002f77_26     ; inserted

; Entry 100002f77; block 26; address 1000030e0
L100002f77_26:
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    CMP RAX, qword [RBP + -40]
    JNE L100002f77_28
    JMP L100002f77_27     ; inserted

; Entry 100002f77; block 27; address 1000030f0
L100002f77_27:
    ADD RSP, 32
    POP RBX
    POP R12
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100002f77; block 28; address 1000030fd
L100002f77_28:
    CALL ___stack_chk_fail



; ---------------------
; Function: 0x100003102
; ---------------------
; Entry 100003102; block 0; address 100003102
L100003102_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 208
    TEST AL, AL
    JE L100003102_2
    JMP L100003102_1     ; inserted

; Entry 100003102; block 1; address 100003111
L100003102_1:
    MOVAPS oword [RBP + -128], XMM0
    MOVAPS oword [RBP + -112], XMM1
    MOVAPS oword [RBP + -96], XMM2
    MOVAPS oword [RBP + -80], XMM3
    MOVAPS oword [RBP + -64], XMM4
    MOVAPS oword [RBP + -48], XMM5
    MOVAPS oword [RBP + -32], XMM6
    MOVAPS oword [RBP + -16], XMM7
    JMP L100003102_2     ; inserted

; Entry 100003102; block 2; address 100003131
L100003102_2:
    MOV qword [RBP + -168], RSI
    MOV qword [RBP + -160], RDX
    MOV qword [RBP + -152], RCX
    MOV qword [RBP + -144], R8
    MOV qword [RBP + -136], R9
    CMP byte [rel L__DATA___bss + 28], 0
    JNE L100003102_4
    JMP L100003102_3     ; inserted

; Entry 100003102; block 3; address 10000315d
L100003102_3:
    LEA RAX, [RBP + -176]
    LEA RSI, [RBP + -208]
    MOV qword [RSI + 16], RAX
    LEA RAX, [RBP + 16]
    MOV qword [RSI + 8], RAX
    MOV RAX, 206158430216
    MOV qword [RSI], RAX
    CALL _vwarn
    JMP L100003102_4     ; inserted

; Entry 100003102; block 4; address 100003189
L100003102_4:
    MOV EDI, 2
    CALL _exit



; ---------------------
; Function: 0x100003193
; ---------------------
; Entry 100003193; block 0; address 100003193
L100003193_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 208
    TEST AL, AL
    JE L100003193_2
    JMP L100003193_1     ; inserted

; Entry 100003193; block 1; address 1000031a2
L100003193_1:
    MOVAPS oword [RBP + -160], XMM0
    MOVAPS oword [RBP + -144], XMM1
    MOVAPS oword [RBP + -128], XMM2
    MOVAPS oword [RBP + -112], XMM3
    MOVAPS oword [RBP + -96], XMM4
    MOVAPS oword [RBP + -80], XMM5
    MOVAPS oword [RBP + -64], XMM6
    MOVAPS oword [RBP + -48], XMM7
    JMP L100003193_2     ; inserted

; Entry 100003193; block 2; address 1000031c8
L100003193_2:
    MOV qword [RBP + -200], RSI
    MOV qword [RBP + -192], RDX
    MOV qword [RBP + -184], RCX
    MOV qword [RBP + -176], R8
    MOV qword [RBP + -168], R9
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -8], RAX
    CMP byte [rel L__DATA___bss + 28], 0
    JE L100003193_4
    JMP L100003193_3     ; inserted

; Entry 100003193; block 3; address 100003202
L100003193_3:
    CMP dword [rel L__DATA___bss + 44], 0
    JNE L100003193_6
    JMP L100003193_5     ; inserted

; Entry 100003193; block 4; address 10000322e
L100003193_4:
    LEA RAX, [RBP + -208]
    LEA RSI, [RBP + -32]
    MOV qword [RSI + 16], RAX
    LEA RAX, [RBP + 16]
    MOV qword [RSI + 8], RAX
    MOV RAX, 206158430216
    MOV qword [RSI], RAX
    CALL _vwarnx
    JMP L100003193_9     ; inserted

; Entry 100003193; block 5; address 10000320b
L100003193_5:
    MOV dword [rel L__DATA___bss + 44], 1
    JMP L100003193_6     ; inserted

; Entry 100003193; block 6; address 100003215
L100003193_6:
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    CMP RAX, qword [RBP + -8]
    JNE L100003193_8
    JMP L100003193_7     ; inserted

; Entry 100003193; block 7; address 100003225
L100003193_7:
    ADD RSP, 208
    POP RBP
    RET 

; Entry 100003193; block 8; address 100003259
L100003193_8:
    CALL ___stack_chk_fail

; Entry 100003193; block 9; address 100003257
L100003193_9:
    JMP L100003193_3



; ---------------------
; Function: 0x10000325e
; ---------------------
; Entry 10000325e; block 0; address 10000325e
L10000325e_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 208
    TEST AL, AL
    JE L10000325e_2
    JMP L10000325e_1     ; inserted

; Entry 10000325e; block 1; address 10000326d
L10000325e_1:
    MOVAPS oword [RBP + -160], XMM0
    MOVAPS oword [RBP + -144], XMM1
    MOVAPS oword [RBP + -128], XMM2
    MOVAPS oword [RBP + -112], XMM3
    MOVAPS oword [RBP + -96], XMM4
    MOVAPS oword [RBP + -80], XMM5
    MOVAPS oword [RBP + -64], XMM6
    MOVAPS oword [RBP + -48], XMM7
    JMP L10000325e_2     ; inserted

; Entry 10000325e; block 2; address 100003293
L10000325e_2:
    MOV qword [RBP + -200], RSI
    MOV qword [RBP + -192], RDX
    MOV qword [RBP + -184], RCX
    MOV qword [RBP + -176], R8
    MOV qword [RBP + -168], R9
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -8], RAX
    CMP byte [rel L__DATA___bss + 28], 0
    JE L10000325e_4
    JMP L10000325e_3     ; inserted

; Entry 10000325e; block 3; address 1000032cd
L10000325e_3:
    CMP dword [rel L__DATA___bss + 44], 0
    JNE L10000325e_6
    JMP L10000325e_5     ; inserted

; Entry 10000325e; block 4; address 1000032f9
L10000325e_4:
    LEA RAX, [RBP + -208]
    LEA RSI, [RBP + -32]
    MOV qword [RSI + 16], RAX
    LEA RAX, [RBP + 16]
    MOV qword [RSI + 8], RAX
    MOV RAX, 206158430216
    MOV qword [RSI], RAX
    CALL _vwarn
    JMP L10000325e_9     ; inserted

; Entry 10000325e; block 5; address 1000032d6
L10000325e_5:
    MOV dword [rel L__DATA___bss + 44], 1
    JMP L10000325e_6     ; inserted

; Entry 10000325e; block 6; address 1000032e0
L10000325e_6:
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    CMP RAX, qword [RBP + -8]
    JNE L10000325e_8
    JMP L10000325e_7     ; inserted

; Entry 10000325e; block 7; address 1000032f0
L10000325e_7:
    ADD RSP, 208
    POP RBP
    RET 

; Entry 10000325e; block 8; address 100003324
L10000325e_8:
    CALL ___stack_chk_fail

; Entry 10000325e; block 9; address 100003322
L10000325e_9:
    JMP L10000325e_3



; ---------------------
; Function: 0x100003329
; ---------------------
; Entry 100003329; block 0; address 100003329
L100003329_0:
    PUSH RBP
    MOV RBP, RSP
    CMP byte [RDI], 31
    JNE L100003329_2
    JMP L100003329_1     ; inserted

; Entry 100003329; block 1; address 100003332
L100003329_1:
    MOV CL, byte [RDI + 1]
    XOR EAX, EAX
    CMP CL, 139
    JE L100003329_4
    JMP L100003329_3     ; inserted

; Entry 100003329; block 2; address 100003341
L100003329_2:
    MOVZX EAX, word [RDI]
    XOR EAX, 23106
    MOVZX ECX, byte [RDI + 2]
    XOR ECX, 104
    OR CX, AX
    JNE L100003329_6
    JMP L100003329_5     ; inserted

; Entry 100003329; block 3; address 10000333c
L100003329_3:
    CMP CL, 158
    JE L100003329_4
    JMP L100003329_2     ; inserted

; Entry 100003329; block 4; address 100003397
L100003329_4:
    POP RBP
    RET 

; Entry 100003329; block 5; address 100003355
L100003329_5:
    MOV CL, byte [RDI + 3]
    ADD CL, 208
    MOV EAX, 1
    CMP CL, 10
    JB L100003329_4
    JMP L100003329_6     ; inserted

; Entry 100003329; block 6; address 100003365
L100003329_6:
    MOVZX EAX, word [RDI]
    CMP EAX, 40223
    JE L100003329_8
    JMP L100003329_7     ; inserted

; Entry 100003329; block 7; address 10000336f
L100003329_7:
    MOVZX EAX, word [RDI]
    CMP EAX, 7711
    JE L100003329_10
    JMP L100003329_9     ; inserted

; Entry 100003329; block 8; address 10000338b
L100003329_8:
    MOV EAX, 2
    JMP L100003329_4

; Entry 100003329; block 9; address 100003379
L100003329_9:
    XOR EAX, EAX
    CMP dword [RDI], 1484404733
    SETNE AL
    ADD EAX, EAX
    ADD EAX, 4
    JMP L100003329_4

; Entry 100003329; block 10; address 100003392
L100003329_10:
    MOV EAX, 3
    JMP L100003329_4     ; inserted



; ---------------------
; Function: 0x100003399
; ---------------------
; Entry 100003399; block 0; address 100003399
L100003399_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 65544
    MOV R15D, ECX
    MOV R14, RDX
    MOV R13, RSI
    MOV RSI, RDI
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -48], RAX
    MOV EDI, 1
    MOV RDX, R13
    CALL _write
    JMP L100003399_1     ; inserted

; Entry 100003399; block 1; address 1000033d4
L100003399_1:
    CMP RAX, 18446744073709551615
    JE L100003399_3
    JMP L100003399_2     ; inserted

; Entry 100003399; block 2; address 1000033da
L100003399_2:
    CMP RAX, R13
    JNE L100003399_3
    JMP L100003399_4     ; inserted

; Entry 100003399; block 3; address 100003439
L100003399_3:
    LEA RDI, [rel L__TEXT___cstring_493]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L100003399_16     ; inserted

; Entry 100003399; block 4; address 1000033df
L100003399_4:
    LEA RSI, [RBP + -65584]
    MOV EDX, 65536
    MOV EDI, R15D
    CALL _read
    JMP L100003399_5     ; inserted

; Entry 100003399; block 5; address 1000033f3
L100003399_5:
    TEST RAX, RAX
    JE L100003399_7
    JMP L100003399_6     ; inserted

; Entry 100003399; block 6; address 1000033f8
L100003399_6:
    MOV RBX, RAX
    LEA R12, [RBP + -65584]
    JMP L100003399_14     ; inserted

; Entry 100003399; block 7; address 100003471
L100003399_7:
    TEST R14, R14
    JE L100003399_19
    JMP L100003399_18     ; inserted

; Entry 100003399; block 8; address 100003407
L100003399_8:
    MOV EDI, 1
    MOV RSI, R12
    MOV RDX, RBX
    CALL _write
    JMP L100003399_10     ; inserted

; Entry 100003399; block 9; address 100003450
L100003399_9:
    LEA RDI, [rel L__TEXT___cstring_509]
    MOV ESI, R15D
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L100003399_17     ; inserted

; Entry 100003399; block 10; address 100003417
L100003399_10:
    CMP RAX, RBX
    JNE L100003399_12
    JMP L100003399_11     ; inserted

; Entry 100003399; block 11; address 10000341c
L100003399_11:
    ADD R13, RBX
    MOV EDX, 65536
    MOV EDI, R15D
    MOV RSI, R12
    CALL _read
    JMP L100003399_13     ; inserted

; Entry 100003399; block 12; address 100003463
L100003399_12:
    LEA RDI, [rel L__TEXT___cstring_493]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L100003399_7     ; inserted

; Entry 100003399; block 13; address 10000342f
L100003399_13:
    MOV RBX, RAX
    TEST RAX, RAX
    JNE L100003399_14
    JMP L100003399_15     ; inserted

; Entry 100003399; block 14; address 100003402
L100003399_14:
    TEST RBX, RBX
    JS L100003399_9
    JMP L100003399_8     ; inserted

; Entry 100003399; block 15; address 100003437
L100003399_15:
    JMP L100003399_7

; Entry 100003399; block 16; address 100003447
L100003399_16:
    MOV R13, 18446744073709551615
    JMP L100003399_19

; Entry 100003399; block 17; address 100003461
L100003399_17:
    JMP L100003399_7

; Entry 100003399; block 18; address 100003476
L100003399_18:
    MOV qword [R14], R13
    JMP L100003399_19     ; inserted

; Entry 100003399; block 19; address 100003479
L100003399_19:
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    CMP RAX, qword [RBP + -48]
    JNE L100003399_21
    JMP L100003399_20     ; inserted

; Entry 100003399; block 20; address 100003489
L100003399_20:
    MOV RAX, R13
    ADD RSP, 65544
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100003399; block 21; address 10000349e
L100003399_21:
    CALL ___stack_chk_fail



; ---------------------
; Function: 0x1000034a3
; ---------------------
; Entry 1000034a3; block 0; address 1000034a3
L1000034a3_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 200
    MOV R13, R9
    MOV R15, R8
    MOV R12, RCX
    MOV R14, RDX
    MOV dword [RBP + -108], ESI
    MOV dword [RBP + -112], EDI
    MOV EDI, 65536
    CALL _malloc
    JMP L1000034a3_1     ; inserted

; Entry 1000034a3; block 1; address 1000034d3
L1000034a3_1:
    TEST RAX, RAX
    JE L1000034a3_3
    JMP L1000034a3_2     ; inserted

; Entry 1000034a3; block 2; address 1000034dc
L1000034a3_2:
    MOV RBX, RAX
    MOV qword [RBP + -232], R13
    MOV qword [RBP + -240], R15
    MOV EDI, 65536
    CALL _malloc
    JMP L1000034a3_4     ; inserted

; Entry 1000034a3; block 3; address 100003bba
L1000034a3_3:
    LEA RDI, [rel L__TEXT___cstring_525]
    XOR EAX, EAX
    CALL L100003102_0

; Entry 1000034a3; block 4; address 1000034f7
L1000034a3_4:
    TEST RAX, RAX
    JE L1000034a3_3
    JMP L1000034a3_5     ; inserted

; Entry 1000034a3; block 5; address 100003500
L1000034a3_5:
    MOV R15, RAX
    XORPS XMM0, XMM0
    LEA RAX, [RBP + -224]
    MOVAPS oword [RAX], XMM0
    MOVAPS oword [RAX + 32], XMM0
    MOVAPS oword [RAX + 16], XMM0
    MOVAPS oword [RAX + 80], XMM0
    MOVAPS oword [RAX + 96], XMM0
    MOVAPS oword [RAX + 64], XMM0
    MOVAPS oword [RAX + 48], XMM0
    MOV qword [RBP + -104], R12
    MOV dword [RAX + 8], R12D
    MOV qword [RAX], R14
    MOV dword [RAX + 32], 65536
    MOV qword [RBP + -96], RBX
    MOV qword [RAX + 24], RBX
    MOV qword [RAX + 80], 0
    MOV dword [RBP + -72], 4294967294
    LEA RBX, [rel L__TEXT___text + 6572]
    XOR R12D, R12D
    MOV dword [RBP + -60], 0
    MOV dword [RBP + -44], 0
    XOR R13D, R13D
    XOR EAX, EAX
    MOV qword [RBP + -88], RAX
    XOR EAX, EAX
    MOV qword [RBP + -80], RAX
    XOR R14D, R14D
    XOR EAX, EAX
    MOV qword [RBP + -56], RAX
    JMP L1000034a3_19     ; inserted

; Entry 1000034a3; block 6; address 10000359d
L1000034a3_6:
    OR DL, AL
    JE L1000034a3_7
    JMP L1000034a3_8     ; inserted

; Entry 1000034a3; block 7; address 10000360f
L1000034a3_7:
    TEST ECX, ECX
    JE L1000034a3_18
    JMP L1000034a3_17     ; inserted

; Entry 1000034a3; block 8; address 1000035a1
L1000034a3_8:
    XOR R13D, R13D
    MOV EAX, 0
    TEST ECX, ECX
    JE L1000034a3_10
    JMP L1000034a3_9     ; inserted

; Entry 1000034a3; block 9; address 1000035ad
L1000034a3_9:
    MOV RSI, qword [RBP + -224]
    MOV EDX, ECX
    MOV ECX, 65536
    MOV RDI, R15
    CALL ___memmove_chk
    JMP L1000034a3_11     ; inserted

; Entry 1000034a3; block 10; address 1000035c9
L1000034a3_10:
    MOV qword [RBP + -224], R15
    MOV ESI, EAX
    ADD RSI, R15
    MOV EDX, 65536
    SUB EDX, EAX
    MOV EDI, dword [RBP + -112]
    CALL _read
    JMP L1000034a3_12     ; inserted

; Entry 1000034a3; block 11; address 1000035c3
L1000034a3_11:
    MOV EAX, dword [RBP + -216]
    JMP L1000034a3_10     ; inserted

; Entry 1000034a3; block 12; address 1000035e4
L1000034a3_12:
    TEST RAX, RAX
    JE L1000034a3_14
    JMP L1000034a3_13     ; inserted

; Entry 1000034a3; block 13; address 1000035e9
L1000034a3_13:
    CMP RAX, 18446744073709551615
    JNE L1000034a3_16
    JMP L1000034a3_15     ; inserted

; Entry 1000034a3; block 14; address 1000035f4
L1000034a3_14:
    MOV R13D, 1
    JMP L1000034a3_16     ; inserted

; Entry 1000034a3; block 15; address 1000035ef
L1000034a3_15:
    LEA RDI, [rel L__TEXT___cstring_539]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L1000034a3_28     ; inserted

; Entry 1000034a3; block 16; address 1000035fa
L1000034a3_16:
    MOV ECX, dword [RBP + -216]
    ADD ECX, EAX
    MOV dword [RBP + -216], ECX
    ADD qword [RBP + -104], RAX
    XOR R14D, R14D
    JMP L1000034a3_7     ; inserted

; Entry 1000034a3; block 17; address 100003617
L1000034a3_17:
    MOV EAX, R12D
    CMP R12D, 15
    JA L1000034a3_19
    JMP L1000034a3_20     ; inserted

; Entry 1000034a3; block 18; address 100003a56
L1000034a3_18:
    TEST R12D, R12D
    JE L1000034a3_25
    JMP L1000034a3_24     ; inserted

; Entry 1000034a3; block 19; address 100003581
L1000034a3_19:
    MOV EAX, R13D
    MOV ECX, dword [RBP + -216]
    MOV R13D, EAX
    TEST ECX, ECX
    SETE AL
    TEST R14D, R14D
    SETNE DL
    TEST R13D, R13D
    JNE L1000034a3_7
    JMP L1000034a3_6     ; inserted

; Entry 1000034a3; block 20; address 100003624
L1000034a3_20:
    MOVSXD RAX, dword [RBX + RAX * 4]
    ADD RAX, RBX
    MOV RAX, qword [RBP + -224]
    CMP byte [RAX], 31
    JNE L1000034a3_22
    JMP L1000034a3_21     ; inserted

; Entry 1000034a3; block 21; address 1000036c3
L1000034a3_21:
    INC RAX
    MOV qword [RBP + -224], RAX
    DEC ECX
    MOV dword [RBP + -216], ECX
    XOR EAX, EAX
    MOV qword [RBP + -80], RAX
    XOR EDI, EDI
    XOR ESI, ESI
    XOR EDX, EDX
    CALL _crc32
    JMP L1000034a3_23     ; inserted

; Entry 1000034a3; block 22; address 100003af1
L1000034a3_22:
    CMP qword [RBP + -104], 0
    JLE L1000034a3_36
    JMP L1000034a3_35     ; inserted

; Entry 1000034a3; block 23; address 1000036e6
L1000034a3_23:
    MOV qword [RBP + -88], RAX
    MOV R12D, 1
    JMP L1000034a3_19

; Entry 1000034a3; block 24; address 100003a5b
L1000034a3_24:
    TEST R13D, R13D
    JE L1000034a3_25
    JMP L1000034a3_26     ; inserted

; Entry 1000034a3; block 25; address 100003aa5
L1000034a3_25:
    CMP R12D, 13
    JB L1000034a3_30
    JMP L1000034a3_29     ; inserted

; Entry 1000034a3; block 26; address 100003a60
L1000034a3_26:
    LEA RDI, [rel L__TEXT___cstring_560]
    MOV RSI, qword [RBP + -232]
    XOR EAX, EAX
    CALL L100003193_0
    JMP L1000034a3_27     ; inserted

; Entry 1000034a3; block 27; address 100003a75
L1000034a3_27:
    JMP L1000034a3_28

; Entry 1000034a3; block 28; address 100003a9d
L1000034a3_28:
    MOV qword [RBP + -56], 18446744073709551615
    JMP L1000034a3_25     ; inserted

; Entry 1000034a3; block 29; address 100003aab
L1000034a3_29:
    LEA RDI, [RBP + -224]
    CALL _inflateEnd
    JMP L1000034a3_30     ; inserted

; Entry 1000034a3; block 30; address 100003ab7
L1000034a3_30:
    MOV RDI, R15
    CALL _free
    JMP L1000034a3_31     ; inserted

; Entry 1000034a3; block 31; address 100003abf
L1000034a3_31:
    MOV RDI, qword [RBP + -96]
    CALL _free
    JMP L1000034a3_32     ; inserted

; Entry 1000034a3; block 32; address 100003ac8
L1000034a3_32:
    MOV RAX, qword [RBP + -240]
    TEST RAX, RAX
    JE L1000034a3_34
    JMP L1000034a3_33     ; inserted

; Entry 1000034a3; block 33; address 100003ad4
L1000034a3_33:
    MOV RCX, qword [RBP + -104]
    MOV qword [RAX], RCX
    JMP L1000034a3_34     ; inserted

; Entry 1000034a3; block 34; address 100003adb
L1000034a3_34:
    MOV RAX, qword [RBP + -56]
    ADD RSP, 200
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 1000034a3; block 35; address 100003afc
L1000034a3_35:
    LEA RDI, [rel L__TEXT___cstring_587]
    MOV RSI, qword [RBP + -232]
    XOR EAX, EAX
    CALL L100003193_0
    JMP L1000034a3_37     ; inserted

; Entry 1000034a3; block 36; address 100003b8c
L1000034a3_36:
    LEA RDI, [rel L__TEXT___cstring_616]
    XOR EAX, EAX
    CALL L100003193_0
    JMP L1000034a3_38     ; inserted

; Entry 1000034a3; block 37; address 100003b11
L1000034a3_37:
    MOV dword [rel L__DATA___bss + 44], 2
    JMP L1000034a3_30

; Entry 1000034a3; block 38; address 100003b9a
L1000034a3_38:
    MOV qword [RBP + -56], 18446744073709551615
    JMP L1000034a3_30



; ---------------------
; Function: 0x100003c24
; ---------------------
; Entry 100003c24; block 0; address 100003c24
L100003c24_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 104
    MOV R15, R8
    MOV R14, RCX
    MOV R12, RDX
    MOV dword [RBP + -48], ESI
    MOV dword [RBP + -52], EDI
    MOV RBX, qword [rel L__DATA___bss + 56]
    TEST RBX, RBX
    JNE L100003c24_2
    JMP L100003c24_1     ; inserted

; Entry 100003c24; block 1; address 100003c50
L100003c24_1:
    MOV EDI, 65536
    CALL _malloc
    JMP L100003c24_3     ; inserted

; Entry 100003c24; block 2; address 100003c64
L100003c24_2:
    MOV RAX, qword [rel L__DATA___bss + 64]
    TEST RAX, RAX
    JNE L100003c24_5
    JMP L100003c24_4     ; inserted

; Entry 100003c24; block 3; address 100003c5a
L100003c24_3:
    MOV RBX, RAX
    MOV qword [rel L__DATA___bss + 56], RAX
    JMP L100003c24_2     ; inserted

; Entry 100003c24; block 4; address 100003c70
L100003c24_4:
    MOV EDI, 65536
    CALL _malloc
    JMP L100003c24_6     ; inserted

; Entry 100003c24; block 5; address 100003c81
L100003c24_5:
    TEST RBX, RBX
    JE L100003c24_8
    JMP L100003c24_7     ; inserted

; Entry 100003c24; block 6; address 100003c7a
L100003c24_6:
    MOV qword [rel L__DATA___bss + 64], RAX
    JMP L100003c24_5     ; inserted

; Entry 100003c24; block 7; address 100003c8a
L100003c24_7:
    TEST RAX, RAX
    JE L100003c24_8
    JMP L100003c24_9     ; inserted

; Entry 100003c24; block 8; address 100003ec2
L100003c24_8:
    LEA RDI, [rel L__TEXT___cstring_332]
    JMP L100003c24_55

; Entry 100003c24; block 9; address 100003c93
L100003c24_9:
    XORPS XMM0, XMM0
    LEA RDI, [RBP + -144]
    MOVUPS oword [RDI + 56], XMM0
    MOV qword [RDI + 72], 0
    XOR ESI, ESI
    XOR EDX, EDX
    CALL _BZ2_bzDecompressInit
    JMP L100003c24_10     ; inserted

; Entry 100003c24; block 10; address 100003cb2
L100003c24_10:
    TEST EAX, EAX
    JNE L100003c24_12
    JMP L100003c24_11     ; inserted

; Entry 100003c24; block 11; address 100003cba
L100003c24_11:
    MOV dword [RBP + -136], R14D
    MOV qword [RBP + -144], R12
    TEST R15, R15
    JE L100003c24_14
    JMP L100003c24_13     ; inserted

; Entry 100003c24; block 12; address 100003ecb
L100003c24_12:
    LEA RDI, [rel L__TEXT___cstring_972]
    JMP L100003c24_56     ; inserted

; Entry 100003c24; block 13; address 100003ccd
L100003c24_13:
    MOV qword [R15], R14
    JMP L100003c24_14     ; inserted

; Entry 100003c24; block 14; address 100003cd0
L100003c24_14:
    LEA R12, [RBP + -144]
    LEA RBX, [rel L__TEXT___text + 7376]
    XOR R14D, R14D
    MOV dword [RBP + -44], 0
    XOR EAX, EAX
    MOV qword [RBP + -64], RAX
    XOR R13D, R13D
    JMP L100003c24_31     ; inserted

; Entry 100003c24; block 15; address 100003cfa
L100003c24_15:
    MOV EAX, dword [RBP + -136]
    OR EAX, R14D
    JNE L100003c24_18
    JMP L100003c24_17     ; inserted

; Entry 100003c24; block 16; address 100003e79
L100003c24_16:
    CMP R13D, 4
    JNE L100003c24_52
    JMP L100003c24_51     ; inserted

; Entry 100003c24; block 17; address 100003d05
L100003c24_17:
    MOV RSI, qword [rel L__DATA___bss + 56]
    MOV EDX, 65536
    MOV EDI, dword [RBP + -52]
    CALL _read
    JMP L100003c24_19     ; inserted

; Entry 100003c24; block 18; address 100003d45
L100003c24_18:
    MOV RAX, qword [rel L__DATA___bss + 64]
    MOV qword [RBP + -120], RAX
    MOV dword [RBP + -112], 65536
    MOV RDI, R12
    CALL _BZ2_bzDecompress
    JMP L100003c24_23     ; inserted

; Entry 100003c24; block 19; address 100003d19
L100003c24_19:
    TEST RAX, RAX
    JS L100003c24_21
    JMP L100003c24_20     ; inserted

; Entry 100003c24; block 20; address 100003d22
L100003c24_20:
    SETE CL
    MOVZX R14D, CL
    MOV RCX, qword [rel L__DATA___bss + 56]
    MOV qword [RBP + -144], RCX
    MOV dword [RBP + -136], EAX
    TEST R15, R15
    JE L100003c24_18
    JMP L100003c24_22     ; inserted

; Entry 100003c24; block 21; address 100003eb4
L100003c24_21:
    LEA RDI, [rel L__TEXT___cstring_983]
    JMP L100003c24_55     ; inserted

; Entry 100003c24; block 22; address 100003d42
L100003c24_22:
    ADD qword [R15], RAX
    JMP L100003c24_18     ; inserted

; Entry 100003c24; block 23; address 100003d5f
L100003c24_23:
    MOV R13D, EAX
    LEA EAX, [R13 + 5]
    CMP EAX, 9
    JA L100003c24_25
    JMP L100003c24_24     ; inserted

; Entry 100003c24; block 24; address 100003d6b
L100003c24_24:
    MOVSXD RAX, dword [RBX + RAX * 4]
    ADD RAX, RBX
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100003d72 + 8*EAX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 100003d74,100003d8d,100003da6,100003dbf,100003dd5

; Entry 100003c24; block 25; address 100003dbf
L100003c24_25:
    LEA RDI, [rel L__TEXT___cstring_1095]
    MOV ESI, R13D
    XOR EAX, EAX
    CALL L100003193_0
    JMP L100003c24_34     ; inserted

; Entry 100003c24; block 26; address 100003d74
L100003c24_26:
    LEA RDI, [rel L__TEXT___cstring_1050]
    XOR EAX, EAX
    CALL L100003193_0
    JMP L100003c24_30     ; inserted

; Entry 100003c24; block 27; address 100003d8d
L100003c24_27:
    LEA RDI, [rel L__TEXT___cstring_1023]
    XOR EAX, EAX
    CALL L100003193_0
    JMP L100003c24_32     ; inserted

; Entry 100003c24; block 28; address 100003da6
L100003c24_28:
    LEA RDI, [rel L__TEXT___cstring_1075]
    XOR EAX, EAX
    CALL L100003193_0
    JMP L100003c24_33     ; inserted

; Entry 100003c24; block 29; address 100003dd5
L100003c24_29:
    TEST R14D, R14D
    JE L100003c24_36
    JMP L100003c24_35     ; inserted

; Entry 100003c24; block 30; address 100003d82
L100003c24_30:
    MOV R13D, 4294967291
    JMP L100003c24_31

; Entry 100003c24; block 31; address 100003cf1
L100003c24_31:
    TEST R13D, R13D
    JNE L100003c24_16
    JMP L100003c24_15     ; inserted

; Entry 100003c24; block 32; address 100003d9b
L100003c24_32:
    MOV R13D, 4294967292
    JMP L100003c24_31

; Entry 100003c24; block 33; address 100003db4
L100003c24_33:
    MOV R13D, 4294967293
    JMP L100003c24_31

; Entry 100003c24; block 34; address 100003dd0
L100003c24_34:
    JMP L100003c24_31

; Entry 100003c24; block 35; address 100003dda
L100003c24_35:
    TEST R13D, R13D
    JNE L100003c24_36
    JMP L100003c24_37     ; inserted

; Entry 100003c24; block 36; address 100003def
L100003c24_36:
    CMP byte [rel L__DATA___bss + 36], 0
    JNE L100003c24_40
    JMP L100003c24_39     ; inserted

; Entry 100003c24; block 37; address 100003ddf
L100003c24_37:
    MOV R13D, 4
    CMP dword [RBP + -44], 1
    JNE L100003c24_38
    JMP L100003c24_36     ; inserted

; Entry 100003c24; block 38; address 100003ed9
L100003c24_38:
    LEA RDI, [rel L__TEXT___cstring_988]
    JMP L100003c24_56

; Entry 100003c24; block 39; address 100003df8
L100003c24_39:
    MOV EAX, dword [RBP + -112]
    CMP EAX, 65536
    JE L100003c24_40
    JMP L100003c24_41     ; inserted

; Entry 100003c24; block 40; address 100003e25
L100003c24_40:
    MOV dword [RBP + -44], 0
    TEST R14D, R14D
    JNE L100003c24_31
    JMP L100003c24_45     ; inserted

; Entry 100003c24; block 41; address 100003e02
L100003c24_41:
    MOV RSI, qword [rel L__DATA___bss + 64]
    MOV EDX, 65536
    SUB EDX, EAX
    MOV EDI, dword [RBP + -48]
    CALL _write
    JMP L100003c24_42     ; inserted

; Entry 100003c24; block 42; address 100003e18
L100003c24_42:
    TEST RAX, RAX
    JS L100003c24_44
    JMP L100003c24_43     ; inserted

; Entry 100003c24; block 43; address 100003e21
L100003c24_43:
    ADD qword [RBP + -64], RAX
    JMP L100003c24_40     ; inserted

; Entry 100003c24; block 44; address 100003ee2
L100003c24_44:
    LEA RDI, [rel L__TEXT___cstring_1003]
    JMP L100003c24_55

; Entry 100003c24; block 45; address 100003e35
L100003c24_45:
    MOV dword [RBP + -44], 0
    CMP R13D, 4
    JNE L100003c24_31
    JMP L100003c24_46     ; inserted

; Entry 100003c24; block 46; address 100003e46
L100003c24_46:
    MOV RDI, R12
    CALL _BZ2_bzDecompressEnd
    JMP L100003c24_47     ; inserted

; Entry 100003c24; block 47; address 100003e4e
L100003c24_47:
    TEST EAX, EAX
    JNE L100003c24_49
    JMP L100003c24_48     ; inserted

; Entry 100003c24; block 48; address 100003e52
L100003c24_48:
    XOR R13D, R13D
    MOV RDI, R12
    XOR ESI, ESI
    XOR EDX, EDX
    CALL _BZ2_bzDecompressInit
    JMP L100003c24_50     ; inserted

; Entry 100003c24; block 49; address 100003e70
L100003c24_49:
    LEA RDI, [rel L__TEXT___cstring_1009]
    JMP L100003c24_56

; Entry 100003c24; block 50; address 100003e61
L100003c24_50:
    MOV dword [RBP + -44], 1
    TEST EAX, EAX
    JE L100003c24_31
    JMP L100003c24_49     ; inserted

; Entry 100003c24; block 51; address 100003e7f
L100003c24_51:
    LEA RDI, [RBP + -144]
    CALL _BZ2_bzDecompressEnd
    JMP L100003c24_53     ; inserted

; Entry 100003c24; block 52; address 100003e9e
L100003c24_52:
    MOV RAX, 18446744073709551615
    JMP L100003c24_54     ; inserted

; Entry 100003c24; block 53; address 100003e8b
L100003c24_53:
    MOV ECX, EAX
    XOR EAX, EAX
    CMP ECX, 1
    SBB RAX, RAX
    NOT RAX
    OR RAX, qword [RBP + -64]
    JMP L100003c24_54

; Entry 100003c24; block 54; address 100003ea5
L100003c24_54:
    ADD RSP, 104
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100003c24; block 55; address 100003ebb
L100003c24_55:
    XOR EAX, EAX
    CALL L100003102_0

; Entry 100003c24; block 56; address 100003ed2
L100003c24_56:
    XOR EAX, EAX
    CALL L100004973_0



; ---------------------
; Function: 0x100003f14
; ---------------------
; Entry 100003f14; block 0; address 100003f14
L100003f14_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R14
    PUSH RBX
    MOV R14D, EDI
    MOV EDI, 1
    MOV ESI, 690208
    CALL _calloc
    JMP L100003f14_1     ; inserted

; Entry 100003f14; block 1; address 100003f2d
L100003f14_1:
    TEST RAX, RAX
    JE L100003f14_3
    JMP L100003f14_2     ; inserted

; Entry 100003f14; block 2; address 100003f32
L100003f14_2:
    MOV RBX, RAX
    MOV qword [RAX + 690056], 69001
    MOV dword [RAX + 690072], 128
    MOV qword [RAX + 690088], 10000
    MOV qword [RAX + 690104], 1
    LEA RSI, [rel L__TEXT___cstring_1119]
    MOV EDI, R14D
    CALL _fdopen
    JMP L100003f14_4     ; inserted

; Entry 100003f14; block 3; address 100003f9d
L100003f14_3:
    XOR EAX, EAX
    POP RBX
    POP R14
    POP RBP
    RET 

; Entry 100003f14; block 4; address 100003f6f
L100003f14_4:
    MOV qword [RBX], RAX
    TEST RAX, RAX
    JE L100003f14_6
    JMP L100003f14_5     ; inserted

; Entry 100003f14; block 5; address 100003f77
L100003f14_5:
    LEA RSI, [rel L__TEXT___text + 10216]
    LEA R8, [rel L__TEXT___text + 11576]
    MOV RDI, RBX
    XOR EDX, EDX
    XOR ECX, ECX
    POP RBX
    POP R14
    POP RBP
    JMP _funopen

; Entry 100003f14; block 6; address 100003f95
L100003f14_6:
    MOV RDI, RBX
    CALL _free
    JMP L100003f14_3     ; inserted



; ---------------------
; Function: 0x100003fa4
; ---------------------
; Entry 100003fa4; block 0; address 100003fa4
L100003fa4_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    PUSH RAX
    MOV qword [RBP + -48], R8
    MOV RBX, RCX
    MOV R14, RDX
    MOV R15, RSI
    MOV R12, RDI
    MOV EDI, 65536
    CALL _malloc
    JMP L100003fa4_1     ; inserted

; Entry 100003fa4; block 1; address 100003fcc
L100003fa4_1:
    TEST RAX, RAX
    JE L100003fa4_3
    JMP L100003fa4_2     ; inserted

; Entry 100003fa4; block 2; address 100003fd1
L100003fa4_2:
    MOV R13, RAX
    MOV qword [rel L__DATA___bss + 72], RBX
    TEST RBX, RBX
    CMOVE R14, RBX
    MOV qword [rel L__DATA___bss + 80], R14
    MOV ESI, 1
    MOV EDX, 65536
    MOV RDI, RAX
    MOV RCX, R12
    CALL _fread
    JMP L100003fa4_4     ; inserted

; Entry 100003fa4; block 3; address 10000404b
L100003fa4_3:
    MOV RBX, 18446744073709551615
    JMP L100003fa4_17

; Entry 100003fa4; block 4; address 100003ffe
L100003fa4_4:
    TEST RAX, RAX
    JE L100003fa4_6
    JMP L100003fa4_5     ; inserted

; Entry 100003fa4; block 5; address 100004003
L100003fa4_5:
    MOV R14, RAX
    XOR EBX, EBX
    JMP L100003fa4_12     ; inserted

; Entry 100003fa4; block 6; address 100004054
L100003fa4_6:
    XOR EBX, EBX
    JMP L100003fa4_14     ; inserted

; Entry 100003fa4; block 7; address 100004011
L100003fa4_7:
    MOV ESI, 1
    MOV RDI, R13
    MOV RDX, R14
    MOV RCX, R15
    CALL _fwrite
    JMP L100003fa4_9     ; inserted

; Entry 100003fa4; block 8; address 100004029
L100003fa4_8:
    ADD RBX, R14
    MOV ESI, 1
    MOV EDX, 65536
    MOV RDI, R13
    MOV RCX, R12
    CALL _fread
    JMP L100003fa4_11     ; inserted

; Entry 100003fa4; block 9; address 100004024
L100003fa4_9:
    CMP RAX, R14
    JNE L100003fa4_10
    JMP L100003fa4_8     ; inserted

; Entry 100003fa4; block 10; address 10000406b
L100003fa4_10:
    MOV RBX, 18446744073709551615
    JMP L100003fa4_16     ; inserted

; Entry 100003fa4; block 11; address 100004041
L100003fa4_11:
    MOV R14, RAX
    TEST RAX, RAX
    JNE L100003fa4_12
    JMP L100003fa4_13     ; inserted

; Entry 100003fa4; block 12; address 100004008
L100003fa4_12:
    CMP byte [rel L__DATA___bss + 36], 0
    JNE L100003fa4_8
    JMP L100003fa4_7     ; inserted

; Entry 100003fa4; block 13; address 100004049
L100003fa4_13:
    JMP L100003fa4_14

; Entry 100003fa4; block 14; address 100004056
L100003fa4_14:
    MOV RCX, qword [RBP + -48]
    TEST RCX, RCX
    JE L100003fa4_16
    JMP L100003fa4_15     ; inserted

; Entry 100003fa4; block 15; address 10000405f
L100003fa4_15:
    MOV RAX, qword [rel L__DATA___bss + 88]
    MOV qword [RCX], RAX
    JMP L100003fa4_16

; Entry 100003fa4; block 16; address 100004072
L100003fa4_16:
    MOV RDI, R13
    CALL _free
    JMP L100003fa4_17     ; inserted

; Entry 100003fa4; block 17; address 10000407a
L100003fa4_17:
    MOV RAX, RBX
    ADD RSP, 8
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 



; ---------------------
; Function: 0x10000408c
; ---------------------
; Entry 10000408c; block 0; address 10000408c
L10000408c_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 152
    MOV qword [RBP + -48], R8
    MOV RBX, RCX
    MOV R12, RDX
    MOV R13D, ESI
    CALL _dup
    JMP L10000408c_1     ; inserted

; Entry 10000408c; block 1; address 1000040b2
L10000408c_1:
    CMP EAX, 18446744073709551615
    JE L10000408c_3
    JMP L10000408c_2     ; inserted

; Entry 10000408c; block 2; address 1000040bb
L10000408c_2:
    MOV R14D, EAX
    MOV EDI, R13D
    CALL _dup
    JMP L10000408c_4     ; inserted

; Entry 10000408c; block 3; address 1000044d3
L10000408c_3:
    LEA RDI, [rel L__TEXT___cstring_1121]
    JMP L10000408c_85

; Entry 10000408c; block 4; address 1000040c6
L10000408c_4:
    CMP EAX, 18446744073709551615
    JE L10000408c_3
    JMP L10000408c_5     ; inserted

; Entry 10000408c; block 5; address 1000040cf
L10000408c_5:
    MOV R13D, EAX
    TEST RBX, RBX
    JE L10000408c_7
    JMP L10000408c_6     ; inserted

; Entry 10000408c; block 6; address 1000040d7
L10000408c_6:
    LEA RDI, [RBP + -71]
    MOV ECX, 7
    MOV RSI, R12
    MOV RDX, RBX
    CALL ___memcpy_chk
    JMP L10000408c_7     ; inserted

; Entry 10000408c; block 7; address 1000040eb
L10000408c_7:
    LEA RSI, [RBX + RBP]
    ADD RSI, 18446744073709551545
    MOV EDX, 7
    SUB RDX, RBX
    MOV EDI, R14D
    CALL _read
    JMP L10000408c_8     ; inserted

; Entry 10000408c; block 8; address 100004103
L10000408c_8:
    TEST RAX, RAX
    JS L10000408c_10
    JMP L10000408c_9     ; inserted

; Entry 10000408c; block 9; address 10000410c
L10000408c_9:
    MOV RAX, qword [RBP + -48]
    TEST RAX, RAX
    JE L10000408c_12
    JMP L10000408c_11     ; inserted

; Entry 10000408c; block 10; address 1000044dc
L10000408c_10:
    LEA RDI, [rel L__TEXT___cstring_1125]
    JMP L10000408c_85

; Entry 10000408c; block 11; address 100004115
L10000408c_11:
    ADD qword [RAX], 7
    JMP L10000408c_12     ; inserted

; Entry 10000408c; block 12; address 100004119
L10000408c_12:
    MOV qword [RBP + -120], 0
    XOR EAX, EAX
    XOR ECX, ECX
    JMP L10000408c_13     ; inserted

; Entry 10000408c; block 13; address 100004125
L10000408c_13:
    MOV RDX, RCX
    SHL RDX, 8
    MOVZX ECX, byte [RBP + RAX-1]
    OR RCX, RDX
    INC RAX
    CMP RAX, 4
    JNE L10000408c_13
    JMP L10000408c_14     ; inserted

; Entry 10000408c; block 14; address 10000413d
L10000408c_14:
    MOV qword [RBP + -120], RCX
    MOVZX EAX, byte [RBP + -65]
    MOV dword [RBP + -164], EAX
    MOV qword [RBP + -56], RAX
    DEC EAX
    CMP AL, 24
    JAE L10000408c_16
    JMP L10000408c_15     ; inserted

; Entry 10000408c; block 15; address 100004159
L10000408c_15:
    LEA RSI, [rel L__TEXT___cstring_1119]
    MOV EDI, R14D
    CALL _fdopen
    JMP L10000408c_17     ; inserted

; Entry 10000408c; block 16; address 1000044e5
L10000408c_16:
    LEA RDI, [rel L__TEXT___cstring_1151]
    JMP L10000408c_84

; Entry 10000408c; block 17; address 100004168
L10000408c_17:
    MOV qword [RBP + -112], RAX
    TEST RAX, RAX
    JE L10000408c_19
    JMP L10000408c_18     ; inserted

; Entry 10000408c; block 18; address 100004175
L10000408c_18:
    MOV R12, RAX
    LEA RSI, [rel L__TEXT___cstring_1212]
    MOV EDI, R13D
    CALL _fdopen
    JMP L10000408c_20     ; inserted

; Entry 10000408c; block 19; address 1000044ee
L10000408c_19:
    LEA RDI, [rel L__TEXT___cstring_1182]
    JMP L10000408c_85

; Entry 10000408c; block 20; address 100004187
L10000408c_20:
    MOV qword [RBP + -104], RAX
    TEST RAX, RAX
    JE L10000408c_22
    JMP L10000408c_21     ; inserted

; Entry 10000408c; block 21; address 100004194
L10000408c_21:
    MOV ESI, 4
    MOV R13, qword [RBP + -56]
    MOV RDI, R13
    CALL _calloc
    JMP L10000408c_23     ; inserted

; Entry 10000408c; block 22; address 1000044f7
L10000408c_22:
    LEA RDI, [rel L__TEXT___cstring_1214]
    JMP L10000408c_85

; Entry 10000408c; block 23; address 1000041a5
L10000408c_23:
    MOV R14, RAX
    MOV qword [RBP + -152], RAX
    MOV ESI, 4
    MOV RDI, R13
    CALL _calloc
    JMP L10000408c_24     ; inserted

; Entry 10000408c; block 24; address 1000041bc
L10000408c_24:
    MOV R15, RAX
    MOV qword [RBP + -160], RAX
    MOV ESI, 8
    MOV RDI, R13
    CALL _calloc
    JMP L10000408c_25     ; inserted

; Entry 10000408c; block 25; address 1000041d3
L10000408c_25:
    MOV qword [RBP + -64], RAX
    MOV qword [RBP + -128], RAX
    TEST R14, R14
    JE L10000408c_27
    JMP L10000408c_26     ; inserted

; Entry 10000408c; block 26; address 1000041e4
L10000408c_26:
    TEST R15, R15
    JE L10000408c_27
    JMP L10000408c_28     ; inserted

; Entry 10000408c; block 27; address 1000044ca
L10000408c_27:
    LEA RDI, [rel L__TEXT___cstring_1245]
    JMP L10000408c_85

; Entry 10000408c; block 28; address 1000041ed
L10000408c_28:
    CMP qword [RBP + -64], 0
    JE L10000408c_27
    JMP L10000408c_29     ; inserted

; Entry 10000408c; block 29; address 1000041f8
L10000408c_29:
    XOR EBX, EBX
    DEC R13
    MOV dword [RBP + -164], R13D
    MOV dword [RBP + -168], 1
    MOV EAX, 0
    MOV qword [RBP + -88], R13
    CMOVNS RAX, R13
    MOV qword [RBP + -96], RAX
    LEA R14, [RAX + 1]
    MOV R13D, 1
    JMP L10000408c_33     ; inserted

; Entry 10000408c; block 30; address 100004231
L10000408c_30:
    CMP EAX, 18446744073709551615
    JE L10000408c_32
    JMP L10000408c_31     ; inserted

; Entry 10000408c; block 31; address 10000423a
L10000408c_31:
    MOVZX EAX, AL
    MOV dword [R15 + RBX * 4], EAX
    ADD R13D, EAX
    INC RBX
    CMP R14, RBX
    JNE L10000408c_33
    JMP L10000408c_34     ; inserted

; Entry 10000408c; block 32; address 1000044bc
L10000408c_32:
    LEA RDI, [rel L__TEXT___cstring_1252]
    JMP L10000408c_85     ; inserted

; Entry 10000408c; block 33; address 100004229
L10000408c_33:
    MOV RDI, R12
    CALL _fgetc
    JMP L10000408c_30     ; inserted

; Entry 10000408c; block 34; address 10000424c
L10000408c_34:
    MOV dword [RBP + -168], R13D
    MOV RAX, qword [RBP + -48]
    TEST RAX, RAX
    JE L10000408c_36
    JMP L10000408c_35     ; inserted

; Entry 10000408c; block 35; address 10000425c
L10000408c_35:
    MOV RCX, qword [RBP + -88]
    ADD qword [RAX], RCX
    JMP L10000408c_36     ; inserted

; Entry 10000408c; block 36; address 100004263
L10000408c_36:
    CMP R13D, 257
    JAE L10000408c_38
    JMP L10000408c_37     ; inserted

; Entry 10000408c; block 37; address 100004270
L10000408c_37:
    MOV ESI, R13D
    MOV EDI, 1
    CALL _calloc
    JMP L10000408c_39     ; inserted

; Entry 10000408c; block 38; address 100004500
L10000408c_38:
    LEA RDI, [rel L__TEXT___cstring_1281]
    JMP L10000408c_84

; Entry 10000408c; block 39; address 10000427d
L10000408c_39:
    MOV qword [RBP + -144], RAX
    MOV qword [RBP + -136], RAX
    TEST RAX, RAX
    JE L10000408c_27
    JMP L10000408c_40     ; inserted

; Entry 10000408c; block 40; address 100004294
L10000408c_40:
    MOV R14, RAX
    MOV RCX, qword [RBP + -56]
    MOV EAX, dword [R15 + RCX * 4 + -4]
    INC EAX
    MOV dword [R15 + RCX * 4 + -4], EAX
    TEST CL, CL
    JE L10000408c_42
    JMP L10000408c_41     ; inserted

; Entry 10000408c; block 41; address 1000042ab
L10000408c_41:
    XOR R13D, R13D
    MOV RCX, qword [RBP + -48]
    JMP L10000408c_52     ; inserted

; Entry 10000408c; block 42; address 100004316
L10000408c_42:
    INC EAX
    MOV dword [R15 + RCX * 4 + -4], EAX
    LEA RDI, [RBP + -168]
    XOR R14D, R14D
    XOR ESI, ESI
    CALL L100004f61_0
    JMP L10000408c_54     ; inserted

; Entry 10000408c; block 43; address 1000042c2
L10000408c_43:
    XOR EBX, EBX
    JMP L10000408c_48     ; inserted

; Entry 10000408c; block 44; address 1000042eb
L10000408c_44:
    TEST RCX, RCX
    JE L10000408c_51
    JMP L10000408c_50     ; inserted

; Entry 10000408c; block 45; address 1000042cc
L10000408c_45:
    CMP EAX, 18446744073709551615
    JE L10000408c_47
    JMP L10000408c_46     ; inserted

; Entry 10000408c; block 46; address 1000042d5
L10000408c_46:
    MOV byte [R14 + RBX], AL
    MOV EAX, dword [R15 + R13 * 4]
    INC RBX
    CMP EBX, EAX
    JL L10000408c_48
    JMP L10000408c_49     ; inserted

; Entry 10000408c; block 47; address 1000044a5
L10000408c_47:
    LEA RDI, [rel L__TEXT___cstring_1298]
    JMP L10000408c_84     ; inserted

; Entry 10000408c; block 48; address 1000042c4
L10000408c_48:
    MOV RDI, R12
    CALL _fgetc
    JMP L10000408c_45     ; inserted

; Entry 10000408c; block 49; address 1000042e4
L10000408c_49:
    ADD R14, RBX
    MOV RCX, qword [RBP + -48]
    JMP L10000408c_44     ; inserted

; Entry 10000408c; block 50; address 1000042f0
L10000408c_50:
    CDQE 
    ADD qword [RCX], RAX
    JMP L10000408c_51     ; inserted

; Entry 10000408c; block 51; address 1000042f5
L10000408c_51:
    LEA RAX, [R13 + 1]
    CMP R13, qword [RBP + -96]
    MOV R13, RAX
    JNE L10000408c_52
    JMP L10000408c_53     ; inserted

; Entry 10000408c; block 52; address 1000042b2
L10000408c_52:
    MOV RAX, qword [RBP + -64]
    MOV qword [RAX + R13 * 8], R14
    MOV EAX, dword [R15 + R13 * 4]
    TEST EAX, EAX
    JLE L10000408c_44
    JMP L10000408c_43     ; inserted

; Entry 10000408c; block 53; address 100004302
L10000408c_53:
    MOV qword [RBP + -136], R14
    MOV RCX, qword [RBP + -56]
    MOV EAX, dword [R15 + RCX * 4 + -4]
    MOV R12, qword [RBP + -112]
    JMP L10000408c_42     ; inserted

; Entry 10000408c; block 54; address 10000432e
L10000408c_54:
    MOV RDI, R12
    CALL _fgetc
    JMP L10000408c_55     ; inserted

; Entry 10000408c; block 55; address 100004336
L10000408c_55:
    CMP EAX, 18446744073709551615
    JE L10000408c_57
    JMP L10000408c_56     ; inserted

; Entry 10000408c; block 56; address 10000433f
L10000408c_56:
    MOV R15D, EAX
    MOV RAX, qword [RBP + -152]
    MOV qword [RBP + -64], RAX
    MOV RAX, qword [RBP + -160]
    MOV qword [RBP + -96], RAX
    MOV RAX, qword [RBP + -128]
    MOV qword [RBP + -88], RAX
    MOV RAX, qword [RBP + -136]
    MOV qword [RBP + -192], RAX
    MOV RAX, qword [RBP + -120]
    MOV qword [RBP + -80], RAX
    MOV RAX, qword [RBP + -104]
    MOV qword [RBP + -184], RAX
    MOV EAX, dword [RBP + -164]
    MOV dword [RBP + -56], EAX
    MOV RAX, qword [RBP + -112]
    MOV qword [RBP + -176], RAX
    XOR EBX, EBX
    XOR R12D, R12D
    JMP L10000408c_73     ; inserted

; Entry 10000408c; block 57; address 100004442
L10000408c_57:
    MOV R14, qword [RBP + -120]
    XOR EBX, EBX
    JMP L10000408c_75     ; inserted

; Entry 10000408c; block 58; address 1000043a3
L10000408c_58:
    INC qword [RAX]
    JMP L10000408c_59     ; inserted

; Entry 10000408c; block 59; address 1000043a6
L10000408c_59:
    MOV R13D, 7
    JMP L10000408c_70     ; inserted

; Entry 10000408c; block 60; address 1000043ca
L10000408c_60:
    CMP R14D, dword [RBP + -56]
    JGE L10000408c_63
    JMP L10000408c_62     ; inserted

; Entry 10000408c; block 61; address 1000043d9
L10000408c_61:
    MOV RDX, qword [RBP + -96]
    CMP ECX, dword [RDX + RAX * 4]
    JG L10000408c_63
    JMP L10000408c_64     ; inserted

; Entry 10000408c; block 62; address 1000043d4
L10000408c_62:
    INC R14D
    JMP L10000408c_69

; Entry 10000408c; block 63; address 1000044b3
L10000408c_63:
    LEA RDI, [rel L__TEXT___cstring_1321]
    JMP L10000408c_84

; Entry 10000408c; block 64; address 1000043e6
L10000408c_64:
    MOVSXD RCX, ECX
    MOV RDX, qword [RBP + -88]
    ADD RCX, qword [RDX + RAX * 8]
    CMP RCX, qword [RBP + -192]
    JNE L10000408c_66
    JMP L10000408c_65     ; inserted

; Entry 10000408c; block 65; address 1000043fa
L10000408c_65:
    CMP RBX, qword [RBP + -80]
    JE L10000408c_67
    JMP L10000408c_66     ; inserted

; Entry 10000408c; block 66; address 100004400
L10000408c_66:
    MOVSX EDI, byte [RCX]
    MOV RSI, qword [RBP + -184]
    CALL _fputc
    JMP L10000408c_68     ; inserted

; Entry 10000408c; block 67; address 10000443c
L10000408c_67:
    MOV R14, qword [RBP + -80]
    JMP L10000408c_76

; Entry 10000408c; block 68; address 10000440f
L10000408c_68:
    INC RBX
    XOR R12D, R12D
    XOR R14D, R14D
    JMP L10000408c_69     ; inserted

; Entry 10000408c; block 69; address 100004418
L10000408c_69:
    ADD R13D, 18446744073709551615
    JB L10000408c_70
    JMP L10000408c_71     ; inserted

; Entry 10000408c; block 70; address 1000043ac
L10000408c_70:
    MOV EAX, R15D
    MOV ECX, R13D
    SHR EAX, CL
    AND EAX, 1
    LEA R12D, [RAX + R12 * 2]
    MOVSXD RAX, R14D
    MOV ECX, R12D
    MOV RDX, qword [RBP + -64]
    SUB ECX, dword [RDX + RAX * 4]
    JGE L10000408c_61
    JMP L10000408c_60     ; inserted

; Entry 10000408c; block 71; address 10000441e
L10000408c_71:
    MOV RDI, qword [RBP + -176]
    CALL _fgetc
    JMP L10000408c_72     ; inserted

; Entry 10000408c; block 72; address 10000442a
L10000408c_72:
    MOV R15D, EAX
    CMP EAX, 18446744073709551615
    JNE L10000408c_73
    JMP L10000408c_74     ; inserted

; Entry 10000408c; block 73; address 10000439a
L10000408c_73:
    MOV RAX, qword [RBP + -48]
    TEST RAX, RAX
    JE L10000408c_59
    JMP L10000408c_58     ; inserted

; Entry 10000408c; block 74; address 100004436
L10000408c_74:
    MOV R14, qword [RBP + -80]
    JMP L10000408c_75

; Entry 10000408c; block 75; address 100004448
L10000408c_75:
    CMP RBX, R14
    JNE L10000408c_77
    JMP L10000408c_76     ; inserted

; Entry 10000408c; block 76; address 100004451
L10000408c_76:
    MOV RDI, qword [RBP + -160]
    CALL _free
    JMP L10000408c_78     ; inserted

; Entry 10000408c; block 77; address 100004509
L10000408c_77:
    LEA RDI, [rel L__TEXT___cstring_1334]
    JMP L10000408c_84

; Entry 10000408c; block 78; address 10000445d
L10000408c_78:
    MOV RDI, qword [RBP + -152]
    CALL _free
    JMP L10000408c_79     ; inserted

; Entry 10000408c; block 79; address 100004469
L10000408c_79:
    MOV RDI, qword [RBP + -144]
    CALL _free
    JMP L10000408c_80     ; inserted

; Entry 10000408c; block 80; address 100004475
L10000408c_80:
    MOV RDI, qword [RBP + -128]
    CALL _free
    JMP L10000408c_81     ; inserted

; Entry 10000408c; block 81; address 10000447e
L10000408c_81:
    MOV RDI, qword [RBP + -112]
    CALL _fclose
    JMP L10000408c_82     ; inserted

; Entry 10000408c; block 82; address 100004487
L10000408c_82:
    MOV RDI, qword [RBP + -104]
    CALL _fclose
    JMP L10000408c_83     ; inserted

; Entry 10000408c; block 83; address 100004490
L10000408c_83:
    MOV RAX, R14
    ADD RSP, 152
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 10000408c; block 84; address 1000044ac
L10000408c_84:
    XOR EAX, EAX
    CALL L100004973_0

; Entry 10000408c; block 85; address 1000044c3
L10000408c_85:
    XOR EAX, EAX
    CALL L100003102_0



; ---------------------
; Function: 0x100004512
; ---------------------
; Entry 100004512; block 0; address 100004512
L100004512_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 2232
    MOV R14, RCX
    MOV dword [RBP + -2104], ESI
    MOV R12D, EDI
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -48], RAX
    XORPS XMM0, XMM0
    MOVUPS oword [RBP + -2144], XMM0
    MOVUPS oword [RBP + -2160], XMM0
    MOVUPS oword [RBP + -2176], XMM0
    MOVUPS oword [RBP + -2192], XMM0
    MOVUPS oword [RBP + -2208], XMM0
    MOVUPS oword [RBP + -2224], XMM0
    MOVUPS oword [RBP + -2240], XMM0
    MOV qword [RBP + -2128], 0
    TEST R8, R8
    LEA RBX, [RBP + -2264]
    CMOVNE RBX, R8
    LEA RDI, [RBP + -1072]
    MOV qword [RBP + -2256], RDI
    MOV R15D, 1024
    MOV ECX, 1024
    MOV RSI, RDX
    MOV RDX, R14
    CALL ___memcpy_chk
    JMP L100004512_1     ; inserted

; Entry 100004512; block 1; address 1000045b1
L100004512_1:
    LEA RSI, [R14 + RBP]
    ADD RSI, 18446744073709550544
    SUB R15, R14
    MOV dword [RBP + -2100], R12D
    MOV EDI, R12D
    MOV RDX, R15
    CALL _read
    JMP L100004512_2     ; inserted

; Entry 100004512; block 2; address 1000045d1
L100004512_2:
    MOV qword [RBP + -2248], RAX
    CMP RAX, 18446744073709551615
    JE L100004512_4
    JMP L100004512_3     ; inserted

; Entry 100004512; block 3; address 1000045e2
L100004512_3:
    ADD RAX, R14
    LEA RDI, [RBP + -2256]
    MOV qword [RDI + 8], RAX
    MOV qword [RBX], RAX
    MOV RSI, 18446744073709551615
    MOV EDX, 10
    CALL _lzma_stream_decoder
    JMP L100004512_5     ; inserted

; Entry 100004512; block 4; address 100004788
L100004512_4:
    LEA RDI, [rel L__TEXT___cstring_1348]
    JMP L100004512_35     ; inserted

; Entry 100004512; block 5; address 100004604
L100004512_5:
    TEST EAX, EAX
    JNE L100004512_7
    JMP L100004512_6     ; inserted

; Entry 100004512; block 6; address 10000460c
L100004512_6:
    MOV qword [RBP + -2120], RBX
    XORPS XMM0, XMM0
    LEA RDI, [RBP + -2256]
    MOVUPS oword [RDI + 24], XMM0
    XOR R13D, R13D
    XOR ESI, ESI
    CALL _lzma_code
    JMP L100004512_8     ; inserted

; Entry 100004512; block 7; address 1000047d8
L100004512_7:
    LEA RDI, [rel L__TEXT___cstring_1360]
    JMP L100004512_43

; Entry 100004512; block 8; address 10000462b
L100004512_8:
    TEST EAX, EAX
    MOV R15D, dword [RBP + -2100]
    JNE L100004512_10
    JMP L100004512_9     ; inserted

; Entry 100004512; block 9; address 10000463a
L100004512_9:
    LEA RAX, [RBP + -2096]
    LEA R14, [RBP + -2256]
    MOV qword [R14 + 24], RAX
    MOV qword [R14 + 32], 1024
    XOR EBX, EBX
    JMP L100004512_24     ; inserted

; Entry 100004512; block 10; address 1000047e1
L100004512_10:
    LEA RDI, [rel L__TEXT___cstring_1390]
    JMP L100004512_43     ; inserted

; Entry 100004512; block 11; address 10000466a
L100004512_11:
    LEA RSI, [RBP + -1072]
    MOV qword [RBP + -2256], RSI
    MOV EDX, 1024
    MOV EDI, R15D
    CALL _read
    JMP L100004512_13     ; inserted

; Entry 100004512; block 12; address 1000046ab
L100004512_12:
    MOV R13D, EBX
    JMP L100004512_15     ; inserted

; Entry 100004512; block 13; address 100004685
L100004512_13:
    MOV qword [RBP + -2248], RAX
    MOV R13D, 3
    TEST RAX, RAX
    JE L100004512_15
    JMP L100004512_14     ; inserted

; Entry 100004512; block 14; address 100004697
L100004512_14:
    CMP RAX, 18446744073709551615
    JE L100004512_4
    JMP L100004512_16     ; inserted

; Entry 100004512; block 15; address 1000046ae
L100004512_15:
    MOV RDI, R14
    MOV ESI, R13D
    CALL _lzma_code
    JMP L100004512_17     ; inserted

; Entry 100004512; block 16; address 1000046a1
L100004512_16:
    MOV RCX, qword [RBP + -2120]
    ADD qword [RCX], RAX
    JMP L100004512_12     ; inserted

; Entry 100004512; block 17; address 1000046b9
L100004512_17:
    MOV R12D, EAX
    MOV RAX, qword [RBP + -2224]
    TEST R12D, R12D
    JNE L100004512_19
    JMP L100004512_18     ; inserted

; Entry 100004512; block 18; address 1000046c8
L100004512_18:
    MOV EBX, R13D
    TEST RAX, RAX
    JNE L100004512_20
    JMP L100004512_19     ; inserted

; Entry 100004512; block 19; address 1000046d0
L100004512_19:
    MOV R15D, 1024
    SUB R15, RAX
    MOV EDI, dword [RBP + -2104]
    LEA RSI, [RBP + -2096]
    MOV RDX, R15
    CALL _write
    JMP L100004512_21     ; inserted

; Entry 100004512; block 20; address 100004660
L100004512_20:
    CMP qword [RBP + -2248], 0
    JNE L100004512_12
    JMP L100004512_11     ; inserted

; Entry 100004512; block 21; address 1000046ee
L100004512_21:
    CMP RAX, R15
    JNE L100004512_23
    JMP L100004512_22     ; inserted

; Entry 100004512; block 22; address 1000046f7
L100004512_22:
    MOV RBX, qword [RBP + -2112]
    ADD RBX, R15
    LEA RAX, [RBP + -2096]
    MOV qword [RBP + -2232], RAX
    MOV qword [RBP + -2224], 1024
    TEST R12D, R12D
    MOV R15D, dword [RBP + -2100]
    JE L100004512_24
    JMP L100004512_25     ; inserted

; Entry 100004512; block 23; address 100004796
L100004512_23:
    LEA RDI, [rel L__TEXT___cstring_1414]
    JMP L100004512_35

; Entry 100004512; block 24; address 100004656
L100004512_24:
    MOV qword [RBP + -2112], RBX
    MOV EBX, R13D
    JMP L100004512_20     ; inserted

; Entry 100004512; block 25; address 10000472a
L100004512_25:
    CMP R12D, 1
    JNE L100004512_27
    JMP L100004512_26     ; inserted

; Entry 100004512; block 26; address 100004730
L100004512_26:
    CMP qword [RBP + -2248], 0
    JNE L100004512_29
    JMP L100004512_28     ; inserted

; Entry 100004512; block 27; address 10000479f
L100004512_27:
    LEA EAX, [R12 + -5]
    CMP EAX, 5
    JA L100004512_37
    JMP L100004512_36     ; inserted

; Entry 100004512; block 28; address 10000473e
L100004512_28:
    LEA RSI, [RBP + -1072]
    MOV EDX, 1
    MOV EDI, R15D
    CALL _read
    JMP L100004512_30     ; inserted

; Entry 100004512; block 29; address 1000047cf
L100004512_29:
    LEA RSI, [rel L__TEXT___cstring_1486]
    JMP L100004512_41

; Entry 100004512; block 30; address 100004752
L100004512_30:
    TEST RAX, RAX
    JNE L100004512_29
    JMP L100004512_31     ; inserted

; Entry 100004512; block 31; address 100004757
L100004512_31:
    LEA RDI, [RBP + -2256]
    CALL _lzma_end
    JMP L100004512_32     ; inserted

; Entry 100004512; block 32; address 100004763
L100004512_32:
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    CMP RAX, qword [RBP + -48]
    JNE L100004512_34
    JMP L100004512_33     ; inserted

; Entry 100004512; block 33; address 100004773
L100004512_33:
    MOV RAX, RBX
    ADD RSP, 2232
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100004512; block 34; address 1000047ec
L100004512_34:
    CALL ___stack_chk_fail

; Entry 100004512; block 35; address 10000478f
L100004512_35:
    XOR EAX, EAX
    CALL L100003102_0

; Entry 100004512; block 36; address 1000047a9
L100004512_36:
    LEA RSI, [rel L__TEXT___cstring_1427]
    LEA RCX, [rel L__TEXT___text + 9744]
    MOVSXD RAX, dword [RCX + RAX * 4]
    ADD RAX, RCX
    MOV RAX, QWORD PTR [L_JUMP_TABLE_1000047be + 8*EAX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 1000047c0,100004802,10000481b,10000480b,1000047cf

; Entry 100004512; block 37; address 1000047f1
L100004512_37:
    LEA RDI, [rel L__TEXT___cstring_1547]
    MOV ESI, R12D
    JMP L100004512_44     ; inserted

; Entry 100004512; block 38; address 1000047c0
L100004512_38:
    MOV EDI, 12
    CALL _strerror
    JMP L100004512_42     ; inserted

; Entry 100004512; block 39; address 100004802
L100004512_39:
    LEA RSI, [rel L__TEXT___cstring_1526]
    JMP L100004512_41

; Entry 100004512; block 40; address 10000480b
L100004512_40:
    LEA RSI, [rel L__TEXT___cstring_1454]
    JMP L100004512_41

; Entry 100004512; block 41; address 10000481b
L100004512_41:
    LEA RDI, [rel L__TEXT___cstring_1566]
    XOR EAX, EAX
    CALL L100004973_0

; Entry 100004512; block 42; address 1000047ca
L100004512_42:
    MOV RSI, RAX
    JMP L100004512_41

; Entry 100004512; block 43; address 1000047e8
L100004512_43:
    MOV ESI, EAX
    JMP L100004512_44

; Entry 100004512; block 44; address 1000047fb
L100004512_44:
    XOR EAX, EAX
    CALL L100004973_0



; ---------------------
; Function: 0x100004844
; ---------------------
; Entry 100004844; block 0; address 100004844
L100004844_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    PUSH RAX
    MOV R14, RCX
    MOV R15, RDX
    MOV R12, RSI
    TEST RDI, RDI
    JE L100004844_2
    JMP L100004844_1     ; inserted

; Entry 100004844; block 1; address 100004860
L100004844_1:
    MOV RBX, RDI
    MOV RAX, qword [rel ___stderrp]
    MOV R13, qword [RAX]
    CALL _strlen
    JMP L100004844_3     ; inserted

; Entry 100004844; block 2; address 10000489c
L100004844_2:
    MOV RBX, qword [rel ___stderrp]
    MOV RDX, qword [RBX]
    MOV RDI, R15
    MOV RSI, R14
    CALL L100004fb3_0
    JMP L100004844_4     ; inserted

; Entry 100004844; block 3; address 100004872
L100004844_3:
    CMP RAX, 7
    LEA RAX, [rel L__TEXT___cstring_1577]
    LEA RCX, [rel L__TEXT___cstring_1580]
    CMOVB RCX, RAX
    LEA RSI, [rel L__TEXT___cstring_1569]
    MOV RDI, R13
    MOV RDX, RBX
    XOR EAX, EAX
    CALL _fprintf
    JMP L100004844_2     ; inserted

; Entry 100004844; block 4; address 1000048b1
L100004844_4:
    TEST R12, R12
    JE L100004844_6
    JMP L100004844_5     ; inserted

; Entry 100004844; block 5; address 1000048b6
L100004844_5:
    MOV RDI, qword [RBX]
    LEA RSI, [rel L__TEXT___cstring_1582]
    MOV RDX, R12
    XOR EAX, EAX
    CALL _fprintf
    JMP L100004844_6     ; inserted

; Entry 100004844; block 6; address 1000048ca
L100004844_6:
    MOV RSI, qword [RBX]
    MOV EDI, 10
    CALL _fputc
    JMP L100004844_7     ; inserted

; Entry 100004844; block 7; address 1000048d7
L100004844_7:
    MOV RDI, qword [RBX]
    ADD RSP, 8
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    JMP _fflush



; ---------------------
; Function: 0x1000048ed
; ---------------------
; Entry 1000048ed; block 0; address 1000048ed
L1000048ed_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R12
    PUSH RBX
    MOV EBX, ESI
    MOV R14, RDI
    MOV EAX, dword [rel L__DATA___bss + 44]
    OR EAX, ESI
    JNE L1000048ed_2
    JMP L1000048ed_1     ; inserted

; Entry 1000048ed; block 1; address 100004907
L1000048ed_1:
    MOV dword [rel L__DATA___bss + 44], 1
    JMP L1000048ed_2     ; inserted

; Entry 1000048ed; block 2; address 100004911
L1000048ed_2:
    MOV R12, qword [rel ___stderrp]
    MOV R15, qword [R12]
    MOV RDI, R14
    CALL _strlen
    JMP L1000048ed_3     ; inserted

; Entry 1000048ed; block 3; address 100004924
L1000048ed_3:
    CMP RAX, 7
    LEA RAX, [rel L__TEXT___cstring_1577]
    LEA RCX, [rel L__TEXT___cstring_1580]
    CMOVB RCX, RAX
    TEST EBX, EBX
    LEA RAX, [rel L__TEXT___cstring_1630]
    LEA R8, [rel L__TEXT___cstring_1627]
    CMOVE R8, RAX
    LEA RSI, [rel L__TEXT___cstring_1616]
    MOV RDI, R15
    MOV RDX, R14
    XOR EAX, EAX
    CALL _fprintf
    JMP L1000048ed_4     ; inserted

; Entry 1000048ed; block 4; address 100004962
L1000048ed_4:
    MOV RDI, qword [R12]
    POP RBX
    POP R12
    POP R14
    POP R15
    POP RBP
    JMP _fflush



; ---------------------
; Function: 0x100004973
; ---------------------
; Entry 100004973; block 0; address 100004973
L100004973_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 208
    TEST AL, AL
    JE L100004973_2
    JMP L100004973_1     ; inserted

; Entry 100004973; block 1; address 100004982
L100004973_1:
    MOVAPS oword [RBP + -128], XMM0
    MOVAPS oword [RBP + -112], XMM1
    MOVAPS oword [RBP + -96], XMM2
    MOVAPS oword [RBP + -80], XMM3
    MOVAPS oword [RBP + -64], XMM4
    MOVAPS oword [RBP + -48], XMM5
    MOVAPS oword [RBP + -32], XMM6
    MOVAPS oword [RBP + -16], XMM7
    JMP L100004973_2     ; inserted

; Entry 100004973; block 2; address 1000049a2
L100004973_2:
    MOV qword [RBP + -168], RSI
    MOV qword [RBP + -160], RDX
    MOV qword [RBP + -152], RCX
    MOV qword [RBP + -144], R8
    MOV qword [RBP + -136], R9
    CMP byte [rel L__DATA___bss + 28], 0
    JNE L100004973_4
    JMP L100004973_3     ; inserted

; Entry 100004973; block 3; address 1000049ce
L100004973_3:
    LEA RAX, [RBP + -176]
    LEA RSI, [RBP + -208]
    MOV qword [RSI + 16], RAX
    LEA RAX, [RBP + 16]
    MOV qword [RSI + 8], RAX
    MOV RAX, 206158430216
    MOV qword [RSI], RAX
    CALL _vwarnx
    JMP L100004973_4     ; inserted

; Entry 100004973; block 4; address 1000049fa
L100004973_4:
    MOV EDI, 2
    CALL _exit



; ---------------------
; Function: 0x100004f61
; ---------------------
; Entry 100004f61; block 0; address 100004f61
L100004f61_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R14
    PUSH RBX
    MOV R14D, ESI
    MOV RBX, RDI
    CMP dword [RDI + 4], ESI
    JLE L100004f61_2
    JMP L100004f61_1     ; inserted

; Entry 100004f61; block 1; address 100004f73
L100004f61_1:
    LEA ESI, [R14 + 1]
    MOV RDI, RBX
    CALL L100004f61_0
    JMP L100004f61_3     ; inserted

; Entry 100004f61; block 2; address 100004fa0
L100004f61_2:
    MOV RAX, qword [RBX + 16]
    MOVSXD RCX, R14D
    MOV dword [RAX + RCX * 4], 0
    JMP L100004f61_4     ; inserted

; Entry 100004f61; block 3; address 100004f7f
L100004f61_3:
    MOV RAX, qword [RBX + 8]
    MOV RCX, qword [RBX + 16]
    MOVSXD RDX, R14D
    MOV EAX, dword [RAX + RDX * 4 + 4]
    ADD EAX, dword [RCX + RDX * 4 + 4]
    MOV ESI, EAX
    SHR ESI, 31
    ADD ESI, EAX
    SAR ESI, 1
    MOV dword [RCX + RDX * 4], ESI
    JMP L100004f61_4

; Entry 100004f61; block 4; address 100004fae
L100004f61_4:
    POP RBX
    POP R14
    POP RBP
    RET 



; ---------------------
; Function: 0x100004fb3
; ---------------------
; Entry 100004fb3; block 0; address 100004fb3
L100004fb3_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R14
    PUSH RBX
    SUB RSP, 16
    MOV R14, RDX
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -24], RAX
    MOV RCX, RSI
    SHR RCX, 63
    ADD RCX, RSI
    SAR RCX, 1
    MOV RAX, RDI
    SUB RAX, RCX
    TEST RAX, RAX
    JLE L100004fb3_2
    JMP L100004fb3_1     ; inserted

; Entry 100004fb3; block 1; address 100004fe7
L100004fb3_1:
    CMP RDI, 1048577
    JL L100004fb3_4
    JMP L100004fb3_3     ; inserted

; Entry 100004fb3; block 2; address 100005007
L100004fb3_2:
    MOV ECX, 4294966297
    JMP L100004fb3_10

; Entry 100004fb3; block 3; address 100004ff0
L100004fb3_3:
    MOV RCX, RDI
    JMP L100004fb3_5     ; inserted

; Entry 100004fb3; block 4; address 10000500e
L100004fb3_4:
    MOV RCX, RDI
    JMP L100004fb3_7     ; inserted

; Entry 100004fb3; block 5; address 100004ff3
L100004fb3_5:
    SAR RAX, 1
    SHR RCX, 1
    CMP RDI, 2097153
    MOV RDI, RCX
    JA L100004fb3_5
    JMP L100004fb3_6     ; inserted

; Entry 100004fb3; block 6; address 100005005
L100004fb3_6:
    JMP L100004fb3_7

; Entry 100004fb3; block 7; address 100005011
L100004fb3_7:
    TEST RCX, RCX
    JE L100004fb3_9
    JMP L100004fb3_8     ; inserted

; Entry 100004fb3; block 8; address 100005016
L100004fb3_8:
    IMUL EAX, EAX, 2000
    XOR EDX, EDX
    DIV ECX
    MOV ECX, EAX
    ADD ECX, 4294966296
    JMP L100004fb3_10

; Entry 100004fb3; block 9; address 10000502a
L100004fb3_9:
    XOR ECX, ECX
    JMP L100004fb3_10     ; inserted

; Entry 100004fb3; block 10; address 10000502c
L100004fb3_10:
    LEA RDX, [rel L__TEXT___cstring_1603]
    LEA RBX, [RBP + -32]
    MOV ESI, 8
    MOV RDI, RBX
    XOR EAX, EAX
    CALL _snprintf
    JMP L100004fb3_11     ; inserted

; Entry 100004fb3; block 11; address 100005046
L100004fb3_11:
    CDQE 
    MOV CL, byte [RAX + RBX-1]
    MOV byte [RAX + RBX-1], CL
    MOV byte [RAX + RBX-1], 46
    LEA RSI, [rel L__TEXT___cstring_1610]
    MOV RDI, R14
    MOV RDX, RBX
    XOR EAX, EAX
    CALL _fprintf
    JMP L100004fb3_12     ; inserted

; Entry 100004fb3; block 12; address 100005069
L100004fb3_12:
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    CMP RAX, qword [RBP + -24]
    JNE L100004fb3_14
    JMP L100004fb3_13     ; inserted

; Entry 100004fb3; block 13; address 100005079
L100004fb3_13:
    ADD RSP, 16
    POP RBX
    POP R14
    POP RBP
    RET 

; Entry 100004fb3; block 14; address 100005082
L100004fb3_14:
    CALL ___stack_chk_fail



; ---------------------
; Function: 0x100005087
; ---------------------
; Entry 100005087; block 0; address 100005087
L100005087_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 168
    MOV R15D, R8D
    MOV R12, RCX
    MOV qword [RBP + -56], RDX
    MOV dword [RBP + -60], ESI
    MOV dword [RBP + -76], EDI
    MOV EDI, 65536
    CALL _malloc
    JMP L100005087_1     ; inserted

; Entry 100005087; block 1; address 1000050b5
L100005087_1:
    MOV RBX, RAX
    MOV EDI, 65536
    CALL _malloc
    JMP L100005087_2     ; inserted

; Entry 100005087; block 2; address 1000050c2
L100005087_2:
    TEST RBX, RBX
    JE L100005087_4
    JMP L100005087_3     ; inserted

; Entry 100005087; block 3; address 1000050cb
L100005087_3:
    TEST RAX, RAX
    JE L100005087_4
    JMP L100005087_5     ; inserted

; Entry 100005087; block 4; address 100005483
L100005087_4:
    LEA RDI, [rel L__TEXT___cstring_525]
    JMP L100005087_55

; Entry 100005087; block 5; address 1000050d4
L100005087_5:
    MOV RDI, RBX
    MOV qword [RBP + -72], RAX
    XORPS XMM0, XMM0
    MOVAPS oword [RBP + -112], XMM0
    MOVAPS oword [RBP + -128], XMM0
    MOVAPS oword [RBP + -144], XMM0
    MOVAPS oword [RBP + -160], XMM0
    MOVAPS oword [RBP + -176], XMM0
    MOVAPS oword [RBP + -192], XMM0
    MOVAPS oword [RBP + -208], XMM0
    XOR EAX, EAX
    CMP byte [rel L__DATA___bss + 20], 0
    CMOVNE R15D, EAX
    LEA RAX, [rel L__TEXT___cstring_126]
    CMOVNE R12, RAX
    XOR R10D, R10D
    CMP byte [R12], 0
    SETNE R10B
    SHL R10D, 3
    MOV EAX, 255
    MOV R11D, R15D
    AND R11D, EAX
    MOV R13D, R15D
    SHR R13D, 8
    AND R13D, EAX
    MOV EBX, R15D
    SHR EBX, 16
    AND EBX, EAX
    SHR R15D, 24
    MOV EAX, dword [rel L__DATA___data + 0]
    XOR ECX, ECX
    CMP EAX, 9
    SETE CL
    ADD ECX, ECX
    CMP EAX, 1
    MOV R14D, 4
    CMOVNE R14D, ECX
    LEA RDX, [rel L__TEXT___cstring_1701]
    MOV ESI, 65536
    MOV qword [RBP + -88], RDI
    MOV ECX, 31
    MOV R8D, 139
    MOV R9D, 8
    XOR EAX, EAX
    MOV qword [RBP + -48], R12
    PUSH R12
    PUSH 3
    PUSH R14
    PUSH R15
    PUSH RBX
    PUSH R13
    PUSH R11
    PUSH R10
    CALL _snprintf
    JMP L100005087_6     ; inserted

; Entry 100005087; block 6; address 1000051aa
L100005087_6:
    ADD RSP, 64
    CMP EAX, 65536
    JGE L100005087_8
    JMP L100005087_7     ; inserted

; Entry 100005087; block 7; address 1000051b9
L100005087_7:
    MOV RCX, qword [RBP + -48]
    CMP byte [RCX], 1
    SBB EAX, 18446744073709551615
    CDQE 
    MOV R14, qword [RBP + -88]
    LEA RCX, [R14 + RAX]
    LEA RDI, [RBP + -208]
    MOV qword [RDI + 24], RCX
    MOV ECX, 65536
    SUB ECX, EAX
    MOV dword [RDI + 32], ECX
    MOV ESI, dword [rel L__DATA___data + 0]
    LEA RAX, [rel L__TEXT___cstring_695]
    MOV EDX, 8
    MOV ECX, 4294967281
    MOV R8D, 8
    XOR R9D, R9D
    PUSH 112
    PUSH RAX
    CALL _deflateInit2_
    JMP L100005087_9     ; inserted

; Entry 100005087; block 8; address 10000548c
L100005087_8:
    LEA RDI, [rel L__TEXT___cstring_1724]
    JMP L100005087_55     ; inserted

; Entry 100005087; block 9; address 10000520a
L100005087_9:
    ADD RSP, 16
    TEST EAX, EAX
    JE L100005087_11
    JMP L100005087_10     ; inserted

; Entry 100005087; block 10; address 100005212
L100005087_10:
    LEA RDI, [rel L__TEXT___cstring_1733]
    XOR R13D, R13D
    JMP L100005087_29

; Entry 100005087; block 11; address 100005221
L100005087_11:
    XOR EAX, EAX
    MOV qword [RBP + -48], RAX
    XOR EDI, EDI
    XOR ESI, ESI
    XOR EDX, EDX
    CALL _crc32
    JMP L100005087_12     ; inserted

; Entry 100005087; block 12; address 100005232
L100005087_12:
    MOV R12, RAX
    LEA RBX, [RBP + -208]
    XOR R13D, R13D
    JMP L100005087_27     ; inserted

; Entry 100005087; block 13; address 100005248
L100005087_13:
    MOV EDX, 65536
    MOV EDI, dword [RBP + -60]
    MOV RSI, R14
    CALL _write
    JMP L100005087_15     ; inserted

; Entry 100005087; block 14; address 10000527c
L100005087_14:
    CMP dword [RBP + -200], 0
    JNE L100005087_19
    JMP L100005087_18     ; inserted

; Entry 100005087; block 15; address 100005258
L100005087_15:
    CMP RAX, 65536
    JNE L100005087_17
    JMP L100005087_16     ; inserted

; Entry 100005087; block 16; address 100005264
L100005087_16:
    ADD R13, 65536
    MOV qword [RBP + -184], R14
    MOV dword [RBP + -176], 65536
    JMP L100005087_14     ; inserted

; Entry 100005087; block 17; address 100005386
L100005087_17:
    LEA RDI, [rel L__TEXT___cstring_1003]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L100005087_42     ; inserted

; Entry 100005087; block 18; address 100005285
L100005087_18:
    MOV EDX, 65536
    MOV EDI, dword [RBP + -76]
    MOV RSI, qword [RBP + -72]
    CALL _read
    JMP L100005087_20     ; inserted

; Entry 100005087; block 19; address 1000052cf
L100005087_19:
    MOV RDI, RBX
    XOR ESI, ESI
    CALL _deflate
    JMP L100005087_26     ; inserted

; Entry 100005087; block 20; address 100005296
L100005087_20:
    TEST RAX, RAX
    JS L100005087_22
    JMP L100005087_21     ; inserted

; Entry 100005087; block 21; address 10000529f
L100005087_21:
    JE L100005087_24
    JMP L100005087_23     ; inserted

; Entry 100005087; block 22; address 1000053a0
L100005087_22:
    LEA RDI, [rel L__TEXT___cstring_983]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L100005087_44     ; inserted

; Entry 100005087; block 23; address 1000052a5
L100005087_23:
    MOV R15, RAX
    MOV RDI, R12
    MOV R12, qword [RBP + -72]
    MOV RSI, R12
    MOV EDX, R15D
    CALL _crc32
    JMP L100005087_25     ; inserted

; Entry 100005087; block 24; address 10000536b
L100005087_24:
    LEA RDI, [RBP + -208]
    MOV ESI, 4
    CALL _deflate
    JMP L100005087_35     ; inserted

; Entry 100005087; block 25; address 1000052ba
L100005087_25:
    ADD qword [RBP + -48], R15
    MOV qword [RBP + -208], R12
    MOV R12, RAX
    MOV dword [RBP + -200], R15D
    JMP L100005087_19     ; inserted

; Entry 100005087; block 26; address 1000052d9
L100005087_26:
    CMP EAX, 2
    JB L100005087_27
    JMP L100005087_28     ; inserted

; Entry 100005087; block 27; address 10000523f
L100005087_27:
    CMP dword [RBP + -176], 0
    JNE L100005087_14
    JMP L100005087_13     ; inserted

; Entry 100005087; block 28; address 1000052e2
L100005087_28:
    LEA RDI, [rel L__TEXT___cstring_1753]
    JMP L100005087_29     ; inserted

; Entry 100005087; block 29; address 1000052e9
L100005087_29:
    XOR EAX, EAX
    CALL L100003193_0
    JMP L100005087_30     ; inserted

; Entry 100005087; block 30; address 1000052f0
L100005087_30:
    MOV qword [RBP + -48], 18446744073709551615
    JMP L100005087_43     ; inserted

; Entry 100005087; block 31; address 100005305
L100005087_31:
    MOV RDI, R14
    CALL _free
    JMP L100005087_32     ; inserted

; Entry 100005087; block 32; address 10000530d
L100005087_32:
    TEST RBX, RBX
    JE L100005087_34
    JMP L100005087_33     ; inserted

; Entry 100005087; block 33; address 100005312
L100005087_33:
    MOV qword [RBX], R13
    JMP L100005087_34     ; inserted

; Entry 100005087; block 34; address 100005315
L100005087_34:
    MOV RAX, qword [RBP + -48]
    ADD RSP, 168
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100005087; block 35; address 10000537c
L100005087_35:
    CMP EAX, 2
    JB L100005087_40
    JMP L100005087_41     ; inserted

; Entry 100005087; block 36; address 100005346
L100005087_36:
    CMP RAX, 18446744073709551615
    JE L100005087_17
    JMP L100005087_37     ; inserted

; Entry 100005087; block 37; address 10000534c
L100005087_37:
    CMP RAX, RBX
    JNE L100005087_17
    JMP L100005087_38     ; inserted

; Entry 100005087; block 38; address 100005351
L100005087_38:
    ADD R13, RBX
    MOV qword [RBP + -184], R14
    MOV dword [RBP + -176], 65536
    CMP R15D, 1
    JE L100005087_39
    JMP L100005087_24     ; inserted

; Entry 100005087; block 39; address 1000053b3
L100005087_39:
    LEA RDI, [RBP + -208]
    CALL _deflateEnd
    JMP L100005087_45     ; inserted

; Entry 100005087; block 40; address 10000532b
L100005087_40:
    MOV R15D, EAX
    MOV RBX, qword [RBP + -184]
    SUB RBX, R14
    MOV EDI, dword [RBP + -60]
    MOV RSI, R14
    MOV RDX, RBX
    CALL _write
    JMP L100005087_36     ; inserted

; Entry 100005087; block 41; address 100005381
L100005087_41:
    JMP L100005087_28

; Entry 100005087; block 42; address 100005394
L100005087_42:
    MOV R13, 18446744073709551615
    JMP L100005087_43

; Entry 100005087; block 43; address 1000052f8
L100005087_43:
    MOV RBX, qword [RBP + -56]
    JMP L100005087_53     ; inserted

; Entry 100005087; block 44; address 1000053ae
L100005087_44:
    JMP L100005087_30

; Entry 100005087; block 45; address 1000053bf
L100005087_45:
    TEST EAX, EAX
    JE L100005087_47
    JMP L100005087_46     ; inserted

; Entry 100005087; block 46; address 1000053c3
L100005087_46:
    LEA RDI, [rel L__TEXT___cstring_1768]
    JMP L100005087_29

; Entry 100005087; block 47; address 1000053cf
L100005087_47:
    MOV EAX, 255
    MOV ECX, R12D
    AND ECX, EAX
    MOV R8D, R12D
    SHR R8D, 8
    AND R8D, EAX
    MOV R9D, R12D
    SHR R9D, 16
    AND R9D, EAX
    SHR R12D, 24
    MOV RDX, qword [RBP + -48]
    MOV R10D, EDX
    AND R10D, EAX
    MOV R11D, EDX
    SHR R11D, 8
    AND R11D, EAX
    MOV EBX, EDX
    SHR EBX, 16
    AND EBX, EAX
    MOV R15, R14
    MOV R14D, EDX
    SHR R14D, 24
    SUB RSP, 8
    LEA RDX, [rel L__TEXT___cstring_1786]
    MOV ESI, 65536
    MOV RDI, R15
    XOR EAX, EAX
    PUSH R14
    PUSH RBX
    PUSH R11
    PUSH R10
    PUSH R12
    CALL _snprintf
    JMP L100005087_48     ; inserted

; Entry 100005087; block 48; address 100005439
L100005087_48:
    ADD RSP, 48
    CMP EAX, 8
    JNE L100005087_8
    JMP L100005087_49     ; inserted

; Entry 100005087; block 49; address 100005442
L100005087_49:
    MOV EDX, 8
    MOV EDI, dword [RBP + -60]
    MOV RSI, R15
    CALL _write
    JMP L100005087_50     ; inserted

; Entry 100005087; block 50; address 100005452
L100005087_50:
    CMP RAX, 8
    MOV RBX, qword [RBP + -56]
    MOV R14, R15
    JNE L100005087_52
    JMP L100005087_51     ; inserted

; Entry 100005087; block 51; address 10000545f
L100005087_51:
    ADD R13, 8
    JMP L100005087_53

; Entry 100005087; block 52; address 100005468
L100005087_52:
    LEA RDI, [rel L__TEXT___cstring_1003]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L100005087_54     ; inserted

; Entry 100005087; block 53; address 1000052fc
L100005087_53:
    MOV RDI, qword [RBP + -72]
    CALL _free
    JMP L100005087_31     ; inserted

; Entry 100005087; block 54; address 100005476
L100005087_54:
    MOV qword [RBP + -48], 18446744073709551615
    JMP L100005087_53

; Entry 100005087; block 55; address 100005493
L100005087_55:
    XOR EAX, EAX
    CALL L100003102_0



; ---------------------
; Function: 0x10000549a
; ---------------------
; Entry 10000549a; block 0; address 10000549a
L10000549a_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 2456
    MOV R14, RSI
    MOV R13, RDI
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -48], RAX
    MOV qword [rel L__DATA___bss + 96], RDI
    CMP byte [rel L__DATA___bss + 0], 1
    JNE L10000549a_2
    JMP L10000549a_1     ; inserted

; Entry 10000549a; block 1; address 1000054d2
L10000549a_1:
    MOV RDI, R13
    XOR ESI, ESI
    XOR EAX, EAX
    CALL _open
    JMP L10000549a_3     ; inserted

; Entry 10000549a; block 2; address 100005541
L10000549a_2:
    MOV RDI, R13
    XOR ESI, ESI
    XOR EAX, EAX
    CALL _open
    JMP L10000549a_12     ; inserted

; Entry 10000549a; block 3; address 1000054de
L10000549a_3:
    MOV R15D, EAX
    TEST EAX, EAX
    JS L10000549a_5
    JMP L10000549a_4     ; inserted

; Entry 10000549a; block 4; address 1000054e9
L10000549a_4:
    LEA RBX, [RBP + -2256]
    MOV EDX, 1024
    MOV ECX, 1024
    MOV RDI, RBX
    MOV RSI, R13
    CALL ___strlcpy_chk
    JMP L10000549a_6     ; inserted

; Entry 10000549a; block 5; address 1000055b1
L10000549a_5:
    LEA RDI, [rel L__TEXT___cstring_1905]
    MOV RSI, R13
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_20     ; inserted

; Entry 10000549a; block 6; address 100005505
L10000549a_6:
    MOV RDI, RBX
    MOV ESI, 1
    CALL L10000610c_0
    JMP L10000549a_7     ; inserted

; Entry 10000549a; block 7; address 100005512
L10000549a_7:
    TEST RAX, RAX
    JNE L10000549a_9
    JMP L10000549a_8     ; inserted

; Entry 10000549a; block 8; address 10000551b
L10000549a_8:
    TEST byte [rel L__DATA___bss + 4], 1
    JNE L10000549a_9
    JMP L10000549a_10     ; inserted

; Entry 10000549a; block 9; address 10000560b
L10000549a_9:
    LEA RSI, [RBP + -2336]
    MOV EDX, 4
    MOV EDI, R15D
    CALL _read
    JMP L10000549a_27     ; inserted

; Entry 10000549a; block 10; address 100005528
L10000549a_10:
    TEST byte [rel L__DATA___bss + 8], 1
    JNE L10000549a_9
    JMP L10000549a_11     ; inserted

; Entry 10000549a; block 11; address 100005535
L10000549a_11:
    LEA RDI, [rel L__TEXT___cstring_1919]
    JMP L10000549a_34

; Entry 10000549a; block 12; address 10000554d
L10000549a_12:
    CMP EAX, 18446744073709551615
    JE L10000549a_14
    JMP L10000549a_13     ; inserted

; Entry 10000549a; block 13; address 100005556
L10000549a_13:
    MOV EBX, EAX
    XORPS XMM0, XMM0
    LEA RSI, [RBP + -1232]
    MOVAPS oword [RSI + 128], XMM0
    MOVAPS oword [RSI + 112], XMM0
    MOVAPS oword [RSI + 96], XMM0
    MOVAPS oword [RSI + 80], XMM0
    MOVAPS oword [RSI + 64], XMM0
    MOVAPS oword [RSI + 48], XMM0
    MOVAPS oword [RSI + 32], XMM0
    MOVAPS oword [RSI + 16], XMM0
    MOVAPS oword [RSI], XMM0
    MOV EDI, EAX
    CALL _fstat$INODE64
    JMP L10000549a_15     ; inserted

; Entry 10000549a; block 14; address 1000055e0
L10000549a_14:
    LEA RDI, [rel L__TEXT___cstring_1905]
    MOV RSI, R13
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_24     ; inserted

; Entry 10000549a; block 15; address 10000558f
L10000549a_15:
    TEST EAX, EAX
    JE L10000549a_17
    JMP L10000549a_16     ; inserted

; Entry 10000549a; block 16; address 100005597
L10000549a_16:
    LEA RDI, [rel L__TEXT___cstring_2620]
    MOV RSI, R13
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_18     ; inserted

; Entry 10000549a; block 17; address 100005659
L10000549a_17:
    MOV R15D, 1
    CMP byte [rel L__DATA___bss + 4], 0
    JE L10000549a_36
    JMP L10000549a_35     ; inserted

; Entry 10000549a; block 18; address 1000055a8
L10000549a_18:
    MOV EDI, EBX
    CALL _close
    JMP L10000549a_19     ; inserted

; Entry 10000549a; block 19; address 1000055af
L10000549a_19:
    JMP L10000549a_24

; Entry 10000549a; block 20; address 1000055c2
L10000549a_20:
    MOV RBX, 18446744073709551615
    CMP R15D, 18446744073709551615
    JE L10000549a_22
    JMP L10000549a_21     ; inserted

; Entry 10000549a; block 21; address 1000055d3
L10000549a_21:
    MOV EDI, R15D
    CALL _close
    JMP L10000549a_23     ; inserted

; Entry 10000549a; block 22; address 10000577d
L10000549a_22:
    CMP byte [rel L__DATA___bss + 40], 1
    JNE L10000549a_60
    JMP L10000549a_59     ; inserted

; Entry 10000549a; block 23; address 1000055db
L10000549a_23:
    JMP L10000549a_22

; Entry 10000549a; block 24; address 1000055f1
L10000549a_24:
    MOV RCX, 18446744073709551615
    JMP L10000549a_43     ; inserted

; Entry 10000549a; block 25; address 100005602
L10000549a_25:
    MOV RBX, qword [R14 + 96]
    JMP L10000549a_63

; Entry 10000549a; block 26; address 1000057dc
L10000549a_26:
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    CMP RAX, qword [RBP + -48]
    JNE L10000549a_67
    JMP L10000549a_66     ; inserted

; Entry 10000549a; block 27; address 10000561f
L10000549a_27:
    CMP RAX, 4
    JNE L10000549a_29
    JMP L10000549a_28     ; inserted

; Entry 10000549a; block 28; address 100005629
L10000549a_28:
    LEA RDI, [RBP + -2336]
    CALL L100003329_0
    JMP L10000549a_30     ; inserted

; Entry 10000549a; block 29; address 1000056aa
L10000549a_29:
    CMP byte [rel L__DATA___bss + 12], 0
    JNE L10000549a_45
    JMP L10000549a_44     ; inserted

; Entry 10000549a; block 30; address 100005635
L10000549a_30:
    MOV EBX, EAX
    CMP EAX, 6
    JNE L10000549a_32
    JMP L10000549a_31     ; inserted

; Entry 10000549a; block 31; address 100005640
L10000549a_31:
    TEST byte [rel L__DATA___bss + 12], 1
    JNE L10000549a_32
    JMP L10000549a_33     ; inserted

; Entry 10000549a; block 32; address 100005722
L10000549a_32:
    XOR R12D, R12D
    TEST EBX, EBX
    JNE L10000549a_54
    JMP L10000549a_53     ; inserted

; Entry 10000549a; block 33; address 10000564d
L10000549a_33:
    LEA RDI, [rel L__TEXT___cstring_1963]
    JMP L10000549a_34

; Entry 10000549a; block 34; address 100005764
L10000549a_34:
    MOV RSI, R13
    XOR EAX, EAX
    CALL L100003193_0
    JMP L10000549a_45     ; inserted

; Entry 10000549a; block 35; address 100005668
L10000549a_35:
    MOV RDI, R13
    CALL _basename
    JMP L10000549a_37     ; inserted

; Entry 10000549a; block 36; address 1000056d7
L10000549a_36:
    MOVZX EDX, word [RBP + -1226]
    MOV AL, byte [rel L__DATA___bss + 12]
    CMP EDX, 2
    JB L10000549a_50
    JMP L10000549a_49     ; inserted

; Entry 10000549a; block 37; address 100005670
L10000549a_37:
    MOV R8D, dword [RBP + -1184]
    LEA RDX, [RBP + -2296]
    MOV EDI, EBX
    MOV ESI, R15D
    MOV RCX, RAX
    CALL L100005087_0
    JMP L10000549a_38     ; inserted

; Entry 10000549a; block 38; address 10000568b
L10000549a_38:
    CMP byte [rel L__DATA___bss + 4], 1
    JNE L10000549a_40
    JMP L10000549a_39     ; inserted

; Entry 10000549a; block 39; address 100005698
L10000549a_39:
    MOV RCX, RAX
    CMP RAX, 18446744073709551615
    JNE L10000549a_42
    JMP L10000549a_41     ; inserted

; Entry 10000549a; block 40; address 100005802
L10000549a_40:
    LEA RSI, [RBP + -2480]
    MOV EDI, R15D
    CALL _fstat$INODE64
    JMP L10000549a_68     ; inserted

; Entry 10000549a; block 41; address 1000056a5
L10000549a_41:
    JMP L10000549a_43

; Entry 10000549a; block 42; address 100005cdc
L10000549a_42:
    MOV RCX, qword [RBP + -2296]
    JMP L10000549a_43

; Entry 10000549a; block 43; address 1000055f8
L10000549a_43:
    CMP RCX, 18446744073709551615
    JE L10000549a_26
    JMP L10000549a_25     ; inserted

; Entry 10000549a; block 44; address 1000056b7
L10000549a_44:
    CMP RAX, 18446744073709551615
    JNE L10000549a_47
    JMP L10000549a_46     ; inserted

; Entry 10000549a; block 45; address 10000576e
L10000549a_45:
    MOV EDI, R15D
    CALL _close
    JMP L10000549a_58     ; inserted

; Entry 10000549a; block 46; address 1000056c1
L10000549a_46:
    LEA RDI, [rel L__TEXT___cstring_1949]
    MOV RSI, R13
    JMP L10000549a_147     ; inserted

; Entry 10000549a; block 47; address 10000575d
L10000549a_47:
    LEA RDI, [rel L__TEXT___cstring_560]
    JMP L10000549a_34     ; inserted

; Entry 10000549a; block 48; address 1000056d2
L10000549a_48:
    JMP L10000549a_45

; Entry 10000549a; block 49; address 1000056ed
L10000549a_49:
    TEST AL, 1
    JNE L10000549a_50
    JMP L10000549a_51     ; inserted

; Entry 10000549a; block 50; address 100005833
L10000549a_50:
    TEST AL, 1
    JNE L10000549a_73
    JMP L10000549a_72     ; inserted

; Entry 10000549a; block 51; address 1000056f5
L10000549a_51:
    DEC EDX
    CMP EDX, 1
    LEA RAX, [rel L__TEXT___cstring_126]
    LEA RCX, [rel L__TEXT___cstring_2673]
    CMOVE RCX, RAX
    LEA RDI, [rel L__TEXT___cstring_2638]
    MOV RSI, R13
    XOR EAX, EAX
    CALL L100003193_0
    JMP L10000549a_52     ; inserted

; Entry 10000549a; block 52; address 10000571d
L10000549a_52:
    JMP L10000549a_18

; Entry 10000549a; block 53; address 10000572d
L10000549a_53:
    TEST byte [rel L__DATA___bss + 24], 1
    JE L10000549a_54
    JMP L10000549a_55     ; inserted

; Entry 10000549a; block 54; address 100005b2a
L10000549a_54:
    MOV qword [RBP + -2264], R12
    XOR R12D, R12D
    MOV EDI, R15D
    XOR ESI, ESI
    XOR EDX, EDX
    CALL _lseek
    JMP L10000549a_110     ; inserted

; Entry 10000549a; block 55; address 10000573a
L10000549a_55:
    LEA RSI, [RBP + -2480]
    MOV EDX, 4
    MOV ECX, 4
    MOV EDI, R15D
    CALL _pread
    JMP L10000549a_56     ; inserted

; Entry 10000549a; block 56; address 100005753
L10000549a_56:
    CMP RAX, 4
    JAE L10000549a_57
    JMP L10000549a_47     ; inserted

; Entry 10000549a; block 57; address 1000059f2
L10000549a_57:
    CMP RAX, 18446744073709551615
    JE L10000549a_101
    JMP L10000549a_100     ; inserted

; Entry 10000549a; block 58; address 100005776
L10000549a_58:
    MOV RBX, 18446744073709551615
    JMP L10000549a_22     ; inserted

; Entry 10000549a; block 59; address 100005786
L10000549a_59:
    TEST byte [rel L__DATA___bss + 36], 1
    JE L10000549a_60
    JMP L10000549a_61     ; inserted

; Entry 10000549a; block 60; address 1000057a1
L10000549a_60:
    CMP RBX, 18446744073709551615
    JE L10000549a_26
    JMP L10000549a_62     ; inserted

; Entry 10000549a; block 61; address 10000578f
L10000549a_61:
    XOR ESI, ESI
    CMP RBX, 18446744073709551615
    SETNE SIL
    MOV RDI, R13
    CALL L1000048ed_0
    JMP L10000549a_60     ; inserted

; Entry 10000549a; block 62; address 1000057a7
L10000549a_62:
    MOV RCX, qword [R14 + 96]
    JMP L10000549a_63     ; inserted

; Entry 10000549a; block 63; address 1000057ab
L10000549a_63:
    CMP byte [rel L__DATA___bss + 36], 0
    JNE L10000549a_26
    JMP L10000549a_64     ; inserted

; Entry 10000549a; block 64; address 1000057b4
L10000549a_64:
    TEST byte [rel L__DATA___bss + 40], 1
    JE L10000549a_26
    JMP L10000549a_65     ; inserted

; Entry 10000549a; block 65; address 1000057bd
L10000549a_65:
    XOR EAX, EAX
    CMP byte [rel L__DATA___bss + 4], 0
    LEA RSI, [RBP + -2256]
    CMOVNE RSI, RAX
    MOV RDI, R13
    MOV RDX, RBX
    CALL L100004844_0
    JMP L10000549a_26     ; inserted

; Entry 10000549a; block 66; address 1000057f0
L10000549a_66:
    ADD RSP, 2456
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 10000549a; block 67; address 1000060ef
L10000549a_67:
    CALL ___stack_chk_fail

; Entry 10000549a; block 68; address 100005811
L10000549a_68:
    TEST EAX, EAX
    JE L10000549a_70
    JMP L10000549a_69     ; inserted

; Entry 10000549a; block 69; address 100005819
L10000549a_69:
    LEA RDI, [rel L__TEXT___cstring_2620]
    LEA RSI, [RBP + -2256]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_71     ; inserted

; Entry 10000549a; block 70; address 100005909
L10000549a_70:
    MOV RDX, qword [RBP + -2384]
    MOV RCX, qword [RBP + -2296]
    CMP RDX, RCX
    JNE L10000549a_88
    JMP L10000549a_87     ; inserted

; Entry 10000549a; block 71; address 10000582e
L10000549a_71:
    JMP L10000549a_139

; Entry 10000549a; block 72; address 100005837
L10000549a_72:
    MOV RDI, R13
    XOR ESI, ESI
    CALL L10000610c_0
    JMP L10000549a_74     ; inserted

; Entry 10000549a; block 73; address 100005864
L10000549a_73:
    MOV R8, qword [rel L__DATA___data + 16]
    LEA RDX, [rel L__TEXT___cstring_2713]
    LEA R12, [RBP + -2256]
    MOV ESI, 1024
    MOV RDI, R12
    MOV RCX, R13
    XOR EAX, EAX
    CALL _snprintf
    JMP L10000549a_78     ; inserted

; Entry 10000549a; block 74; address 100005841
L10000549a_74:
    TEST RAX, RAX
    JE L10000549a_73
    JMP L10000549a_75     ; inserted

; Entry 10000549a; block 75; address 100005846
L10000549a_75:
    MOV RDX, qword [RAX]
    CMP byte [RDX], 0
    JE L10000549a_73
    JMP L10000549a_76     ; inserted

; Entry 10000549a; block 76; address 10000584e
L10000549a_76:
    LEA RDI, [rel L__TEXT___cstring_2675]
    MOV RSI, R13
    XOR EAX, EAX
    CALL L100003193_0
    JMP L10000549a_77     ; inserted

; Entry 10000549a; block 77; address 10000585f
L10000549a_77:
    JMP L10000549a_18

; Entry 10000549a; block 78; address 10000588b
L10000549a_78:
    CMP EAX, 1024
    JB L10000549a_80
    JMP L10000549a_79     ; inserted

; Entry 10000549a; block 79; address 100005892
L10000549a_79:
    MOVSXD RDX, dword [rel L__DATA___data + 24]
    SUB R12, RDX
    ADD R12, 1023
    MOV RSI, qword [rel L__DATA___data + 16]
    INC RDX
    MOV RDI, R12
    CALL _memcpy
    JMP L10000549a_80     ; inserted

; Entry 10000549a; block 80; address 1000058b5
L10000549a_80:
    LEA RDI, [RBP + -2256]
    CALL L100006193_0
    JMP L10000549a_81     ; inserted

; Entry 10000549a; block 81; address 1000058c1
L10000549a_81:
    TEST EAX, EAX
    JE L10000549a_18
    JMP L10000549a_82     ; inserted

; Entry 10000549a; block 82; address 1000058c9
L10000549a_82:
    CMP byte [rel L__DATA___bss + 4], 0
    JNE L10000549a_35
    JMP L10000549a_83     ; inserted

; Entry 10000549a; block 83; address 1000058d6
L10000549a_83:
    LEA R12, [RBP + -2256]
    MOV RDI, R12
    MOV ESI, 2561
    MOV EDX, 384
    XOR EAX, EAX
    CALL _open
    JMP L10000549a_84     ; inserted

; Entry 10000549a; block 84; address 1000058f1
L10000549a_84:
    CMP EAX, 18446744073709551615
    JE L10000549a_86
    JMP L10000549a_85     ; inserted

; Entry 10000549a; block 85; address 1000058fa
L10000549a_85:
    MOV R15D, EAX
    MOV qword [rel L__DATA___bss + 48], R12
    JMP L10000549a_35

; Entry 10000549a; block 86; address 100005cfa
L10000549a_86:
    LEA RDI, [rel L__TEXT___cstring_2718]
    LEA RSI, [RBP + -2256]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_145     ; inserted

; Entry 10000549a; block 87; address 100005920
L10000549a_87:
    MOV EDI, EBX
    MOV ESI, R15D
    XOR EDX, EDX
    MOV ECX, 5
    CALL _fcopyfile
    JMP L10000549a_89     ; inserted

; Entry 10000549a; block 88; address 100005c8f
L10000549a_88:
    LEA RDI, [rel L__TEXT___cstring_2746]
    LEA RSI, [RBP + -2256]
    XOR EAX, EAX
    CALL L100003193_0
    JMP L10000549a_139     ; inserted

; Entry 10000549a; block 89; address 100005931
L10000549a_89:
    XORPS XMM0, XMM0
    LEA RSI, [RBP + -2336]
    MOVAPS oword [RSI], XMM0
    MOV qword [RSI + 16], 0
    MOV word [RSI], 5
    MOV dword [RSI + 4], 16384
    LEA RDX, [RBP + -192]
    MOV ECX, 36
    MOV EDI, R15D
    XOR R8D, R8D
    CALL _fgetattrlist
    JMP L10000549a_90     ; inserted

; Entry 10000549a; block 90; address 100005969
L10000549a_90:
    TEST EAX, EAX
    JNE L10000549a_92
    JMP L10000549a_91     ; inserted

; Entry 10000549a; block 91; address 10000596d
L10000549a_91:
    CMP dword [RBP + -192], 36
    JNE L10000549a_92
    JMP L10000549a_93     ; inserted

; Entry 10000549a; block 92; address 10000599b
L10000549a_92:
    LEA RSI, [RBP + -1232]
    LEA RDX, [RBP + -2256]
    MOV EDI, R15D
    CALL L1000062a6_0
    JMP L10000549a_94     ; inserted

; Entry 10000549a; block 93; address 100005976
L10000549a_93:
    LEA RDX, [RBP + -188]
    MOV qword [RDX], 0
    LEA RSI, [RBP + -2336]
    MOV ECX, 32
    MOV EDI, R15D
    XOR R8D, R8D
    CALL _fsetattrlist
    JMP L10000549a_92     ; inserted

; Entry 10000549a; block 94; address 1000059b1
L10000549a_94:
    MOV qword [rel L__DATA___bss + 48], 0
    MOV EDI, EBX
    CALL _close
    JMP L10000549a_95     ; inserted

; Entry 10000549a; block 95; address 1000059c3
L10000549a_95:
    MOV EDI, R15D
    CALL _close
    JMP L10000549a_96     ; inserted

; Entry 10000549a; block 96; address 1000059cb
L10000549a_96:
    CMP EAX, 18446744073709551615
    JNE L10000549a_98
    JMP L10000549a_97     ; inserted

; Entry 10000549a; block 97; address 1000059d0
L10000549a_97:
    LEA RDI, [rel L__TEXT___cstring_2257]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_98     ; inserted

; Entry 10000549a; block 98; address 1000059de
L10000549a_98:
    LEA RSI, [RBP + -1232]
    MOV RDI, R13
    CALL L1000063eb_0
    JMP L10000549a_99     ; inserted

; Entry 10000549a; block 99; address 1000059ed
L10000549a_99:
    JMP L10000549a_42

; Entry 10000549a; block 100; address 1000059fc
L10000549a_100:
    MOVZX ESI, byte [RBP + -2477]
    MOVZX EDX, byte [RBP + -2478]
    MOVZX ECX, byte [RBP + -2479]
    MOVZX R12D, byte [RBP + -2480]
    TEST byte [RBP + -2333], 8
    JE L10000549a_103
    JMP L10000549a_102     ; inserted

; Entry 10000549a; block 101; address 100005ce8
L10000549a_101:
    CMP byte [rel L__DATA___bss + 12], 0
    JE L10000549a_46
    JMP L10000549a_144     ; inserted

; Entry 10000549a; block 102; address 100005a26
L10000549a_102:
    MOV dword [RBP + -2268], ESI
    MOV qword [RBP + -2280], RDX
    MOV qword [RBP + -2288], RCX
    LEA RSI, [RBP + -1232]
    MOV EDX, 1024
    MOV ECX, 10
    MOV EDI, R15D
    CALL _pread
    JMP L10000549a_104     ; inserted

; Entry 10000549a; block 103; address 100005b13
L10000549a_103:
    SHL ESI, 24
    SHL RDX, 16
    MOVSXD RAX, ESI
    OR RAX, RDX
    SHL RCX, 8
    OR RCX, RAX
    OR R12, RCX
    JMP L10000549a_54     ; inserted

; Entry 10000549a; block 104; address 100005a53
L10000549a_104:
    TEST RAX, RAX
    JS L10000549a_46
    JMP L10000549a_105     ; inserted

; Entry 10000549a; block 105; address 100005a5c
L10000549a_105:
    CMP byte [RBP + -1232], 0
    MOV RCX, qword [RBP + -2288]
    MOV RDX, qword [RBP + -2280]
    MOV ESI, dword [RBP + -2268]
    JE L10000549a_103
    JMP L10000549a_106     ; inserted

; Entry 10000549a; block 106; address 100005a7d
L10000549a_106:
    MOV byte [RBP + RAX-1], 0
    LEA RDI, [RBP + -1232]
    MOV ESI, 47
    MOV qword [RBP + -2304], RAX
    CALL _strrchr
    JMP L10000549a_107     ; inserted

; Entry 10000549a; block 107; address 100005a9d
L10000549a_107:
    MOV qword [RBP + -2264], R12
    LEA R12, [RAX + 1]
    TEST RAX, RAX
    LEA RAX, [RBP + -1232]
    CMOVE R12, RAX
    MOV RDI, R13
    MOV ESI, 47
    CALL _strrchr
    JMP L10000549a_108     ; inserted

; Entry 10000549a; block 108; address 100005ac3
L10000549a_108:
    LEA ECX, [RAX + 1]
    TEST RAX, RAX
    CMOVE ECX, R13D
    SUB ECX, R13D
    MOV qword [RSP], R12
    MOV R12, qword [RBP + -2264]
    LEA RDX, [rel L__TEXT___cstring_1986]
    LEA RDI, [RBP + -2256]
    MOV ESI, 1024
    MOV R8, R13
    MOV R9, qword [RBP + -2304]
    XOR EAX, EAX
    CALL _snprintf
    JMP L10000549a_109     ; inserted

; Entry 10000549a; block 109; address 100005aff
L10000549a_109:
    MOV ESI, dword [RBP + -2268]
    MOV RDX, qword [RBP + -2280]
    MOV RCX, qword [RBP + -2288]
    JMP L10000549a_103     ; inserted

; Entry 10000549a; block 110; address 100005b40
L10000549a_110:
    XORPS XMM0, XMM0
    MOVAPS oword [RBP + -2480], XMM0
    MOVAPS oword [RBP + -2464], XMM0
    MOVAPS oword [RBP + -2448], XMM0
    MOVAPS oword [RBP + -2432], XMM0
    MOVAPS oword [RBP + -2416], XMM0
    MOVAPS oword [RBP + -2400], XMM0
    MOVAPS oword [RBP + -2384], XMM0
    MOVAPS oword [RBP + -2368], XMM0
    MOVAPS oword [RBP + -2352], XMM0
    CMP byte [rel L__DATA___bss + 8], 0
    JNE L10000549a_112
    JMP L10000549a_111     ; inserted

; Entry 10000549a; block 111; address 100005b8f
L10000549a_111:
    MOV CL, byte [rel L__DATA___bss + 4]
    TEST CL, 1
    JE L10000549a_112
    JMP L10000549a_113     ; inserted

; Entry 10000549a; block 112; address 100005c36
L10000549a_112:
    LEA RSI, [RBP + -2480]
    MOV EDI, R15D
    CALL _fstat$INODE64
    JMP L10000549a_132     ; inserted

; Entry 10000549a; block 113; address 100005b9e
L10000549a_113:
    MOV EAX, 1
    TEST R12B, 1
    JNE L10000549a_115
    JMP L10000549a_114     ; inserted

; Entry 10000549a; block 114; address 100005ba9
L10000549a_114:
    TEST CL, 1
    JNE L10000549a_115
    JMP L10000549a_116     ; inserted

; Entry 10000549a; block 115; address 100005bff
L10000549a_115:
    DEC EBX
    CMP EBX, 5
    JA L10000549a_125
    JMP L10000549a_124     ; inserted

; Entry 10000549a; block 116; address 100005bae
L10000549a_116:
    LEA R12, [RBP + -2256]
    MOV RDI, R12
    MOV ESI, 2561
    MOV EDX, 384
    XOR EAX, EAX
    CALL _open
    JMP L10000549a_117     ; inserted

; Entry 10000549a; block 117; address 100005bc9
L10000549a_117:
    CMP EAX, 1
    JNE L10000549a_119
    JMP L10000549a_118     ; inserted

; Entry 10000549a; block 118; address 100005bce
L10000549a_118:
    MOV EDI, 1
    CALL _dup
    JMP L10000549a_120     ; inserted

; Entry 10000549a; block 119; address 100005bef
L10000549a_119:
    CMP EAX, 18446744073709551615
    JE L10000549a_123
    JMP L10000549a_122     ; inserted

; Entry 10000549a; block 120; address 100005bd8
L10000549a_120:
    MOV R12D, EAX
    MOV EDI, 1
    CALL _close
    JMP L10000549a_121     ; inserted

; Entry 10000549a; block 121; address 100005be5
L10000549a_121:
    MOV EAX, R12D
    LEA R12, [RBP + -2256]
    JMP L10000549a_119     ; inserted

; Entry 10000549a; block 122; address 100005bf8
L10000549a_122:
    MOV qword [rel L__DATA___bss + 48], R12
    JMP L10000549a_115     ; inserted

; Entry 10000549a; block 123; address 100005d23
L10000549a_123:
    LEA RDI, [rel L__TEXT___cstring_1905]
    JMP L10000549a_207     ; inserted

; Entry 10000549a; block 124; address 100005c0a
L10000549a_124:
    LEA RDX, [rel L__TEXT___text + 16088]
    MOVSXD RCX, dword [RDX + RBX * 4]
    ADD RCX, RDX
    MOV RCX, QWORD PTR [L_JUMP_TABLE_100005c18 + 8*EBX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RCX; TARGETS: 100005c1a,100005d78,100005d91,100005daa,100005dc3

; Entry 10000549a; block 125; address 100005dc3
L10000549a_125:
    CMP byte [rel L__DATA___bss + 8], 1
    JNE L10000549a_161
    JMP L10000549a_160     ; inserted

; Entry 10000549a; block 126; address 100005c1a
L10000549a_126:
    CMP byte [rel L__DATA___bss + 8], 1
    JNE L10000549a_131
    JMP L10000549a_130     ; inserted

; Entry 10000549a; block 127; address 100005d78
L10000549a_127:
    MOV R12D, EAX
    CMP byte [rel L__DATA___bss + 8], 1
    JNE L10000549a_154
    JMP L10000549a_153     ; inserted

; Entry 10000549a; block 128; address 100005d91
L10000549a_128:
    CMP byte [rel L__DATA___bss + 8], 1
    JNE L10000549a_157
    JMP L10000549a_156     ; inserted

; Entry 10000549a; block 129; address 100005daa
L10000549a_129:
    CMP byte [rel L__DATA___bss + 8], 1
    JNE L10000549a_159
    JMP L10000549a_158     ; inserted

; Entry 10000549a; block 130; address 100005c27
L10000549a_130:
    MOV R12D, EAX
    LEA RDI, [rel L__TEXT___cstring_2029]
    JMP L10000549a_155

; Entry 10000549a; block 131; address 100005e2e
L10000549a_131:
    MOV EDI, R15D
    MOV R12D, EAX
    MOV ESI, EAX
    XOR EDX, EDX
    XOR ECX, ECX
    XOR R8D, R8D
    CALL L100003c24_0
    JMP L10000549a_166     ; inserted

; Entry 10000549a; block 132; address 100005c45
L10000549a_132:
    TEST EAX, EAX
    JNE L10000549a_45
    JMP L10000549a_133     ; inserted

; Entry 10000549a; block 133; address 100005c4d
L10000549a_133:
    MOVZX EDX, word [RBP + -2474]
    CMP EDX, 2
    JB L10000549a_135
    JMP L10000549a_134     ; inserted

; Entry 10000549a; block 134; address 100005c5d
L10000549a_134:
    TEST byte [rel L__DATA___bss + 8], 1
    JNE L10000549a_135
    JMP L10000549a_136     ; inserted

; Entry 10000549a; block 135; address 100005d36
L10000549a_135:
    MOV RAX, qword [RBP + -2264]
    TEST RAX, RAX
    JE L10000549a_149
    JMP L10000549a_148     ; inserted

; Entry 10000549a; block 136; address 100005c6a
L10000549a_136:
    TEST byte [rel L__DATA___bss + 12], 1
    JNE L10000549a_135
    JMP L10000549a_137     ; inserted

; Entry 10000549a; block 137; address 100005c77
L10000549a_137:
    DEC EDX
    LEA RDI, [rel L__TEXT___cstring_1995]
    MOV RSI, R13
    XOR EAX, EAX
    CALL L100003193_0
    JMP L10000549a_138     ; inserted

; Entry 10000549a; block 138; address 100005c8a
L10000549a_138:
    JMP L10000549a_45

; Entry 10000549a; block 139; address 100005ca4
L10000549a_139:
    MOV EDI, R15D
    CALL _close
    JMP L10000549a_140     ; inserted

; Entry 10000549a; block 140; address 100005cac
L10000549a_140:
    CMP EAX, 18446744073709551615
    JNE L10000549a_142
    JMP L10000549a_141     ; inserted

; Entry 10000549a; block 141; address 100005cb1
L10000549a_141:
    LEA RDI, [rel L__TEXT___cstring_2257]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_142     ; inserted

; Entry 10000549a; block 142; address 100005cbf
L10000549a_142:
    LEA RDI, [rel L__TEXT___cstring_2796]
    MOV RSI, R13
    XOR EAX, EAX
    CALL L100003193_0
    JMP L10000549a_143     ; inserted

; Entry 10000549a; block 143; address 100005cd0
L10000549a_143:
    LEA RDI, [RBP + -2256]
    CALL _unlink
    JMP L10000549a_42     ; inserted

; Entry 10000549a; block 144; address 100005cf5
L10000549a_144:
    JMP L10000549a_45

; Entry 10000549a; block 145; address 100005d0f
L10000549a_145:
    MOV RAX, qword [rel ___stdinp]
    MOV RDI, qword [RAX]
    CALL _fclose
    JMP L10000549a_146     ; inserted

; Entry 10000549a; block 146; address 100005d1e
L10000549a_146:
    JMP L10000549a_24

; Entry 10000549a; block 147; address 1000056cb
L10000549a_147:
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_48     ; inserted

; Entry 10000549a; block 148; address 100005d42
L10000549a_148:
    TEST byte [rel L__DATA___bss + 20], 1
    JNE L10000549a_149
    JMP L10000549a_150     ; inserted

; Entry 10000549a; block 149; address 100005d52
L10000549a_149:
    LEA RDI, [RBP + -2256]
    CALL L100006193_0
    JMP L10000549a_151     ; inserted

; Entry 10000549a; block 150; address 100005d4b
L10000549a_150:
    MOV qword [RBP + -2432], RAX
    JMP L10000549a_149     ; inserted

; Entry 10000549a; block 151; address 100005d5e
L10000549a_151:
    TEST EAX, EAX
    JE L10000549a_45
    JMP L10000549a_152     ; inserted

; Entry 10000549a; block 152; address 100005d66
L10000549a_152:
    MOV CL, byte [rel L__DATA___bss + 4]
    MOV R12B, byte [rel L__DATA___bss + 8]
    JMP L10000549a_113

; Entry 10000549a; block 153; address 100005d88
L10000549a_153:
    LEA RDI, [rel L__TEXT___cstring_2052]
    JMP L10000549a_155

; Entry 10000549a; block 154; address 100005e47
L10000549a_154:
    MOV EDI, R15D
    CALL L100003f14_0
    JMP L10000549a_167     ; inserted

; Entry 10000549a; block 155; address 100005e09
L10000549a_155:
    XOR EAX, EAX
    CALL L100003193_0
    JMP L10000549a_163     ; inserted

; Entry 10000549a; block 156; address 100005d9e
L10000549a_156:
    MOV R12D, EAX
    LEA RDI, [rel L__TEXT___cstring_2164]
    JMP L10000549a_155

; Entry 10000549a; block 157; address 100005f14
L10000549a_157:
    MOV EDI, R15D
    MOV R12D, EAX
    MOV ESI, EAX
    XOR EDX, EDX
    XOR ECX, ECX
    XOR R8D, R8D
    CALL L10000408c_0
    JMP L10000549a_186     ; inserted

; Entry 10000549a; block 158; address 100005db7
L10000549a_158:
    MOV R12D, EAX
    LEA RDI, [rel L__TEXT___cstring_2188]
    JMP L10000549a_155

; Entry 10000549a; block 159; address 100005f2a
L10000549a_159:
    MOV EDI, R15D
    MOV R12D, EAX
    MOV ESI, EAX
    XOR EDX, EDX
    XOR ECX, ECX
    XOR R8D, R8D
    CALL L100004512_0
    JMP L10000549a_188     ; inserted

; Entry 10000549a; block 160; address 100005dd0
L10000549a_160:
    MOV RCX, qword [RBP + -2432]
    MOV RSI, qword [RBP + -2384]
    LEA RDX, [RBP + -2256]
    MOV EDI, R15D
    CALL L100002f77_0
    JMP L10000549a_162     ; inserted

; Entry 10000549a; block 161; address 100005f40
L10000549a_161:
    MOV EDI, R15D
    MOV R12D, EAX
    MOV ESI, EAX
    XOR EDX, EDX
    XOR ECX, ECX
    XOR R8D, R8D
    MOV R9, R13
    CALL L1000034a3_0
    JMP L10000549a_189     ; inserted

; Entry 10000549a; block 162; address 100005ded
L10000549a_162:
    JMP L10000549a_45

; Entry 10000549a; block 163; address 100005e10
L10000549a_163:
    MOV EDI, R15D
    CALL _close
    JMP L10000549a_164     ; inserted

; Entry 10000549a; block 164; address 100005e18
L10000549a_164:
    MOV RBX, 18446744073709551615
    CMP R12D, 1
    JNE L10000549a_21
    JMP L10000549a_165     ; inserted

; Entry 10000549a; block 165; address 100005e29
L10000549a_165:
    JMP L10000549a_22

; Entry 10000549a; block 166; address 100005e42
L10000549a_166:
    JMP L10000549a_187

; Entry 10000549a; block 167; address 100005e4f
L10000549a_167:
    TEST RAX, RAX
    JE L10000549a_169
    JMP L10000549a_168     ; inserted

; Entry 10000549a; block 168; address 100005e58
L10000549a_168:
    MOV RBX, RAX
    MOV EDI, R12D
    CALL _dup
    JMP L10000549a_170     ; inserted

; Entry 10000549a; block 169; address 10000602e
L10000549a_169:
    LEA RDI, [rel L__TEXT___cstring_2080]
    MOV RSI, R13
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_211     ; inserted

; Entry 10000549a; block 170; address 100005e63
L10000549a_170:
    LEA RSI, [rel L__TEXT___cstring_1212]
    MOV EDI, EAX
    CALL _fdopen
    JMP L10000549a_171     ; inserted

; Entry 10000549a; block 171; address 100005e71
L10000549a_171:
    TEST RAX, RAX
    JE L10000549a_173
    JMP L10000549a_172     ; inserted

; Entry 10000549a; block 172; address 100005e7a
L10000549a_172:
    MOV RDI, RBX
    MOV qword [RBP + -2264], RAX
    MOV RSI, RAX
    XOR EDX, EDX
    XOR ECX, ECX
    XOR R8D, R8D
    CALL L100003fa4_0
    JMP L10000549a_174     ; inserted

; Entry 10000549a; block 173; address 10000605e
L10000549a_173:
    LEA RDI, [rel L__TEXT___cstring_2100]
    LEA RSI, [RBP + -2256]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_213     ; inserted

; Entry 10000549a; block 174; address 100005e93
L10000549a_174:
    MOV RDI, RBX
    MOV qword [RBP + -2280], RBX
    MOV RBX, RAX
    CALL _ferror
    JMP L10000549a_175     ; inserted

; Entry 10000549a; block 175; address 100005ea5
L10000549a_175:
    MOV dword [RBP + -2288], EAX
    MOV RDI, qword [RBP + -2280]
    CALL _fclose
    JMP L10000549a_176     ; inserted

; Entry 10000549a; block 176; address 100005eb7
L10000549a_176:
    OR EAX, dword [RBP + -2288]
    JE L10000549a_178
    JMP L10000549a_177     ; inserted

; Entry 10000549a; block 177; address 100005ebf
L10000549a_177:
    LEA RDI, [rel L__TEXT___cstring_2121]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_179     ; inserted

; Entry 10000549a; block 178; address 100005ee5
L10000549a_178:
    MOV RDI, qword [RBP + -2264]
    CALL _fclose
    JMP L10000549a_181     ; inserted

; Entry 10000549a; block 179; address 100005ecd
L10000549a_179:
    LEA RDI, [RBP + -2256]
    CALL _unlink
    JMP L10000549a_180     ; inserted

; Entry 10000549a; block 180; address 100005ed9
L10000549a_180:
    MOV RDI, qword [RBP + -2264]
    CALL _fclose
    JMP L10000549a_178     ; inserted

; Entry 10000549a; block 181; address 100005ef1
L10000549a_181:
    TEST EAX, EAX
    JE L10000549a_183
    JMP L10000549a_182     ; inserted

; Entry 10000549a; block 182; address 100005ef5
L10000549a_182:
    LEA RDI, [rel L__TEXT___cstring_2142]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_184     ; inserted

; Entry 10000549a; block 183; address 100005f6a
L10000549a_183:
    MOV EDI, R15D
    CALL _close
    JMP L10000549a_190     ; inserted

; Entry 10000549a; block 184; address 100005f03
L10000549a_184:
    LEA RDI, [RBP + -2256]
    CALL _unlink
    JMP L10000549a_185     ; inserted

; Entry 10000549a; block 185; address 100005f0f
L10000549a_185:
    JMP L10000549a_163

; Entry 10000549a; block 186; address 100005f28
L10000549a_186:
    JMP L10000549a_187

; Entry 10000549a; block 187; address 100005f67
L10000549a_187:
    MOV RBX, RAX
    JMP L10000549a_183     ; inserted

; Entry 10000549a; block 188; address 100005f3e
L10000549a_188:
    JMP L10000549a_187

; Entry 10000549a; block 189; address 100005f57
L10000549a_189:
    JMP L10000549a_187

; Entry 10000549a; block 190; address 100005f72
L10000549a_190:
    TEST EAX, EAX
    JE L10000549a_192
    JMP L10000549a_191     ; inserted

; Entry 10000549a; block 191; address 100005f76
L10000549a_191:
    LEA RDI, [rel L__TEXT___cstring_2236]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_192     ; inserted

; Entry 10000549a; block 192; address 100005f84
L10000549a_192:
    CMP R12D, 1
    JE L10000549a_194
    JMP L10000549a_193     ; inserted

; Entry 10000549a; block 193; address 100005f8a
L10000549a_193:
    MOV EDI, R12D
    CALL _close
    JMP L10000549a_195     ; inserted

; Entry 10000549a; block 194; address 100005fa4
L10000549a_194:
    CMP RBX, 18446744073709551615
    JE L10000549a_198
    JMP L10000549a_197     ; inserted

; Entry 10000549a; block 195; address 100005f92
L10000549a_195:
    TEST EAX, EAX
    JE L10000549a_194
    JMP L10000549a_196     ; inserted

; Entry 10000549a; block 196; address 100005f96
L10000549a_196:
    LEA RDI, [rel L__TEXT___cstring_2257]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_194     ; inserted

; Entry 10000549a; block 197; address 100005faa
L10000549a_197:
    CMP byte [rel L__DATA___bss + 36], 0
    JNE L10000549a_22
    JMP L10000549a_199     ; inserted

; Entry 10000549a; block 198; address 100006003
L10000549a_198:
    CMP byte [rel L__DATA___bss + 4], 0
    JNE L10000549a_209
    JMP L10000549a_208     ; inserted

; Entry 10000549a; block 199; address 100005fb7
L10000549a_199:
    CMP byte [rel L__DATA___bss + 4], 0
    JNE L10000549a_22
    JMP L10000549a_200     ; inserted

; Entry 10000549a; block 200; address 100005fc4
L10000549a_200:
    LEA RDI, [RBP + -2256]
    MOV ESI, 2
    XOR EDX, EDX
    XOR EAX, EAX
    CALL _open
    JMP L10000549a_201     ; inserted

; Entry 10000549a; block 201; address 100005fd9
L10000549a_201:
    CMP EAX, 18446744073709551615
    JE L10000549a_203
    JMP L10000549a_202     ; inserted

; Entry 10000549a; block 202; address 100005fde
L10000549a_202:
    MOV R15D, EAX
    LEA RSI, [RBP + -192]
    MOV EDI, EAX
    CALL _fstat$INODE64
    JMP L10000549a_204     ; inserted

; Entry 10000549a; block 203; address 100006044
L10000549a_203:
    LEA RDI, [rel L__TEXT___cstring_2301]
    LEA RSI, [RBP + -2256]
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L10000549a_212     ; inserted

; Entry 10000549a; block 204; address 100005fef
L10000549a_204:
    TEST EAX, EAX
    JE L10000549a_206
    JMP L10000549a_205     ; inserted

; Entry 10000549a; block 205; address 100005ff7
L10000549a_205:
    LEA RDI, [rel L__TEXT___cstring_2338]
    JMP L10000549a_207

; Entry 10000549a; block 206; address 100006080
L10000549a_206:
    MOV RDX, qword [RBP + -96]
    CMP RDX, RBX
    JNE L10000549a_216
    JMP L10000549a_215     ; inserted

; Entry 10000549a; block 207; address 100005d2a
L10000549a_207:
    LEA RSI, [RBP + -2256]
    JMP L10000549a_147

; Entry 10000549a; block 208; address 10000600c
L10000549a_208:
    LEA RDI, [RBP + -2256]
    CALL _unlink
    JMP L10000549a_209     ; inserted

; Entry 10000549a; block 209; address 100006018
L10000549a_209:
    LEA RDI, [rel L__TEXT___cstring_2279]
    MOV RSI, R13
    XOR EAX, EAX
    CALL L100003193_0
    JMP L10000549a_210     ; inserted

; Entry 10000549a; block 210; address 100006029
L10000549a_210:
    JMP L10000549a_58

; Entry 10000549a; block 211; address 10000603f
L10000549a_211:
    JMP L10000549a_163

; Entry 10000549a; block 212; address 100006059
L10000549a_212:
    JMP L10000549a_58

; Entry 10000549a; block 213; address 100006073
L10000549a_213:
    MOV RDI, RBX
    CALL _fclose
    JMP L10000549a_214     ; inserted

; Entry 10000549a; block 214; address 10000607b
L10000549a_214:
    JMP L10000549a_163

; Entry 10000549a; block 215; address 100006089
L10000549a_215:
    LEA R12, [RBP + -2480]
    LEA RDX, [RBP + -2256]
    MOV EDI, R15D
    MOV RSI, R12
    CALL L1000062a6_0
    JMP L10000549a_217     ; inserted

; Entry 10000549a; block 216; address 1000060c5
L10000549a_216:
    LEA RDI, [rel L__TEXT___cstring_2375]
    MOV RSI, RBX
    XOR EAX, EAX
    CALL L100003193_0
    JMP L10000549a_220     ; inserted

; Entry 10000549a; block 217; address 1000060a2
L10000549a_217:
    MOV qword [rel L__DATA___bss + 48], 0
    MOV EDI, R15D
    CALL _close
    JMP L10000549a_218     ; inserted

; Entry 10000549a; block 218; address 1000060b5
L10000549a_218:
    MOV RDI, R13
    MOV RSI, R12
    CALL L1000063eb_0
    JMP L10000549a_219     ; inserted

; Entry 10000549a; block 219; address 1000060c0
L10000549a_219:
    JMP L10000549a_22

; Entry 10000549a; block 220; address 1000060d6
L10000549a_220:
    MOV EDI, R15D
    CALL _close
    JMP L10000549a_221     ; inserted

; Entry 10000549a; block 221; address 1000060de
L10000549a_221:
    LEA RDI, [RBP + -2256]
    CALL _unlink
    JMP L10000549a_222     ; inserted

; Entry 10000549a; block 222; address 1000060ea
L10000549a_222:
    JMP L10000549a_58



; ---------------------
; Function: 0x10000610c
; ---------------------
; Entry 10000610c; block 0; address 10000610c
L10000610c_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    PUSH RAX
    MOV dword [RBP + -44], ESI
    MOV RBX, RDI
    CALL _strlen
    JMP L10000610c_1     ; inserted

; Entry 10000610c; block 1; address 100006125
L10000610c_1:
    MOV R12, RAX
    MOVSXD R15, R12D
    ADD R15, RBX
    LEA R13, [rel L__DATA___data + 16]
    XOR R14D, R14D
    JMP L10000610c_6     ; inserted

; Entry 10000610c; block 2; address 100006142
L10000610c_2:
    MOV RBX, R15
    SUB RBX, RAX
    MOV RDI, qword [R14 + R13]
    MOV RSI, RBX
    CALL _strcmp
    JMP L10000610c_4     ; inserted

; Entry 10000610c; block 3; address 100006158
L10000610c_3:
    ADD R14, 24
    CMP R14, 336
    JNE L10000610c_6
    JMP L10000610c_7     ; inserted

; Entry 10000610c; block 4; address 100006154
L10000610c_4:
    TEST EAX, EAX
    JE L10000610c_5
    JMP L10000610c_3     ; inserted

; Entry 10000610c; block 5; address 10000616a
L10000610c_5:
    LEA R15, [R14 + R13]
    CMP dword [RBP + -44], 0
    JE L10000610c_9
    JMP L10000610c_8     ; inserted

; Entry 10000610c; block 6; address 100006138
L10000610c_6:
    MOVSXD RAX, dword [R14 + R13 + 8]
    CMP EAX, R12D
    JGE L10000610c_3
    JMP L10000610c_2     ; inserted

; Entry 10000610c; block 7; address 100006165
L10000610c_7:
    XOR R15D, R15D
    JMP L10000610c_9

; Entry 10000610c; block 8; address 100006174
L10000610c_8:
    MOV RSI, qword [R14 + R13 + 16]
    MOV RDI, RBX
    CALL _strcpy
    JMP L10000610c_9     ; inserted

; Entry 10000610c; block 9; address 100006181
L10000610c_9:
    MOV RAX, R15
    ADD RSP, 8
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 



; ---------------------
; Function: 0x100006193
; ---------------------
; Entry 100006193; block 0; address 100006193
L100006193_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R12
    PUSH RBX
    SUB RSP, 176
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -40], RAX
    MOV EBX, 1
    CMP byte [rel L__DATA___bss + 8], 0
    JNE L100006193_2
    JMP L100006193_1     ; inserted

; Entry 100006193; block 1; address 1000061c5
L100006193_1:
    MOV R14, RDI
    LEA RSI, [RBP + -200]
    CALL _stat$INODE64
    JMP L100006193_3     ; inserted

; Entry 100006193; block 2; address 10000627f
L100006193_2:
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    CMP RAX, qword [RBP + -40]
    JNE L100006193_17
    JMP L100006193_16     ; inserted

; Entry 100006193; block 3; address 1000061d4
L100006193_3:
    TEST EAX, EAX
    JNE L100006193_2
    JMP L100006193_4     ; inserted

; Entry 100006193; block 4; address 1000061dc
L100006193_4:
    CMP byte [rel L__DATA___bss + 12], 1
    JE L100006193_6
    JMP L100006193_5     ; inserted

; Entry 100006193; block 5; address 1000061e5
L100006193_5:
    XOR EDI, EDI
    CALL _isatty
    JMP L100006193_7     ; inserted

; Entry 100006193; block 6; address 100006246
L100006193_6:
    MOV RDI, R14
    CALL _unlink
    JMP L100006193_13     ; inserted

; Entry 100006193; block 7; address 1000061ec
L100006193_7:
    TEST EAX, EAX
    JE L100006193_9
    JMP L100006193_8     ; inserted

; Entry 100006193; block 8; address 1000061f0
L100006193_8:
    MOVZX EAX, word [rel L__TEXT___const + 169]
    LEA R15, [RBP + -56]
    MOV word [R15 + 8], AX
    MOV RAX, qword [rel L__TEXT___const + 161]
    MOV qword [R15], RAX
    MOV R12, qword [rel ___stderrp]
    MOV RDI, qword [R12]
    LEA RSI, [rel L__TEXT___cstring_2431]
    MOV RDX, R14
    XOR EAX, EAX
    CALL _fprintf
    JMP L100006193_10     ; inserted

; Entry 100006193; block 9; address 100006250
L100006193_9:
    LEA RDI, [rel L__TEXT___cstring_2506]
    MOV RSI, R14
    XOR EAX, EAX
    CALL L100003193_0
    JMP L100006193_14     ; inserted

; Entry 100006193; block 10; address 100006226
L100006193_10:
    MOV RAX, qword [rel ___stdinp]
    MOV RDX, qword [RAX]
    MOV RDI, R15
    MOV ESI, 9
    CALL _fgets
    JMP L100006193_11     ; inserted

; Entry 100006193; block 11; address 10000623d
L100006193_11:
    MOV AL, byte [R15]
    OR AL, 32
    CMP AL, 121
    JNE L100006193_12
    JMP L100006193_6     ; inserted

; Entry 100006193; block 12; address 100006263
L100006193_12:
    MOV RCX, qword [R12]
    LEA RDI, [rel L__TEXT___cstring_2488]
    MOV ESI, 17
    MOV EDX, 1
    CALL _fwrite
    JMP L100006193_15     ; inserted

; Entry 100006193; block 13; address 10000624e
L100006193_13:
    JMP L100006193_2

; Entry 100006193; block 14; address 100006261
L100006193_14:
    JMP L100006193_15

; Entry 100006193; block 15; address 10000627d
L100006193_15:
    XOR EBX, EBX
    JMP L100006193_2     ; inserted

; Entry 100006193; block 16; address 10000628f
L100006193_16:
    MOV EAX, EBX
    ADD RSP, 176
    POP RBX
    POP R12
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100006193; block 17; address 1000062a1
L100006193_17:
    CALL ___stack_chk_fail



; ---------------------
; Function: 0x1000062a6
; ---------------------
; Entry 1000062a6; block 0; address 1000062a6
L1000062a6_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R12
    PUSH RBX
    SUB RSP, 96
    MOV R12D, EDI
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -40], RAX
    TEST RSI, RSI
    JE L1000062a6_2
    JMP L1000062a6_1     ; inserted

; Entry 1000062a6; block 1; address 1000062cf
L1000062a6_1:
    MOV R14, RDX
    MOVZX EBX, word [RSI + 4]
    MOV EAX, dword [RSI + 16]
    MOV EDX, dword [RSI + 20]
    MOV RCX, qword [RSI + 40]
    MOV qword [RBP + -112], RCX
    MOVUPS XMM0, oword [RSI + 24]
    MOVAPS oword [RBP + -128], XMM0
    MOVUPS XMM0, oword [RSI + 48]
    MOVAPS oword [RBP + -96], XMM0
    MOV R15D, dword [RSI + 116]
    MOV EDI, R12D
    MOV ESI, EAX
    CALL _fchown
    JMP L1000062a6_3     ; inserted

; Entry 1000062a6; block 2; address 1000063a3
L1000062a6_2:
    MOV EDI, 18
    CALL _umask
    JMP L1000062a6_20     ; inserted

; Entry 1000062a6; block 3; address 100006302
L1000062a6_3:
    TEST EAX, EAX
    JNS L1000062a6_5
    JMP L1000062a6_4     ; inserted

; Entry 1000062a6; block 4; address 100006306
L1000062a6_4:
    CALL ___error
    JMP L1000062a6_6     ; inserted

; Entry 1000062a6; block 5; address 100006327
L1000062a6_5:
    AND EBX, 3583
    MOV EDI, R12D
    MOV ESI, EBX
    CALL _fchmod
    JMP L1000062a6_9     ; inserted

; Entry 1000062a6; block 6; address 10000630b
L1000062a6_6:
    CMP dword [RAX], 1
    JE L1000062a6_8
    JMP L1000062a6_7     ; inserted

; Entry 1000062a6; block 7; address 100006310
L1000062a6_7:
    LEA RDI, [rel L__TEXT___cstring_2536]
    MOV RSI, R14
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L1000062a6_8     ; inserted

; Entry 1000062a6; block 8; address 100006321
L1000062a6_8:
    AND EBX, 62463
    JMP L1000062a6_5     ; inserted

; Entry 1000062a6; block 9; address 100006337
L1000062a6_9:
    TEST EAX, EAX
    JNS L1000062a6_11
    JMP L1000062a6_10     ; inserted

; Entry 1000062a6; block 10; address 10000633b
L1000062a6_10:
    LEA RDI, [rel L__TEXT___cstring_2556]
    MOV RSI, R14
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L1000062a6_11     ; inserted

; Entry 1000062a6; block 11; address 10000634c
L1000062a6_11:
    MOVUPS XMM0, oword [RBP + -120]
    LEA RSI, [RBP + -80]
    MOVAPS oword [RSI], XMM0
    MOVAPS XMM0, oword [RBP + -96]
    MOVAPS oword [RSI + 16], XMM0
    MOV EDI, R12D
    CALL L10000221c_0
    JMP L1000062a6_12     ; inserted

; Entry 1000062a6; block 12; address 100006367
L1000062a6_12:
    TEST EAX, EAX
    JNS L1000062a6_14
    JMP L1000062a6_13     ; inserted

; Entry 1000062a6; block 13; address 10000636b
L1000062a6_13:
    LEA RDI, [rel L__TEXT___cstring_2576]
    MOV RSI, R14
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L1000062a6_14     ; inserted

; Entry 1000062a6; block 14; address 10000637c
L1000062a6_14:
    TEST R15D, R15D
    JE L1000062a6_16
    JMP L1000062a6_15     ; inserted

; Entry 1000062a6; block 15; address 100006381
L1000062a6_15:
    MOV EDI, R12D
    MOV ESI, R15D
    CALL _fchflags
    JMP L1000062a6_17     ; inserted

; Entry 1000062a6; block 16; address 1000063c9
L1000062a6_16:
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    CMP RAX, qword [RBP + -40]
    JNE L1000062a6_23
    JMP L1000062a6_22     ; inserted

; Entry 1000062a6; block 17; address 10000638c
L1000062a6_17:
    TEST EAX, EAX
    JNS L1000062a6_16
    JMP L1000062a6_18     ; inserted

; Entry 1000062a6; block 18; address 100006390
L1000062a6_18:
    LEA RDI, [rel L__TEXT___cstring_2598]
    MOV RSI, R14
    XOR EAX, EAX
    CALL L10000325e_0
    JMP L1000062a6_19     ; inserted

; Entry 1000062a6; block 19; address 1000063a1
L1000062a6_19:
    JMP L1000062a6_16

; Entry 1000062a6; block 20; address 1000063ad
L1000062a6_20:
    MOV EBX, EAX
    MOV ESI, EBX
    NOT ESI
    AND ESI, 438
    MOV EDI, R12D
    CALL _fchmod
    JMP L1000062a6_21     ; inserted

; Entry 1000062a6; block 21; address 1000063c1
L1000062a6_21:
    MOVZX EDI, BX
    CALL _umask
    JMP L1000062a6_16     ; inserted

; Entry 1000062a6; block 22; address 1000063d9
L1000062a6_22:
    ADD RSP, 96
    POP RBX
    POP R12
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 1000062a6; block 23; address 1000063e6
L1000062a6_23:
    CALL ___stack_chk_fail



; ---------------------
; Function: 0x1000063eb
; ---------------------
; Entry 1000063eb; block 0; address 1000063eb
L1000063eb_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R14
    PUSH RBX
    SUB RSP, 144
    CMP byte [rel L__DATA___bss + 16], 0
    JNE L1000063eb_2
    JMP L1000063eb_1     ; inserted

; Entry 1000063eb; block 1; address 100006402
L1000063eb_1:
    MOV R14, RSI
    MOV RBX, RDI
    XORPS XMM0, XMM0
    LEA RSI, [RBP + -160]
    MOVAPS oword [RSI + 128], XMM0
    MOVAPS oword [RSI + 112], XMM0
    MOVAPS oword [RSI + 96], XMM0
    MOVAPS oword [RSI + 80], XMM0
    MOVAPS oword [RSI + 64], XMM0
    MOVAPS oword [RSI + 48], XMM0
    MOVAPS oword [RSI + 32], XMM0
    MOVAPS oword [RSI + 16], XMM0
    MOVAPS oword [RSI], XMM0
    CALL _stat$INODE64
    JMP L1000063eb_3     ; inserted

; Entry 1000063eb; block 2; address 100006461
L1000063eb_2:
    ADD RSP, 144
    POP RBX
    POP R14
    POP RBP
    RET 

; Entry 1000063eb; block 3; address 10000643d
L1000063eb_3:
    TEST EAX, EAX
    JNE L1000063eb_2
    JMP L1000063eb_4     ; inserted

; Entry 1000063eb; block 4; address 100006441
L1000063eb_4:
    MOV EAX, dword [RBP + -160]
    CMP EAX, dword [R14]
    JNE L1000063eb_2
    JMP L1000063eb_5     ; inserted

; Entry 1000063eb; block 5; address 10000644c
L1000063eb_5:
    MOV RAX, qword [RBP + -152]
    CMP RAX, qword [R14 + 8]
    JNE L1000063eb_2
    JMP L1000063eb_6     ; inserted

; Entry 1000063eb; block 6; address 100006459
L1000063eb_6:
    MOV RDI, RBX
    CALL _unlink
    JMP L1000063eb_2     ; inserted




section .data
L__TEXT___cstring_0: db `GZIP`, 0
L__TEXT___cstring_5: db `gunzip`, 0
L__TEXT___cstring_12: db `zcat`, 0
L__TEXT___cstring_17: db `gzcat`, 0
L__TEXT___cstring_23: db `123456789acdfhklLNnqrS:tVv`, 0
L__TEXT___cstring_50: db `%s: option --ascii ignored on this system\n`, 0
L__TEXT___cstring_93: db `incorrect suffix: '%s': too long`, 0
L__TEXT___cstring_126: db ``, 0
L__TEXT___cstring_127: db `(totals)`, 0
L__TEXT___cstring_136: db `stdout`, 0
L__TEXT___cstring_143: db `to-stdout`, 0
L__TEXT___cstring_153: db `decompress`, 0
L__TEXT___cstring_164: db `uncompress`, 0
L__TEXT___cstring_175: db `force`, 0
L__TEXT___cstring_181: db `help`, 0
L__TEXT___cstring_186: db `keep`, 0
L__TEXT___cstring_191: db `list`, 0
L__TEXT___cstring_196: db `no-name`, 0
L__TEXT___cstring_204: db `name`, 0
L__TEXT___cstring_209: db `quiet`, 0
L__TEXT___cstring_215: db `recursive`, 0
L__TEXT___cstring_225: db `suffix`, 0
L__TEXT___cstring_232: db `test`, 0
L__TEXT___cstring_237: db `verbose`, 0
L__TEXT___cstring_245: db `version`, 0
L__TEXT___cstring_253: db `fast`, 0
L__TEXT___cstring_258: db `best`, 0
L__TEXT___cstring_263: db `ascii`, 0
L__TEXT___cstring_269: db `license`, 0
L__TEXT___cstring_277: db `.gz`, 0
L__TEXT___cstring_281: db `.z`, 0
L__TEXT___cstring_284: db `-gz`, 0
L__TEXT___cstring_288: db `-z`, 0
L__TEXT___cstring_291: db `_z`, 0
L__TEXT___cstring_294: db `.taz`, 0
L__TEXT___cstring_299: db `.tar`, 0
L__TEXT___cstring_304: db `.tgz`, 0
L__TEXT___cstring_309: db `.bz2`, 0
L__TEXT___cstring_314: db `.tbz`, 0
L__TEXT___cstring_319: db `.tbz2`, 0
L__TEXT___cstring_325: db `.Z`, 0
L__TEXT___cstring_328: db `.xz`, 0
L__TEXT___cstring_332: db `malloc`, 0
L__TEXT___cstring_339: db `strndup`, 0
L__TEXT___cstring_347: db `standard input is a terminal -- ignoring`, 0
L__TEXT___cstring_388: db `fstat`, 0
L__TEXT___cstring_394: db `can't read stdin`, 0
L__TEXT___cstring_411: db `(stdin): unexpected end of file`, 0
L__TEXT___cstring_443: db `unknown compression format`, 0
L__TEXT___cstring_470: db `(stdin)`, 0
L__TEXT___cstring_478: db `zopen of stdin`, 0
L__TEXT___cstring_493: db `write to stdout`, 0
L__TEXT___cstring_509: db `read from fd %d`, 0
L__TEXT___cstring_525: db `malloc failed`, 0
L__TEXT___cstring_539: db `failed to read stdin`, 0
L__TEXT___cstring_560: db `%s: unexpected end of file`, 0
L__TEXT___cstring_587: db `%s: trailing garbage ignored`, 0
L__TEXT___cstring_616: db `input not gziped (MAGIC0)`, 0
L__TEXT___cstring_642: db `input not gziped (MAGIC1)`, 0
L__TEXT___cstring_668: db `unknown compression method`, 0
L__TEXT___cstring_695: db `1.2.11`, 0
L__TEXT___cstring_702: db `failed to inflateInit`, 0
L__TEXT___cstring_724: db `Z_NEED_DICT error`, 0
L__TEXT___cstring_742: db `data stream error`, 0
L__TEXT___cstring_760: db `internal stream error`, 0
L__TEXT___cstring_782: db `memory allocation error`, 0
L__TEXT___cstring_806: db `unknown error from inflate(): %d`, 0
L__TEXT___cstring_839: db `error writing to output`, 0
L__TEXT___cstring_863: db `truncated input`, 0
L__TEXT___cstring_879: db `invalid compressed data--crc error`, 0
L__TEXT___cstring_914: db `invalid compressed data--length error`, 0
L__TEXT___cstring_952: db `decompression error`, 0
L__TEXT___cstring_972: db `bzip2 init`, 0
L__TEXT___cstring_983: db `read`, 0
L__TEXT___cstring_988: db `truncated file`, 0
L__TEXT___cstring_1003: db `write`, 0
L__TEXT___cstring_1009: db `bzip2 re-init`, 0
L__TEXT___cstring_1023: db `bzip2 data integrity error`, 0
L__TEXT___cstring_1050: db `bzip2 magic number error`, 0
L__TEXT___cstring_1075: db `bzip2 out of memory`, 0
L__TEXT___cstring_1095: db `unknown bzip2 error: %d`, 0
L__TEXT___cstring_1119: db `r`, 0
L__TEXT___cstring_1121: db `dup`, 0
L__TEXT___cstring_1125: db `Error reading pack header`, 0
L__TEXT___cstring_1151: db `Huffman tree has insane levels`, 0
L__TEXT___cstring_1182: db `Can not fdopen() input stream`, 0
L__TEXT___cstring_1212: db `w`, 0
L__TEXT___cstring_1214: db `Can not fdopen() output stream`, 0
L__TEXT___cstring_1245: db `calloc`, 0
L__TEXT___cstring_1252: db `File appears to be truncated`, 0
L__TEXT___cstring_1281: db `Bad symbol table`, 0
L__TEXT___cstring_1298: db `Symbol table truncated`, 0
L__TEXT___cstring_1321: db `File corrupt`, 0
L__TEXT___cstring_1334: db `Premature EOF`, 0
L__TEXT___cstring_1348: db `read failed`, 0
L__TEXT___cstring_1360: db `Can't initialize decoder (%d)`, 0
L__TEXT___cstring_1390: db `Can't read headers (%d)`, 0
L__TEXT___cstring_1414: db `write failed`, 0
L__TEXT___cstring_1427: db `File format not recognized`, 0
L__TEXT___cstring_1454: db `Unsupported compression options`, 0
L__TEXT___cstring_1486: db `File is corrupt`, 0
L__TEXT___cstring_1502: db `Unexpected end of input`, 0
L__TEXT___cstring_1526: db `Reached memory limit`, 0
L__TEXT___cstring_1547: db `Unknown error (%d)`, 0
L__TEXT___cstring_1566: db `%s`, 0
L__TEXT___cstring_1569: db `%s:%s  `, 0
L__TEXT___cstring_1577: db `\t\t`, 0
L__TEXT___cstring_1580: db `\t`, 0
L__TEXT___cstring_1582: db ` -- replaced with %s`, 0
L__TEXT___cstring_1603: db `%2.2d.`, 0
L__TEXT___cstring_1610: db `%5s%%`, 0
L__TEXT___cstring_1616: db `%s:%s  %s\n`, 0
L__TEXT___cstring_1627: db `OK`, 0
L__TEXT___cstring_1630: db `NOT OK`, 0
L__TEXT___cstring_1637: db `standard output is a terminal -- ignoring`, 0
L__TEXT___cstring_1679: db `Can't stat stdin`, 0
L__TEXT___cstring_1696: db `time`, 0
L__TEXT___cstring_1701: db `%c%c%c%c%c%c%c%c%c%c%s`, 0
L__TEXT___cstring_1724: db `snprintf`, 0
L__TEXT___cstring_1733: db `deflateInit2 failed`, 0
L__TEXT___cstring_1753: db `deflate failed`, 0
L__TEXT___cstring_1768: db `deflateEnd failed`, 0
L__TEXT___cstring_1786: db `%c%c%c%c%c%c%c%c`, 0
L__TEXT___cstring_1803: db `bin/zcat`, 0
L__TEXT___cstring_1812: db `Unix2003`, 0
L__TEXT___cstring_1821: db `can't stat: %s (%s)`, 0
L__TEXT___cstring_1841: db `%s is a directory`, 0
L__TEXT___cstring_1859: db `%s is not a regular file`, 0
L__TEXT___cstring_1884: db `couldn't fts_open %s`, 0
L__TEXT___cstring_1905: db `can't open %s`, 0
L__TEXT___cstring_1919: db `%s: unknown suffix -- ignored`, 0
L__TEXT___cstring_1949: db `can't read %s`, 0
L__TEXT___cstring_1963: db `%s: not in gzip format`, 0
L__TEXT___cstring_1986: db `%.*s%.*s`, 0
L__TEXT___cstring_1995: db `%s has %d other links -- skipping`, 0
L__TEXT___cstring_2029: db `no -l with bzip2 files`, 0
L__TEXT___cstring_2052: db `no -l with Lempel-Ziv files`, 0
L__TEXT___cstring_2080: db `zdopen for read: %s`, 0
L__TEXT___cstring_2100: db `fdopen for write: %s`, 0
L__TEXT___cstring_2121: db `failed infile fclose`, 0
L__TEXT___cstring_2142: db `failed outfile fclose`, 0
L__TEXT___cstring_2164: db `no -l with packed files`, 0
L__TEXT___cstring_2188: db `no -l with xz files`, 0
L__TEXT___cstring_2208: db `no -l for unknown filetypes`, 0
L__TEXT___cstring_2236: db `couldn't close input`, 0
L__TEXT___cstring_2257: db `couldn't close output`, 0
L__TEXT___cstring_2279: db `%s: uncompress failed`, 0
L__TEXT___cstring_2301: db `couldn't open (leaving original): %s`, 0
L__TEXT___cstring_2338: db `couldn't stat (leaving original): %s`, 0
L__TEXT___cstring_2375: db `stat gave different size: %ju != %ju (leaving original)`, 0
L__TEXT___cstring_2431: db `%s already exists -- do you wish to overwrite (y or n)? `, 0
L__TEXT___cstring_2488: db `\tnot overwriting\n`, 0
L__TEXT___cstring_2506: db `%s already exists -- skipping`, 0
L__TEXT___cstring_2536: db `couldn't fchown: %s`, 0
L__TEXT___cstring_2556: db `couldn't fchmod: %s`, 0
L__TEXT___cstring_2576: db `couldn't futimens: %s`, 0
L__TEXT___cstring_2598: db `couldn't fchflags: %s`, 0
L__TEXT___cstring_2620: db `couldn't stat: %s`, 0
L__TEXT___cstring_2638: db `%s has %d other link%s -- skipping`, 0
L__TEXT___cstring_2673: db `s`, 0
L__TEXT___cstring_2675: db `%s already has %s suffix -- unchanged`, 0
L__TEXT___cstring_2713: db `%s%s`, 0
L__TEXT___cstring_2718: db `could not create output: %s`, 0
L__TEXT___cstring_2746: db `output file: %s wrong size (%ju != %ju), deleting`, 0
L__TEXT___cstring_2796: db `leaving original %s`, 0
L__TEXT___cstring_2816: db `method  crc     date  time  `, 0
L__TEXT___cstring_2845: db `read of uncompressed size`, 0
L__TEXT___cstring_2871: db `                            `, 0
L__TEXT___cstring_2900: db `%5s %08x %11s `, 0
L__TEXT___cstring_2915: db `defla`, 0
L__TEXT___cstring_2921: db `%12llu %12llu `, 0
L__TEXT___cstring_2936: db ` %s\n`, 0
L__TEXT___cstring_2941: db `%s\n`, 0
L__TEXT___cstring_2945: db `usage: %s [-123456789acdfhklLNnqrtVv] [-S .suffix] [<file> [<file> ...]]\n -1 --fast            fastest (worst) compression\n -2 .. -8             set compression level\n -9 --best            best (slowest) compression\n -c --stdout          write to stdout, keep original files\n    --to-stdout\n -d --decompress      uncompress files\n    --uncompress\n -f --force           force overwriting & compress links\n -h --help            display this help\n -k --keep            don't delete input files during operation\n -l --list            list compressed file contents\n -N --name            save or restore original file name and time stamp\n -n --no-name         don't save original file name or time stamp\n -q --quiet           output no warnings\n -r --recursive       recursively compress files in directories\n -S .suf              use suffix .suf instead of .gz\n    --suffix .suf\n -t --test            test compressed file\n -V --version         display program version\n -v --verbose         print extra statistics\n`, 0
L__TEXT___cstring_3954: db `%s (based on FreeBSD gzip 20150113)\n`, 0
L__TEXT___cstring_3991: db `  compressed uncompressed  ratio uncompressed_name`, 0

section .bss
L__DATA___bss: resb 128


section .data
L_DATASECTION:
db 06h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 09dh
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 03h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 06h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 09dh
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 03h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 06h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 0a1h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 02h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 06h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 0a4h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 03h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 06h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 0a8h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 02h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 06h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 0abh
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 02h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 06h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 0aeh
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 04h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 0b3h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 0b8h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 04h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 0b3h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 0bdh
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 04h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 06h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 0c2h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 04h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 0b3h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 0c7h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 05h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 0b3h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 0cdh
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 02h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 06h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 0d0h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 03h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 06h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 09dh
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 03h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 06h
db 070h
db 00h
db 00h
db 01h
db 00h
db 00h
db 00h
db 01fh
db 09dh


section .data
L_JUMP_TABLE_100002625:
dq L10000240c_58
dq L10000240c_68
dq L10000240c_59
dq L10000240c_60
dq L10000240c_68
dq L10000240c_61
dq L10000240c_68
dq L10000240c_68
dq L10000240c_68
dq L10000240c_68
dq L10000240c_62
dq L10000240c_63
dq L10000240c_68
dq L10000240c_64
dq L10000240c_68
dq L10000240c_68
dq L10000240c_65
dq L10000240c_66
dq L10000240c_68
dq L10000240c_67
dq L10000240c_68
L_JUMP_TABLE_1000029cf:
dq L10000240c_138
dq L10000240c_137
dq L10000240c_137
dq L10000240c_138
dq L10000240c_139
dq L10000240c_137
L_JUMP_TABLE_100002d09:
dq L100002c11_29
dq L100002c11_30
dq L100002c11_31
dq L100002c11_32
L_JUMP_TABLE_100003d72:
dq L100003c24_26
dq L100003c24_27
dq L100003c24_28
dq L100003c24_25
dq L100003c24_25
dq L100003c24_29
dq L100003c24_25
dq L100003c24_25
dq L100003c24_25
L_JUMP_TABLE_1000047be:
dq L100004512_38
dq L100004512_39
dq L100004512_41
dq L100004512_40
dq L100004512_29
L_JUMP_TABLE_100005c18:
dq L10000549a_126
dq L10000549a_127
dq L10000549a_128
dq L10000549a_129
dq L10000549a_125
