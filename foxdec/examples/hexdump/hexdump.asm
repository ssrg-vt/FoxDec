extern ___assert_rtn
extern ___bzero
extern ___error
extern ___maskrune
extern ___sprintf_chk
extern ___stack_chk_fail
extern _asprintf
extern _atoi
extern _bcmp
extern _calloc
extern _err
extern _errx
extern _exit
extern _fclose
extern _ferror
extern _fgets
extern _fileno
extern _fopen
extern _fprintf
extern _fread
extern _free
extern _freopen
extern _fseeko
extern _fstat$INODE64
extern _ftell
extern _fwrite
extern _getchar
extern _getopt
extern _index
extern _lseek
extern _malloc
extern _mbrtowc
extern _printf
extern _puts
extern _rindex
extern _setlocale
extern _strcat
extern _strcmp
extern _strcpy
extern _strlen
extern _strncpy
extern _strtoll
extern _strtoul
extern _ungetc
extern _warn
extern _warnx
extern _wcwidth
extern __DefaultRuneLocale
extern ___mb_cur_max
extern ___stack_chk_guard
extern ___stderrp
extern ___stdinp
extern _optarg
extern _optind
extern dyld_stub_binder


section .text

default rel

; ---------------------
; Function: 0x100000b6c
; ---------------------
; Entry 100000b6c; block 0; address 100000b6c
L100000b6c_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 40
    MOV R14, RDI
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -48], RAX
    MOV EAX, dword [RDI + 40]
    TEST EAX, EAX
    JLE L100000b6c_2
    JMP L100000b6c_1     ; inserted

; Entry 100000b6c; block 1; address 100000b95
L100000b6c_1:
    DEC EAX
    MOV dword [R14 + 40], EAX
    LEA RBX, [rel L__TEXT___cstring_0]
    JMP L100000b6c_24     ; inserted

; Entry 100000b6c; block 2; address 100000bda
L100000b6c_2:
    MOV RBX, RDX
    MOV R15, RSI
    MOVZX EDI, byte [RSI]
    CMP EDI, 14
    JAE L100000b6c_7
    JMP L100000b6c_6     ; inserted

; Entry 100000b6c; block 3; address 100000bb7
L100000b6c_3:
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    CMP RAX, qword [RBP + -48]
    JNE L100000b6c_5
    JMP L100000b6c_4     ; inserted

; Entry 100000b6c; block 4; address 100000bcb
L100000b6c_4:
    ADD RSP, 40
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100000b6c; block 5; address 100000db4
L100000b6c_5:
    CALL ___stack_chk_fail

; Entry 100000b6c; block 6; address 100000be8
L100000b6c_6:
    MOV EAX, 16257
    BT EAX, EDI
    JB L100000b6c_8
    JMP L100000b6c_7     ; inserted

; Entry 100000b6c; block 7; address 100000bf6
L100000b6c_7:
    LEA R12, [rel L__DATA___common + 32]
    CMP dword [R12], 0
    JE L100000b6c_10
    JMP L100000b6c_9     ; inserted

; Entry 100000b6c; block 8; address 100000c79
L100000b6c_8:
    MOVSX RAX, DIL
    LEA RCX, [rel L__DATA_CONST___const + 256]
    MOV RBX, qword [RCX + RAX * 8]
    JMP L100000b6c_24

; Entry 100000b6c; block 9; address 100000c04
L100000b6c_9:
    MOV RAX, qword [rel ___mb_cur_max]
    CMP dword [RAX], 2
    JL L100000b6c_10
    JMP L100000b6c_11     ; inserted

; Entry 100000b6c; block 10; address 100000c72
L100000b6c_10:
    MOV dword [RBP + -64], EDI
    XOR EBX, EBX
    JMP L100000b6c_25

; Entry 100000b6c; block 11; address 100000c10
L100000b6c_11:
    LEA R12, [R14 + 48]
    XOR EAX, EAX
    MOV qword [RBP + -72], RAX
    LEA R13, [RBP + -78]
    JMP L100000b6c_23     ; inserted

; Entry 100000b6c; block 12; address 100000c30
L100000b6c_12:
    CMP RAX, 18446744073709551615
    JE L100000b6c_14
    JMP L100000b6c_13     ; inserted

; Entry 100000b6c; block 13; address 100000c3a
L100000b6c_13:
    TEST RAX, RAX
    JE L100000b6c_16
    JMP L100000b6c_15     ; inserted

; Entry 100000b6c; block 14; address 100000d4c
L100000b6c_14:
    XORPS XMM0, XMM0
    MOVUPS oword [R12 + 112], XMM0
    MOVUPS oword [R12 + 96], XMM0
    MOVUPS oword [R12 + 80], XMM0
    MOVUPS oword [R12 + 64], XMM0
    MOVUPS oword [R12 + 48], XMM0
    MOVUPS oword [R12 + 32], XMM0
    MOVUPS oword [R12 + 16], XMM0
    MOVUPS oword [R12], XMM0
    MOVZX EAX, byte [R15]
    MOV dword [RBP + -64], EAX
    JMP L100000b6c_30     ; inserted

; Entry 100000b6c; block 15; address 100000c3f
L100000b6c_15:
    CMP R15, R13
    JNE L100000b6c_18
    JMP L100000b6c_17     ; inserted

; Entry 100000b6c; block 16; address 100000c8d
L100000b6c_16:
    MOV EAX, 1
    JMP L100000b6c_21     ; inserted

; Entry 100000b6c; block 17; address 100000c44
L100000b6c_17:
    CMP RAX, 18446744073709551614
    JE L100000b6c_19
    JMP L100000b6c_18     ; inserted

; Entry 100000b6c; block 18; address 100000c4e
L100000b6c_18:
    CMP RAX, 18446744073709551614
    JNE L100000b6c_21
    JMP L100000b6c_20     ; inserted

; Entry 100000b6c; block 19; address 100000d49
L100000b6c_19:
    MOV R15, R13
    JMP L100000b6c_14     ; inserted

; Entry 100000b6c; block 20; address 100000c54
L100000b6c_20:
    MOV RAX, qword [rel ___mb_cur_max]
    MOVSXD RSI, dword [RAX]
    MOV RDI, R13
    CALL L10000173e_0
    JMP L100000b6c_22     ; inserted

; Entry 100000b6c; block 21; address 100000c92
L100000b6c_21:
    MOV RCX, qword [RBP + -72]
    LEA EBX, [RAX + RCX]
    DEC EBX
    MOV EDI, dword [RBP + -64]
    LEA R12, [rel L__DATA___common + 32]
    JMP L100000b6c_25     ; inserted

; Entry 100000b6c; block 22; address 100000c66
L100000b6c_22:
    MOV qword [RBP + -72], RBX
    MOV RBX, RAX
    MOV R15, R13
    JMP L100000b6c_23

; Entry 100000b6c; block 23; address 100000c1e
L100000b6c_23:
    LEA RDI, [RBP + -64]
    MOV RSI, R15
    MOV RDX, RBX
    MOV RCX, R12
    CALL _mbrtowc
    JMP L100000b6c_12     ; inserted

; Entry 100000b6c; block 24; address 100000ba2
L100000b6c_24:
    MOV RAX, qword [R14 + 16]
    MOV byte [RAX], 115
    MOV RDI, qword [R14 + 24]
    MOV RSI, RBX
    XOR EAX, EAX
    CALL _printf
    JMP L100000b6c_3     ; inserted

; Entry 100000b6c; block 25; address 100000ca5
L100000b6c_25:
    CMP EDI, 127
    JA L100000b6c_27
    JMP L100000b6c_26     ; inserted

; Entry 100000b6c; block 26; address 100000caa
L100000b6c_26:
    MOV ECX, EDI
    MOV RDX, qword [rel __DefaultRuneLocale]
    MOV EAX, 262144
    AND EAX, dword [RDX + RCX * 4 + 60]
    JMP L100000b6c_28

; Entry 100000b6c; block 27; address 100000cbe
L100000b6c_27:
    MOV ESI, 262144
    CALL ___maskrune
    JMP L100000b6c_28     ; inserted

; Entry 100000b6c; block 28; address 100000cc8
L100000b6c_28:
    TEST EAX, EAX
    JE L100000b6c_30
    JMP L100000b6c_29     ; inserted

; Entry 100000b6c; block 29; address 100000cd0
L100000b6c_29:
    CMP dword [R12], 0
    MOV RAX, qword [R14 + 16]
    JE L100000b6c_32
    JMP L100000b6c_31     ; inserted

; Entry 100000b6c; block 30; address 100000d85
L100000b6c_30:
    LEA RBX, [RBP + -58]
    MOVZX R8D, byte [R15]
    LEA RCX, [rel L__TEXT___cstring_91]
    MOV EDX, 10
    MOV RDI, RBX
    XOR ESI, ESI
    XOR EAX, EAX
    CALL ___sprintf_chk
    JMP L100000b6c_41     ; inserted

; Entry 100000b6c; block 31; address 100000cdb
L100000b6c_31:
    MOV byte [RAX], 67
    MOV RDI, qword [R14 + 24]
    LEA RSI, [rel L__TEXT___cstring_27]
    CALL _strcmp
    JMP L100000b6c_33     ; inserted

; Entry 100000b6c; block 32; address 100000d33
L100000b6c_32:
    MOV byte [RAX], 99
    MOV RDI, qword [R14 + 24]
    MOV ESI, dword [RBP + -64]
    XOR EAX, EAX
    CALL _printf
    JMP L100000b6c_40     ; inserted

; Entry 100000b6c; block 33; address 100000cee
L100000b6c_33:
    TEST EAX, EAX
    JNE L100000b6c_35
    JMP L100000b6c_34     ; inserted

; Entry 100000b6c; block 34; address 100000cf6
L100000b6c_34:
    MOV EDI, dword [RBP + -64]
    CALL _wcwidth
    JMP L100000b6c_36     ; inserted

; Entry 100000b6c; block 35; address 100000daa
L100000b6c_35:
    CALL L100003326_0

; Entry 100000b6c; block 36; address 100000cfe
L100000b6c_36:
    TEST EAX, EAX
    JS L100000b6c_38
    JMP L100000b6c_37     ; inserted

; Entry 100000b6c; block 37; address 100000d06
L100000b6c_37:
    MOV ESI, 3
    XOR ECX, ECX
    SUB ESI, EAX
    CMOVS ESI, ECX
    MOV ECX, dword [RBP + -64]
    LEA RDI, [rel L__TEXT___cstring_84]
    LEA RDX, [rel L__TEXT___cstring_90]
    XOR EAX, EAX
    CALL _printf
    JMP L100000b6c_39     ; inserted

; Entry 100000b6c; block 38; address 100000daf
L100000b6c_38:
    CALL L100003303_0

; Entry 100000b6c; block 39; address 100000d2a
L100000b6c_39:
    MOV dword [R14 + 40], EBX
    JMP L100000b6c_3

; Entry 100000b6c; block 40; address 100000d44
L100000b6c_40:
    JMP L100000b6c_3

; Entry 100000b6c; block 41; address 100000da5
L100000b6c_41:
    JMP L100000b6c_24



; ---------------------
; Function: 0x100000db9
; ---------------------
; Entry 100000db9; block 0; address 100000db9
L100000db9_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R14
    PUSH RBX
    MOV R14, RSI
    MOV RBX, RDI
    MOVZX EDI, byte [RSI]
    CMP RDI, 31
    JA L100000db9_2
    JMP L100000db9_1     ; inserted

; Entry 100000db9; block 1; address 100000dcf
L100000db9_1:
    MOV RAX, qword [RBX + 16]
    MOV byte [RAX], 115
    LEA RAX, [rel L__DATA___common + 32]
    CMP dword [RAX], 0
    MOV AL, byte [R14]
    MOV RDI, qword [RBX + 24]
    JE L100000db9_4
    JMP L100000db9_3     ; inserted

; Entry 100000db9; block 2; address 100000df6
L100000db9_2:
    CMP DIL, 127
    JNE L100000db9_7
    JMP L100000db9_6     ; inserted

; Entry 100000db9; block 3; address 100000de9
L100000db9_3:
    CMP AL, 10
    JNE L100000db9_4
    JMP L100000db9_5     ; inserted

; Entry 100000db9; block 4; address 100000e10
L100000db9_4:
    MOVZX EAX, AL
    LEA RCX, [rel L__DATA_CONST___const + 0]
    MOV RSI, qword [RCX + RAX * 8]
    JMP L100000db9_8

; Entry 100000db9; block 5; address 100000ded
L100000db9_5:
    LEA RSI, [rel L__TEXT___cstring_211]
    JMP L100000db9_8

; Entry 100000db9; block 6; address 100000dfc
L100000db9_6:
    MOV RAX, qword [RBX + 16]
    MOV byte [RAX], 115
    MOV RDI, qword [RBX + 24]
    LEA RSI, [rel L__TEXT___cstring_214]
    JMP L100000db9_8

; Entry 100000db9; block 7; address 100000e20
L100000db9_7:
    CMP DIL, 32
    JNE L100000db9_10
    JMP L100000db9_9     ; inserted

; Entry 100000db9; block 8; address 100000e44
L100000db9_8:
    XOR EAX, EAX
    POP RBX
    POP R14
    POP RBP
    JMP _printf

; Entry 100000db9; block 9; address 100000e26
L100000db9_9:
    LEA RAX, [rel L__DATA___common + 32]
    CMP dword [RAX], 0
    JE L100000db9_10
    JMP L100000db9_11     ; inserted

; Entry 100000db9; block 10; address 100000e4f
L100000db9_10:
    TEST DIL, DIL
    JS L100000db9_13
    JMP L100000db9_12     ; inserted

; Entry 100000db9; block 11; address 100000e32
L100000db9_11:
    MOV RAX, qword [RBX + 16]
    MOV byte [RAX], 115
    MOV RDI, qword [RBX + 24]
    LEA RSI, [rel L__TEXT___cstring_218]
    JMP L100000db9_8     ; inserted

; Entry 100000db9; block 12; address 100000e54
L100000db9_12:
    MOV RCX, qword [rel __DefaultRuneLocale]
    MOV EAX, 262144
    AND EAX, dword [RCX + RDI * 4 + 60]
    JMP L100000db9_14

; Entry 100000db9; block 13; address 100000e66
L100000db9_13:
    MOV ESI, 262144
    CALL ___maskrune
    JMP L100000db9_14     ; inserted

; Entry 100000db9; block 14; address 100000e70
L100000db9_14:
    MOV RCX, qword [RBX + 16]
    TEST EAX, EAX
    JE L100000db9_16
    JMP L100000db9_15     ; inserted

; Entry 100000db9; block 15; address 100000e78
L100000db9_15:
    MOV byte [RCX], 99
    JMP L100000db9_17

; Entry 100000db9; block 16; address 100000e7d
L100000db9_16:
    MOV byte [RCX], 120
    JMP L100000db9_17     ; inserted

; Entry 100000db9; block 17; address 100000e80
L100000db9_17:
    MOV RDI, qword [RBX + 24]
    MOVZX ESI, byte [R14]
    XOR EAX, EAX
    POP RBX
    POP R14
    POP RBP
    JMP _printf



; ---------------------
; Function: 0x100000e94
; ---------------------
; Entry 100000e94; block 0; address 100000e94
L100000e94_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 56
    CALL L1000012e8_0
    JMP L100000e94_1     ; inserted

; Entry 100000e94; block 1; address 100000eaa
L100000e94_1:
    MOV qword [RBP + -56], RAX
    TEST RAX, RAX
    JE L100000e94_3
    JMP L100000e94_2     ; inserted

; Entry 100000e94; block 2; address 100000eb7
L100000e94_2:
    LEA R14, [rel L__TEXT___text + 1820]
    XOR R15D, R15D
    JMP L100000e94_77     ; inserted

; Entry 100000e94; block 3; address 100001209
L100000e94_3:
    LEA RAX, [rel L__DATA___common + 40]
    MOV RAX, qword [RAX]
    TEST RAX, RAX
    JE L100000e94_79
    JMP L100000e94_78     ; inserted

; Entry 100000e94; block 4; address 100000ed4
L100000e94_4:
    MOV RCX, qword [rel L__DATA___bss + 0]
    MOV qword [RBP + -64], RCX
    JMP L100000e94_75     ; inserted

; Entry 100000e94; block 5; address 1000011f7
L100000e94_5:
    CALL L1000012e8_0
    JMP L100000e94_76     ; inserted

; Entry 100000e94; block 6; address 100000ef0
L100000e94_6:
    MOV RAX, qword [RBP + -64]
    MOV R13, qword [RBP + -56]
    JMP L100000e94_74     ; inserted

; Entry 100000e94; block 7; address 1000011dc
L100000e94_7:
    MOV RAX, qword [RBP + -72]
    MOV RAX, qword [RAX]
    MOV RCX, qword [RBP + -64]
    MOV qword [rel L__DATA___bss + 0], RCX
    TEST RAX, RAX
    JNE L100000e94_75
    JMP L100000e94_5     ; inserted

; Entry 100000e94; block 8; address 100000f02
L100000e94_8:
    MOV R12D, dword [RCX + 20]
    TEST R12D, R12D
    JE L100000e94_10
    JMP L100000e94_9     ; inserted

; Entry 100000e94; block 9; address 100000f0f
L100000e94_9:
    MOV qword [RBP + -80], RCX
    JMP L100000e94_73     ; inserted

; Entry 100000e94; block 10; address 1000011d0
L100000e94_10:
    MOV RCX, qword [RCX]
    TEST RCX, RCX
    JNE L100000e94_74
    JMP L100000e94_7     ; inserted

; Entry 100000e94; block 11; address 100000f20
L100000e94_11:
    MOV RCX, qword [rel L__DATA___bss + 8]
    TEST RCX, RCX
    JE L100000e94_14
    JMP L100000e94_13     ; inserted

; Entry 100000e94; block 12; address 1000011c3
L100000e94_12:
    DEC R12D
    MOV RCX, qword [RBP + -80]
    JNE L100000e94_73
    JMP L100000e94_10     ; inserted

; Entry 100000e94; block 13; address 100000f2c
L100000e94_13:
    CMP RAX, RCX
    JL L100000e94_14
    JMP L100000e94_15     ; inserted

; Entry 100000e94; block 14; address 100000f44
L100000e94_14:
    CMP R12D, 1
    JNE L100000e94_18
    JMP L100000e94_17     ; inserted

; Entry 100000e94; block 15; address 100000f31
L100000e94_15:
    MOVZX EAX, word [RBX + 8]
    TEST EAX, 1026
    JNE L100000e94_14
    JMP L100000e94_16     ; inserted

; Entry 100000e94; block 16; address 100000f3c
L100000e94_16:
    MOV RDI, RBX
    CALL L1000015b3_0
    JMP L100000e94_14     ; inserted

; Entry 100000e94; block 17; address 100000f4a
L100000e94_17:
    MOV RAX, qword [RBX + 32]
    TEST RAX, RAX
    JE L100000e94_18
    JMP L100000e94_19     ; inserted

; Entry 100000e94; block 18; address 100000f59
L100000e94_18:
    MOV EAX, dword [RBX + 8]
    CMP EAX, 63
    JG L100000e94_21
    JMP L100000e94_20     ; inserted

; Entry 100000e94; block 19; address 100000f53
L100000e94_19:
    MOV R15B, byte [RAX]
    MOV byte [RAX], 0
    JMP L100000e94_18     ; inserted

; Entry 100000e94; block 20; address 100000f61
L100000e94_20:
    LEA ECX, [RAX + -1]
    CMP ECX, 7
    JA L100000e94_23
    JMP L100000e94_22     ; inserted

; Entry 100000e94; block 21; address 100000f82
L100000e94_21:
    CMP EAX, 255
    JLE L100000e94_29
    JMP L100000e94_28     ; inserted

; Entry 100000e94; block 22; address 100000f69
L100000e94_22:
    MOVSXD RAX, dword [R14 + RCX * 4]
    ADD RAX, R14
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100000f70 + 8*ECX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 100000f72,100001018,100001183,100001028

; Entry 100000e94; block 23; address 100000fba
L100000e94_23:
    CMP EAX, 16
    JE L100000e94_37
    JMP L100000e94_36     ; inserted

; Entry 100000e94; block 24; address 100000f72
L100000e94_24:
    MOV RDI, qword [RBX + 24]
    MOV RSI, qword [rel L__DATA___bss + 0]
    JMP L100000e94_35

; Entry 100000e94; block 25; address 100001018
L100000e94_25:
    MOV RDI, qword [RBX + 24]
    LEA RSI, [rel L__TEXT___cstring_90]
    JMP L100000e94_35

; Entry 100000e94; block 26; address 100001028
L100000e94_26:
    MOV RCX, qword [rel L__DATA___bss + 8]
    TEST RCX, RCX
    JE L100000e94_47
    JMP L100000e94_46     ; inserted

; Entry 100000e94; block 27; address 100001183
L100000e94_27:
    CMP R12D, 1
    JNE L100000e94_70
    JMP L100000e94_69     ; inserted

; Entry 100000e94; block 28; address 100000f89
L100000e94_28:
    CMP EAX, 256
    JE L100000e94_31
    JMP L100000e94_30     ; inserted

; Entry 100000e94; block 29; address 100000ff8
L100000e94_29:
    CMP EAX, 64
    JE L100000e94_44
    JMP L100000e94_43     ; inserted

; Entry 100000e94; block 30; address 100000f94
L100000e94_30:
    CMP EAX, 512
    JE L100000e94_33
    JMP L100000e94_32     ; inserted

; Entry 100000e94; block 31; address 100001059
L100000e94_31:
    MOV RDI, RBX
    MOV RSI, R13
    CALL L100000db9_0
    JMP L100000e94_48     ; inserted

; Entry 100000e94; block 32; address 100000f9f
L100000e94_32:
    CMP EAX, 1024
    JNE L100000e94_27
    JMP L100000e94_34     ; inserted

; Entry 100000e94; block 33; address 100001069
L100000e94_33:
    MOV EAX, dword [RBX + 12]
    DEC EAX
    CMP EAX, 7
    JA L100000e94_27
    JMP L100000e94_49     ; inserted

; Entry 100000e94; block 34; address 100000faa
L100000e94_34:
    MOV RSI, qword [RBX + 24]
    LEA RDI, [rel L__TEXT___cstring_222]
    JMP L100000e94_35

; Entry 100000e94; block 35; address 10000117c
L100000e94_35:
    XOR EAX, EAX
    CALL _printf
    JMP L100000e94_27     ; inserted

; Entry 100000e94; block 36; address 100000fc3
L100000e94_36:
    CMP EAX, 32
    JNE L100000e94_27
    JMP L100000e94_38     ; inserted

; Entry 100000e94; block 37; address 1000010bf
L100000e94_37:
    MOV EAX, dword [RBX + 12]
    CMP EAX, 16
    JE L100000e94_56
    JMP L100000e94_55     ; inserted

; Entry 100000e94; block 38; address 100000fcc
L100000e94_38:
    MOV EAX, dword [RBX + 12]
    DEC EAX
    CMP EAX, 7
    JA L100000e94_27
    JMP L100000e94_39     ; inserted

; Entry 100000e94; block 39; address 100000fda
L100000e94_39:
    LEA RCX, [rel L__TEXT___text + 1884]
    MOVSXD RAX, dword [RCX + RAX * 4]
    ADD RAX, RCX
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100000fe8 + 8*EAX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 100000fea,10000116d,100001183,100001174

; Entry 100000e94; block 40; address 100000fea
L100000e94_40:
    MOV RDI, qword [RBX + 24]
    MOVSX RSI, byte [R13]
    JMP L100000e94_35

; Entry 100000e94; block 41; address 10000116d
L100000e94_41:
    MOVSX RSI, word [R13]
    JMP L100000e94_68

; Entry 100000e94; block 42; address 100001174
L100000e94_42:
    MOVSXD RSI, dword [R13]
    JMP L100000e94_68     ; inserted

; Entry 100000e94; block 43; address 100001001
L100000e94_43:
    CMP EAX, 128
    JNE L100000e94_27
    JMP L100000e94_45     ; inserted

; Entry 100000e94; block 44; address 100001095
L100000e94_44:
    MOV dword [RBP + -44], R15D
    MOV R15, R14
    MOV R14, qword [RBX + 24]
    MOVZX EAX, byte [R13]
    TEST AL, AL
    JS L100000e94_54
    JMP L100000e94_53     ; inserted

; Entry 100000e94; block 45; address 10000100c
L100000e94_45:
    MOV RDI, qword [RBX + 24]
    MOV RSI, R13
    JMP L100000e94_35

; Entry 100000e94; block 46; address 100001038
L100000e94_46:
    SUB RCX, qword [rel L__DATA___bss + 0]
    JMP L100000e94_60

; Entry 100000e94; block 47; address 1000010e4
L100000e94_47:
    LEA RAX, [rel L__DATA___common + 0]
    MOVSXD RCX, dword [RAX]
    MOV RAX, qword [rel L__DATA___bss + 0]
    CQO 
    IDIV RCX
    SUB RCX, RDX
    JMP L100000e94_60     ; inserted

; Entry 100000e94; block 48; address 100001064
L100000e94_48:
    JMP L100000e94_27

; Entry 100000e94; block 49; address 100001077
L100000e94_49:
    LEA RCX, [rel L__TEXT___text + 1852]
    MOVSXD RAX, dword [RCX + RAX * 4]
    ADD RAX, RCX
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100001085 + 8*EAX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 100001087,10000115a,100001183,100001161

; Entry 100000e94; block 50; address 100001087
L100000e94_50:
    MOV RDI, qword [RBX + 24]
    MOVZX ESI, byte [R13]
    JMP L100000e94_35

; Entry 100000e94; block 51; address 10000115a
L100000e94_51:
    MOVZX ESI, word [R13]
    JMP L100000e94_68

; Entry 100000e94; block 52; address 100001161
L100000e94_52:
    MOV ESI, dword [R13]
    JMP L100000e94_68

; Entry 100000e94; block 53; address 1000010a9
L100000e94_53:
    MOV RCX, qword [rel __DefaultRuneLocale]
    TEST byte [RCX + RAX * 4 + 62], 4
    MOV ECX, 46
    CMOVE EAX, ECX
    JMP L100000e94_63

; Entry 100000e94; block 54; address 10000110d
L100000e94_54:
    MOV ESI, 262144
    MOV EDI, EAX
    CALL ___maskrune
    JMP L100000e94_62     ; inserted

; Entry 100000e94; block 55; address 1000010c7
L100000e94_55:
    CMP EAX, 8
    JE L100000e94_58
    JMP L100000e94_57     ; inserted

; Entry 100000e94; block 56; address 100001133
L100000e94_56:
    FLD tword [R13]
    MOV RDI, qword [RBX + 24]
    FSTP tword [RSP]
    XOR EAX, EAX
    CALL _printf
    JMP L100000e94_65     ; inserted

; Entry 100000e94; block 57; address 1000010cc
L100000e94_57:
    CMP EAX, 4
    JNE L100000e94_27
    JMP L100000e94_59     ; inserted

; Entry 100000e94; block 58; address 100001147
L100000e94_58:
    MOVSD XMM0, qword [R13]
    MOV RDI, qword [RBX + 24]
    JMP L100000e94_66     ; inserted

; Entry 100000e94; block 59; address 1000010d5
L100000e94_59:
    MOV RDI, qword [RBX + 24]
    XORPS XMM0, XMM0
    CVTSS2SD XMM0, dword [R13]
    JMP L100000e94_66

; Entry 100000e94; block 60; address 1000010fd
L100000e94_60:
    MOV RDI, RBX
    MOV RSI, R13
    MOV RDX, RCX
    CALL L100000b6c_0
    JMP L100000e94_61     ; inserted

; Entry 100000e94; block 61; address 10000110b
L100000e94_61:
    JMP L100000e94_27

; Entry 100000e94; block 62; address 100001119
L100000e94_62:
    MOV EAX, 46
    JMP L100000e94_63     ; inserted

; Entry 100000e94; block 63; address 10000111e
L100000e94_63:
    MOV RDI, R14
    MOV ESI, EAX
    XOR EAX, EAX
    CALL _printf
    JMP L100000e94_64     ; inserted

; Entry 100000e94; block 64; address 10000112a
L100000e94_64:
    MOV R14, R15
    MOV R15D, dword [RBP + -44]
    JMP L100000e94_27

; Entry 100000e94; block 65; address 100001145
L100000e94_65:
    JMP L100000e94_27

; Entry 100000e94; block 66; address 100001151
L100000e94_66:
    MOV AL, 1
    CALL _printf
    JMP L100000e94_67     ; inserted

; Entry 100000e94; block 67; address 100001158
L100000e94_67:
    JMP L100000e94_27

; Entry 100000e94; block 68; address 100001178
L100000e94_68:
    MOV RDI, qword [RBX + 24]
    JMP L100000e94_35     ; inserted

; Entry 100000e94; block 69; address 100001189
L100000e94_69:
    MOV RAX, qword [RBX + 32]
    TEST RAX, RAX
    JE L100000e94_70
    JMP L100000e94_71     ; inserted

; Entry 100000e94; block 70; address 100001195
L100000e94_70:
    MOVSXD RCX, dword [RBX + 12]
    MOV RAX, qword [rel L__DATA___bss + 0]
    ADD RAX, RCX
    MOV qword [rel L__DATA___bss + 0], RAX
    ADD R13, RCX
    MOV RBX, qword [RBX]
    JMP L100000e94_72

; Entry 100000e94; block 71; address 100001192
L100000e94_71:
    MOV byte [RAX], R15B
    JMP L100000e94_70     ; inserted

; Entry 100000e94; block 72; address 100000f17
L100000e94_72:
    TEST RBX, RBX
    JE L100000e94_12
    JMP L100000e94_11     ; inserted

; Entry 100000e94; block 73; address 100000f13
L100000e94_73:
    MOV RBX, qword [RCX + 8]
    JMP L100000e94_72     ; inserted

; Entry 100000e94; block 74; address 100000ef8
L100000e94_74:
    TEST byte [RCX + 16], 1
    JNE L100000e94_7
    JMP L100000e94_8     ; inserted

; Entry 100000e94; block 75; address 100000edf
L100000e94_75:
    MOV RCX, qword [RAX + 8]
    TEST RCX, RCX
    MOV qword [RBP + -72], RAX
    JE L100000e94_7
    JMP L100000e94_6     ; inserted

; Entry 100000e94; block 76; address 1000011fc
L100000e94_76:
    MOV qword [RBP + -56], RAX
    TEST RAX, RAX
    JNE L100000e94_77
    JMP L100000e94_3     ; inserted

; Entry 100000e94; block 77; address 100000ec1
L100000e94_77:
    LEA RAX, [rel L__DATA___common + 8]
    MOV RAX, qword [RAX]
    TEST RAX, RAX
    JE L100000e94_5
    JMP L100000e94_4     ; inserted

; Entry 100000e94; block 78; address 100001218
L100000e94_78:
    CMP qword [rel L__DATA___bss + 8], 0
    JNE L100000e94_81
    JMP L100000e94_80     ; inserted

; Entry 100000e94; block 79; address 100001277
L100000e94_79:
    ADD RSP, 56
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100000e94; block 80; address 100001222
L100000e94_80:
    MOV RCX, qword [rel L__DATA___bss + 0]
    TEST RCX, RCX
    JE L100000e94_79
    JMP L100000e94_82     ; inserted

; Entry 100000e94; block 81; address 100001235
L100000e94_81:
    MOV RBX, qword [RAX + 8]
    TEST RBX, RBX
    JE L100000e94_79
    JMP L100000e94_83     ; inserted

; Entry 100000e94; block 82; address 10000122e
L100000e94_82:
    MOV qword [rel L__DATA___bss + 8], RCX
    JMP L100000e94_81     ; inserted

; Entry 100000e94; block 83; address 10000123e
L100000e94_83:
    LEA R14, [rel L__TEXT___cstring_222]
    JMP L100000e94_89     ; inserted

; Entry 100000e94; block 84; address 10000124f
L100000e94_84:
    CMP EAX, 1
    JNE L100000e94_87
    JMP L100000e94_86     ; inserted

; Entry 100000e94; block 85; address 100001261
L100000e94_85:
    MOV RSI, qword [RBX + 24]
    MOV RDI, R14
    JMP L100000e94_88     ; inserted

; Entry 100000e94; block 86; address 100001254
L100000e94_86:
    MOV RDI, qword [RBX + 24]
    MOV RSI, qword [rel L__DATA___bss + 8]
    JMP L100000e94_88

; Entry 100000e94; block 87; address 10000126f
L100000e94_87:
    MOV RBX, qword [RBX]
    TEST RBX, RBX
    JNE L100000e94_89
    JMP L100000e94_79     ; inserted

; Entry 100000e94; block 88; address 100001268
L100000e94_88:
    XOR EAX, EAX
    CALL _printf
    JMP L100000e94_87     ; inserted

; Entry 100000e94; block 89; address 100001245
L100000e94_89:
    MOV EAX, dword [RBX + 8]
    CMP EAX, 1024
    JE L100000e94_85
    JMP L100000e94_84     ; inserted



; ---------------------
; Function: 0x1000012e8
; ---------------------
; Entry 1000012e8; block 0; address 1000012e8
L1000012e8_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 24
    MOV R15, qword [rel L__DATA___bss + 24]
    TEST R15, R15
    JE L1000012e8_2
    JMP L1000012e8_1     ; inserted

; Entry 1000012e8; block 1; address 100001305
L1000012e8_1:
    MOV RAX, qword [rel L__DATA___bss + 32]
    MOV qword [rel L__DATA___bss + 24], RAX
    MOV qword [rel L__DATA___bss + 32], R15
    LEA RAX, [rel L__DATA___common + 0]
    MOVSXD RBX, dword [RAX]
    ADD qword [rel L__DATA___bss + 0], RBX
    JMP L1000012e8_7

; Entry 1000012e8; block 2; address 10000132d
L1000012e8_2:
    LEA RAX, [rel L__DATA___common + 0]
    MOV EBX, dword [RAX]
    MOVSXD R14, EBX
    MOV EDI, 1
    MOV RSI, R14
    CALL _calloc
    JMP L1000012e8_3     ; inserted

; Entry 1000012e8; block 3; address 100001346
L1000012e8_3:
    MOV qword [rel L__DATA___bss + 24], RAX
    TEST RAX, RAX
    JE L1000012e8_5
    JMP L1000012e8_4     ; inserted

; Entry 1000012e8; block 4; address 100001356
L1000012e8_4:
    MOV EDI, 1
    MOV RSI, R14
    CALL _calloc
    JMP L1000012e8_6     ; inserted

; Entry 1000012e8; block 5; address 1000015a9
L1000012e8_5:
    CALL L10000334c_0

; Entry 1000012e8; block 6; address 100001363
L1000012e8_6:
    MOV qword [rel L__DATA___bss + 32], RAX
    TEST RAX, RAX
    JE L1000012e8_8
    JMP L1000012e8_7     ; inserted

; Entry 1000012e8; block 7; address 100001373
L1000012e8_7:
    TEST R15, R15
    SETNE byte [RBP + -41]
    LEA R14, [rel L__DATA___data + 12]
    MOV ECX, dword [R14]
    TEST ECX, ECX
    JE L1000012e8_10
    JMP L1000012e8_9     ; inserted

; Entry 1000012e8; block 8; address 1000015ae
L1000012e8_8:
    CALL L10000335c_0

; Entry 1000012e8; block 9; address 10000138c
L1000012e8_9:
    MOV qword [RBP + -56], R15
    XOR R15D, R15D
    MOV R13, qword [rel ___stdinp]
    JMP L1000012e8_38     ; inserted

; Entry 1000012e8; block 10; address 1000014b5
L1000012e8_10:
    XOR R15D, R15D
    JMP L1000012e8_15     ; inserted

; Entry 1000012e8; block 11; address 1000013a7
L1000012e8_11:
    XOR EDI, EDI
    CALL L100001629_0
    JMP L1000012e8_13     ; inserted

; Entry 1000012e8; block 12; address 1000013b9
L1000012e8_12:
    MOV RDI, qword [rel L__DATA___bss + 24]
    ADD RDI, R12
    CMP ECX, EBX
    MOV EAX, EBX
    CMOVL EAX, ECX
    CMP ECX, 18446744073709551615
    CMOVE EAX, EBX
    MOVSXD RDX, EAX
    MOV RCX, qword [R13]
    MOV ESI, 1
    CALL _fread
    JMP L1000012e8_16     ; inserted

; Entry 1000012e8; block 13; address 1000013ae
L1000012e8_13:
    TEST EAX, EAX
    JE L1000012e8_15
    JMP L1000012e8_14     ; inserted

; Entry 1000012e8; block 14; address 1000013b6
L1000012e8_14:
    MOV ECX, dword [R14]
    JMP L1000012e8_12     ; inserted

; Entry 1000012e8; block 15; address 1000014b8
L1000012e8_15:
    LEA RAX, [rel L__DATA___common + 32]
    CMP dword [RAX], 0
    JE L1000012e8_41
    JMP L1000012e8_40     ; inserted

; Entry 1000012e8; block 16; address 1000013e1
L1000012e8_16:
    TEST EAX, EAX
    JNE L1000012e8_18
    JMP L1000012e8_17     ; inserted

; Entry 1000012e8; block 17; address 1000013e5
L1000012e8_17:
    MOV RDI, qword [R13]
    CALL _ferror
    JMP L1000012e8_19     ; inserted

; Entry 1000012e8; block 18; address 10000140e
L1000012e8_18:
    MOV byte [rel L__DATA___bss + 16], 1
    MOV ECX, dword [R14]
    CMP ECX, 18446744073709551615
    JE L1000012e8_26
    JMP L1000012e8_25     ; inserted

; Entry 1000012e8; block 19; address 1000013ee
L1000012e8_19:
    TEST EAX, EAX
    JNE L1000012e8_21
    JMP L1000012e8_20     ; inserted

; Entry 1000012e8; block 20; address 1000013f2
L1000012e8_20:
    MOV byte [rel L__DATA___bss + 16], 0
    MOV ECX, dword [R14]
    XOR EAX, EAX
    TEST ECX, ECX
    JNE L1000012e8_22
    JMP L1000012e8_23     ; inserted

; Entry 1000012e8; block 21; address 100001407
L1000012e8_21:
    CALL L100003381_0
    JMP L1000012e8_24     ; inserted

; Entry 1000012e8; block 22; address 1000013a3
L1000012e8_22:
    TEST AL, 1
    JNE L1000012e8_12
    JMP L1000012e8_11     ; inserted

; Entry 1000012e8; block 23; address 100001402
L1000012e8_23:
    JMP L1000012e8_15

; Entry 1000012e8; block 24; address 10000140c
L1000012e8_24:
    JMP L1000012e8_20

; Entry 1000012e8; block 25; address 10000141d
L1000012e8_25:
    SUB ECX, EAX
    MOV dword [R14], ECX
    JMP L1000012e8_27

; Entry 1000012e8; block 26; address 100001424
L1000012e8_26:
    MOV ECX, 4294967295
    JMP L1000012e8_27     ; inserted

; Entry 1000012e8; block 27; address 100001429
L1000012e8_27:
    SUB EBX, EAX
    JE L1000012e8_29
    JMP L1000012e8_28     ; inserted

; Entry 1000012e8; block 28; address 10000142d
L1000012e8_28:
    ADD R15D, EAX
    JMP L1000012e8_37

; Entry 1000012e8; block 29; address 100001432
L1000012e8_29:
    MOV ECX, dword [rel L__DATA___data + 8]
    CMP qword [RBP + -56], 0
    JE L1000012e8_31
    JMP L1000012e8_30     ; inserted

; Entry 1000012e8; block 30; address 100001443
L1000012e8_30:
    MOV EAX, ECX
    AND EAX, 4294967293
    JE L1000012e8_31
    JMP L1000012e8_32     ; inserted

; Entry 1000012e8; block 31; address 100001591
L1000012e8_31:
    DEC ECX
    CMP ECX, 1
    JA L1000012e8_54
    JMP L1000012e8_57     ; inserted

; Entry 1000012e8; block 32; address 10000144e
L1000012e8_32:
    MOV RDI, qword [rel L__DATA___bss + 24]
    MOV RSI, qword [rel L__DATA___bss + 32]
    LEA RAX, [rel L__DATA___common + 0]
    MOVSXD RDX, dword [RAX]
    CALL _bcmp
    JMP L1000012e8_33     ; inserted

; Entry 1000012e8; block 33; address 10000146b
L1000012e8_33:
    MOV ECX, dword [rel L__DATA___data + 8]
    TEST EAX, EAX
    JNE L1000012e8_31
    JMP L1000012e8_34     ; inserted

; Entry 1000012e8; block 34; address 100001479
L1000012e8_34:
    CMP ECX, 3
    JNE L1000012e8_36
    JMP L1000012e8_35     ; inserted

; Entry 1000012e8; block 35; address 10000147e
L1000012e8_35:
    LEA RDI, [rel L__TEXT___cstring_269]
    CALL _puts
    JMP L1000012e8_36     ; inserted

; Entry 1000012e8; block 36; address 10000148a
L1000012e8_36:
    LEA RAX, [rel L__DATA___common + 0]
    MOVSXD RBX, dword [RAX]
    ADD qword [rel L__DATA___bss + 0], RBX
    MOV dword [rel L__DATA___data + 8], 1
    MOV ECX, dword [R14]
    XOR R15D, R15D
    JMP L1000012e8_37     ; inserted

; Entry 1000012e8; block 37; address 1000014ab
L1000012e8_37:
    TEST ECX, ECX
    JNE L1000012e8_38
    JMP L1000012e8_39     ; inserted

; Entry 1000012e8; block 38; address 10000139a
L1000012e8_38:
    MOVSXD R12, R15D
    MOV AL, byte [rel L__DATA___bss + 16]
    JMP L1000012e8_22     ; inserted

; Entry 1000012e8; block 39; address 1000014b3
L1000012e8_39:
    JMP L1000012e8_15

; Entry 1000012e8; block 40; address 1000014c4
L1000012e8_40:
    MOV RAX, qword [rel L__DATA___bss + 0]
    LEA RCX, [rel L__DATA___common + 24]
    CMP RAX, qword [RCX]
    JL L1000012e8_42
    JMP L1000012e8_41     ; inserted

; Entry 1000012e8; block 41; address 1000014db
L1000012e8_41:
    LEA RAX, [rel L__DATA___common + 0]
    CMP EBX, dword [RAX]
    JNE L1000012e8_44
    JMP L1000012e8_43     ; inserted

; Entry 1000012e8; block 42; address 1000015a4
L1000012e8_42:
    CALL L10000336c_0

; Entry 1000012e8; block 43; address 1000014e6
L1000012e8_43:
    XOR EAX, EAX
    JMP L1000012e8_56

; Entry 1000012e8; block 44; address 1000014ed
L1000012e8_44:
    TEST EBX, EBX
    SETE AL
    CMP dword [rel L__DATA___data + 8], 0
    SETNE CL
    AND CL, AL
    TEST byte [RBP + -41], CL
    JE L1000012e8_46
    JMP L1000012e8_45     ; inserted

; Entry 1000012e8; block 45; address 100001503
L1000012e8_45:
    MOV RDI, qword [rel L__DATA___bss + 24]
    MOV RSI, qword [rel L__DATA___bss + 32]
    MOVSXD R15, R15D
    MOV RDX, R15
    CALL _bcmp
    JMP L1000012e8_47     ; inserted

; Entry 1000012e8; block 46; address 100001537
L1000012e8_46:
    MOVSXD R15, R15D
    JMP L1000012e8_49     ; inserted

; Entry 1000012e8; block 47; address 10000151c
L1000012e8_47:
    TEST EAX, EAX
    JNE L1000012e8_49
    JMP L1000012e8_48     ; inserted

; Entry 1000012e8; block 48; address 100001520
L1000012e8_48:
    CMP dword [rel L__DATA___data + 8], 1
    JE L1000012e8_43
    JMP L1000012e8_50     ; inserted

; Entry 1000012e8; block 49; address 10000153a
L1000012e8_49:
    MOV RDI, qword [rel L__DATA___bss + 24]
    ADD RDI, R15
    MOVSXD RSI, EBX
    CALL ___bzero
    JMP L1000012e8_52     ; inserted

; Entry 1000012e8; block 50; address 100001529
L1000012e8_50:
    LEA RDI, [rel L__TEXT___cstring_269]
    CALL _puts
    JMP L1000012e8_51     ; inserted

; Entry 1000012e8; block 51; address 100001535
L1000012e8_51:
    JMP L1000012e8_43

; Entry 1000012e8; block 52; address 10000154c
L1000012e8_52:
    ADD R15, qword [rel L__DATA___bss + 0]
    MOV qword [rel L__DATA___bss + 8], R15
    CMP dword [R14], 0
    JNE L1000012e8_54
    JMP L1000012e8_53     ; inserted

; Entry 1000012e8; block 53; address 100001560
L1000012e8_53:
    MOV RAX, qword [rel ___stdinp]
    MOV RDI, qword [RAX]
    CALL _ftell
    JMP L1000012e8_55     ; inserted

; Entry 1000012e8; block 54; address 10000157b
L1000012e8_54:
    MOV RAX, qword [rel L__DATA___bss + 24]
    JMP L1000012e8_56     ; inserted

; Entry 1000012e8; block 55; address 10000156f
L1000012e8_55:
    XOR EDI, EDI
    MOV RSI, RAX
    XOR EDX, EDX
    CALL _lseek
    JMP L1000012e8_54     ; inserted

; Entry 1000012e8; block 56; address 100001582
L1000012e8_56:
    ADD RSP, 24
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 1000012e8; block 57; address 100001598
L1000012e8_57:
    MOV dword [rel L__DATA___data + 8], 3
    JMP L1000012e8_54



; ---------------------
; Function: 0x1000015b3
; ---------------------
; Entry 1000015b3; block 0; address 1000015b3
L1000015b3_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH RBX
    PUSH RAX
    MOV dword [RDI + 8], 2
    MOV RAX, qword [RDI + 16]
    MOV byte [RAX], 115
    MOV RAX, qword [RDI + 16]
    MOV byte [RAX + 1], 0
    MOV RBX, qword [RDI + 24]
    JMP L1000015b3_1     ; inserted

; Entry 1000015b3; block 1; address 1000015d7
L1000015b3_1:
    CMP byte [RBX], 37
    LEA RBX, [RBX + 1]
    JNE L1000015b3_1
    JMP L1000015b3_2     ; inserted

; Entry 1000015b3; block 2; address 1000015e0
L1000015b3_2:
    MOV AL, byte [RBX]
    MOV R15, RBX
    TEST AL, AL
    JE L1000015b3_4
    JMP L1000015b3_3     ; inserted

; Entry 1000015b3; block 3; address 1000015e9
L1000015b3_3:
    LEA R14, [rel L__TEXT___cstring_225]
    MOV R15, RBX
    JMP L1000015b3_7     ; inserted

; Entry 1000015b3; block 4; address 10000160e
L1000015b3_4:
    XOR EAX, EAX
    JMP L1000015b3_8     ; inserted

; Entry 1000015b3; block 5; address 1000015fe
L1000015b3_5:
    TEST RAX, RAX
    JE L1000015b3_4
    JMP L1000015b3_6     ; inserted

; Entry 1000015b3; block 6; address 100001603
L1000015b3_6:
    MOV AL, byte [R15 + 1]
    INC R15
    TEST AL, AL
    JNE L1000015b3_7
    JMP L1000015b3_4     ; inserted

; Entry 1000015b3; block 7; address 1000015f3
L1000015b3_7:
    MOVSX ESI, AL
    MOV RDI, R14
    CALL _index
    JMP L1000015b3_5     ; inserted

; Entry 1000015b3; block 8; address 100001610
L1000015b3_8:
    MOV CL, byte [R15 + RAX]
    MOV byte [RBX + RAX], CL
    INC RAX
    TEST CL, CL
    JNE L1000015b3_8
    JMP L1000015b3_9     ; inserted

; Entry 1000015b3; block 9; address 10000161e
L1000015b3_9:
    ADD RSP, 8
    POP RBX
    POP R14
    POP R15
    POP RBP
    RET 



; ---------------------
; Function: 0x100001629
; ---------------------
; Entry 100001629; block 0; address 100001629
L100001629_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    PUSH RAX
    TEST RDI, RDI
    JE L100001629_2
    JMP L100001629_1     ; inserted

; Entry 100001629; block 1; address 10000163c
L100001629_1:
    MOV qword [rel L__DATA___bss + 40], RDI
    JMP L100001629_17     ; inserted

; Entry 100001629; block 2; address 100001657
L100001629_2:
    MOV RAX, qword [rel L__DATA___bss + 40]
    LEA RBX, [rel L__DATA___common + 24]
    LEA R12, [rel L__TEXT___cstring_263]
    MOV R13, qword [rel ___stdinp]
    LEA R15, [rel L__TEXT___cstring_261]
    LEA R14, [rel L__DATA___common + 16]
    JMP L100001629_15     ; inserted

; Entry 100001629; block 3; address 100001689
L100001629_3:
    MOV RDX, qword [R13]
    MOV RSI, R15
    CALL _freopen
    JMP L100001629_5     ; inserted

; Entry 100001629; block 4; address 1000016bb
L100001629_4:
    MOV EAX, dword [rel L__DATA___bss + 48]
    LEA ECX, [RAX + 1]
    MOV dword [rel L__DATA___bss + 48], ECX
    TEST EAX, EAX
    JNE L100001629_11
    JMP L100001629_10     ; inserted

; Entry 100001629; block 5; address 100001695
L100001629_5:
    TEST RAX, RAX
    JE L100001629_7
    JMP L100001629_6     ; inserted

; Entry 100001629; block 6; address 10000169a
L100001629_6:
    MOV dword [rel L__DATA___bss + 48], 1
    CMP qword [RBX], 0
    JE L100001629_9
    JMP L100001629_8     ; inserted

; Entry 100001629; block 7; address 100001701
L100001629_7:
    MOV RAX, qword [rel L__DATA___bss + 40]
    MOV RSI, qword [RAX]
    LEA RDI, [rel L__TEXT___cstring_222]
    XOR EAX, EAX
    CALL _warn
    JMP L100001629_18     ; inserted

; Entry 100001629; block 8; address 1000016aa
L100001629_8:
    MOV RAX, qword [rel L__DATA___bss + 40]
    MOV RDI, qword [RAX]
    MOV ESI, 1
    JMP L100001629_12

; Entry 100001629; block 9; address 1000016de
L100001629_9:
    MOV RAX, qword [rel L__DATA___bss + 40]
    CMP qword [RAX], 0
    JE L100001629_14
    JMP L100001629_13     ; inserted

; Entry 100001629; block 10; address 1000016ce
L100001629_10:
    XOR ESI, ESI
    CMP qword [RBX], 0
    MOV RDI, R12
    JE L100001629_9
    JMP L100001629_12     ; inserted

; Entry 100001629; block 11; address 100001737
L100001629_11:
    XOR EAX, EAX
    JMP L100001629_19

; Entry 100001629; block 12; address 1000016d9
L100001629_12:
    CALL L1000017c9_0
    JMP L100001629_9     ; inserted

; Entry 100001629; block 13; address 1000016eb
L100001629_13:
    ADD RAX, 8
    MOV qword [rel L__DATA___bss + 40], RAX
    JMP L100001629_14     ; inserted

; Entry 100001629; block 14; address 1000016f6
L100001629_14:
    CMP qword [RBX], 0
    JNE L100001629_15
    JMP L100001629_16     ; inserted

; Entry 100001629; block 15; address 100001681
L100001629_15:
    MOV RDI, qword [RAX]
    TEST RDI, RDI
    JE L100001629_4
    JMP L100001629_3     ; inserted

; Entry 100001629; block 16; address 1000016fc
L100001629_16:
    JMP L100001629_17

; Entry 100001629; block 17; address 100001643
L100001629_17:
    MOV EAX, 1
    JMP L100001629_19     ; inserted

; Entry 100001629; block 18; address 100001719
L100001629_18:
    MOV dword [R14], 1
    MOV RAX, qword [rel L__DATA___bss + 40]
    ADD RAX, 8
    MOV qword [rel L__DATA___bss + 40], RAX
    JMP L100001629_15

; Entry 100001629; block 19; address 100001648
L100001629_19:
    ADD RSP, 8
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 



; ---------------------
; Function: 0x10000173e
; ---------------------
; Entry 10000173e; block 0; address 10000173e
L10000173e_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R12
    PUSH RBX
    LEA RAX, [rel L__DATA___data + 12]
    MOVSXD RAX, dword [RAX]
    CMP RAX, RSI
    MOV R15, RSI
    CMOVB R15, RAX
    CMP RAX, 18446744073709551615
    CMOVE R15, RSI
    TEST R15, R15
    JE L10000173e_2
    JMP L10000173e_1     ; inserted

; Entry 10000173e; block 1; address 10000176a
L10000173e_1:
    MOV R14, RDI
    XOR EBX, EBX
    JMP L10000173e_6     ; inserted

; Entry 10000173e; block 2; address 1000017ba
L10000173e_2:
    XOR R15D, R15D
    JMP L10000173e_13     ; inserted

; Entry 10000173e; block 3; address 100001774
L10000173e_3:
    CMP EAX, 18446744073709551615
    JE L10000173e_5
    JMP L10000173e_4     ; inserted

; Entry 10000173e; block 4; address 100001779
L10000173e_4:
    MOV byte [R14 + RBX], AL
    INC RBX
    CMP R15, RBX
    JNE L10000173e_6
    JMP L10000173e_7     ; inserted

; Entry 10000173e; block 5; address 10000178a
L10000173e_5:
    ADD R14, RBX
    MOV R15, RBX
    JMP L10000173e_8     ; inserted

; Entry 10000173e; block 6; address 10000176f
L10000173e_6:
    CALL _getchar
    JMP L10000173e_3     ; inserted

; Entry 10000173e; block 7; address 100001785
L10000173e_7:
    ADD R14, RBX
    JMP L10000173e_8

; Entry 10000173e; block 8; address 100001790
L10000173e_8:
    TEST R15, R15
    JE L10000173e_2
    JMP L10000173e_9     ; inserted

; Entry 10000173e; block 9; address 100001795
L10000173e_9:
    XOR EBX, EBX
    MOV R12, qword [rel ___stdinp]
    JMP L10000173e_11     ; inserted

; Entry 10000173e; block 10; address 1000017ad
L10000173e_10:
    DEC RBX
    MOV RAX, R15
    ADD RAX, RBX
    JNE L10000173e_11
    JMP L10000173e_12     ; inserted

; Entry 10000173e; block 11; address 10000179e
L10000173e_11:
    MOVZX EDI, byte [R14 + RBX-1]
    MOV RSI, qword [R12]
    CALL _ungetc
    JMP L10000173e_10     ; inserted

; Entry 10000173e; block 12; address 1000017b8
L10000173e_12:
    JMP L10000173e_13

; Entry 10000173e; block 13; address 1000017bd
L10000173e_13:
    MOV RAX, R15
    POP RBX
    POP R12
    POP R14
    POP R15
    POP RBP
    RET 



; ---------------------
; Function: 0x1000017c9
; ---------------------
; Entry 1000017c9; block 0; address 1000017c9
L1000017c9_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH RBX
    SUB RSP, 152
    MOV R14, RDI
    TEST ESI, ESI
    JE L1000017c9_2
    JMP L1000017c9_1     ; inserted

; Entry 1000017c9; block 1; address 1000017e0
L1000017c9_1:
    MOV RAX, qword [rel ___stdinp]
    MOV RDI, qword [RAX]
    CALL _fileno
    JMP L1000017c9_3     ; inserted

; Entry 1000017c9; block 2; address 10000182b
L1000017c9_2:
    MOV RAX, qword [rel ___stdinp]
    MOV RDI, qword [RAX]
    LEA R15, [rel L__DATA___common + 24]
    MOV RSI, qword [R15]
    XOR EDX, EDX
    CALL _fseeko
    JMP L1000017c9_9     ; inserted

; Entry 1000017c9; block 3; address 1000017ef
L1000017c9_3:
    LEA RSI, [RBP + -168]
    MOV EDI, EAX
    CALL _fstat$INODE64
    JMP L1000017c9_4     ; inserted

; Entry 1000017c9; block 4; address 1000017fd
L1000017c9_4:
    TEST EAX, EAX
    JNE L1000017c9_6
    JMP L1000017c9_5     ; inserted

; Entry 1000017c9; block 5; address 100001805
L1000017c9_5:
    MOVZX EAX, word [RBP + -164]
    AND EAX, 61440
    CMP EAX, 32768
    JNE L1000017c9_2
    JMP L1000017c9_7     ; inserted

; Entry 1000017c9; block 6; address 1000018ae
L1000017c9_6:
    LEA RSI, [rel L__TEXT___cstring_222]
    MOV EDI, 1
    MOV RDX, R14
    XOR EAX, EAX
    CALL _err

; Entry 1000017c9; block 7; address 100001818
L1000017c9_7:
    LEA RAX, [rel L__DATA___common + 24]
    MOV RCX, qword [RAX]
    MOV RAX, qword [RBP + -72]
    SUB RCX, RAX
    JGE L1000017c9_8
    JMP L1000017c9_2     ; inserted

; Entry 1000017c9; block 8; address 10000188f
L1000017c9_8:
    ADD qword [rel L__DATA___bss + 0], RAX
    JMP L1000017c9_19     ; inserted

; Entry 1000017c9; block 9; address 100001846
L1000017c9_9:
    TEST EAX, EAX
    JE L1000017c9_11
    JMP L1000017c9_10     ; inserted

; Entry 1000017c9; block 10; address 10000184a
L1000017c9_10:
    CALL ___error
    JMP L1000017c9_12     ; inserted

; Entry 1000017c9; block 11; address 100001881
L1000017c9_11:
    MOV RAX, qword [R15]
    ADD qword [rel L__DATA___bss + 0], RAX
    XOR ECX, ECX
    JMP L1000017c9_19

; Entry 1000017c9; block 12; address 10000184f
L1000017c9_12:
    CMP dword [RAX], 29
    JNE L1000017c9_6
    JMP L1000017c9_13     ; inserted

; Entry 1000017c9; block 13; address 100001854
L1000017c9_13:
    MOV RCX, qword [R15]
    XOR EBX, EBX
    TEST RCX, RCX
    JLE L1000017c9_15
    JMP L1000017c9_14     ; inserted

; Entry 1000017c9; block 14; address 10000185e
L1000017c9_14:
    CALL _getchar
    JMP L1000017c9_16     ; inserted

; Entry 1000017c9; block 15; address 100001875
L1000017c9_15:
    ADD qword [rel L__DATA___bss + 0], RBX
    SUB RCX, RBX
    JMP L1000017c9_19

; Entry 1000017c9; block 16; address 100001863
L1000017c9_16:
    MOV RCX, qword [R15]
    CMP EAX, 18446744073709551615
    JE L1000017c9_15
    JMP L1000017c9_17     ; inserted

; Entry 1000017c9; block 17; address 10000186b
L1000017c9_17:
    INC RBX
    CMP RCX, RBX
    JG L1000017c9_14
    JMP L1000017c9_18     ; inserted

; Entry 1000017c9; block 18; address 100001873
L1000017c9_18:
    MOV EBX, EBX
    JMP L1000017c9_15     ; inserted

; Entry 1000017c9; block 19; address 100001896
L1000017c9_19:
    LEA RAX, [rel L__DATA___common + 24]
    MOV qword [RAX], RCX
    ADD RSP, 152
    POP RBX
    POP R14
    POP R15
    POP RBP
    RET 



; ---------------------
; Function: 0x1000018c4
; ---------------------
; Entry 1000018c4; block 0; address 1000018c4
L1000018c4_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R14
    PUSH RBX
    SUB RSP, 16
    MOV RBX, RSI
    MOV R14D, EDI
    MOV qword [RBP + -24], RSI
    LEA RSI, [rel L__TEXT___cstring_90]
    XOR EDI, EDI
    CALL _setlocale
    JMP L1000018c4_1     ; inserted

; Entry 1000018c4; block 1; address 1000018e7
L1000018c4_1:
    MOV RDI, qword [RBX]
    MOV ESI, 111
    CALL _rindex
    JMP L1000018c4_2     ; inserted

; Entry 1000018c4; block 2; address 1000018f4
L1000018c4_2:
    TEST RAX, RAX
    JE L1000018c4_4
    JMP L1000018c4_3     ; inserted

; Entry 1000018c4; block 3; address 1000018f9
L1000018c4_3:
    LEA RSI, [rel L__TEXT___cstring_271]
    MOV RDI, RAX
    CALL _strcmp
    JMP L1000018c4_5     ; inserted

; Entry 1000018c4; block 4; address 10000191a
L1000018c4_4:
    LEA RSI, [RBP + -24]
    MOV EDI, R14D
    CALL L100001990_0
    JMP L1000018c4_8     ; inserted

; Entry 1000018c4; block 5; address 100001908
L1000018c4_5:
    TEST EAX, EAX
    JNE L1000018c4_4
    JMP L1000018c4_6     ; inserted

; Entry 1000018c4; block 6; address 10000190c
L1000018c4_6:
    LEA RSI, [RBP + -24]
    MOV EDI, R14D
    CALL L100001cf0_0
    JMP L1000018c4_7     ; inserted

; Entry 1000018c4; block 7; address 100001918
L1000018c4_7:
    JMP L1000018c4_8

; Entry 1000018c4; block 8; address 100001926
L1000018c4_8:
    MOV dword [rel L__DATA___common + 0], 0
    MOV RBX, qword [rel L__DATA___common + 8]
    TEST RBX, RBX
    JNE L1000018c4_10
    JMP L1000018c4_9     ; inserted

; Entry 1000018c4; block 9; address 10000193c
L1000018c4_9:
    MOV RDI, qword [RBP + -24]
    CALL L100001629_0
    JMP L1000018c4_11     ; inserted

; Entry 1000018c4; block 10; address 100001955
L1000018c4_10:
    MOV RDI, RBX
    CALL L100002b34_0
    JMP L1000018c4_13     ; inserted

; Entry 1000018c4; block 11; address 100001945
L1000018c4_11:
    CALL L100000e94_0
    JMP L1000018c4_12     ; inserted

; Entry 1000018c4; block 12; address 10000194a
L1000018c4_12:
    MOV EDI, dword [rel L__DATA___common + 16]
    CALL _exit

; Entry 1000018c4; block 13; address 10000195d
L1000018c4_13:
    MOV dword [RBX + 16], EAX
    CMP dword [rel L__DATA___common + 0], EAX
    JGE L1000018c4_15
    JMP L1000018c4_14     ; inserted

; Entry 1000018c4; block 14; address 100001968
L1000018c4_14:
    MOV dword [rel L__DATA___common + 0], EAX
    JMP L1000018c4_15     ; inserted

; Entry 1000018c4; block 15; address 10000196e
L1000018c4_15:
    MOV RBX, qword [RBX]
    TEST RBX, RBX
    JNE L1000018c4_10
    JMP L1000018c4_16     ; inserted

; Entry 1000018c4; block 16; address 100001976
L1000018c4_16:
    MOV RBX, qword [rel L__DATA___common + 8]
    JMP L1000018c4_19     ; inserted

; Entry 1000018c4; block 17; address 100001982
L1000018c4_17:
    MOV RDI, RBX
    CALL L100002cc8_0
    JMP L1000018c4_18     ; inserted

; Entry 1000018c4; block 18; address 10000198a
L1000018c4_18:
    MOV RBX, qword [RBX]
    JMP L1000018c4_19

; Entry 1000018c4; block 19; address 10000197d
L1000018c4_19:
    TEST RBX, RBX
    JE L1000018c4_9
    JMP L1000018c4_17     ; inserted



; ---------------------
; Function: 0x100001990
; ---------------------
; Entry 100001990; block 0; address 100001990
L100001990_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 24
    MOV R15D, EDI
    MOV qword [RBP + -56], RSI
    MOV RBX, qword [RSI]
    MOV RDI, qword [RBX]
    MOV ESI, 104
    CALL _rindex
    JMP L100001990_1     ; inserted

; Entry 100001990; block 1; address 1000019b8
L100001990_1:
    MOV qword [RBP + -48], RAX
    TEST RAX, RAX
    JE L100001990_3
    JMP L100001990_2     ; inserted

; Entry 100001990; block 2; address 1000019c1
L100001990_2:
    LEA RSI, [rel L__TEXT___cstring_274]
    MOV RDI, RAX
    CALL _strcmp
    JMP L100001990_4     ; inserted

; Entry 100001990; block 3; address 1000019f8
L100001990_3:
    LEA R12, [rel L__TEXT___cstring_355]
    LEA R14, [rel L__TEXT___text + 4328]
    JMP L100001990_11     ; inserted

; Entry 100001990; block 4; address 1000019d0
L100001990_4:
    TEST EAX, EAX
    JNE L100001990_3
    JMP L100001990_5     ; inserted

; Entry 100001990; block 5; address 1000019d4
L100001990_5:
    LEA RDI, [rel L__TEXT___cstring_277]
    CALL L1000027ed_0
    JMP L100001990_6     ; inserted

; Entry 100001990; block 6; address 1000019e0
L100001990_6:
    LEA RDI, [rel L__TEXT___cstring_289]
    CALL L1000027ed_0
    JMP L100001990_7     ; inserted

; Entry 100001990; block 7; address 1000019ec
L100001990_7:
    LEA RDI, [rel L__TEXT___cstring_332]
    CALL L1000027ed_0
    JMP L100001990_3     ; inserted

; Entry 100001990; block 8; address 100001a14
L100001990_8:
    CMP EAX, 97
    JLE L100001990_10
    JMP L100001990_9     ; inserted

; Entry 100001990; block 9; address 100001a19
L100001990_9:
    ADD EAX, 18446744073709551518
    CMP EAX, 22
    JA L100001990_11
    JMP L100001990_12     ; inserted

; Entry 100001990; block 10; address 100001a44
L100001990_10:
    CMP EAX, 67
    JNE L100001990_25
    JMP L100001990_24     ; inserted

; Entry 100001990; block 11; address 100001a06
L100001990_11:
    MOV EDI, R15D
    MOV RSI, RBX
    MOV RDX, R12
    CALL _getopt
    JMP L100001990_8     ; inserted

; Entry 100001990; block 12; address 100001a21
L100001990_12:
    MOVSXD RAX, dword [R14 + RAX * 4]
    ADD RAX, R14
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100001a28 + 8*EAX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 100001a2a,100001a73,100001a90,100001aad,100001ac1,100001a06,100001ad5,100001afd,100001b1a,100001b69

; Entry 100001990; block 13; address 100001a2a
L100001990_13:
    LEA RDI, [rel L__TEXT___cstring_371]
    CALL L1000027ed_0
    JMP L100001990_22     ; inserted

; Entry 100001990; block 14; address 100001a73
L100001990_14:
    LEA RDI, [rel L__TEXT___cstring_371]
    CALL L1000027ed_0
    JMP L100001990_29     ; inserted

; Entry 100001990; block 15; address 100001a90
L100001990_15:
    LEA RDI, [rel L__TEXT___cstring_371]
    CALL L1000027ed_0
    JMP L100001990_31     ; inserted

; Entry 100001990; block 16; address 100001aad
L100001990_16:
    MOV RAX, qword [rel _optarg]
    MOV RDI, qword [RAX]
    CALL L1000027ed_0
    JMP L100001990_33     ; inserted

; Entry 100001990; block 17; address 100001ac1
L100001990_17:
    MOV RAX, qword [rel _optarg]
    MOV RDI, qword [RAX]
    CALL L1000026c0_0
    JMP L100001990_34     ; inserted

; Entry 100001990; block 18; address 100001ad5
L100001990_18:
    MOV RAX, qword [rel _optarg]
    MOV R13, qword [RAX]
    MOV RDI, R13
    CALL _atoi
    JMP L100001990_35     ; inserted

; Entry 100001990; block 19; address 100001afd
L100001990_19:
    LEA RDI, [rel L__TEXT___cstring_371]
    CALL L1000027ed_0
    JMP L100001990_37     ; inserted

; Entry 100001990; block 20; address 100001b1a
L100001990_20:
    MOV RAX, qword [rel _optarg]
    MOV RDI, qword [RAX]
    LEA RSI, [RBP + -48]
    XOR EDX, EDX
    CALL _strtoll
    JMP L100001990_39     ; inserted

; Entry 100001990; block 21; address 100001b69
L100001990_21:
    LEA RAX, [rel L__DATA___data + 8]
    MOV dword [RAX], 0
    JMP L100001990_11

; Entry 100001990; block 22; address 100001a36
L100001990_22:
    LEA RDI, [rel L__TEXT___cstring_383]
    CALL L1000027ed_0
    JMP L100001990_23     ; inserted

; Entry 100001990; block 23; address 100001a42
L100001990_23:
    JMP L100001990_11

; Entry 100001990; block 24; address 100001a4d
L100001990_24:
    LEA RDI, [rel L__TEXT___cstring_277]
    CALL L1000027ed_0
    JMP L100001990_26     ; inserted

; Entry 100001990; block 25; address 100001b98
L100001990_25:
    CMP EAX, 18446744073709551615
    JE L100001990_48
    JMP L100001990_47     ; inserted

; Entry 100001990; block 26; address 100001a59
L100001990_26:
    LEA RDI, [rel L__TEXT___cstring_289]
    CALL L1000027ed_0
    JMP L100001990_27     ; inserted

; Entry 100001990; block 27; address 100001a65
L100001990_27:
    LEA RDI, [rel L__TEXT___cstring_332]
    CALL L1000027ed_0
    JMP L100001990_28     ; inserted

; Entry 100001990; block 28; address 100001a71
L100001990_28:
    JMP L100001990_11

; Entry 100001990; block 29; address 100001a7f
L100001990_29:
    LEA RDI, [rel L__TEXT___cstring_413]
    CALL L1000027ed_0
    JMP L100001990_30     ; inserted

; Entry 100001990; block 30; address 100001a8b
L100001990_30:
    JMP L100001990_11

; Entry 100001990; block 31; address 100001a9c
L100001990_31:
    LEA RDI, [rel L__TEXT___cstring_443]
    CALL L1000027ed_0
    JMP L100001990_32     ; inserted

; Entry 100001990; block 32; address 100001aa8
L100001990_32:
    JMP L100001990_11

; Entry 100001990; block 33; address 100001abc
L100001990_33:
    JMP L100001990_11

; Entry 100001990; block 34; address 100001ad0
L100001990_34:
    JMP L100001990_11

; Entry 100001990; block 35; address 100001ae7
L100001990_35:
    LEA RCX, [rel L__DATA___data + 12]
    MOV dword [RCX], EAX
    TEST EAX, EAX
    JNS L100001990_11
    JMP L100001990_36     ; inserted

; Entry 100001990; block 36; address 100001af8
L100001990_36:
    LEA RSI, [rel L__TEXT___cstring_474]
    MOV EDI, 1
    MOV RDX, R13
    XOR EAX, EAX
    CALL _errx

; Entry 100001990; block 37; address 100001b09
L100001990_37:
    LEA RDI, [rel L__TEXT___cstring_495]
    CALL L1000027ed_0
    JMP L100001990_38     ; inserted

; Entry 100001990; block 38; address 100001b15
L100001990_38:
    JMP L100001990_11

; Entry 100001990; block 39; address 100001b2f
L100001990_39:
    MOV qword [rel L__DATA___common + 24], RAX
    TEST RAX, RAX
    JS L100001990_41
    JMP L100001990_40     ; inserted

; Entry 100001990; block 40; address 100001b3f
L100001990_40:
    MOV RCX, qword [RBP + -48]
    MOVSX ECX, byte [RCX]
    CMP ECX, 106
    JG L100001990_43
    JMP L100001990_42     ; inserted

; Entry 100001990; block 41; address 100001c48
L100001990_41:
    CALL L1000033a2_0

; Entry 100001990; block 42; address 100001b4b
L100001990_42:
    CMP ECX, 98
    JE L100001990_45
    JMP L100001990_44     ; inserted

; Entry 100001990; block 43; address 100001bab
L100001990_43:
    CMP ECX, 107
    JE L100001990_51
    JMP L100001990_50     ; inserted

; Entry 100001990; block 44; address 100001b50
L100001990_44:
    CMP ECX, 103
    JNE L100001990_11
    JMP L100001990_46     ; inserted

; Entry 100001990; block 45; address 100001bc9
L100001990_45:
    SHL RAX, 9
    MOV qword [rel L__DATA___common + 24], RAX
    JMP L100001990_11

; Entry 100001990; block 46; address 100001b59
L100001990_46:
    SHL RAX, 30
    MOV qword [rel L__DATA___common + 24], RAX
    JMP L100001990_11

; Entry 100001990; block 47; address 100001b9d
L100001990_47:
    CMP EAX, 63
    JNE L100001990_11
    JMP L100001990_49     ; inserted

; Entry 100001990; block 48; address 100001be9
L100001990_48:
    LEA RAX, [rel L__DATA___common + 8]
    CMP qword [RAX], 0
    JNE L100001990_54
    JMP L100001990_53     ; inserted

; Entry 100001990; block 49; address 100001ba6
L100001990_49:
    CALL L100001cb0_0

; Entry 100001990; block 50; address 100001bb0
L100001990_50:
    CMP ECX, 109
    JNE L100001990_11
    JMP L100001990_52     ; inserted

; Entry 100001990; block 51; address 100001bd9
L100001990_51:
    SHL RAX, 10
    MOV qword [rel L__DATA___common + 24], RAX
    JMP L100001990_11

; Entry 100001990; block 52; address 100001bb9
L100001990_52:
    SHL RAX, 20
    MOV qword [rel L__DATA___common + 24], RAX
    JMP L100001990_11

; Entry 100001990; block 53; address 100001bf6
L100001990_53:
    LEA RDI, [rel L__TEXT___cstring_371]
    CALL L1000027ed_0
    JMP L100001990_55     ; inserted

; Entry 100001990; block 54; address 100001c0e
L100001990_54:
    MOV RAX, qword [rel _optind]
    MOVSXD RAX, dword [RAX]
    SHL RAX, 3
    MOV RCX, qword [RBP + -56]
    ADD qword [RCX], RAX
    ADD RSP, 24
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100001990; block 55; address 100001c02
L100001990_55:
    LEA RDI, [rel L__TEXT___cstring_576]
    CALL L1000027ed_0
    JMP L100001990_54     ; inserted



; ---------------------
; Function: 0x100001cb0
; ---------------------
; Entry 100001cb0; block 0; address 100001cb0
L100001cb0_0:
    PUSH RBP
    MOV RBP, RSP
    MOV RAX, qword [rel ___stderrp]
    MOV RDI, qword [RAX]
    LEA RSI, [rel L__TEXT___cstring_606]
    LEA RDX, [rel L__TEXT___cstring_619]
    LEA R8, [rel L__TEXT___cstring_716]
    LEA RCX, [rel L__TEXT___cstring_680]
    MOV R9, RCX
    XOR EAX, EAX
    CALL _fprintf
    JMP L100001cb0_1     ; inserted

; Entry 100001cb0; block 1; address 100001ce4
L100001cb0_1:
    MOV EDI, 1
    CALL _exit



; ---------------------
; Function: 0x100001cf0
; ---------------------
; Entry 100001cf0; block 0; address 100001cf0
L100001cf0_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 24
    MOV RBX, RSI
    MOV R15D, EDI
    LEA RDI, [rel L__TEXT___cstring_777]
    CALL L1000027ed_0
    JMP L100001cf0_1     ; inserted

; Entry 100001cf0; block 1; address 100001d13
L100001cf0_1:
    LEA RDI, [rel L__TEXT___cstring_789]
    CALL L1000027ed_0
    JMP L100001cf0_2     ; inserted

; Entry 100001cf0; block 2; address 100001d1f
L100001cf0_2:
    MOV dword [rel L__DATA___common + 32], 1
    MOV qword [RBP + -48], RBX
    MOV R14, qword [RBX]
    LEA R12, [rel L__TEXT___cstring_802]
    LEA R13, [rel L__TEXT___text + 5764]
    JMP L100001cf0_27     ; inserted

; Entry 100001cf0; block 3; address 100001d4c
L100001cf0_3:
    LEA ECX, [RAX + -65]
    CMP ECX, 55
    JA L100001cf0_5
    JMP L100001cf0_4     ; inserted

; Entry 100001cf0; block 4; address 100001d58
L100001cf0_4:
    MOVSXD RAX, dword [R13 + RCX * 4]
    ADD RAX, R13
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100001d60 + 8*ECX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 100001da8,100001d70,1000021c6,100001e02,100001d7e,100001d8c,100001d62,100001e13,100001e3b,100001e4c,100001e5d,100001e6e,100001e7f,100001e90,100001d9a,100001ea1,100001eb2,100001f16,100001f27,100001f3b

; Entry 100001cf0; block 5; address 100001fdf
L100001cf0_5:
    CMP EAX, 18446744073709551615
    JNE L100001cf0_25
    JMP L100001cf0_70     ; inserted

; Entry 100001cf0; block 6; address 100001d62
L100001cf0_6:
    LEA RDI, [rel L__TEXT___cstring_885]
    CALL L1000022d0_0
    JMP L100001cf0_26     ; inserted

; Entry 100001cf0; block 7; address 100001d70
L100001cf0_7:
    LEA RDI, [rel L__TEXT___cstring_859]
    CALL L1000022d0_0
    JMP L100001cf0_28     ; inserted

; Entry 100001cf0; block 8; address 100001d7e
L100001cf0_8:
    LEA RDI, [rel L__TEXT___cstring_873]
    CALL L1000022d0_0
    JMP L100001cf0_29     ; inserted

; Entry 100001cf0; block 9; address 100001d8c
L100001cf0_9:
    LEA RDI, [rel L__TEXT___cstring_879]
    CALL L1000022d0_0
    JMP L100001cf0_30     ; inserted

; Entry 100001cf0; block 10; address 100001d9a
L100001cf0_10:
    LEA RDI, [rel L__TEXT___cstring_882]
    CALL L1000022d0_0
    JMP L100001cf0_31     ; inserted

; Entry 100001cf0; block 11; address 100001da8
L100001cf0_11:
    MOV RAX, qword [rel _optarg]
    MOV RDX, qword [RAX]
    MOV AL, byte [RDX]
    MOVSX ECX, AL
    CMP ECX, 110
    JG L100001cf0_33
    JMP L100001cf0_32     ; inserted

; Entry 100001cf0; block 12; address 100001e02
L100001cf0_12:
    LEA RDI, [rel L__TEXT___cstring_870]
    CALL L1000022d0_0
    JMP L100001cf0_38     ; inserted

; Entry 100001cf0; block 13; address 100001e13
L100001cf0_13:
    MOV RAX, qword [rel _optarg]
    MOV RBX, qword [RAX]
    MOV RDI, RBX
    CALL _atoi
    JMP L100001cf0_39     ; inserted

; Entry 100001cf0; block 14; address 100001e3b
L100001cf0_14:
    LEA RDI, [rel L__TEXT___cstring_934]
    CALL L1000022d0_0
    JMP L100001cf0_41     ; inserted

; Entry 100001cf0; block 15; address 100001e4c
L100001cf0_15:
    LEA RDI, [rel L__TEXT___cstring_857]
    CALL L1000022d0_0
    JMP L100001cf0_42     ; inserted

; Entry 100001cf0; block 16; address 100001e5d
L100001cf0_16:
    LEA RDI, [rel L__TEXT___cstring_862]
    CALL L1000022d0_0
    JMP L100001cf0_43     ; inserted

; Entry 100001cf0; block 17; address 100001e6e
L100001cf0_17:
    LEA RDI, [rel L__TEXT___cstring_865]
    CALL L1000022d0_0
    JMP L100001cf0_44     ; inserted

; Entry 100001cf0; block 18; address 100001e7f
L100001cf0_18:
    LEA RDI, [rel L__TEXT___cstring_867]
    CALL L1000022d0_0
    JMP L100001cf0_45     ; inserted

; Entry 100001cf0; block 19; address 100001e90
L100001cf0_19:
    LEA RDI, [rel L__TEXT___cstring_876]
    CALL L1000022d0_0
    JMP L100001cf0_46     ; inserted

; Entry 100001cf0; block 20; address 100001ea1
L100001cf0_20:
    LEA RDI, [rel L__TEXT___cstring_888]
    CALL L1000022d0_0
    JMP L100001cf0_47     ; inserted

; Entry 100001cf0; block 21; address 100001eb2
L100001cf0_21:
    CALL ___error
    JMP L100001cf0_48     ; inserted

; Entry 100001cf0; block 22; address 100001f16
L100001cf0_22:
    LEA RDI, [rel L__TEXT___cstring_937]
    CALL L1000022d0_0
    JMP L100001cf0_58     ; inserted

; Entry 100001cf0; block 23; address 100001f27
L100001cf0_23:
    MOV RAX, qword [rel _optarg]
    MOV RDI, qword [RAX]
    CALL L1000022d0_0
    JMP L100001cf0_59     ; inserted

; Entry 100001cf0; block 24; address 100001f3b
L100001cf0_24:
    LEA RAX, [rel L__DATA___data + 8]
    MOV dword [RAX], 0
    JMP L100001cf0_27

; Entry 100001cf0; block 25; address 1000021c6
L100001cf0_25:
    CALL L100002640_0

; Entry 100001cf0; block 26; address 100001d6e
L100001cf0_26:
    JMP L100001cf0_27

; Entry 100001cf0; block 27; address 100001d3e
L100001cf0_27:
    MOV EDI, R15D
    MOV RSI, R14
    MOV RDX, R12
    CALL _getopt
    JMP L100001cf0_3     ; inserted

; Entry 100001cf0; block 28; address 100001d7c
L100001cf0_28:
    JMP L100001cf0_27

; Entry 100001cf0; block 29; address 100001d8a
L100001cf0_29:
    JMP L100001cf0_27

; Entry 100001cf0; block 30; address 100001d98
L100001cf0_30:
    JMP L100001cf0_27

; Entry 100001cf0; block 31; address 100001da6
L100001cf0_31:
    JMP L100001cf0_27

; Entry 100001cf0; block 32; address 100001dc0
L100001cf0_32:
    CMP ECX, 100
    JE L100001cf0_35
    JMP L100001cf0_34     ; inserted

; Entry 100001cf0; block 33; address 100001f4d
L100001cf0_33:
    CMP ECX, 120
    JE L100001cf0_35
    JMP L100001cf0_60     ; inserted

; Entry 100001cf0; block 34; address 100001dc9
L100001cf0_34:
    CMP ECX, 110
    JNE L100001cf0_37
    JMP L100001cf0_36     ; inserted

; Entry 100001cf0; block 35; address 100001f5b
L100001cf0_35:
    LEA RDX, [rel L__DATA___common + 8]
    MOV RCX, qword [RDX]
    MOV RCX, qword [RCX + 8]
    MOV RCX, qword [RCX + 32]
    MOV byte [RCX + 7], AL
    MOV RAX, qword [rel _optarg]
    MOV RAX, qword [RAX]
    MOV AL, byte [RAX]
    MOV RCX, qword [RDX]
    MOV RCX, qword [RCX]
    MOV RCX, qword [RCX + 8]
    MOV RCX, qword [RCX + 32]
    MOV byte [RCX + 7], AL
    JMP L100001cf0_27

; Entry 100001cf0; block 36; address 100001dd2
L100001cf0_36:
    LEA RAX, [rel L__DATA___common + 8]
    MOV RAX, qword [RAX]
    MOV RCX, qword [RAX + 8]
    LEA RDX, [rel L__DATA___data + 16]
    MOV qword [RCX + 32], RDX
    MOV RAX, qword [RAX]
    MOV RAX, qword [RAX + 8]
    LEA RCX, [rel L__DATA___data + 20]
    MOV qword [RAX + 32], RCX
    JMP L100001cf0_27

; Entry 100001cf0; block 37; address 1000021dc
L100001cf0_37:
    LEA RSI, [rel L__TEXT___cstring_832]
    MOV EDI, 1
    JMP L100001cf0_117     ; inserted

; Entry 100001cf0; block 38; address 100001e0e
L100001cf0_38:
    JMP L100001cf0_27

; Entry 100001cf0; block 39; address 100001e25
L100001cf0_39:
    LEA RCX, [rel L__DATA___data + 12]
    MOV dword [RCX], EAX
    TEST EAX, EAX
    JG L100001cf0_27
    JMP L100001cf0_40     ; inserted

; Entry 100001cf0; block 40; address 100001e36
L100001cf0_40:
    LEA RSI, [rel L__TEXT___cstring_915]
    MOV EDI, 1
    MOV RDX, RBX
    JMP L100001cf0_117

; Entry 100001cf0; block 41; address 100001e47
L100001cf0_41:
    JMP L100001cf0_27

; Entry 100001cf0; block 42; address 100001e58
L100001cf0_42:
    JMP L100001cf0_27

; Entry 100001cf0; block 43; address 100001e69
L100001cf0_43:
    JMP L100001cf0_27

; Entry 100001cf0; block 44; address 100001e7a
L100001cf0_44:
    JMP L100001cf0_27

; Entry 100001cf0; block 45; address 100001e8b
L100001cf0_45:
    JMP L100001cf0_27

; Entry 100001cf0; block 46; address 100001e9c
L100001cf0_46:
    JMP L100001cf0_27

; Entry 100001cf0; block 47; address 100001ead
L100001cf0_47:
    JMP L100001cf0_27

; Entry 100001cf0; block 48; address 100001eb7
L100001cf0_48:
    MOV dword [RAX], 0
    MOV RAX, qword [rel _optarg]
    MOV RDI, qword [RAX]
    LEA RSI, [RBP + -56]
    XOR EDX, EDX
    CALL _strtoll
    JMP L100001cf0_49     ; inserted

; Entry 100001cf0; block 49; address 100001ed2
L100001cf0_49:
    LEA RSI, [rel L__DATA___common + 24]
    MOV qword [RSI], RAX
    MOV RBX, qword [RBP + -56]
    MOV DL, byte [RBX]
    MOV RCX, 18446744073709551615
    CMP DL, 102
    JLE L100001cf0_51
    JMP L100001cf0_50     ; inserted

; Entry 100001cf0; block 50; address 100001ef2
L100001cf0_50:
    CMP DL, 103
    JE L100001cf0_53
    JMP L100001cf0_52     ; inserted

; Entry 100001cf0; block 51; address 100001f92
L100001cf0_51:
    TEST DL, DL
    JE L100001cf0_62
    JMP L100001cf0_61     ; inserted

; Entry 100001cf0; block 52; address 100001efb
L100001cf0_52:
    CMP DL, 107
    JE L100001cf0_55
    JMP L100001cf0_54     ; inserted

; Entry 100001cf0; block 53; address 100001fa1
L100001cf0_53:
    SHL RAX, 30
    JMP L100001cf0_64

; Entry 100001cf0; block 54; address 100001f04
L100001cf0_54:
    CMP DL, 109
    JNE L100001cf0_57
    JMP L100001cf0_56     ; inserted

; Entry 100001cf0; block 55; address 100001fa7
L100001cf0_55:
    SHL RAX, 10
    JMP L100001cf0_64     ; inserted

; Entry 100001cf0; block 56; address 100001f0d
L100001cf0_56:
    SHL RAX, 20
    JMP L100001cf0_64

; Entry 100001cf0; block 57; address 100001fae
L100001cf0_57:
    MOV qword [RSI], RCX
    JMP L100001cf0_62     ; inserted

; Entry 100001cf0; block 58; address 100001f22
L100001cf0_58:
    JMP L100001cf0_27

; Entry 100001cf0; block 59; address 100001f36
L100001cf0_59:
    JMP L100001cf0_27

; Entry 100001cf0; block 60; address 100001f52
L100001cf0_60:
    CMP ECX, 111
    JNE L100001cf0_37
    JMP L100001cf0_35     ; inserted

; Entry 100001cf0; block 61; address 100001f96
L100001cf0_61:
    CMP DL, 98
    JNE L100001cf0_57
    JMP L100001cf0_63     ; inserted

; Entry 100001cf0; block 62; address 100001fb1
L100001cf0_62:
    CALL ___error
    JMP L100001cf0_65     ; inserted

; Entry 100001cf0; block 63; address 100001f9b
L100001cf0_63:
    SHL RAX, 9
    JMP L100001cf0_64

; Entry 100001cf0; block 64; address 100001fab
L100001cf0_64:
    MOV RCX, RAX
    JMP L100001cf0_57     ; inserted

; Entry 100001cf0; block 65; address 100001fb6
L100001cf0_65:
    CMP dword [RAX], 0
    JNE L100001cf0_67
    JMP L100001cf0_66     ; inserted

; Entry 100001cf0; block 66; address 100001fbb
L100001cf0_66:
    LEA RAX, [rel L__DATA___common + 24]
    CMP qword [RAX], 0
    JS L100001cf0_67
    JMP L100001cf0_68     ; inserted

; Entry 100001cf0; block 67; address 100001fda
L100001cf0_67:
    CALL L1000033c1_0

; Entry 100001cf0; block 68; address 100001fc8
L100001cf0_68:
    MOV RDI, RBX
    CALL _strlen
    JMP L100001cf0_69     ; inserted

; Entry 100001cf0; block 69; address 100001fd0
L100001cf0_69:
    CMP RAX, 2
    JB L100001cf0_27
    JMP L100001cf0_67     ; inserted

; Entry 100001cf0; block 70; address 100001fe8
L100001cf0_70:
    LEA RAX, [rel L__DATA___common + 8]
    MOV RAX, qword [RAX]
    MOV RAX, qword [RAX]
    CMP qword [RAX], 0
    JNE L100001cf0_72
    JMP L100001cf0_71     ; inserted

; Entry 100001cf0; block 71; address 100001ffb
L100001cf0_71:
    LEA RDI, [rel L__TEXT___cstring_940]
    CALL L1000022d0_0
    JMP L100001cf0_72     ; inserted

; Entry 100001cf0; block 72; address 100002007
L100001cf0_72:
    MOV RAX, qword [rel _optind]
    MOVSXD RCX, dword [RAX]
    MOV RAX, RCX
    SHL RAX, 3
    MOV R13, qword [RBP + -48]
    ADD RAX, qword [R13]
    MOV qword [R13], RAX
    SUB R15D, ECX
    JE L100001cf0_74
    JMP L100001cf0_73     ; inserted

; Entry 100001cf0; block 73; address 10000202d
L100001cf0_73:
    CMP R15D, 1
    JNE L100001cf0_76
    JMP L100001cf0_75     ; inserted

; Entry 100001cf0; block 74; address 100002171
L100001cf0_74:
    ADD RSP, 24
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100001cf0; block 75; address 100002033
L100001cf0_75:
    MOV RDI, qword [RAX]
    CMP byte [RDI], 43
    JE L100001cf0_78
    JMP L100001cf0_77     ; inserted

; Entry 100001cf0; block 76; address 100002040
L100001cf0_76:
    MOV RDI, qword [RAX + 8]
    MOVZX EAX, byte [RDI]
    CMP RAX, 43
    JNE L100001cf0_79
    JMP L100001cf0_78     ; inserted

; Entry 100001cf0; block 77; address 10000203b
L100001cf0_77:
    JMP L100001cf0_74

; Entry 100001cf0; block 78; address 10000204d
L100001cf0_78:
    INC RDI
    JMP L100001cf0_86     ; inserted

; Entry 100001cf0; block 79; address 100002074
L100001cf0_79:
    CMP R15D, 2
    JL L100001cf0_74
    JMP L100001cf0_85     ; inserted

; Entry 100001cf0; block 80; address 100002059
L100001cf0_80:
    CMP AL, 120
    JNE L100001cf0_83
    JMP L100001cf0_82     ; inserted

; Entry 100001cf0; block 81; address 1000020a4
L100001cf0_81:
    CMP byte [RDI + 1], 120
    JNE L100001cf0_83
    JMP L100001cf0_90     ; inserted

; Entry 100001cf0; block 82; address 10000205d
L100001cf0_82:
    MOVZX ECX, byte [RDI + 1]
    MOV RDX, qword [rel __DefaultRuneLocale]
    TEST byte [RDX + RCX * 4 + 62], 1
    JE L100001cf0_83
    JMP L100001cf0_84     ; inserted

; Entry 100001cf0; block 83; address 1000020d1
L100001cf0_83:
    MOV RDX, qword [rel __DefaultRuneLocale]
    MOV CL, 1
    XOR R14D, R14D
    TEST byte [RDX + RAX * 4 + 61], 4
    JNE L100001cf0_95
    JMP L100001cf0_94     ; inserted

; Entry 100001cf0; block 84; address 10000206f
L100001cf0_84:
    INC RDI
    JMP L100001cf0_91

; Entry 100001cf0; block 85; address 10000207e
L100001cf0_85:
    MOV RCX, qword [rel __DefaultRuneLocale]
    TEST byte [RCX + RAX * 4 + 61], 4
    JNE L100001cf0_86
    JMP L100001cf0_87     ; inserted

; Entry 100001cf0; block 86; address 100002050
L100001cf0_86:
    MOVZX EAX, byte [RDI]
    CMP RAX, 48
    JE L100001cf0_81
    JMP L100001cf0_80     ; inserted

; Entry 100001cf0; block 87; address 10000208c
L100001cf0_87:
    CMP AL, 120
    JNE L100001cf0_74
    JMP L100001cf0_88     ; inserted

; Entry 100001cf0; block 88; address 100002094
L100001cf0_88:
    MOVZX EAX, byte [RDI + 1]
    TEST byte [RCX + RAX * 4 + 62], 1
    JNE L100001cf0_86
    JMP L100001cf0_89     ; inserted

; Entry 100001cf0; block 89; address 10000209f
L100001cf0_89:
    JMP L100001cf0_74

; Entry 100001cf0; block 90; address 1000020aa
L100001cf0_90:
    ADD RDI, 2
    JMP L100001cf0_91     ; inserted

; Entry 100001cf0; block 91; address 1000020ae
L100001cf0_91:
    LEA RBX, [RDI + -1]
    MOV RCX, qword [rel __DefaultRuneLocale]
    JMP L100001cf0_92     ; inserted

; Entry 100001cf0; block 92; address 1000020b9
L100001cf0_92:
    MOVZX EAX, byte [RBX + 1]
    INC RBX
    TEST byte [RCX + RAX * 4 + 62], 1
    JNE L100001cf0_92
    JMP L100001cf0_93     ; inserted

; Entry 100001cf0; block 93; address 1000020c7
L100001cf0_93:
    XOR ECX, ECX
    MOV R14D, 16
    JMP L100001cf0_96

; Entry 100001cf0; block 94; address 1000020e4
L100001cf0_94:
    MOV RBX, RDI
    JMP L100001cf0_96

; Entry 100001cf0; block 95; address 1000020e9
L100001cf0_95:
    MOV RBX, RDI
    JMP L100001cf0_97     ; inserted

; Entry 100001cf0; block 96; address 1000020fa
L100001cf0_96:
    CMP RDI, RBX
    JE L100001cf0_74
    JMP L100001cf0_98     ; inserted

; Entry 100001cf0; block 97; address 1000020ec
L100001cf0_97:
    MOVZX EAX, byte [RBX + 1]
    INC RBX
    TEST byte [RDX + RAX * 4 + 61], 4
    JNE L100001cf0_97
    JMP L100001cf0_96     ; inserted

; Entry 100001cf0; block 98; address 1000020ff
L100001cf0_98:
    CMP AL, 46
    JNE L100001cf0_100
    JMP L100001cf0_99     ; inserted

; Entry 100001cf0; block 99; address 100002103
L100001cf0_99:
    TEST CL, CL
    JE L100001cf0_74
    JMP L100001cf0_101     ; inserted

; Entry 100001cf0; block 100; address 100002118
L100001cf0_100:
    TEST CL, CL
    MOV EDX, 8
    CMOVE EDX, R14D
    MOV R12, RBX
    JMP L100001cf0_102     ; inserted

; Entry 100001cf0; block 101; address 100002107
L100001cf0_101:
    LEA R12, [RBX + 1]
    MOV R14D, 10
    MOV EDX, 10
    JMP L100001cf0_102

; Entry 100001cf0; block 102; address 100002126
L100001cf0_102:
    LEA R15, [RBP + -64]
    MOV RSI, R15
    CALL _strtoll
    JMP L100001cf0_103     ; inserted

; Entry 100001cf0; block 103; address 100002132
L100001cf0_103:
    LEA RDX, [rel L__DATA___common + 24]
    MOV qword [RDX], RAX
    CMP qword [R15], RBX
    JNE L100001cf0_105
    JMP L100001cf0_104     ; inserted

; Entry 100001cf0; block 104; address 100002141
L100001cf0_104:
    MOV CL, byte [R12]
    CMP CL, 98
    JE L100001cf0_107
    JMP L100001cf0_106     ; inserted

; Entry 100001cf0; block 105; address 10000216a
L100001cf0_105:
    MOV qword [RDX], 0
    JMP L100001cf0_74     ; inserted

; Entry 100001cf0; block 106; address 10000214a
L100001cf0_106:
    CMP CL, 66
    JNE L100001cf0_109
    JMP L100001cf0_108     ; inserted

; Entry 100001cf0; block 107; address 100002158
L100001cf0_107:
    SHL RAX, 9
    MOV qword [RDX], RAX
    INC R12
    JMP L100001cf0_110     ; inserted

; Entry 100001cf0; block 108; address 10000214f
L100001cf0_108:
    SHL RAX, 10
    MOV qword [RDX], RAX
    JMP L100001cf0_110

; Entry 100001cf0; block 109; address 100002166
L100001cf0_109:
    TEST CL, CL
    JE L100001cf0_111
    JMP L100001cf0_105     ; inserted

; Entry 100001cf0; block 110; address 100002162
L100001cf0_110:
    MOV CL, byte [R12]
    JMP L100001cf0_109     ; inserted

; Entry 100001cf0; block 111; address 100002180
L100001cf0_111:
    CMP R14D, 16
    JE L100001cf0_113
    JMP L100001cf0_112     ; inserted

; Entry 100001cf0; block 112; address 100002186
L100001cf0_112:
    CMP R14D, 10
    JNE L100001cf0_115
    JMP L100001cf0_114     ; inserted

; Entry 100001cf0; block 113; address 100002190
L100001cf0_113:
    MOV AL, 120
    JMP L100001cf0_116     ; inserted

; Entry 100001cf0; block 114; address 10000218c
L100001cf0_114:
    MOV AL, 100
    JMP L100001cf0_116

; Entry 100001cf0; block 115; address 1000021b8
L100001cf0_115:
    MOV RAX, qword [R13]
    MOV qword [RAX + 8], 0
    JMP L100001cf0_74

; Entry 100001cf0; block 116; address 100002192
L100001cf0_116:
    LEA RDX, [rel L__DATA___common + 8]
    MOV RCX, qword [RDX]
    MOV RCX, qword [RCX + 8]
    MOV RCX, qword [RCX + 32]
    MOV byte [RCX + 7], AL
    MOV RCX, qword [RDX]
    MOV RCX, qword [RCX]
    MOV RCX, qword [RCX + 8]
    MOV RCX, qword [RCX + 32]
    MOV byte [RCX + 7], AL
    JMP L100001cf0_115     ; inserted

; Entry 100001cf0; block 117; address 1000021e8
L100001cf0_117:
    XOR EAX, EAX
    CALL _errx



; ---------------------
; Function: 0x1000022d0
; ---------------------
; Entry 1000022d0; block 0; address 1000022d0
L1000022d0_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 24
    MOV R15B, byte [RDI]
    JMP L1000022d0_40     ; inserted

; Entry 1000022d0; block 1; address 1000022ed
L1000022d0_1:
    MOVSX R13D, R15B
    LEA R12, [RDI + 1]
    LEA EAX, [R13 + -97]
    CMP EAX, 5
    JA L1000022d0_4
    JMP L1000022d0_3     ; inserted

; Entry 1000022d0; block 2; address 1000025be
L1000022d0_2:
    ADD RSP, 24
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 1000022d0; block 3; address 1000022fe
L1000022d0_3:
    LEA RCX, [rel L__TEXT___text + 6844]
    MOVSXD RAX, dword [RCX + RAX * 4]
    ADD RAX, RCX
    MOV RAX, QWORD PTR [L_JUMP_TABLE_10000230c + 8*EAX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 10000230e,1000025d2,100002376,100002332

; Entry 1000022d0; block 4; address 100002317
L1000022d0_4:
    LEA EAX, [R13 + -111]
    CMP EAX, 9
    JA L1000022d0_8
    JMP L1000022d0_9     ; inserted

; Entry 1000022d0; block 5; address 10000230e
L1000022d0_5:
    LEA RDI, [rel L__TEXT___cstring_1062]
    JMP L1000022d0_20

; Entry 1000022d0; block 6; address 100002332
L1000022d0_6:
    MOVSX EAX, byte [R12]
    CMP EAX, 75
    JG L1000022d0_11
    JMP L1000022d0_10     ; inserted

; Entry 1000022d0; block 7; address 100002376
L1000022d0_7:
    LEA RDI, [rel L__TEXT___cstring_1080]
    JMP L1000022d0_20     ; inserted

; Entry 1000022d0; block 8; address 1000025d2
L1000022d0_8:
    LEA RSI, [rel L__TEXT___cstring_1098]
    MOV EDI, 1
    MOV EDX, R13D
    XOR EAX, EAX
    CALL _errx

; Entry 1000022d0; block 9; address 100002324
L1000022d0_9:
    MOV ECX, 577
    BT ECX, EAX
    JAE L1000022d0_8
    JMP L1000022d0_6     ; inserted

; Entry 1000022d0; block 10; address 10000233c
L1000022d0_10:
    CMP EAX, 67
    JE L1000022d0_13
    JMP L1000022d0_12     ; inserted

; Entry 1000022d0; block 11; address 100002359
L1000022d0_11:
    CMP EAX, 76
    JE L1000022d0_17
    JMP L1000022d0_16     ; inserted

; Entry 1000022d0; block 12; address 100002345
L1000022d0_12:
    CMP EAX, 73
    JNE L1000022d0_15
    JMP L1000022d0_14     ; inserted

; Entry 1000022d0; block 13; address 10000242a
L1000022d0_13:
    ADD RDI, 2
    MOV R14D, 1
    JMP L1000022d0_19

; Entry 1000022d0; block 14; address 10000234a
L1000022d0_14:
    ADD RDI, 2
    MOV R14D, 4
    JMP L1000022d0_19

; Entry 1000022d0; block 15; address 1000023be
L1000022d0_15:
    MOVZX EAX, AL
    MOV R14D, 4
    MOV RCX, qword [rel __DefaultRuneLocale]
    TEST byte [RCX + RAX * 4 + 61], 4
    JE L1000022d0_23
    JMP L1000022d0_22     ; inserted

; Entry 1000022d0; block 16; address 100002362
L1000022d0_16:
    CMP EAX, 83
    JNE L1000022d0_15
    JMP L1000022d0_18     ; inserted

; Entry 1000022d0; block 17; address 100002436
L1000022d0_17:
    ADD RDI, 2
    MOV R14D, 8
    JMP L1000022d0_19     ; inserted

; Entry 1000022d0; block 18; address 100002367
L1000022d0_18:
    ADD RDI, 2
    MOV R14D, 2
    JMP L1000022d0_19

; Entry 1000022d0; block 19; address 100002440
L1000022d0_19:
    MOV R12, RDI
    JMP L1000022d0_23     ; inserted

; Entry 1000022d0; block 20; address 10000237d
L1000022d0_20:
    CALL L100002689_0
    JMP L1000022d0_21     ; inserted

; Entry 1000022d0; block 21; address 100002382
L1000022d0_21:
    JMP L1000022d0_39

; Entry 1000022d0; block 22; address 1000023d5
L1000022d0_22:
    CALL ___error
    JMP L1000022d0_24     ; inserted

; Entry 1000022d0; block 23; address 100002443
L1000022d0_23:
    LEA ECX, [R14 * 8]
    MOV RAX, 18446744073709551615
    SHL RAX, CL
    NOT RAX
    XOR ECX, ECX
    CMP R15B, 120
    SETE CL
    ADD RCX, 3
    XOR ESI, ESI
    JMP L1000022d0_33     ; inserted

; Entry 1000022d0; block 24; address 1000023da
L1000022d0_24:
    MOV dword [RAX], 0
    MOV RDI, R12
    LEA RSI, [RBP + -56]
    MOV EDX, 10
    CALL _strtoul
    JMP L1000022d0_25     ; inserted

; Entry 1000022d0; block 25; address 1000023f1
L1000022d0_25:
    MOV R14, RAX
    CALL ___error
    JMP L1000022d0_26     ; inserted

; Entry 1000022d0; block 26; address 1000023f9
L1000022d0_26:
    TEST R14, R14
    JE L1000022d0_28
    JMP L1000022d0_27     ; inserted

; Entry 1000022d0; block 27; address 100002402
L1000022d0_27:
    CMP dword [RAX], 0
    JNE L1000022d0_28
    JMP L1000022d0_29     ; inserted

; Entry 1000022d0; block 28; address 1000025ed
L1000022d0_28:
    LEA RSI, [rel L__TEXT___cstring_1144]
    MOV EDI, 1
    MOV RDX, R12
    JMP L1000022d0_41     ; inserted

; Entry 1000022d0; block 29; address 10000240b
L1000022d0_29:
    CMP R14, 8
    JA L1000022d0_31
    JMP L1000022d0_30     ; inserted

; Entry 1000022d0; block 30; address 100002415
L1000022d0_30:
    MOV EAX, 278
    BT RAX, R14
    JAE L1000022d0_31
    JMP L1000022d0_32     ; inserted

; Entry 1000022d0; block 31; address 100002603
L1000022d0_31:
    LEA RSI, [rel L__TEXT___cstring_1161]
    MOV EDI, 1
    MOV RDX, R14
    JMP L1000022d0_41

; Entry 1000022d0; block 32; address 100002424
L1000022d0_32:
    MOV R12, qword [RBP + -56]
    JMP L1000022d0_23

; Entry 1000022d0; block 33; address 100002467
L1000022d0_33:
    INC ESI
    SHR RAX, CL
    TEST RAX, RAX
    JNE L1000022d0_33
    JMP L1000022d0_34     ; inserted

; Entry 1000022d0; block 34; address 100002471
L1000022d0_34:
    CMP R15B, 117
    LEA RBX, [rel L__TEXT___cstring_1213]
    LEA R9, [rel L__TEXT___cstring_90]
    CMOVE RBX, R9
    XOR ECX, ECX
    CMP R15B, 100
    CMOVE RBX, R9
    SETE CL
    LEA R10D, [RCX + RSI]
    MOV EAX, 16
    XOR EDX, EDX
    DIV R14
    LEA R8D, [R14 * 4]
    SUB R8D, ECX
    SUB R8D, ESI
    SUB RSP, 8
    LEA RDI, [RBP + -48]
    LEA RSI, [rel L__TEXT___cstring_1186]
    MOV RDX, RAX
    MOV RCX, R14
    XOR EAX, EAX
    PUSH R13
    PUSH R10
    PUSH RBX
    CALL _asprintf
    JMP L1000022d0_35     ; inserted

; Entry 1000022d0; block 35; address 1000024d1
L1000022d0_35:
    ADD RSP, 32
    MOV RDI, qword [RBP + -48]
    TEST RDI, RDI
    JNE L1000022d0_37
    JMP L1000022d0_36     ; inserted

; Entry 1000022d0; block 36; address 1000024e2
L1000022d0_36:
    CALL L1000033e0_0

; Entry 1000022d0; block 37; address 10000259c
L1000022d0_37:
    CALL L100002689_0
    JMP L1000022d0_38     ; inserted

; Entry 1000022d0; block 38; address 1000025a1
L1000022d0_38:
    MOV RDI, qword [RBP + -48]
    CALL _free
    JMP L1000022d0_39     ; inserted

; Entry 1000022d0; block 39; address 1000025aa
L1000022d0_39:
    MOV R15B, byte [R12]
    MOV RDI, R12
    JMP L1000022d0_40

; Entry 1000022d0; block 40; address 1000022e4
L1000022d0_40:
    TEST R15B, R15B
    JE L1000022d0_2
    JMP L1000022d0_1     ; inserted

; Entry 1000022d0; block 41; address 1000025fc
L1000022d0_41:
    XOR EAX, EAX
    CALL _errx



; ---------------------
; Function: 0x100002640
; ---------------------
; Entry 100002640; block 0; address 100002640
L100002640_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH RBX
    PUSH RAX
    MOV RBX, qword [rel ___stderrp]
    MOV RCX, qword [RBX]
    LEA RDI, [rel L__TEXT___cstring_943]
    MOV ESI, 77
    MOV EDX, 1
    CALL _fwrite
    JMP L100002640_1     ; inserted

; Entry 100002640; block 1; address 100002666
L100002640_1:
    MOV RCX, qword [RBX]
    LEA RDI, [rel L__TEXT___cstring_1021]
    MOV ESI, 40
    MOV EDX, 1
    CALL _fwrite
    JMP L100002640_2     ; inserted

; Entry 100002640; block 2; address 10000267f
L100002640_2:
    MOV EDI, 1
    CALL _exit



; ---------------------
; Function: 0x100002689
; ---------------------
; Entry 100002689; block 0; address 100002689
L100002689_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH RBX
    PUSH RAX
    MOV RBX, RDI
    CMP byte [rel L__DATA___bss + 52], 1
    JNE L100002689_2
    JMP L100002689_1     ; inserted

; Entry 100002689; block 1; address 10000269b
L100002689_1:
    LEA RDI, [rel L__TEXT___cstring_1132]
    CALL L1000027ed_0
    JMP L100002689_2     ; inserted

; Entry 100002689; block 2; address 1000026a7
L100002689_2:
    MOV RDI, RBX
    CALL L1000027ed_0
    JMP L100002689_3     ; inserted

; Entry 100002689; block 3; address 1000026af
L100002689_3:
    MOV byte [rel L__DATA___bss + 52], 1
    ADD RSP, 8
    POP RBX
    POP RBP
    RET 



; ---------------------
; Function: 0x1000026c0
; ---------------------
; Entry 1000026c0; block 0; address 1000026c0
L1000026c0_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 2072
    MOV RBX, RDI
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -48], RAX
    LEA RSI, [rel L__TEXT___cstring_261]
    CALL _fopen
    JMP L1000026c0_1     ; inserted

; Entry 1000026c0; block 1; address 1000026f1
L1000026c0_1:
    TEST RAX, RAX
    JE L1000026c0_3
    JMP L1000026c0_2     ; inserted

; Entry 1000026c0; block 2; address 1000026fa
L1000026c0_2:
    MOV R14, RAX
    LEA R15, [RBP + -2112]
    MOV RDI, R15
    MOV ESI, 2049
    MOV RDX, RAX
    CALL _fgets
    JMP L1000026c0_4     ; inserted

; Entry 1000026c0; block 3; address 1000027d2
L1000026c0_3:
    LEA RSI, [rel L__TEXT___cstring_222]
    MOV EDI, 1
    MOV RDX, RBX
    XOR EAX, EAX
    CALL _err

; Entry 1000026c0; block 4; address 100002714
L1000026c0_4:
    TEST RAX, RAX
    JE L1000026c0_6
    JMP L1000026c0_5     ; inserted

; Entry 1000026c0; block 5; address 10000271d
L1000026c0_5:
    MOV R12, qword [rel __DefaultRuneLocale]
    MOV R13D, 16384
    JMP L1000026c0_23     ; inserted

; Entry 1000026c0; block 6; address 1000027a8
L1000026c0_6:
    MOV RDI, R14
    CALL _fclose
    JMP L1000026c0_24     ; inserted

; Entry 1000026c0; block 7; address 100002737
L1000026c0_7:
    TEST RAX, RAX
    JE L1000026c0_9
    JMP L1000026c0_8     ; inserted

; Entry 1000026c0; block 8; address 10000273c
L1000026c0_8:
    MOV byte [RAX], 0
    MOV AL, byte [RBP + -2112]
    TEST AL, AL
    JE L1000026c0_11
    JMP L1000026c0_10     ; inserted

; Entry 1000026c0; block 9; address 10000278e
L1000026c0_9:
    CALL L100003403_0
    JMP L1000026c0_11     ; inserted

; Entry 1000026c0; block 10; address 100002749
L1000026c0_10:
    MOV RBX, R15
    JMP L1000026c0_17     ; inserted

; Entry 1000026c0; block 11; address 100002793
L1000026c0_11:
    MOV RDI, R15
    MOV ESI, 2049
    MOV RDX, R14
    CALL _fgets
    JMP L1000026c0_22     ; inserted

; Entry 1000026c0; block 12; address 100002750
L1000026c0_12:
    MOVZX EAX, AL
    MOV EAX, dword [R12 + RAX * 4 + 60]
    AND EAX, R13D
    JMP L1000026c0_14

; Entry 1000026c0; block 13; address 10000275d
L1000026c0_13:
    MOVZX EDI, AL
    MOV ESI, 16384
    CALL ___maskrune
    JMP L1000026c0_14     ; inserted

; Entry 1000026c0; block 14; address 10000276a
L1000026c0_14:
    TEST EAX, EAX
    JE L1000026c0_16
    JMP L1000026c0_15     ; inserted

; Entry 1000026c0; block 15; address 10000276e
L1000026c0_15:
    MOV AL, byte [RBX + 1]
    INC RBX
    TEST AL, AL
    JNE L1000026c0_17
    JMP L1000026c0_18     ; inserted

; Entry 1000026c0; block 16; address 10000277a
L1000026c0_16:
    MOV AL, byte [RBX]
    CMP AL, 35
    JE L1000026c0_11
    JMP L1000026c0_19     ; inserted

; Entry 1000026c0; block 17; address 10000274c
L1000026c0_17:
    TEST AL, AL
    JS L1000026c0_13
    JMP L1000026c0_12     ; inserted

; Entry 1000026c0; block 18; address 100002778
L1000026c0_18:
    JMP L1000026c0_11

; Entry 1000026c0; block 19; address 100002780
L1000026c0_19:
    TEST AL, AL
    JE L1000026c0_11
    JMP L1000026c0_20     ; inserted

; Entry 1000026c0; block 20; address 100002784
L1000026c0_20:
    MOV RDI, RBX
    CALL L1000027ed_0
    JMP L1000026c0_21     ; inserted

; Entry 1000026c0; block 21; address 10000278c
L1000026c0_21:
    JMP L1000026c0_11

; Entry 1000026c0; block 22; address 1000027a3
L1000026c0_22:
    TEST RAX, RAX
    JNE L1000026c0_23
    JMP L1000026c0_6     ; inserted

; Entry 1000026c0; block 23; address 10000272a
L1000026c0_23:
    MOV RDI, R15
    MOV ESI, 10
    CALL _index
    JMP L1000026c0_7     ; inserted

; Entry 1000026c0; block 24; address 1000027b0
L1000026c0_24:
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    CMP RAX, qword [RBP + -48]
    JNE L1000026c0_26
    JMP L1000026c0_25     ; inserted

; Entry 1000026c0; block 25; address 1000027c0
L1000026c0_25:
    ADD RSP, 2072
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 1000026c0; block 26; address 1000027e8
L1000026c0_26:
    CALL ___stack_chk_fail



; ---------------------
; Function: 0x1000027ed
; ---------------------
; Entry 1000027ed; block 0; address 1000027ed
L1000027ed_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 24
    MOV R13, RDI
    MOV EDI, 1
    MOV ESI, 24
    CALL _calloc
    JMP L1000027ed_1     ; inserted

; Entry 1000027ed; block 1; address 100002810
L1000027ed_1:
    TEST RAX, RAX
    JE L1000027ed_3
    JMP L1000027ed_2     ; inserted

; Entry 1000027ed; block 2; address 100002819
L1000027ed_2:
    MOV R12, RAX
    LEA RAX, [rel L__DATA___common + 8]
    CMP qword [RAX], 0
    JE L1000027ed_5
    JMP L1000027ed_4     ; inserted

; Entry 1000027ed; block 3; address 100002a7d
L1000027ed_3:
    CALL L100003426_0

; Entry 1000027ed; block 4; address 100002829
L1000027ed_4:
    MOV RAX, qword [rel L__DATA___bss + 56]
    JMP L1000027ed_5     ; inserted

; Entry 1000027ed; block 5; address 100002830
L1000027ed_5:
    MOV qword [RAX], R12
    MOV qword [rel L__DATA___bss + 56], R12
    ADD R12, 8
    MOV R14, qword [rel __DefaultRuneLocale]
    MOV R15D, 16384
    MOV qword [RBP + -48], R13
    JMP L1000027ed_66     ; inserted

; Entry 1000027ed; block 6; address 10000285b
L1000027ed_6:
    MOV EAX, dword [R14 + RBX * 4 + 60]
    AND EAX, R15D
    JMP L1000027ed_8

; Entry 1000027ed; block 7; address 100002865
L1000027ed_7:
    MOV ESI, 16384
    MOV EDI, EBX
    CALL ___maskrune
    JMP L1000027ed_8     ; inserted

; Entry 1000027ed; block 8; address 100002871
L1000027ed_8:
    INC R13
    TEST EAX, EAX
    JNE L1000027ed_9
    JMP L1000027ed_10     ; inserted

; Entry 1000027ed; block 9; address 100002852
L1000027ed_9:
    MOVZX EBX, byte [R13 + 1]
    TEST BL, BL
    JS L1000027ed_7
    JMP L1000027ed_6     ; inserted

; Entry 1000027ed; block 10; address 100002878
L1000027ed_10:
    TEST BL, BL
    JE L1000027ed_12
    JMP L1000027ed_11     ; inserted

; Entry 1000027ed; block 11; address 100002880
L1000027ed_11:
    MOV EDI, 1
    MOV ESI, 40
    CALL _calloc
    JMP L1000027ed_13     ; inserted

; Entry 1000027ed; block 12; address 100002a5b
L1000027ed_12:
    ADD RSP, 24
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 1000027ed; block 13; address 10000288f
L1000027ed_13:
    TEST RAX, RAX
    JE L1000027ed_15
    JMP L1000027ed_14     ; inserted

; Entry 1000027ed; block 14; address 100002898
L1000027ed_14:
    MOV qword [R12], RAX
    MOV qword [RBP + -56], RAX
    MOV dword [RAX + 20], 1
    MOVZX EBX, byte [R13]
    MOV EAX, dword [R14 + RBX * 4 + 60]
    BT EAX, 10
    JB L1000027ed_17
    JMP L1000027ed_16     ; inserted

; Entry 1000027ed; block 15; address 100002a73
L1000027ed_15:
    CALL L100003436_0

; Entry 1000027ed; block 16; address 1000028b7
L1000027ed_16:
    MOV R12, R13
    JMP L1000027ed_33

; Entry 1000027ed; block 17; address 1000028bf
L1000027ed_17:
    MOV R12, R13
    JAE L1000027ed_19
    JMP L1000027ed_18     ; inserted

; Entry 1000027ed; block 18; address 1000028c4
L1000027ed_18:
    MOV R12, R13
    JMP L1000027ed_20     ; inserted

; Entry 1000027ed; block 19; address 1000028db
L1000027ed_19:
    TEST BL, BL
    JS L1000027ed_22
    JMP L1000027ed_21     ; inserted

; Entry 1000027ed; block 20; address 1000028c7
L1000027ed_20:
    MOVZX EBX, byte [R12 + 1]
    INC R12
    MOV EAX, dword [R14 + RBX * 4 + 60]
    BT EAX, 10
    JB L1000027ed_20
    JMP L1000027ed_19     ; inserted

; Entry 1000027ed; block 21; address 1000028df
L1000027ed_21:
    AND EAX, 16384
    JMP L1000027ed_23

; Entry 1000027ed; block 22; address 1000028e6
L1000027ed_22:
    MOVZX EDI, BL
    MOV ESI, 16384
    CALL ___maskrune
    JMP L1000027ed_23     ; inserted

; Entry 1000027ed; block 23; address 1000028f3
L1000027ed_23:
    CMP BL, 47
    JE L1000027ed_25
    JMP L1000027ed_24     ; inserted

; Entry 1000027ed; block 24; address 1000028f8
L1000027ed_24:
    TEST EAX, EAX
    JE L1000027ed_26
    JMP L1000027ed_25     ; inserted

; Entry 1000027ed; block 25; address 100002900
L1000027ed_25:
    MOV RDI, R13
    CALL _atoi
    JMP L1000027ed_27     ; inserted

; Entry 1000027ed; block 26; address 100002a6a
L1000027ed_26:
    MOV RDI, qword [RBP + -48]
    CALL L100002a82_0

; Entry 1000027ed; block 27; address 100002908
L1000027ed_27:
    MOV RCX, qword [RBP + -56]
    MOV dword [RCX + 20], EAX
    MOV dword [RCX + 16], 2
    JMP L1000027ed_31     ; inserted

; Entry 1000027ed; block 28; address 100002921
L1000027ed_28:
    MOV EAX, dword [R14 + RDI * 4 + 60]
    AND EAX, R15D
    JMP L1000027ed_30

; Entry 1000027ed; block 29; address 10000292b
L1000027ed_29:
    MOV ESI, 16384
    CALL ___maskrune
    JMP L1000027ed_30     ; inserted

; Entry 1000027ed; block 30; address 100002935
L1000027ed_30:
    INC R12
    TEST EAX, EAX
    JNE L1000027ed_31
    JMP L1000027ed_32     ; inserted

; Entry 1000027ed; block 31; address 100002916
L1000027ed_31:
    MOVZX EDI, byte [R12 + 1]
    TEST DIL, DIL
    JS L1000027ed_29
    JMP L1000027ed_28     ; inserted

; Entry 1000027ed; block 32; address 10000293c
L1000027ed_32:
    MOV BL, byte [R12]
    JMP L1000027ed_33     ; inserted

; Entry 1000027ed; block 33; address 100002940
L1000027ed_33:
    CMP BL, 47
    JNE L1000027ed_35
    JMP L1000027ed_34     ; inserted

; Entry 1000027ed; block 34; address 100002945
L1000027ed_34:
    MOVZX EDI, byte [R12 + 1]
    TEST DIL, DIL
    JS L1000027ed_37
    JMP L1000027ed_36     ; inserted

; Entry 1000027ed; block 35; address 10000296f
L1000027ed_35:
    MOVZX EAX, BL
    MOV EAX, dword [R14 + RAX * 4 + 60]
    BT EAX, 10
    JB L1000027ed_41
    JMP L1000027ed_40     ; inserted

; Entry 1000027ed; block 36; address 100002950
L1000027ed_36:
    MOV EAX, dword [R14 + RDI * 4 + 60]
    AND EAX, R15D
    JMP L1000027ed_38

; Entry 1000027ed; block 37; address 10000295a
L1000027ed_37:
    MOV ESI, 16384
    CALL ___maskrune
    JMP L1000027ed_38     ; inserted

; Entry 1000027ed; block 38; address 100002964
L1000027ed_38:
    INC R12
    TEST EAX, EAX
    JNE L1000027ed_34
    JMP L1000027ed_39     ; inserted

; Entry 1000027ed; block 39; address 10000296b
L1000027ed_39:
    MOV BL, byte [R12]
    JMP L1000027ed_35     ; inserted

; Entry 1000027ed; block 40; address 10000297d
L1000027ed_40:
    MOV R13, R12
    JMP L1000027ed_55

; Entry 1000027ed; block 41; address 100002982
L1000027ed_41:
    MOV R13, R12
    JAE L1000027ed_43
    JMP L1000027ed_42     ; inserted

; Entry 1000027ed; block 42; address 100002987
L1000027ed_42:
    MOV R13, R12
    JMP L1000027ed_44     ; inserted

; Entry 1000027ed; block 43; address 10000299d
L1000027ed_43:
    TEST BL, BL
    JS L1000027ed_46
    JMP L1000027ed_45     ; inserted

; Entry 1000027ed; block 44; address 10000298a
L1000027ed_44:
    MOVZX EBX, byte [R13 + 1]
    INC R13
    MOV EAX, dword [R14 + RBX * 4 + 60]
    BT EAX, 10
    JB L1000027ed_44
    JMP L1000027ed_43     ; inserted

; Entry 1000027ed; block 45; address 1000029a1
L1000027ed_45:
    AND EAX, 16384
    JMP L1000027ed_47

; Entry 1000027ed; block 46; address 1000029a8
L1000027ed_46:
    MOVZX EDI, BL
    MOV ESI, 16384
    CALL ___maskrune
    JMP L1000027ed_47     ; inserted

; Entry 1000027ed; block 47; address 1000029b5
L1000027ed_47:
    TEST EAX, EAX
    JE L1000027ed_26
    JMP L1000027ed_48     ; inserted

; Entry 1000027ed; block 48; address 1000029bd
L1000027ed_48:
    MOV RDI, R12
    CALL _atoi
    JMP L1000027ed_49     ; inserted

; Entry 1000027ed; block 49; address 1000029c5
L1000027ed_49:
    MOV RCX, qword [RBP + -56]
    MOV dword [RCX + 24], EAX
    JMP L1000027ed_53     ; inserted

; Entry 1000027ed; block 50; address 1000029d6
L1000027ed_50:
    MOV EAX, dword [R14 + RDI * 4 + 60]
    AND EAX, R15D
    JMP L1000027ed_52

; Entry 1000027ed; block 51; address 1000029e0
L1000027ed_51:
    MOV ESI, 16384
    CALL ___maskrune
    JMP L1000027ed_52     ; inserted

; Entry 1000027ed; block 52; address 1000029ea
L1000027ed_52:
    INC R13
    TEST EAX, EAX
    JNE L1000027ed_53
    JMP L1000027ed_54     ; inserted

; Entry 1000027ed; block 53; address 1000029cc
L1000027ed_53:
    MOVZX EDI, byte [R13 + 1]
    TEST DIL, DIL
    JS L1000027ed_51
    JMP L1000027ed_50     ; inserted

; Entry 1000027ed; block 54; address 1000029f1
L1000027ed_54:
    MOV BL, byte [R13]
    JMP L1000027ed_55     ; inserted

; Entry 1000027ed; block 55; address 1000029f5
L1000027ed_55:
    CMP BL, 34
    JNE L1000027ed_26
    JMP L1000027ed_56     ; inserted

; Entry 1000027ed; block 56; address 1000029fa
L1000027ed_56:
    LEA R15, [R13 + 1]
    ADD R13, 2
    XOR EBX, EBX
    JMP L1000027ed_59     ; inserted

; Entry 1000027ed; block 57; address 100002a0c
L1000027ed_57:
    INC R13
    INC RBX
    TEST AL, AL
    JNE L1000027ed_59
    JMP L1000027ed_60     ; inserted

; Entry 1000027ed; block 58; address 100002a18
L1000027ed_58:
    LEA RDI, [RBX + 1]
    CALL _malloc
    JMP L1000027ed_61     ; inserted

; Entry 1000027ed; block 59; address 100002a04
L1000027ed_59:
    MOV AL, byte [R13 + -1]
    CMP AL, 34
    JE L1000027ed_58
    JMP L1000027ed_57     ; inserted

; Entry 1000027ed; block 60; address 100002a16
L1000027ed_60:
    JMP L1000027ed_26

; Entry 1000027ed; block 61; address 100002a21
L1000027ed_61:
    MOV R12, qword [RBP + -56]
    MOV qword [R12 + 32], RAX
    TEST RAX, RAX
    JE L1000027ed_63
    JMP L1000027ed_62     ; inserted

; Entry 1000027ed; block 62; address 100002a2f
L1000027ed_62:
    MOV RDI, RAX
    MOV RSI, R15
    MOV RDX, RBX
    CALL _strncpy
    JMP L1000027ed_64     ; inserted

; Entry 1000027ed; block 63; address 100002a78
L1000027ed_63:
    CALL L100003446_0

; Entry 1000027ed; block 64; address 100002a3d
L1000027ed_64:
    MOV RAX, qword [R12 + 32]
    MOV byte [RAX + RBX], 0
    MOV RDI, qword [R12 + 32]
    CALL L100002a9c_0
    JMP L1000027ed_65     ; inserted

; Entry 1000027ed; block 65; address 100002a50
L1000027ed_65:
    MOV R15D, 16384
    JMP L1000027ed_66

; Entry 1000027ed; block 66; address 10000284f
L1000027ed_66:
    DEC R13
    JMP L1000027ed_9     ; inserted



; ---------------------
; Function: 0x100002a82
; ---------------------
; Entry 100002a82; block 0; address 100002a82
L100002a82_0:
    PUSH RBP
    MOV RBP, RSP
    MOV RDX, RDI
    LEA RSI, [rel L__TEXT___cstring_1401]
    MOV EDI, 1
    XOR EAX, EAX
    CALL _errx



; ---------------------
; Function: 0x100002a9c
; ---------------------
; Entry 100002a9c; block 0; address 100002a9c
L100002a9c_0:
    PUSH RBP
    MOV RBP, RSP
    LEA RAX, [rel L__TEXT___text + 8100]
    MOV RCX, RDI
    JMP L100002a9c_17     ; inserted

; Entry 100002a9c; block 1; address 100002ab1
L100002a9c_1:
    TEST DL, DL
    JNE L100002a9c_4
    JMP L100002a9c_3     ; inserted

; Entry 100002a9c; block 2; address 100002ab7
L100002a9c_2:
    MOV DL, byte [RDI + 1]
    MOVSX ESI, DL
    CMP ESI, 109
    JLE L100002a9c_6
    JMP L100002a9c_5     ; inserted

; Entry 100002a9c; block 3; address 100002ab5
L100002a9c_3:
    MOV byte [RCX], 0
    POP RBP
    RET 

; Entry 100002a9c; block 4; address 100002b01
L100002a9c_4:
    INC RDI
    INC RCX
    JMP L100002a9c_17

; Entry 100002a9c; block 5; address 100002ac2
L100002a9c_5:
    ADD ESI, 18446744073709551506
    CMP ESI, 8
    JA L100002a9c_8
    JMP L100002a9c_7     ; inserted

; Entry 100002a9c; block 6; address 100002ad7
L100002a9c_6:
    CMP ESI, 97
    JE L100002a9c_13
    JMP L100002a9c_12     ; inserted

; Entry 100002a9c; block 7; address 100002aca
L100002a9c_7:
    MOVSXD RSI, dword [RAX + RSI * 4]
    ADD RSI, RAX
    MOV RSI, QWORD PTR [L_JUMP_TABLE_100002ad1 + 8*ESI] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RSI; TARGETS: 100002ad3,100002afc,100002aea,100002aee

; Entry 100002a9c; block 8; address 100002afc
L100002a9c_8:
    INC RDI
    MOV byte [RCX], DL
    JMP L100002a9c_4     ; inserted

; Entry 100002a9c; block 9; address 100002ad3
L100002a9c_9:
    MOV DL, 10
    JMP L100002a9c_8

; Entry 100002a9c; block 10; address 100002aea
L100002a9c_10:
    MOV DL, 13
    JMP L100002a9c_8

; Entry 100002a9c; block 11; address 100002aee
L100002a9c_11:
    MOV DL, 9
    JMP L100002a9c_8

; Entry 100002a9c; block 12; address 100002adc
L100002a9c_12:
    CMP ESI, 98
    JE L100002a9c_15
    JMP L100002a9c_14     ; inserted

; Entry 100002a9c; block 13; address 100002af6
L100002a9c_13:
    MOV DL, 7
    JMP L100002a9c_8

; Entry 100002a9c; block 14; address 100002ae1
L100002a9c_14:
    CMP ESI, 102
    JNE L100002a9c_8
    JMP L100002a9c_16     ; inserted

; Entry 100002a9c; block 15; address 100002afa
L100002a9c_15:
    MOV DL, 8
    JMP L100002a9c_8     ; inserted

; Entry 100002a9c; block 16; address 100002ae6
L100002a9c_16:
    MOV DL, 12
    JMP L100002a9c_8

; Entry 100002a9c; block 17; address 100002aaa
L100002a9c_17:
    MOV DL, byte [RDI]
    CMP DL, 92
    JE L100002a9c_2
    JMP L100002a9c_1     ; inserted



; ---------------------
; Function: 0x100002b34
; ---------------------
; Entry 100002b34; block 0; address 100002b34
L100002b34_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    PUSH RAX
    MOV R13, qword [RDI + 8]
    XOR EAX, EAX
    TEST R13, R13
    JE L100002b34_2
    JMP L100002b34_1     ; inserted

; Entry 100002b34; block 1; address 100002b51
L100002b34_1:
    LEA R15, [rel L__TEXT___cstring_1450]
    MOV R12, qword [rel __DefaultRuneLocale]
    JMP L100002b34_32     ; inserted

; Entry 100002b34; block 2; address 100002c51
L100002b34_2:
    ADD RSP, 8
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100002b34; block 3; address 100002b6c
L100002b34_3:
    MOV dword [RBP + -48], EAX
    MOV RBX, qword [R13 + 32]
    XOR R14D, R14D
    MOV dword [RBP + -44], 0
    JMP L100002b34_31     ; inserted

; Entry 100002b34; block 4; address 100002c3c
L100002b34_4:
    IMUL R14D, dword [R13 + 20]
    ADD EAX, R14D
    MOV R13, qword [R13]
    TEST R13, R13
    JNE L100002b34_32
    JMP L100002b34_2     ; inserted

; Entry 100002b34; block 5; address 100002b83
L100002b34_5:
    TEST AL, AL
    JNE L100002b34_8
    JMP L100002b34_7     ; inserted

; Entry 100002b34; block 6; address 100002b90
L100002b34_6:
    INC RBX
    JMP L100002b34_10     ; inserted

; Entry 100002b34; block 7; address 100002b8b
L100002b34_7:
    MOV EAX, dword [RBP + -48]
    JMP L100002b34_4     ; inserted

; Entry 100002b34; block 8; address 100002c31
L100002b34_8:
    INC RBX
    JMP L100002b34_31

; Entry 100002b34; block 9; address 100002b9e
L100002b34_9:
    INC RBX
    TEST RAX, RAX
    JNE L100002b34_10
    JMP L100002b34_11     ; inserted

; Entry 100002b34; block 10; address 100002b93
L100002b34_10:
    MOVZX ESI, byte [RBX]
    MOV RDI, R15
    CALL _index
    JMP L100002b34_9     ; inserted

; Entry 100002b34; block 11; address 100002ba6
L100002b34_11:
    MOV AL, byte [RBX + -1]
    CMP AL, 46
    JNE L100002b34_13
    JMP L100002b34_12     ; inserted

; Entry 100002b34; block 12; address 100002bad
L100002b34_12:
    MOVZX EAX, byte [RBX]
    TEST byte [R12 + RAX * 4 + 61], 4
    JE L100002b34_15
    JMP L100002b34_14     ; inserted

; Entry 100002b34; block 13; address 100002bd4
L100002b34_13:
    DEC RBX
    JMP L100002b34_15     ; inserted

; Entry 100002b34; block 14; address 100002bb8
L100002b34_14:
    MOV RDI, RBX
    CALL _atoi
    JMP L100002b34_16     ; inserted

; Entry 100002b34; block 15; address 100002bd7
L100002b34_15:
    CMP AL, 94
    JLE L100002b34_20
    JMP L100002b34_19     ; inserted

; Entry 100002b34; block 16; address 100002bc0
L100002b34_16:
    MOV dword [RBP + -44], EAX
    JMP L100002b34_17     ; inserted

; Entry 100002b34; block 17; address 100002bc3
L100002b34_17:
    MOVZX EAX, byte [RBX + 1]
    INC RBX
    TEST byte [R12 + RAX * 4 + 61], 4
    JNE L100002b34_17
    JMP L100002b34_18     ; inserted

; Entry 100002b34; block 18; address 100002bd2
L100002b34_18:
    JMP L100002b34_15

; Entry 100002b34; block 19; address 100002bdb
L100002b34_19:
    ADD AL, 161
    CMP AL, 25
    JA L100002b34_8
    JMP L100002b34_21     ; inserted

; Entry 100002b34; block 20; address 100002bfa
L100002b34_20:
    CMP AL, 69
    JE L100002b34_23
    JMP L100002b34_27     ; inserted

; Entry 100002b34; block 21; address 100002be1
L100002b34_21:
    MOVZX EAX, AL
    LEA RCX, [rel L__TEXT___text + 8436]
    MOVSXD RAX, dword [RCX + RAX * 4]
    ADD RAX, RCX
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100002bf2 + 8*AL] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 100002c0e,100002c31,100002c28,100002bf4,100002c08,100002c2d

; Entry 100002b34; block 22; address 100002bf4
L100002b34_22:
    ADD R14D, 4
    JMP L100002b34_8

; Entry 100002b34; block 23; address 100002c08
L100002b34_23:
    ADD R14D, 8
    JMP L100002b34_8

; Entry 100002b34; block 24; address 100002c0e
L100002b34_24:
    MOV AL, byte [RBX + 1]
    INC RBX
    ADD AL, 157
    CMP AL, 18
    JA L100002b34_8
    JMP L100002b34_30     ; inserted

; Entry 100002b34; block 25; address 100002c28
L100002b34_25:
    INC R14D
    JMP L100002b34_8

; Entry 100002b34; block 26; address 100002c2d
L100002b34_26:
    ADD R14D, dword [RBP + -44]
    JMP L100002b34_8     ; inserted

; Entry 100002b34; block 27; address 100002bfe
L100002b34_27:
    CMP AL, 71
    JE L100002b34_23
    JMP L100002b34_28     ; inserted

; Entry 100002b34; block 28; address 100002c02
L100002b34_28:
    CMP AL, 88
    JE L100002b34_22
    JMP L100002b34_29     ; inserted

; Entry 100002b34; block 29; address 100002c06
L100002b34_29:
    JMP L100002b34_8

; Entry 100002b34; block 30; address 100002c1a
L100002b34_30:
    MOVZX EAX, AL
    MOV ECX, 270337
    BT RCX, RAX
    JAE L100002b34_8
    JMP L100002b34_25     ; inserted

; Entry 100002b34; block 31; address 100002b7d
L100002b34_31:
    MOV AL, byte [RBX]
    CMP AL, 37
    JE L100002b34_6
    JMP L100002b34_5     ; inserted

; Entry 100002b34; block 32; address 100002b5f
L100002b34_32:
    MOV R14D, dword [R13 + 24]
    TEST R14D, R14D
    JNE L100002b34_4
    JMP L100002b34_3     ; inserted



; ---------------------
; Function: 0x100002cc8
; ---------------------
; Entry 100002cc8; block 0; address 100002cc8
L100002cc8_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 56
    MOV qword [RBP + -72], RDI
    MOV R13, qword [RDI + 8]
    TEST R13, R13
    JE L100002cc8_2
    JMP L100002cc8_1     ; inserted

; Entry 100002cc8; block 1; address 100002cea
L100002cc8_1:
    XOR EAX, EAX
    MOV qword [RBP + -64], RAX
    MOV RSI, qword [rel __DefaultRuneLocale]
    XOR R12D, R12D
    JMP L100002cc8_89     ; inserted

; Entry 100002cc8; block 2; address 1000031bf
L100002cc8_2:
    ADD RSP, 56
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100002cc8; block 3; address 100002d08
L100002cc8_3:
    LEA RBX, [R13 + 8]
    MOV dword [RBP + -48], 0
    MOV qword [RBP + -88], RBX
    JMP L100002cc8_82     ; inserted

; Entry 100002cc8; block 4; address 1000030e2
L100002cc8_4:
    CMP dword [R13 + 24], 0
    JNE L100002cc8_85
    JMP L100002cc8_84     ; inserted

; Entry 100002cc8; block 5; address 100002d26
L100002cc8_5:
    TEST RAX, RAX
    JE L100002cc8_7
    JMP L100002cc8_6     ; inserted

; Entry 100002cc8; block 6; address 100002d2f
L100002cc8_6:
    MOV R15, RAX
    CMP qword [RBX], 0
    MOV RCX, R12
    MOV RAX, R12
    CMOVE RAX, RBX
    MOV qword [RAX], R15
    LEA R12, [R14 + 1]
    MOV qword [RBP + -56], R14
    JMP L100002cc8_12     ; inserted

; Entry 100002cc8; block 7; address 1000031ce
L100002cc8_7:
    CALL L100003456_0

; Entry 100002cc8; block 8; address 100002d56
L100002cc8_8:
    CMP AL, 37
    JE L100002cc8_11
    JMP L100002cc8_10     ; inserted

; Entry 100002cc8; block 9; address 1000030c8
L100002cc8_9:
    MOV RAX, qword [RBP + -56]
    MOV qword [R15 + 24], RAX
    MOV dword [R15 + 8], 1024
    MOV RSI, qword [rel __DefaultRuneLocale]
    MOV R12, RCX
    JMP L100002cc8_4     ; inserted

; Entry 100002cc8; block 10; address 100002d5a
L100002cc8_10:
    INC R14
    INC R12
    JMP L100002cc8_12

; Entry 100002cc8; block 11; address 100002d62
L100002cc8_11:
    CMP dword [R13 + 24], 0
    LEA RBX, [rel L__TEXT___cstring_1450]
    JE L100002cc8_14
    JMP L100002cc8_13     ; inserted

; Entry 100002cc8; block 12; address 100002d4b
L100002cc8_12:
    MOV AL, byte [R14]
    TEST AL, AL
    JE L100002cc8_9
    JMP L100002cc8_8     ; inserted

; Entry 100002cc8; block 13; address 100002d70
L100002cc8_13:
    MOVZX ESI, byte [R14 + 1]
    INC R14
    LEA RDI, [rel L__TEXT___cstring_1449]
    CALL _index
    JMP L100002cc8_15     ; inserted

; Entry 100002cc8; block 14; address 100002d96
L100002cc8_14:
    MOVZX ESI, byte [R12]
    MOV RDI, RBX
    CALL _index
    JMP L100002cc8_17     ; inserted

; Entry 100002cc8; block 15; address 100002d84
L100002cc8_15:
    TEST RAX, RAX
    JNE L100002cc8_13
    JMP L100002cc8_16     ; inserted

; Entry 100002cc8; block 16; address 100002d89
L100002cc8_16:
    MOV AL, byte [R14]
    MOV ECX, 1
    MOV R12, R14
    JMP L100002cc8_22

; Entry 100002cc8; block 17; address 100002da3
L100002cc8_17:
    INC R12
    TEST RAX, RAX
    JNE L100002cc8_14
    JMP L100002cc8_18     ; inserted

; Entry 100002cc8; block 18; address 100002dab
L100002cc8_18:
    MOV AL, byte [R12 + -1]
    XOR ECX, ECX
    CMP AL, 46
    JNE L100002cc8_20
    JMP L100002cc8_19     ; inserted

; Entry 100002cc8; block 19; address 100002db6
L100002cc8_19:
    MOVZX EAX, byte [R12]
    MOV RDX, qword [rel __DefaultRuneLocale]
    TEST byte [RDX + RAX * 4 + 61], 4
    JE L100002cc8_22
    JMP L100002cc8_21     ; inserted

; Entry 100002cc8; block 20; address 100002df3
L100002cc8_20:
    DEC R12
    JMP L100002cc8_22     ; inserted

; Entry 100002cc8; block 21; address 100002dc9
L100002cc8_21:
    MOV RDI, R12
    CALL _atoi
    JMP L100002cc8_23     ; inserted

; Entry 100002cc8; block 22; address 100002df6
L100002cc8_22:
    LEA R8, [R12 + 1]
    MOV byte [RBP + -43], AL
    MOV byte [RBP + -42], 0
    MOVSX EDX, AL
    CMP EDX, 94
    MOV qword [RBP + -80], R13
    JLE L100002cc8_27
    JMP L100002cc8_26     ; inserted

; Entry 100002cc8; block 23; address 100002dd1
L100002cc8_23:
    MOV qword [RBP + -64], RAX
    MOV RCX, qword [rel __DefaultRuneLocale]
    JMP L100002cc8_24     ; inserted

; Entry 100002cc8; block 24; address 100002ddc
L100002cc8_24:
    MOVZX EAX, byte [R12 + 1]
    INC R12
    TEST byte [RCX + RAX * 4 + 61], 4
    JNE L100002cc8_24
    JMP L100002cc8_25     ; inserted

; Entry 100002cc8; block 25; address 100002dec
L100002cc8_25:
    MOV ECX, 2
    JMP L100002cc8_22

; Entry 100002cc8; block 26; address 100002e0e
L100002cc8_26:
    ADD EDX, 18446744073709551521
    CMP EDX, 25
    JA L100002cc8_29
    JMP L100002cc8_28     ; inserted

; Entry 100002cc8; block 27; address 100002e66
L100002cc8_27:
    CMP EDX, 69
    JE L100002cc8_33
    JMP L100002cc8_44     ; inserted

; Entry 100002cc8; block 28; address 100002e1a
L100002cc8_28:
    MOV ESI, 32
    LEA RDI, [rel L__TEXT___text + 9900]
    MOVSXD RDX, dword [RDI + RDX * 4]
    ADD RDX, RDI
    MOV RDX, QWORD PTR [L_JUMP_TABLE_100002e2d + 8*EDX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RDX; TARGETS: 100002e2f,1000031eb,100002f12,100002e7e,100002eb8,100002e79,100002f2a

; Entry 100002cc8; block 29; address 1000031eb
L100002cc8_29:
    MOV byte [R12 + 1], 0
    JMP L100002cc8_111     ; inserted

; Entry 100002cc8; block 30; address 100002e2f
L100002cc8_30:
    LEA R8, [R12 + 2]
    MOV AL, byte [R12 + 1]
    CMP AL, 109
    JG L100002cc8_37
    JMP L100002cc8_36     ; inserted

; Entry 100002cc8; block 31; address 100002e79
L100002cc8_31:
    MOV ESI, 512
    JMP L100002cc8_32     ; inserted

; Entry 100002cc8; block 32; address 100002e7e
L100002cc8_32:
    MOV dword [R15 + 8], ESI
    MOV byte [RBP + -41], 0
    MOV byte [RBP + -42], AL
    MOV byte [RBP + -43], 113
    MOV EAX, dword [R13 + 24]
    CMP RAX, 8
    JA L100002cc8_47
    JMP L100002cc8_46     ; inserted

; Entry 100002cc8; block 33; address 100002eb8
L100002cc8_33:
    MOV dword [R15 + 8], 16
    MOV ECX, dword [R13 + 24]
    ROL ECX, 30
    CMP ECX, 4
    JA L100002cc8_47
    JMP L100002cc8_51     ; inserted

; Entry 100002cc8; block 34; address 100002f12
L100002cc8_34:
    MOV dword [R15 + 8], 8
    CMP dword [R13 + 24], 1
    JBE L100002cc8_50
    JMP L100002cc8_54     ; inserted

; Entry 100002cc8; block 35; address 100002f2a
L100002cc8_35:
    MOV dword [R15 + 8], 128
    CMP ECX, 1
    JE L100002cc8_56
    JMP L100002cc8_55     ; inserted

; Entry 100002cc8; block 36; address 100002e41
L100002cc8_36:
    CMP AL, 65
    JE L100002cc8_39
    JMP L100002cc8_38     ; inserted

; Entry 100002cc8; block 37; address 100002f4c
L100002cc8_37:
    CMP AL, 110
    JE L100002cc8_62
    JMP L100002cc8_61     ; inserted

; Entry 100002cc8; block 38; address 100002e49
L100002cc8_38:
    CMP AL, 97
    JE L100002cc8_41
    JMP L100002cc8_40     ; inserted

; Entry 100002cc8; block 39; address 100002f8b
L100002cc8_39:
    MOV qword [rel L__DATA___common + 40], R13
    OR byte [R13 + 16], 1
    JMP L100002cc8_41     ; inserted

; Entry 100002cc8; block 40; address 100002e51
L100002cc8_40:
    CMP AL, 99
    JNE L100002cc8_43
    JMP L100002cc8_42     ; inserted

; Entry 100002cc8; block 41; address 100002f97
L100002cc8_41:
    MOV dword [R15 + 8], 1
    MOV AL, byte [R12 + 2]
    LEA ECX, [RAX + -100]
    CMP CL, 20
    JA L100002cc8_68
    JMP L100002cc8_67     ; inserted

; Entry 100002cc8; block 42; address 100002e59
L100002cc8_42:
    MOV dword [R15 + 8], 4
    JMP L100002cc8_66

; Entry 100002cc8; block 43; address 100003206
L100002cc8_43:
    MOV byte [R12 + 2], 0
    JMP L100002cc8_111

; Entry 100002cc8; block 44; address 100002e6b
L100002cc8_44:
    CMP EDX, 71
    JE L100002cc8_33
    JMP L100002cc8_45     ; inserted

; Entry 100002cc8; block 45; address 100002e70
L100002cc8_45:
    CMP EDX, 88
    JNE L100002cc8_29
    JMP L100002cc8_31     ; inserted

; Entry 100002cc8; block 46; address 100002e9b
L100002cc8_46:
    LEA RCX, [rel L__TEXT___text + 10024]
    MOVSXD RAX, dword [RCX + RAX * 4]
    ADD RAX, RCX
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100002ea9 + 8*RAX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 100002eed,100003016,100002eab,1000031dd

; Entry 100002cc8; block 47; address 1000031dd
L100002cc8_47:
    MOV byte [R12 + 1], 0
    JMP L100002cc8_110     ; inserted

; Entry 100002cc8; block 48; address 100002eab
L100002cc8_48:
    MOV dword [R15 + 12], 2
    JMP L100002cc8_53

; Entry 100002cc8; block 49; address 100002eed
L100002cc8_49:
    MOV dword [R15 + 12], 4
    JMP L100002cc8_53

; Entry 100002cc8; block 50; address 100003016
L100002cc8_50:
    MOV dword [R15 + 12], 1
    JMP L100002cc8_53     ; inserted

; Entry 100002cc8; block 51; address 100002ed0
L100002cc8_51:
    LEA RDX, [rel L__TEXT___text + 10004]
    MOVSXD RCX, dword [RDX + RCX * 4]
    ADD RCX, RDX
    MOV RCX, QWORD PTR [L_JUMP_TABLE_100002ede + 8*ECX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RCX; TARGETS: 100002ee0,100002eed,1000031dd

; Entry 100002cc8; block 52; address 100002ee0
L100002cc8_52:
    MOV dword [R15 + 12], 8
    JMP L100002cc8_53

; Entry 100002cc8; block 53; address 10000301e
L100002cc8_53:
    MOV R14, R15
    JMP L100002cc8_60     ; inserted

; Entry 100002cc8; block 54; address 100002f25
L100002cc8_54:
    JMP L100002cc8_47

; Entry 100002cc8; block 55; address 100002f37
L100002cc8_55:
    CMP ECX, 2
    JE L100002cc8_58
    JMP L100002cc8_57     ; inserted

; Entry 100002cc8; block 56; address 100002f71
L100002cc8_56:
    MOV EAX, dword [R13 + 24]
    MOV dword [R15 + 12], EAX
    JMP L100002cc8_53

; Entry 100002cc8; block 57; address 100002f3c
L100002cc8_57:
    TEST ECX, ECX
    MOV R14, R15
    JNE L100002cc8_60
    JMP L100002cc8_59     ; inserted

; Entry 100002cc8; block 58; address 100002f7e
L100002cc8_58:
    MOV RAX, qword [RBP + -64]
    MOV dword [R15 + 12], EAX
    JMP L100002cc8_53

; Entry 100002cc8; block 59; address 100002f47
L100002cc8_59:
    CALL L1000032d2_0

; Entry 100002cc8; block 60; address 100003021
L100002cc8_60:
    MOV R13B, byte [R8]
    MOV byte [R12], 0
    MOV RBX, qword [RBP + -56]
    MOV RDI, RBX
    MOV R15, R8
    CALL _strlen
    JMP L100002cc8_71     ; inserted

; Entry 100002cc8; block 61; address 100002f54
L100002cc8_61:
    CMP AL, 112
    JE L100002cc8_64
    JMP L100002cc8_63     ; inserted

; Entry 100002cc8; block 62; address 100002fd7
L100002cc8_62:
    MOV qword [rel L__DATA___common + 40], R13
    MOV dword [R13 + 16], 1
    MOV dword [R15 + 8], 1024
    MOV byte [RBP + -43], 0
    LEA RAX, [rel L__TEXT___cstring_1291]
    MOV qword [RBP + -56], RAX
    JMP L100002cc8_53

; Entry 100002cc8; block 63; address 100002f5c
L100002cc8_63:
    CMP AL, 117
    JNE L100002cc8_43
    JMP L100002cc8_65     ; inserted

; Entry 100002cc8; block 64; address 100002fff
L100002cc8_64:
    MOV dword [R15 + 8], 64
    MOV byte [RBP + -43], 99
    JMP L100002cc8_66     ; inserted

; Entry 100002cc8; block 65; address 100002f64
L100002cc8_65:
    MOV dword [R15 + 8], 256
    JMP L100002cc8_66

; Entry 100002cc8; block 66; address 10000300b
L100002cc8_66:
    CMP dword [R13 + 24], 1
    JA L100002cc8_70
    JMP L100002cc8_50     ; inserted

; Entry 100002cc8; block 67; address 100002fb0
L100002cc8_67:
    MOVZX ECX, CL
    MOV EDX, 1050625
    BT RDX, RCX
    JAE L100002cc8_68
    JMP L100002cc8_69     ; inserted

; Entry 100002cc8; block 68; address 10000320e
L100002cc8_68:
    MOV byte [R12 + 3], 0
    JMP L100002cc8_111

; Entry 100002cc8; block 69; address 100002fc2
L100002cc8_69:
    MOV R14, R15
    LEA R8, [R12 + 3]
    MOV byte [RBP + -43], 113
    MOV byte [RBP + -42], AL
    MOV byte [RBP + -41], 0
    JMP L100002cc8_60

; Entry 100002cc8; block 70; address 1000031f9
L100002cc8_70:
    MOV byte [R12 + 2], 0
    JMP L100002cc8_110

; Entry 100002cc8; block 71; address 100003038
L100002cc8_71:
    LEA RSI, [RAX + 2]
    MOV EDI, 1
    CALL _calloc
    JMP L100002cc8_72     ; inserted

; Entry 100002cc8; block 72; address 100003046
L100002cc8_72:
    MOV qword [R14 + 24], RAX
    TEST RAX, RAX
    JE L100002cc8_74
    JMP L100002cc8_73     ; inserted

; Entry 100002cc8; block 73; address 100003053
L100002cc8_73:
    MOV RDI, RAX
    MOV RSI, RBX
    CALL _strcpy
    JMP L100002cc8_75     ; inserted

; Entry 100002cc8; block 74; address 1000031d3
L100002cc8_74:
    CALL L100003466_0

; Entry 100002cc8; block 75; address 10000305e
L100002cc8_75:
    MOV RDI, qword [R14 + 24]
    LEA RSI, [RBP + -43]
    CALL _strcat
    JMP L100002cc8_76     ; inserted

; Entry 100002cc8; block 76; address 10000306b
L100002cc8_76:
    MOV byte [R15], R13B
    SUB R12, RBX
    ADD R12, qword [R14 + 24]
    MOV qword [R14 + 16], R12
    MOV RCX, R14
    TEST byte [R14 + 8], 1
    MOV R14, R15
    JNE L100002cc8_78
    JMP L100002cc8_77     ; inserted

; Entry 100002cc8; block 77; address 100003086
L100002cc8_77:
    MOV R13, qword [RBP + -80]
    CMP dword [R13 + 24], 0
    MOV RBX, qword [RBP + -88]
    JE L100002cc8_80
    JMP L100002cc8_79     ; inserted

; Entry 100002cc8; block 78; address 1000030a7
L100002cc8_78:
    MOV RBX, qword [RBP + -88]
    MOV R13, qword [RBP + -80]
    JMP L100002cc8_80     ; inserted

; Entry 100002cc8; block 79; address 100003095
L100002cc8_79:
    CMP dword [RBP + -48], 0
    MOV dword [RBP + -48], 1
    JE L100002cc8_80
    JMP L100002cc8_81     ; inserted

; Entry 100002cc8; block 80; address 1000030af
L100002cc8_80:
    CMP byte [R14], 0
    MOV R12, RCX
    JNE L100002cc8_82
    JMP L100002cc8_83     ; inserted

; Entry 100002cc8; block 81; address 1000030a2
L100002cc8_81:
    CALL L100003476_0

; Entry 100002cc8; block 82; address 100002d17
L100002cc8_82:
    MOV EDI, 1
    MOV ESI, 176
    CALL _calloc
    JMP L100002cc8_5     ; inserted

; Entry 100002cc8; block 83; address 1000030bc
L100002cc8_83:
    MOV R12, RCX
    MOV RSI, qword [rel __DefaultRuneLocale]
    JMP L100002cc8_4

; Entry 100002cc8; block 84; address 1000030e9
L100002cc8_84:
    MOV RAX, qword [R13 + 8]
    TEST RAX, RAX
    JE L100002cc8_85
    JMP L100002cc8_86     ; inserted

; Entry 100002cc8; block 85; address 100003103
L100002cc8_85:
    MOV R13, qword [R13]
    TEST R13, R13
    JNE L100002cc8_89
    JMP L100002cc8_90     ; inserted

; Entry 100002cc8; block 86; address 1000030f2
L100002cc8_86:
    XOR ECX, ECX
    JMP L100002cc8_87     ; inserted

; Entry 100002cc8; block 87; address 1000030f4
L100002cc8_87:
    ADD ECX, dword [RAX + 12]
    MOV RAX, qword [RAX]
    TEST RAX, RAX
    JNE L100002cc8_87
    JMP L100002cc8_88     ; inserted

; Entry 100002cc8; block 88; address 1000030ff
L100002cc8_88:
    MOV dword [R13 + 24], ECX
    JMP L100002cc8_85     ; inserted

; Entry 100002cc8; block 89; address 100002cfa
L100002cc8_89:
    MOV R14, qword [R13 + 32]
    CMP byte [R14], 0
    JE L100002cc8_4
    JMP L100002cc8_3     ; inserted

; Entry 100002cc8; block 90; address 100003110
L100002cc8_90:
    MOV RAX, qword [RBP + -72]
    MOV R12, qword [RAX + 8]
    TEST R12, R12
    JE L100002cc8_2
    JMP L100002cc8_91     ; inserted

; Entry 100002cc8; block 91; address 100003121
L100002cc8_91:
    LEA R14, [rel L__DATA___common + 0]
    MOV R15D, 16384
    JMP L100002cc8_109     ; inserted

; Entry 100002cc8; block 92; address 100003135
L100002cc8_92:
    MOV EAX, dword [R14]
    MOV RCX, qword [RBP + -72]
    SUB EAX, dword [RCX + 16]
    JLE L100002cc8_93
    JMP L100002cc8_94     ; inserted

; Entry 100002cc8; block 93; address 10000315a
L100002cc8_93:
    CMP dword [R12 + 20], 2
    JL L100002cc8_98
    JMP L100002cc8_97     ; inserted

; Entry 100002cc8; block 94; address 100003141
L100002cc8_94:
    TEST byte [R12 + 16], 2
    JNE L100002cc8_93
    JMP L100002cc8_95     ; inserted

; Entry 100002cc8; block 95; address 100003149
L100002cc8_95:
    MOV ECX, dword [R12 + 24]
    TEST ECX, ECX
    JE L100002cc8_93
    JMP L100002cc8_96     ; inserted

; Entry 100002cc8; block 96; address 100003152
L100002cc8_96:
    CDQ 
    IDIV ECX
    ADD dword [R12 + 20], EAX
    JMP L100002cc8_93     ; inserted

; Entry 100002cc8; block 97; address 100003162
L100002cc8_97:
    MOV RAX, qword [R12 + 8]
    JMP L100002cc8_99     ; inserted

; Entry 100002cc8; block 98; address 1000031b2
L100002cc8_98:
    MOV R12, qword [R12]
    TEST R12, R12
    JNE L100002cc8_109
    JMP L100002cc8_2     ; inserted

; Entry 100002cc8; block 99; address 100003167
L100002cc8_99:
    MOV R13, RAX
    MOV RAX, qword [RAX]
    TEST RAX, RAX
    JNE L100002cc8_99
    JMP L100002cc8_100     ; inserted

; Entry 100002cc8; block 100; address 100003172
L100002cc8_100:
    MOV RBX, qword [R13 + 24]
    MOV CL, byte [RBX]
    TEST CL, CL
    JE L100002cc8_98
    JMP L100002cc8_101     ; inserted

; Entry 100002cc8; block 101; address 10000317c
L100002cc8_101:
    DEC RBX
    JMP L100002cc8_106     ; inserted

; Entry 100002cc8; block 102; address 100003186
L100002cc8_102:
    MOV EAX, dword [RSI + RDI * 4 + 60]
    AND EAX, R15D
    JMP L100002cc8_105

; Entry 100002cc8; block 103; address 10000318f
L100002cc8_103:
    MOV ESI, 16384
    CALL ___maskrune
    JMP L100002cc8_104     ; inserted

; Entry 100002cc8; block 104; address 100003199
L100002cc8_104:
    MOV RSI, qword [rel __DefaultRuneLocale]
    JMP L100002cc8_105     ; inserted

; Entry 100002cc8; block 105; address 1000031a0
L100002cc8_105:
    MOV CL, byte [RBX + 2]
    INC RBX
    TEST CL, CL
    JNE L100002cc8_106
    JMP L100002cc8_107     ; inserted

; Entry 100002cc8; block 106; address 10000317f
L100002cc8_106:
    MOVZX EDI, CL
    TEST CL, CL
    JS L100002cc8_103
    JMP L100002cc8_102     ; inserted

; Entry 100002cc8; block 107; address 1000031aa
L100002cc8_107:
    TEST EAX, EAX
    JE L100002cc8_98
    JMP L100002cc8_108     ; inserted

; Entry 100002cc8; block 108; address 1000031ae
L100002cc8_108:
    MOV qword [R13 + 32], RBX
    JMP L100002cc8_98     ; inserted

; Entry 100002cc8; block 109; address 10000312e
L100002cc8_109:
    CMP qword [R12], 0
    JNE L100002cc8_93
    JMP L100002cc8_92     ; inserted

; Entry 100002cc8; block 110; address 1000031e3
L100002cc8_110:
    MOV RDI, R12
    CALL L1000032b8_0

; Entry 100002cc8; block 111; address 1000031f1
L100002cc8_111:
    MOV RDI, R12
    CALL L1000032e9_0



; ---------------------
; Function: 0x1000032b8
; ---------------------
; Entry 1000032b8; block 0; address 1000032b8
L1000032b8_0:
    PUSH RBP
    MOV RBP, RSP
    MOV RDX, RDI
    LEA RSI, [rel L__TEXT___cstring_1340]
    MOV EDI, 1
    XOR EAX, EAX
    CALL _errx



; ---------------------
; Function: 0x1000032d2
; ---------------------
; Entry 1000032d2; block 0; address 1000032d2
L1000032d2_0:
    PUSH RBP
    MOV RBP, RSP
    LEA RSI, [rel L__TEXT___cstring_1359]
    MOV EDI, 1
    XOR EAX, EAX
    CALL _errx



; ---------------------
; Function: 0x1000032e9
; ---------------------
; Entry 1000032e9; block 0; address 1000032e9
L1000032e9_0:
    PUSH RBP
    MOV RBP, RSP
    MOV RDX, RDI
    LEA RSI, [rel L__TEXT___cstring_1418]
    MOV EDI, 1
    XOR EAX, EAX
    CALL _errx



; ---------------------
; Function: 0x100003303
; ---------------------
; Entry 100003303; block 0; address 100003303
L100003303_0:
    PUSH RBP
    MOV RBP, RSP
    LEA RDI, [rel L__TEXT___cstring_31]
    LEA RSI, [rel L__TEXT___cstring_38]
    LEA RCX, [rel L__TEXT___cstring_73]
    MOV EDX, 133
    CALL ___assert_rtn



; ---------------------
; Function: 0x100003326
; ---------------------
; Entry 100003326; block 0; address 100003326
L100003326_0:
    PUSH RBP
    MOV RBP, RSP
    LEA RDI, [rel L__TEXT___cstring_31]
    LEA RSI, [rel L__TEXT___cstring_38]
    LEA RCX, [rel L__TEXT___cstring_45]
    MOV EDX, 131
    CALL ___assert_rtn



; ---------------------
; Function: 0x10000334c
; ---------------------
; Entry 10000334c; block 0; address 10000334c
L10000334c_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH 1
    POP RDI
    XOR ESI, ESI
    XOR EAX, EAX
    CALL _err



; ---------------------
; Function: 0x10000335c
; ---------------------
; Entry 10000335c; block 0; address 10000335c
L10000335c_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH 1
    POP RDI
    XOR ESI, ESI
    XOR EAX, EAX
    CALL _err



; ---------------------
; Function: 0x10000336c
; ---------------------
; Entry 10000336c; block 0; address 10000336c
L10000336c_0:
    PUSH RBP
    MOV RBP, RSP
    LEA RSI, [rel L__TEXT___cstring_231]
    PUSH 1
    POP RDI
    XOR EAX, EAX
    CALL _errx



; ---------------------
; Function: 0x100003381
; ---------------------
; Entry 100003381; block 0; address 100003381
L100003381_0:
    PUSH RBP
    MOV RBP, RSP
    MOV RAX, qword [rel L__DATA___bss + 40]
    MOV RSI, qword [RAX + -8]
    LEA RDI, [rel L__TEXT___cstring_222]
    XOR EAX, EAX
    POP RBP
    JMP _warn



; ---------------------
; Function: 0x1000033a2
; ---------------------
; Entry 1000033a2; block 0; address 1000033a2
L1000033a2_0:
    PUSH RBP
    MOV RBP, RSP
    MOV RAX, qword [rel _optarg]
    MOV RDX, qword [RAX]
    LEA RSI, [rel L__TEXT___cstring_525]
    PUSH 1
    POP RDI
    XOR EAX, EAX
    CALL _errx



; ---------------------
; Function: 0x1000033c1
; ---------------------
; Entry 1000033c1; block 0; address 1000033c1
L1000033c1_0:
    PUSH RBP
    MOV RBP, RSP
    MOV RAX, qword [rel _optarg]
    MOV RDX, qword [RAX]
    LEA RSI, [rel L__TEXT___cstring_891]
    PUSH 1
    POP RDI
    XOR EAX, EAX
    CALL _errx



; ---------------------
; Function: 0x1000033e0
; ---------------------
; Entry 1000033e0; block 0; address 1000033e0
L1000033e0_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH 1
    POP RDI
    XOR ESI, ESI
    XOR EAX, EAX
    CALL _err



; ---------------------
; Function: 0x100003403
; ---------------------
; Entry 100003403; block 0; address 100003403
L100003403_0:
    PUSH RBP
    MOV RBP, RSP
    LEA RDI, [rel L__TEXT___cstring_1277]
    XOR EAX, EAX
    CALL _warnx
    JMP L100003403_1     ; inserted

; Entry 100003403; block 1; address 100003415
L100003403_1:
    CALL _getchar
    JMP L100003403_2     ; inserted

; Entry 100003403; block 2; address 10000341a
L100003403_2:
    CMP EAX, 10
    JE L100003403_4
    JMP L100003403_3     ; inserted

; Entry 100003403; block 3; address 10000341f
L100003403_3:
    CMP EAX, 18446744073709551615
    JNE L100003403_1
    JMP L100003403_4     ; inserted

; Entry 100003403; block 4; address 100003424
L100003403_4:
    POP RBP
    RET 



; ---------------------
; Function: 0x100003426
; ---------------------
; Entry 100003426; block 0; address 100003426
L100003426_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH 1
    POP RDI
    XOR ESI, ESI
    XOR EAX, EAX
    CALL _err



; ---------------------
; Function: 0x100003436
; ---------------------
; Entry 100003436; block 0; address 100003436
L100003436_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH 1
    POP RDI
    XOR ESI, ESI
    XOR EAX, EAX
    CALL _err



; ---------------------
; Function: 0x100003446
; ---------------------
; Entry 100003446; block 0; address 100003446
L100003446_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH 1
    POP RDI
    XOR ESI, ESI
    XOR EAX, EAX
    CALL _err



; ---------------------
; Function: 0x100003456
; ---------------------
; Entry 100003456; block 0; address 100003456
L100003456_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH 1
    POP RDI
    XOR ESI, ESI
    XOR EAX, EAX
    CALL _err



; ---------------------
; Function: 0x100003466
; ---------------------
; Entry 100003466; block 0; address 100003466
L100003466_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH 1
    POP RDI
    XOR ESI, ESI
    XOR EAX, EAX
    CALL _err



; ---------------------
; Function: 0x100003476
; ---------------------
; Entry 100003476; block 0; address 100003476
L100003476_0:
    PUSH RBP
    MOV RBP, RSP
    LEA RSI, [rel L__TEXT___cstring_1293]
    PUSH 1
    POP RDI
    XOR EAX, EAX
    CALL _errx




section .data
L__TEXT___cstring_0: db `**`, 0
L__TEXT___cstring_3: db `\\0`, 0
L__TEXT___cstring_6: db `\\a`, 0
L__TEXT___cstring_9: db `\\b`, 0
L__TEXT___cstring_12: db `\\f`, 0
L__TEXT___cstring_15: db `\\n`, 0
L__TEXT___cstring_18: db `\\r`, 0
L__TEXT___cstring_21: db `\\t`, 0
L__TEXT___cstring_24: db `\\v`, 0
L__TEXT___cstring_27: db `%3C`, 0
L__TEXT___cstring_31: db `conv_c`, 0
L__TEXT___cstring_38: db `conv.c`, 0
L__TEXT___cstring_45: db `strcmp(pr->fmt, "%3C") == 0`, 0
L__TEXT___cstring_73: db `width >= 0`, 0
L__TEXT___cstring_84: db `%*s%C`, 0
L__TEXT___cstring_90: db ``, 0
L__TEXT___cstring_91: db `%03o`, 0
L__TEXT___cstring_96: db `nul`, 0
L__TEXT___cstring_100: db `soh`, 0
L__TEXT___cstring_104: db `stx`, 0
L__TEXT___cstring_108: db `etx`, 0
L__TEXT___cstring_112: db `eot`, 0
L__TEXT___cstring_116: db `enq`, 0
L__TEXT___cstring_120: db `ack`, 0
L__TEXT___cstring_124: db `bel`, 0
L__TEXT___cstring_128: db `bs`, 0
L__TEXT___cstring_131: db `ht`, 0
L__TEXT___cstring_134: db `lf`, 0
L__TEXT___cstring_137: db `vt`, 0
L__TEXT___cstring_140: db `ff`, 0
L__TEXT___cstring_143: db `cr`, 0
L__TEXT___cstring_146: db `so`, 0
L__TEXT___cstring_149: db `si`, 0
L__TEXT___cstring_152: db `dle`, 0
L__TEXT___cstring_156: db `dc1`, 0
L__TEXT___cstring_160: db `dc2`, 0
L__TEXT___cstring_164: db `dc3`, 0
L__TEXT___cstring_168: db `dc4`, 0
L__TEXT___cstring_172: db `nak`, 0
L__TEXT___cstring_176: db `syn`, 0
L__TEXT___cstring_180: db `etb`, 0
L__TEXT___cstring_184: db `can`, 0
L__TEXT___cstring_188: db `em`, 0
L__TEXT___cstring_191: db `sub`, 0
L__TEXT___cstring_195: db `esc`, 0
L__TEXT___cstring_199: db `fs`, 0
L__TEXT___cstring_202: db `gs`, 0
L__TEXT___cstring_205: db `rs`, 0
L__TEXT___cstring_208: db `us`, 0
L__TEXT___cstring_211: db `nl`, 0
L__TEXT___cstring_214: db `del`, 0
L__TEXT___cstring_218: db ` sp`, 0
L__TEXT___cstring_222: db `%s`, 0
L__TEXT___cstring_225: db ` -0+#`, 0
L__TEXT___cstring_231: db `cannot skip past end of input`, 0
L__TEXT___cstring_261: db `r`, 0
L__TEXT___cstring_263: db `stdin`, 0
L__TEXT___cstring_269: db `*`, 0
L__TEXT___cstring_271: db `od`, 0
L__TEXT___cstring_274: db `hd`, 0
L__TEXT___cstring_277: db `"%08.8_Ax\n"`, 0
L__TEXT___cstring_289: db `"%08.8_ax  " 8/1 "%02x " "  " 8/1 "%02x " `, 0
L__TEXT___cstring_332: db `"  |" 16/1 "%_p" "|\\n"`, 0
L__TEXT___cstring_355: db `bcCde:f:n:os:vx`, 0
L__TEXT___cstring_371: db `"%07.7_Ax\n"`, 0
L__TEXT___cstring_383: db `"%07.7_ax " 16/1 "%03o " "\\n"`, 0
L__TEXT___cstring_413: db `"%07.7_ax " 16/1 "%3_c " "\\n"`, 0
L__TEXT___cstring_443: db `"%07.7_ax " 8/2 "  %05u " "\\n"`, 0
L__TEXT___cstring_474: db `%s: bad length value`, 0
L__TEXT___cstring_495: db `"%07.7_ax " 8/2 " %06o " "\\n"`, 0
L__TEXT___cstring_525: db `%s: bad skip value`, 0
L__TEXT___cstring_544: db `"%07.7_ax " 8/2 "   %04x " "\\n"`, 0
L__TEXT___cstring_576: db `"%07.7_ax " 16/1 "%02x " "\\n"`, 0
L__TEXT___cstring_606: db `%s\n%s\n%s\n%s\n`, 0
L__TEXT___cstring_619: db `usage: hexdump [-bcCdovx] [-e fmt] [-f fmt_file] [-n length]`, 0
L__TEXT___cstring_680: db `               [-s skip] [file ...]`, 0
L__TEXT___cstring_716: db `       hd      [-bcdovx]  [-e fmt] [-f fmt_file] [-n length]`, 0
L__TEXT___cstring_777: db `"%07.7_Ao\n"`, 0
L__TEXT___cstring_789: db `"%07.7_ao  "`, 0
L__TEXT___cstring_802: db `A:aBbcDdeFfHhIij:LlN:Oost:vXx`, 0
L__TEXT___cstring_832: db `%s: invalid address base`, 0
L__TEXT___cstring_857: db `a`, 0
L__TEXT___cstring_859: db `o2`, 0
L__TEXT___cstring_862: db `o1`, 0
L__TEXT___cstring_865: db `c`, 0
L__TEXT___cstring_867: db `u2`, 0
L__TEXT___cstring_870: db `u4`, 0
L__TEXT___cstring_873: db `fD`, 0
L__TEXT___cstring_876: db `fF`, 0
L__TEXT___cstring_879: db `x4`, 0
L__TEXT___cstring_882: db `x2`, 0
L__TEXT___cstring_885: db `dL`, 0
L__TEXT___cstring_888: db `dI`, 0
L__TEXT___cstring_891: db `%s: invalid skip amount`, 0
L__TEXT___cstring_915: db `%s: invalid length`, 0
L__TEXT___cstring_934: db `o4`, 0
L__TEXT___cstring_937: db `d2`, 0
L__TEXT___cstring_940: db `oS`, 0
L__TEXT___cstring_943: db `usage: od [-aBbcDdeFfHhIiLlOosvXx] [-A base] [-j skip] [-N length] [-t type]\n`, 0
L__TEXT___cstring_1021: db `          [[+]offset[.][Bb]] [file ...]\n`, 0
L__TEXT___cstring_1062: db `16/1 "%3_u " "\\n"`, 0
L__TEXT___cstring_1080: db `16/1 "%3_c " "\\n"`, 0
L__TEXT___cstring_1098: db `%c: unrecognised format character`, 0
L__TEXT___cstring_1132: db `"         "`, 0
L__TEXT___cstring_1144: db `%s: invalid size`, 0
L__TEXT___cstring_1161: db `unsupported int size %lu`, 0
L__TEXT___cstring_1186: db `%lu/%lu "%*s%%%s%d%c" "\\n"`, 0
L__TEXT___cstring_1213: db `0`, 0
L__TEXT___cstring_1215: db `unsupported floating point size %lu`, 0
L__TEXT___cstring_1251: db `%lu/%lu " %%%d.%de " "\\n"`, 0
L__TEXT___cstring_1277: db `line too long`, 0
L__TEXT___cstring_1291: db `\n`, 0
L__TEXT___cstring_1293: db `byte count with multiple conversion characters`, 0
L__TEXT___cstring_1340: db `%s: bad byte count`, 0
L__TEXT___cstring_1359: db `%%s: requires a precision or a byte count`, 0
L__TEXT___cstring_1401: db `"%s": bad format`, 0
L__TEXT___cstring_1418: db `%%%s: bad conversion character`, 0
L__TEXT___cstring_1449: db `.#-+ 0123456789`, 0

section .bss
L__DATA___bss: resb 64

section .bss
L__DATA___common: resb 48


section .data
L_DATASECTION:
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 02h
db 00h
db 00h
db 00h
db 0ffh
db 0ffh
db 0ffh
db 0ffh
db 025h
db 05fh
db 06eh
db 00h
db 020h
db 020h
db 020h
db 020h
db 020h
db 020h
db 020h
db 020h
db 020h
db 00h


section .data
L_JUMP_TABLE_100000f70:
dq L100000e94_24
dq L100000e94_25
dq L100000e94_27
dq L100000e94_26
dq L100000e94_27
dq L100000e94_27
dq L100000e94_27
L_JUMP_TABLE_100000fe8:
dq L100000e94_40
dq L100000e94_41
dq L100000e94_27
dq L100000e94_42
dq L100000e94_27
dq L100000e94_27
dq L100000e94_27
L_JUMP_TABLE_100001085:
dq L100000e94_50
dq L100000e94_51
dq L100000e94_27
dq L100000e94_52
dq L100000e94_27
dq L100000e94_27
dq L100000e94_27
L_JUMP_TABLE_100001a28:
dq L100001990_13
dq L100001990_14
dq L100001990_15
dq L100001990_16
dq L100001990_17
dq L100001990_11
dq L100001990_11
dq L100001990_11
dq L100001990_11
dq L100001990_11
dq L100001990_11
dq L100001990_11
dq L100001990_18
dq L100001990_19
dq L100001990_11
dq L100001990_11
dq L100001990_11
dq L100001990_20
dq L100001990_11
dq L100001990_11
dq L100001990_21
dq L100001990_11
L_JUMP_TABLE_100001d60:
dq L100001cf0_11
dq L100001cf0_7
dq L100001cf0_25
dq L100001cf0_12
dq L100001cf0_25
dq L100001cf0_8
dq L100001cf0_25
dq L100001cf0_9
dq L100001cf0_6
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_6
dq L100001cf0_25
dq L100001cf0_13
dq L100001cf0_14
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_9
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_15
dq L100001cf0_16
dq L100001cf0_17
dq L100001cf0_18
dq L100001cf0_8
dq L100001cf0_19
dq L100001cf0_25
dq L100001cf0_10
dq L100001cf0_20
dq L100001cf0_21
dq L100001cf0_25
dq L100001cf0_6
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_7
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_25
dq L100001cf0_22
dq L100001cf0_23
dq L100001cf0_25
dq L100001cf0_24
dq L100001cf0_25
L_JUMP_TABLE_10000230c:
dq L1000022d0_5
dq L1000022d0_8
dq L1000022d0_7
dq L1000022d0_6
dq L1000022d0_8
L_JUMP_TABLE_100002ad1:
dq L100002a9c_9
dq L100002a9c_8
dq L100002a9c_8
dq L100002a9c_8
dq L100002a9c_10
dq L100002a9c_8
dq L100002a9c_11
dq L100002a9c_8
L_JUMP_TABLE_100002bf2:
dq L100002b34_24
dq L100002b34_8
dq L100002b34_8
dq L100002b34_8
dq L100002b34_25
dq L100002b34_22
dq L100002b34_23
dq L100002b34_23
dq L100002b34_23
dq L100002b34_8
dq L100002b34_22
dq L100002b34_8
dq L100002b34_8
dq L100002b34_8
dq L100002b34_8
dq L100002b34_8
dq L100002b34_22
dq L100002b34_8
dq L100002b34_8
dq L100002b34_8
dq L100002b34_26
dq L100002b34_8
dq L100002b34_22
dq L100002b34_8
dq L100002b34_8
L_JUMP_TABLE_100002e2d:
dq L100002cc8_30
dq L100002cc8_29
dq L100002cc8_29
dq L100002cc8_29
dq L100002cc8_34
dq L100002cc8_32
dq L100002cc8_33
dq L100002cc8_33
dq L100002cc8_33
dq L100002cc8_29
dq L100002cc8_32
dq L100002cc8_29
dq L100002cc8_29
dq L100002cc8_29
dq L100002cc8_29
dq L100002cc8_29
dq L100002cc8_31
dq L100002cc8_29
dq L100002cc8_29
dq L100002cc8_29
dq L100002cc8_35
dq L100002cc8_29
dq L100002cc8_31
dq L100002cc8_29
dq L100002cc8_29
L_JUMP_TABLE_100002ea9:
dq L100002cc8_49
dq L100002cc8_50
dq L100002cc8_48
dq L100002cc8_47
dq L100002cc8_49
dq L100002cc8_47
dq L100002cc8_47
dq L100002cc8_47
L_JUMP_TABLE_100002ede:
dq L100002cc8_52
dq L100002cc8_49
dq L100002cc8_52
dq L100002cc8_47
