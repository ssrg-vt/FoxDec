extern ___bzero
extern ___error
extern ___stack_chk_fail
extern _atoi
extern _calloc
extern _compat_mode
extern _err
extern _errx
extern _exit
extern _expand_number
extern _fnmatch
extern _free
extern _fts_open$INODE64
extern _fts_read$INODE64
extern _fts_set$INODE64
extern _fwrite
extern _getattrlist
extern _getbsize
extern _getopt_long
extern _humanize_number
extern _malloc
extern _printf
extern _puts
extern _setlocale
extern _signal
extern _stat$INODE64
extern _statfs$INODE64
extern _strcmp
extern _strdup
extern _strerror
extern _sysctlbyname
extern _warnx
extern ___stack_chk_guard
extern ___stderrp
extern _optarg
extern _optind
extern dyld_stub_binder


section .text

default rel

; ---------------------
; Function: 0x10000298c
; ---------------------
; Entry 10000298c; block 0; address 10000298c
L10000298c_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 232
    MOV R15, RSI
    MOV R12D, EDI
    LEA RSI, [rel L__TEXT___cstring_0]
    XOR R13D, R13D
    XOR EDI, EDI
    CALL _setlocale
    JMP L10000298c_1     ; inserted

; Entry 10000298c; block 1; address 1000029b7
L10000298c_1:
    MOV byte [rel L__DATA___bss + 0], 0
    MOV dword [rel L__DATA___bss + 4], 0
    LEA RAX, [RBP + -56]
    MOV qword [RAX], R13
    MOV qword [rel L__DATA___bss + 8], 512
    MOV qword [rel L__DATA___bss + 16], R13
    MOV qword [rel L__DATA___bss + 112], R13
    MOV dword [RBP + -68], 2147483647
    MOV R14D, 20
    LEA RBX, [rel L__TEXT___cstring_1]
    MOV dword [RBP + -44], 0
    XOR EAX, EAX
    MOV qword [RBP + -88], RAX
    XOR EAX, EAX
    MOV qword [RBP + -104], RAX
    MOV dword [RBP + -64], 0
    MOV dword [RBP + -60], 0
    MOV dword [RBP + -72], 0
    MOV dword [RBP + -92], R12D
    JMP L10000298c_7     ; inserted

; Entry 10000298c; block 2; address 100002a40
L10000298c_2:
    CMP EAX, 96
    JLE L10000298c_4
    JMP L10000298c_3     ; inserted

; Entry 10000298c; block 3; address 100002a45
L10000298c_3:
    ADD EAX, 18446744073709551519
    CMP EAX, 31
    JA L10000298c_6
    JMP L10000298c_5     ; inserted

; Entry 10000298c; block 4; address 100002a6c
L10000298c_4:
    LEA ECX, [RAX + -65]
    CMP ECX, 11
    JA L10000298c_21
    JMP L10000298c_20     ; inserted

; Entry 10000298c; block 5; address 100002a51
L10000298c_5:
    LEA RCX, [rel L__TEXT___text + 3712]
    MOVSXD RAX, dword [RCX + RAX * 4]
    ADD RAX, RCX
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100002a5f + 8*EAX]
    JMP RAX; TARGETS: 100002a61,100002d15,100002bd2,100002bde,100002c32,100002bb7,100002b9d,100002c26,100002b74,100002bc6,100002a28,100002c4c,100002c63,100002c5a

; Entry 10000298c; block 6; address 100002d15
L10000298c_6:
    CALL L1000038c0_0

; Entry 10000298c; block 7; address 100002a28
L10000298c_7:
    MOV EDI, R12D
    MOV RSI, R15
    MOV RDX, RBX
    LEA RCX, [rel L__DATA___const + 0]
    XOR R8D, R8D
    CALL _getopt_long
    JMP L10000298c_2     ; inserted

; Entry 10000298c; block 8; address 100002a61
L10000298c_8:
    MOV EAX, 1
    MOV qword [RBP + -88], RAX
    JMP L10000298c_7

; Entry 10000298c; block 9; address 100002b74
L10000298c_9:
    MOV dword [rel L__DATA___bss + 4], 0
    MOV qword [rel L__DATA___bss + 16], 1048576
    JMP L10000298c_7

; Entry 10000298c; block 10; address 100002b9d
L10000298c_10:
    MOV dword [rel L__DATA___bss + 4], 0
    MOV qword [rel L__DATA___bss + 16], 1024
    JMP L10000298c_7

; Entry 10000298c; block 11; address 100002bb7
L10000298c_11:
    MOV dword [rel L__DATA___bss + 4], 1
    JMP L10000298c_7

; Entry 10000298c; block 12; address 100002bc6
L10000298c_12:
    MOV byte [rel L__DATA___bss + 24], 1
    JMP L10000298c_7

; Entry 10000298c; block 13; address 100002bd2
L10000298c_13:
    MOV dword [RBP + -60], 1
    JMP L10000298c_7

; Entry 10000298c; block 14; address 100002bde
L10000298c_14:
    CALL ___error
    JMP L10000298c_39     ; inserted

; Entry 10000298c; block 15; address 100002c26
L10000298c_15:
    MOV dword [RBP + -72], 1
    JMP L10000298c_7

; Entry 10000298c; block 16; address 100002c32
L10000298c_16:
    MOV dword [rel L__DATA___bss + 4], 0
    MOV qword [rel L__DATA___bss + 16], 1073741824
    JMP L10000298c_7

; Entry 10000298c; block 17; address 100002c4c
L10000298c_17:
    MOV EAX, 1
    MOV qword [RBP + -104], RAX
    JMP L10000298c_7

; Entry 10000298c; block 18; address 100002c5a
L10000298c_18:
    OR R14D, 64
    JMP L10000298c_7

; Entry 10000298c; block 19; address 100002c63
L10000298c_19:
    MOV RAX, qword [rel _optarg]
    MOV RDI, qword [RAX]
    LEA RSI, [RBP + -56]
    CALL _expand_number
    JMP L10000298c_45     ; inserted

; Entry 10000298c; block 20; address 100002a74
L10000298c_20:
    LEA RDX, [rel L__TEXT___text + 3664]
    MOVSXD RAX, dword [RDX + RCX * 4]
    ADD RAX, RDX
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100002a82 + 8*ECX]
    JMP RAX; TARGETS: 100002a84,100002ac3,100002d15,100002ab1,100002b07

; Entry 10000298c; block 21; address 100002a8d
L10000298c_21:
    CMP EAX, 80
    JNE L10000298c_27
    JMP L10000298c_26     ; inserted

; Entry 10000298c; block 22; address 100002a84
L10000298c_22:
    MOV byte [rel L__DATA___bss + 0], 1
    JMP L10000298c_7

; Entry 10000298c; block 23; address 100002ab1
L10000298c_23:
    MOV R13D, 1
    MOV dword [RBP + -44], 0
    JMP L10000298c_7

; Entry 10000298c; block 24; address 100002ac3
L10000298c_24:
    CALL ___error
    JMP L10000298c_28     ; inserted

; Entry 10000298c; block 25; address 100002b07
L10000298c_25:
    MOV R12, R15
    MOV R15, R13
    MOV EBX, R14D
    MOV RAX, qword [rel _optarg]
    MOV R13, qword [RAX]
    MOV EDI, 1
    MOV ESI, 16
    CALL _calloc
    JMP L10000298c_34     ; inserted

; Entry 10000298c; block 26; address 100002a96
L10000298c_26:
    XOR R13D, R13D
    MOV dword [RBP + -44], 0
    JMP L10000298c_7

; Entry 10000298c; block 27; address 100002cae
L10000298c_27:
    CMP EAX, 18446744073709551615
    JNE L10000298c_6
    JMP L10000298c_48     ; inserted

; Entry 10000298c; block 28; address 100002ac8
L10000298c_28:
    MOV dword [RAX], 0
    MOV RAX, qword [rel _optarg]
    MOV RDI, qword [RAX]
    CALL _atoi
    JMP L10000298c_29     ; inserted

; Entry 10000298c; block 29; address 100002add
L10000298c_29:
    CDQE 
    MOV qword [rel L__DATA___bss + 8], RAX
    CALL ___error
    JMP L10000298c_30     ; inserted

; Entry 10000298c; block 30; address 100002aeb
L10000298c_30:
    CMP dword [RAX], 34
    JE L10000298c_32
    JMP L10000298c_31     ; inserted

; Entry 10000298c; block 31; address 100002af4
L10000298c_31:
    CMP qword [rel L__DATA___bss + 8], 0
    JG L10000298c_7
    JMP L10000298c_33     ; inserted

; Entry 10000298c; block 32; address 100002d1a
L10000298c_32:
    MOV RAX, qword [rel _optarg]
    MOV RSI, qword [RAX]
    LEA RDI, [rel L__TEXT___cstring_26]
    JMP L10000298c_53

; Entry 10000298c; block 33; address 100002b02
L10000298c_33:
    JMP L10000298c_32

; Entry 10000298c; block 34; address 100002b29
L10000298c_34:
    TEST RAX, RAX
    JE L10000298c_36
    JMP L10000298c_35     ; inserted

; Entry 10000298c; block 35; address 100002b32
L10000298c_35:
    MOV R14, RAX
    MOV RDI, R13
    CALL _strdup
    JMP L10000298c_37     ; inserted

; Entry 10000298c; block 36; address 100002c9b
L10000298c_36:
    LEA RSI, [rel L__TEXT___cstring_577]
    JMP L10000298c_250     ; inserted

; Entry 10000298c; block 37; address 100002b3d
L10000298c_37:
    MOV qword [R14], RAX
    TEST RAX, RAX
    JE L10000298c_36
    JMP L10000298c_38     ; inserted

; Entry 10000298c; block 38; address 100002b49
L10000298c_38:
    MOV RAX, qword [rel L__DATA___bss + 112]
    MOV qword [R14 + 8], RAX
    MOV qword [rel L__DATA___bss + 112], R14
    MOV R14D, EBX
    MOV R13, R15
    MOV R15, R12
    MOV R12D, dword [RBP + -92]
    LEA RBX, [rel L__TEXT___cstring_1]
    JMP L10000298c_7

; Entry 10000298c; block 39; address 100002be3
L10000298c_39:
    MOV dword [RAX], 0
    MOV RAX, qword [rel _optarg]
    MOV RDI, qword [RAX]
    CALL _atoi
    JMP L10000298c_40     ; inserted

; Entry 10000298c; block 40; address 100002bf8
L10000298c_40:
    MOV EBX, EAX
    CALL ___error
    JMP L10000298c_41     ; inserted

; Entry 10000298c; block 41; address 100002bff
L10000298c_41:
    MOV dword [RBP + -68], EBX
    TEST EBX, EBX
    LEA RBX, [rel L__TEXT___cstring_1]
    JS L10000298c_43
    JMP L10000298c_42     ; inserted

; Entry 10000298c; block 42; address 100002c11
L10000298c_42:
    MOV dword [RBP + -64], 1
    CMP dword [RAX], 34
    JNE L10000298c_7
    JMP L10000298c_44     ; inserted

; Entry 10000298c; block 43; address 100002d2d
L10000298c_43:
    MOV RAX, qword [rel _optarg]
    MOV RSI, qword [RAX]
    LEA RDI, [rel L__TEXT___cstring_59]
    JMP L10000298c_53     ; inserted

; Entry 10000298c; block 44; address 100002c21
L10000298c_44:
    JMP L10000298c_43

; Entry 10000298c; block 45; address 100002c76
L10000298c_45:
    TEST EAX, EAX
    JNE L10000298c_47
    JMP L10000298c_46     ; inserted

; Entry 10000298c; block 46; address 100002c7a
L10000298c_46:
    CMP qword [RBP + -56], 0
    JNE L10000298c_7
    JMP L10000298c_47     ; inserted

; Entry 10000298c; block 47; address 100002c85
L10000298c_47:
    MOV RAX, qword [rel _optarg]
    MOV RSI, qword [RAX]
    LEA RDI, [rel L__TEXT___cstring_92]
    JMP L10000298c_53

; Entry 10000298c; block 48; address 100002cb3
L10000298c_48:
    OR R14D, R13D
    MOV EBX, R14D
    AND EBX, 4294967277
    OR EBX, 2
    CMP dword [RBP + -44], 0
    CMOVE EBX, R14D
    MOV AL, byte [rel L__DATA___bss + 0]
    TEST AL, AL
    JNE L10000298c_50
    JMP L10000298c_49     ; inserted

; Entry 10000298c; block 49; address 100002cd1
L10000298c_49:
    MOV RCX, qword [rel L__DATA___bss + 8]
    TEST ECX, 511
    JE L10000298c_50
    JMP L10000298c_51     ; inserted

; Entry 10000298c; block 50; address 100002d03
L10000298c_50:
    MOV RCX, qword [RBP + -88]
    MOV RDX, qword [RBP + -104]
    ADD ECX, EDX
    ADD ECX, dword [RBP + -64]
    CMP ECX, 2
    JB L10000298c_52
    JMP L10000298c_6     ; inserted

; Entry 10000298c; block 51; address 100002ce0
L10000298c_51:
    LEA RDX, [RCX + 511]
    TEST RCX, RCX
    CMOVNS RDX, RCX
    AND RDX, 18446744073709551104
    ADD RDX, 512
    MOV qword [rel L__DATA___bss + 8], RDX
    JMP L10000298c_50     ; inserted

; Entry 10000298c; block 52; address 100002d4a
L10000298c_52:
    MOV RCX, qword [rel _optind]
    MOVSXD RCX, dword [RCX]
    LEA R12, [R15 + RCX * 8]
    XOR ECX, ECX
    TEST EDX, EDX
    CMOVE ECX, dword [RBP + -68]
    MOV dword [RBP + -44], ECX
    CMP qword [R12], 0
    JNE L10000298c_56
    JMP L10000298c_55     ; inserted

; Entry 10000298c; block 53; address 100002d3e
L10000298c_53:
    XOR EAX, EAX
    CALL _warnx
    JMP L10000298c_54     ; inserted

; Entry 10000298c; block 54; address 100002d45
L10000298c_54:
    CALL L1000038c0_0

; Entry 10000298c; block 55; address 100002d6a
L10000298c_55:
    LEA RCX, [rel L__DATA___data + 0]
    MOV qword [R15], RCX
    MOV qword [R15 + 8], 0
    MOV R12, R15
    JMP L10000298c_56     ; inserted

; Entry 10000298c; block 56; address 100002d7f
L10000298c_56:
    CMP qword [rel L__DATA___bss + 16], 0
    JNE L10000298c_58
    JMP L10000298c_57     ; inserted

; Entry 10000298c; block 57; address 100002d89
L10000298c_57:
    LEA RSI, [rel L__DATA___bss + 16]
    LEA RDI, [RBP + -116]
    CALL _getbsize
    JMP L10000298c_59     ; inserted

; Entry 10000298c; block 58; address 100002da4
L10000298c_58:
    TEST AL, AL
    JNE L10000298c_62
    JMP L10000298c_61     ; inserted

; Entry 10000298c; block 59; address 100002d99
L10000298c_59:
    CMP byte [rel L__DATA___bss + 0], 0
    JE L10000298c_61
    JMP L10000298c_60     ; inserted

; Entry 10000298c; block 60; address 100002da2
L10000298c_60:
    JMP L10000298c_62

; Entry 10000298c; block 61; address 100002da8
L10000298c_61:
    MOV RAX, qword [rel L__DATA___bss + 8]
    MOV ECX, 511
    LEA RDX, [RAX + RCX]
    TEST RAX, RAX
    CMOVNS RDX, RAX
    SAR RDX, 9
    MOV qword [rel L__DATA___bss + 8], RDX
    MOV RAX, qword [rel L__DATA___bss + 16]
    ADD RCX, RAX
    TEST RAX, RAX
    CMOVNS RCX, RAX
    SAR RCX, 9
    MOV qword [rel L__DATA___bss + 16], RCX
    JMP L10000298c_62     ; inserted

; Entry 10000298c; block 62; address 100002de6
L10000298c_62:
    MOV RAX, qword [RBP + -56]
    TEST RAX, RAX
    JE L10000298c_64
    JMP L10000298c_63     ; inserted

; Entry 10000298c; block 63; address 100002def
L10000298c_63:
    SHR RAX, 9
    IMUL RAX, qword [rel L__DATA___bss + 8]
    XOR EDX, EDX
    DIV qword [rel L__DATA___bss + 16]
    CMP RDX, 1
    SBB RAX, 18446744073709551615
    MOV qword [RBP + -56], RAX
    JMP L10000298c_64     ; inserted

; Entry 10000298c; block 64; address 100002e10
L10000298c_64:
    LEA R14, [RBP + -76]
    MOV dword [R14], 1
    LEA RDI, [rel L__TEXT___cstring_114]
    MOV R8D, 4
    XOR ESI, ESI
    XOR EDX, EDX
    MOV RCX, R14
    CALL _sysctlbyname
    JMP L10000298c_65     ; inserted

; Entry 10000298c; block 65; address 100002e34
L10000298c_65:
    MOV dword [R14], 0
    LEA RSI, [rel L__TEXT___text + 3938]
    MOV EDI, 29
    CALL _signal
    JMP L10000298c_66     ; inserted

; Entry 10000298c; block 66; address 100002e4c
L10000298c_66:
    MOV RDI, R12
    MOV ESI, EBX
    XOR EDX, EDX
    CALL _fts_open$INODE64
    JMP L10000298c_67     ; inserted

; Entry 10000298c; block 67; address 100002e58
L10000298c_67:
    TEST RAX, RAX
    JNE L10000298c_69
    JMP L10000298c_68     ; inserted

; Entry 10000298c; block 68; address 100002e5d
L10000298c_68:
    LEA RSI, [rel L__TEXT___cstring_149]
    JMP L10000298c_248

; Entry 10000298c; block 69; address 100002e69
L10000298c_69:
    MOV R14, RAX
    CALL ___error
    JMP L10000298c_70     ; inserted

; Entry 10000298c; block 70; address 100002e71
L10000298c_70:
    MOV dword [RAX], 0
    MOV RDI, R14
    CALL _fts_read$INODE64
    JMP L10000298c_71     ; inserted

; Entry 10000298c; block 71; address 100002e7f
L10000298c_71:
    TEST RAX, RAX
    JNE L10000298c_73
    JMP L10000298c_72     ; inserted

; Entry 10000298c; block 72; address 100002e84
L10000298c_72:
    XOR EBX, EBX
    JMP L10000298c_243     ; inserted

; Entry 10000298c; block 73; address 100002ec4
L10000298c_73:
    MOV R13, RAX
    JMP L10000298c_241     ; inserted

; Entry 10000298c; block 74; address 100002e8b
L10000298c_74:
    CMP dword [RAX], 0
    JNE L10000298c_76
    JMP L10000298c_75     ; inserted

; Entry 10000298c; block 75; address 100002e94
L10000298c_75:
    CMP dword [RBP + -60], 0
    JE L10000298c_78
    JMP L10000298c_77     ; inserted

; Entry 10000298c; block 76; address 10000373e
L10000298c_76:
    LEA RSI, [rel L__TEXT___cstring_259]
    JMP L10000298c_248     ; inserted

; Entry 10000298c; block 77; address 100002e9e
L10000298c_77:
    CMP dword [rel L__DATA___bss + 4], 0
    JLE L10000298c_80
    JMP L10000298c_79     ; inserted

; Entry 10000298c; block 78; address 10000377e
L10000298c_78:
    CALL L100003a49_0
    JMP L10000298c_249     ; inserted

; Entry 10000298c; block 79; address 100002eab
L10000298c_79:
    MOV RDI, RBX
    CALL L1000039df_0
    JMP L10000298c_81     ; inserted

; Entry 10000298c; block 80; address 100003751
L10000298c_80:
    IMUL RBX, qword [rel L__DATA___bss + 8]
    MOV RAX, RBX
    CQO 
    IDIV qword [rel L__DATA___bss + 16]
    CMP RDX, 1
    SBB RAX, 18446744073709551615
    LEA RDI, [rel L__TEXT___cstring_268]
    MOV RSI, RAX
    XOR EAX, EAX
    CALL _printf
    JMP L10000298c_78     ; inserted

; Entry 10000298c; block 81; address 100002eb3
L10000298c_81:
    LEA RDI, [rel L__TEXT___cstring_603]
    CALL _puts
    JMP L10000298c_82     ; inserted

; Entry 10000298c; block 82; address 100002ebf
L10000298c_82:
    JMP L10000298c_78

; Entry 10000298c; block 83; address 100002ed7
L10000298c_83:
    MOVZX EAX, AX
    LEA RCX, [rel L__TEXT___text + 3840]
    MOVSXD RAX, dword [RCX + RAX * 4]
    ADD RAX, RCX
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100002ee8 + 8*AX]
    JMP RAX; TARGETS: 100002eea,100002f0f,100002f92,1000030d9,100002f2f

; Entry 10000298c; block 84; address 100002f92
L10000298c_84:
    MOV RDI, R13
    CALL L1000038fe_0
    JMP L10000298c_100     ; inserted

; Entry 10000298c; block 85; address 100002eea
L10000298c_85:
    MOV RDI, R13
    CALL L1000038fe_0
    JMP L10000298c_89     ; inserted

; Entry 10000298c; block 86; address 100002f0f
L10000298c_86:
    LEA RDI, [rel L__TEXT___cstring_171]
    LEA RSI, [rel L__TEXT___cstring_178]
    CALL _compat_mode
    JMP L10000298c_93     ; inserted

; Entry 10000298c; block 87; address 100002f2f
L10000298c_87:
    MOV RDI, R13
    CALL L1000038fe_0
    JMP L10000298c_96     ; inserted

; Entry 10000298c; block 88; address 1000030d9
L10000298c_88:
    MOV RBX, qword [R13 + 48]
    MOV EDI, dword [R13 + 56]
    CALL _strerror
    JMP L10000298c_120     ; inserted

; Entry 10000298c; block 89; address 100002ef2
L10000298c_89:
    TEST EAX, EAX
    JE L10000298c_91
    JMP L10000298c_90     ; inserted

; Entry 10000298c; block 90; address 100002efa
L10000298c_90:
    MOV RDI, R14
    MOV RSI, R13
    MOV EDX, 4
    CALL _fts_set$INODE64
    JMP L10000298c_92     ; inserted

; Entry 10000298c; block 91; address 100002fd7
L10000298c_91:
    XORPS XMM0, XMM0
    MOVAPS oword [RBP + -272], XMM0
    MOV qword [RBP + -256], 0
    MOV word [RBP + -272], 5
    MOV dword [RBP + -260], 1
    MOV RDI, qword [R13 + 48]
    MOV ECX, 8
    LEA RSI, [RBP + -272]
    LEA RDX, [RBP + -112]
    XOR R8D, R8D
    CALL _getattrlist
    JMP L10000298c_107     ; inserted

; Entry 10000298c; block 92; address 100002f0a
L10000298c_92:
    JMP L10000298c_95

; Entry 10000298c; block 93; address 100002f22
L10000298c_93:
    TEST AL, AL
    JE L10000298c_95
    JMP L10000298c_94     ; inserted

; Entry 10000298c; block 94; address 100002f2a
L10000298c_94:
    MOV RAX, qword [R13]
    MOV RDX, qword [R13 + 48]
    MOV RCX, qword [RAX + 48]
    LEA RSI, [rel L__TEXT___cstring_187]
    MOV EDI, 1
    XOR EAX, EAX
    CALL _errx

; Entry 10000298c; block 95; address 100003665
L10000298c_95:
    MOV RAX, qword [R13 + 8]
    MOV RBX, qword [RAX + 24]
    CALL ___error
    JMP L10000298c_239     ; inserted

; Entry 10000298c; block 96; address 100002f37
L10000298c_96:
    TEST EAX, EAX
    JNE L10000298c_95
    JMP L10000298c_97     ; inserted

; Entry 10000298c; block 97; address 100002f3f
L10000298c_97:
    MOV RAX, qword [R13 + 96]
    CMP byte [rel L__DATA___bss + 0], 1
    JNE L10000298c_99
    JMP L10000298c_98     ; inserted

; Entry 10000298c; block 98; address 100002f50
L10000298c_98:
    MOV RAX, qword [RAX + 96]
    JMP L10000298c_112

; Entry 10000298c; block 99; address 10000304e
L10000298c_99:
    MOV RAX, qword [RAX + 104]
    JMP L10000298c_112     ; inserted

; Entry 10000298c; block 100; address 100002f9a
L10000298c_100:
    TEST EAX, EAX
    JNE L10000298c_95
    JMP L10000298c_101     ; inserted

; Entry 10000298c; block 101; address 100002fa2
L10000298c_101:
    CMP dword [RBP + -72], 0
    JNE L10000298c_103
    JMP L10000298c_102     ; inserted

; Entry 10000298c; block 102; address 100002fac
L10000298c_102:
    MOV RBX, qword [R13 + 96]
    CMP word [RBX + 6], 2
    JB L10000298c_103
    JMP L10000298c_104     ; inserted

; Entry 10000298c; block 103; address 1000035cd
L10000298c_103:
    MOV RAX, qword [R13 + 96]
    CMP byte [rel L__DATA___bss + 0], 1
    JNE L10000298c_229
    JMP L10000298c_228     ; inserted

; Entry 10000298c; block 104; address 100002fbb
L10000298c_104:
    MOV R12, qword [rel L__DATA___bss + 32]
    TEST R12, R12
    JE L10000298c_106
    JMP L10000298c_105     ; inserted

; Entry 10000298c; block 105; address 100002fcb
L10000298c_105:
    MOV RDI, qword [rel L__DATA___bss + 48]
    JMP L10000298c_151

; Entry 10000298c; block 106; address 10000322a
L10000298c_106:
    MOV qword [rel L__DATA___bss + 48], 8192
    MOV EDI, 65536
    CALL _malloc
    JMP L10000298c_147     ; inserted

; Entry 10000298c; block 107; address 10000301b
L10000298c_107:
    CMP EAX, 18446744073709551615
    JE L10000298c_95
    JMP L10000298c_108     ; inserted

; Entry 10000298c; block 108; address 100003024
L10000298c_108:
    CMP dword [RBP + -108], 1
    JE L10000298c_95
    JMP L10000298c_109     ; inserted

; Entry 10000298c; block 109; address 10000302e
L10000298c_109:
    MOV RBX, qword [R13 + 96]
    MOV R15, qword [rel L__DATA___bss + 72]
    TEST R15, R15
    JE L10000298c_111
    JMP L10000298c_110     ; inserted

; Entry 10000298c; block 110; address 100003042
L10000298c_110:
    MOV RDI, qword [rel L__DATA___bss + 88]
    JMP L10000298c_126

; Entry 10000298c; block 111; address 100003106
L10000298c_111:
    MOV qword [rel L__DATA___bss + 88], 8192
    MOV EDI, 65536
    CALL _malloc
    JMP L10000298c_122     ; inserted

; Entry 10000298c; block 112; address 100003052
L10000298c_112:
    MOV RCX, qword [rel L__DATA___bss + 8]
    CQO 
    IDIV RCX
    CMP RDX, 1
    SBB RAX, 18446744073709551615
    ADD RAX, qword [R13 + 24]
    MOV qword [R13 + 24], RAX
    MOV RDX, qword [R13 + 8]
    ADD qword [RDX + 24], RAX
    MOVSX EAX, word [R13 + 86]
    CMP dword [RBP + -44], EAX
    JL L10000298c_114
    JMP L10000298c_113     ; inserted

; Entry 10000298c; block 113; address 100003084
L10000298c_113:
    MOV RSI, qword [RBP + -56]
    MOV RDI, qword [R13 + 24]
    IMUL RCX, RDI
    MOV RAX, RCX
    CQO 
    IDIV qword [rel L__DATA___bss + 16]
    CMP RDX, 1
    MOV RCX, RAX
    SBB RCX, 18446744073709551615
    CMP RSI, RCX
    JA L10000298c_114
    JMP L10000298c_115     ; inserted

; Entry 10000298c; block 114; address 10000336b
L10000298c_114:
    CMP dword [rel L__DATA___bss + 28], 0
    JE L10000298c_95
    JMP L10000298c_172     ; inserted

; Entry 10000298c; block 115; address 1000030b0
L10000298c_115:
    CMP dword [rel L__DATA___bss + 4], 0
    JLE L10000298c_117
    JMP L10000298c_116     ; inserted

; Entry 10000298c; block 116; address 1000030bd
L10000298c_116:
    CALL L1000039df_0
    JMP L10000298c_118     ; inserted

; Entry 10000298c; block 117; address 10000334e
L10000298c_117:
    CMP RDX, 1
    SBB RAX, 18446744073709551615
    MOV RDX, qword [R13 + 48]
    LEA RDI, [rel L__TEXT___cstring_163]
    MOV RSI, RAX
    XOR EAX, EAX
    CALL _printf
    JMP L10000298c_114     ; inserted

; Entry 10000298c; block 118; address 1000030c2
L10000298c_118:
    MOV RSI, qword [R13 + 48]
    LEA RDI, [rel L__TEXT___cstring_158]
    XOR EAX, EAX
    CALL _printf
    JMP L10000298c_119     ; inserted

; Entry 10000298c; block 119; address 1000030d4
L10000298c_119:
    JMP L10000298c_114

; Entry 10000298c; block 120; address 1000030e6
L10000298c_120:
    LEA RDI, [rel L__TEXT___cstring_228]
    MOV RSI, RBX
    MOV RDX, RAX
    XOR EAX, EAX
    CALL _warnx
    JMP L10000298c_121     ; inserted

; Entry 10000298c; block 121; address 1000030fa
L10000298c_121:
    MOV dword [RBP + -76], 1
    JMP L10000298c_95

; Entry 10000298c; block 122; address 10000311b
L10000298c_122:
    MOV qword [rel L__DATA___bss + 72], RAX
    TEST RAX, RAX
    JE L10000298c_124
    JMP L10000298c_123     ; inserted

; Entry 10000298c; block 123; address 10000312b
L10000298c_123:
    MOV R15, RAX
    MOV ESI, 65536
    MOV RDI, RAX
    CALL ___bzero
    JMP L10000298c_125     ; inserted

; Entry 10000298c; block 124; address 1000037c1
L10000298c_124:
    LEA RSI, [rel L__TEXT___cstring_354]
    JMP L10000298c_250

; Entry 10000298c; block 125; address 10000313b
L10000298c_125:
    MOV EDI, 8192
    JMP L10000298c_126     ; inserted

; Entry 10000298c; block 126; address 100003140
L10000298c_126:
    CMP byte [rel L__DATA___bss + 104], 0
    JNE L10000298c_128
    JMP L10000298c_127     ; inserted

; Entry 10000298c; block 127; address 10000314d
L10000298c_127:
    LEA R12, [RDI + RDI]
    LEA RAX, [R12 + R12 * 4]
    CMP qword [rel L__DATA___bss + 96], RAX
    JBE L10000298c_128
    JMP L10000298c_129     ; inserted

; Entry 10000298c; block 128; address 1000033b6
L10000298c_128:
    MOVSXD RCX, dword [RBX]
    MOV R12, RBX
    MOV RSI, qword [RBX + 8]
    MOV RAX, RCX
    XOR RAX, RSI
    XOR EDX, EDX
    DIV RDI
    MOVSXD RBX, EDX
    MOV RDI, qword [R15 + RBX * 8]
    JMP L10000298c_181     ; inserted

; Entry 10000298c; block 129; address 100003162
L10000298c_129:
    MOV ESI, 8
    MOV RDI, R12
    CALL _calloc
    JMP L10000298c_130     ; inserted

; Entry 10000298c; block 130; address 10000316f
L10000298c_130:
    MOV R15, RAX
    TEST RAX, RAX
    JNE L10000298c_132
    JMP L10000298c_131     ; inserted

; Entry 10000298c; block 131; address 100003177
L10000298c_131:
    MOV RDI, qword [rel L__DATA___bss + 80]
    TEST RDI, RDI
    JE L10000298c_132
    JMP L10000298c_133     ; inserted

; Entry 10000298c; block 132; address 1000031ae
L10000298c_132:
    TEST R15, R15
    JE L10000298c_138
    JMP L10000298c_137     ; inserted

; Entry 10000298c; block 133; address 100003183
L10000298c_133:
    MOV RAX, qword [RDI]
    MOV qword [rel L__DATA___bss + 80], RAX
    CALL _free
    JMP L10000298c_134     ; inserted

; Entry 10000298c; block 134; address 100003192
L10000298c_134:
    MOV RDI, qword [rel L__DATA___bss + 80]
    TEST RDI, RDI
    JNE L10000298c_133
    JMP L10000298c_135     ; inserted

; Entry 10000298c; block 135; address 10000319e
L10000298c_135:
    MOV ESI, 8
    MOV RDI, R12
    CALL _calloc
    JMP L10000298c_136     ; inserted

; Entry 10000298c; block 136; address 1000031ab
L10000298c_136:
    MOV R15, RAX
    JMP L10000298c_132     ; inserted

; Entry 10000298c; block 137; address 1000031b7
L10000298c_137:
    MOV R8, qword [rel L__DATA___bss + 88]
    TEST R8, R8
    JE L10000298c_140
    JMP L10000298c_139     ; inserted

; Entry 10000298c; block 138; address 1000036ba
L10000298c_138:
    MOV byte [rel L__DATA___bss + 104], 1
    LEA RDI, [rel L__TEXT___cstring_397]
    XOR EAX, EAX
    CALL _warnx
    JMP L10000298c_244     ; inserted

; Entry 10000298c; block 139; address 1000031c7
L10000298c_139:
    MOV R9, RBX
    MOV RDI, qword [rel L__DATA___bss + 72]
    XOR ESI, ESI
    JMP L10000298c_145     ; inserted

; Entry 10000298c; block 140; address 100003399
L10000298c_140:
    MOV RDI, qword [rel L__DATA___bss + 72]
    JMP L10000298c_174     ; inserted

; Entry 10000298c; block 141; address 1000031dc
L10000298c_141:
    MOV RAX, qword [RCX]
    MOV qword [RDI + RSI * 8], RAX
    MOVSXD RAX, dword [RCX + 20]
    XOR RAX, qword [RCX + 24]
    XOR EDX, EDX
    DIV R12
    MOVSXD RAX, EDX
    MOV RBX, qword [R15 + RAX * 8]
    MOV EDX, 0
    TEST RBX, RBX
    JE L10000298c_144
    JMP L10000298c_143     ; inserted

; Entry 10000298c; block 142; address 10000321a
L10000298c_142:
    INC RSI
    CMP RSI, R8
    JNE L10000298c_145
    JMP L10000298c_146     ; inserted

; Entry 10000298c; block 143; address 100003201
L10000298c_143:
    MOV qword [RBX + 8], RCX
    MOV RDX, qword [R15 + RAX * 8]
    JMP L10000298c_144     ; inserted

; Entry 10000298c; block 144; address 100003209
L10000298c_144:
    MOV qword [RCX], RDX
    MOV qword [RCX + 8], 0
    MOV qword [R15 + RAX * 8], RCX
    JMP L10000298c_145

; Entry 10000298c; block 145; address 1000031d3
L10000298c_145:
    MOV RCX, qword [RDI + RSI * 8]
    TEST RCX, RCX
    JE L10000298c_142
    JMP L10000298c_141     ; inserted

; Entry 10000298c; block 146; address 100003222
L10000298c_146:
    MOV RBX, R9
    JMP L10000298c_174

; Entry 10000298c; block 147; address 10000323f
L10000298c_147:
    MOV qword [rel L__DATA___bss + 32], RAX
    TEST RAX, RAX
    JE L10000298c_149
    JMP L10000298c_148     ; inserted

; Entry 10000298c; block 148; address 10000324f
L10000298c_148:
    MOV R12, RAX
    MOV ESI, 65536
    MOV RDI, RAX
    CALL ___bzero
    JMP L10000298c_150     ; inserted

; Entry 10000298c; block 149; address 1000037cd
L10000298c_149:
    LEA RSI, [rel L__TEXT___cstring_282]
    JMP L10000298c_250

; Entry 10000298c; block 150; address 10000325f
L10000298c_150:
    MOV EDI, 8192
    JMP L10000298c_151     ; inserted

; Entry 10000298c; block 151; address 100003264
L10000298c_151:
    CMP byte [rel L__DATA___bss + 64], 0
    JNE L10000298c_153
    JMP L10000298c_152     ; inserted

; Entry 10000298c; block 152; address 100003271
L10000298c_152:
    LEA R15, [RDI + RDI]
    LEA RAX, [R15 + R15 * 4]
    CMP qword [rel L__DATA___bss + 56], RAX
    JBE L10000298c_153
    JMP L10000298c_154     ; inserted

; Entry 10000298c; block 153; address 1000034d5
L10000298c_153:
    MOVSXD RCX, dword [RBX]
    MOV RSI, qword [RBX + 8]
    MOV RAX, RCX
    XOR RAX, RSI
    XOR EDX, EDX
    DIV RDI
    MOVSXD R15, EDX
    MOV RDI, qword [R12 + R15 * 8]
    JMP L10000298c_208     ; inserted

; Entry 10000298c; block 154; address 100003286
L10000298c_154:
    MOV ESI, 8
    MOV RDI, R15
    CALL _calloc
    JMP L10000298c_155     ; inserted

; Entry 10000298c; block 155; address 100003293
L10000298c_155:
    MOV R12, RAX
    TEST RAX, RAX
    JNE L10000298c_157
    JMP L10000298c_156     ; inserted

; Entry 10000298c; block 156; address 10000329b
L10000298c_156:
    MOV RDI, qword [rel L__DATA___bss + 40]
    TEST RDI, RDI
    JE L10000298c_157
    JMP L10000298c_158     ; inserted

; Entry 10000298c; block 157; address 1000032d2
L10000298c_157:
    TEST R12, R12
    JE L10000298c_163
    JMP L10000298c_162     ; inserted

; Entry 10000298c; block 158; address 1000032a7
L10000298c_158:
    MOV RAX, qword [RDI]
    MOV qword [rel L__DATA___bss + 40], RAX
    CALL _free
    JMP L10000298c_159     ; inserted

; Entry 10000298c; block 159; address 1000032b6
L10000298c_159:
    MOV RDI, qword [rel L__DATA___bss + 40]
    TEST RDI, RDI
    JNE L10000298c_158
    JMP L10000298c_160     ; inserted

; Entry 10000298c; block 160; address 1000032c2
L10000298c_160:
    MOV ESI, 8
    MOV RDI, R15
    CALL _calloc
    JMP L10000298c_161     ; inserted

; Entry 10000298c; block 161; address 1000032cf
L10000298c_161:
    MOV R12, RAX
    JMP L10000298c_157     ; inserted

; Entry 10000298c; block 162; address 1000032db
L10000298c_162:
    MOV R8, qword [rel L__DATA___bss + 48]
    TEST R8, R8
    JE L10000298c_165
    JMP L10000298c_164     ; inserted

; Entry 10000298c; block 163; address 1000036e2
L10000298c_163:
    MOV byte [rel L__DATA___bss + 64], 1
    LEA RDI, [rel L__TEXT___cstring_315]
    XOR EAX, EAX
    CALL _warnx
    JMP L10000298c_245     ; inserted

; Entry 10000298c; block 164; address 1000032eb
L10000298c_164:
    MOV R9, RBX
    MOV RDI, qword [rel L__DATA___bss + 32]
    XOR ESI, ESI
    JMP L10000298c_170     ; inserted

; Entry 10000298c; block 165; address 1000034b8
L10000298c_165:
    MOV RDI, qword [rel L__DATA___bss + 32]
    JMP L10000298c_201     ; inserted

; Entry 10000298c; block 166; address 100003300
L10000298c_166:
    MOV RAX, qword [RCX]
    MOV qword [RDI + RSI * 8], RAX
    MOVSXD RAX, dword [RCX + 20]
    XOR RAX, qword [RCX + 24]
    XOR EDX, EDX
    DIV R15
    MOVSXD RAX, EDX
    MOV RBX, qword [R12 + RAX * 8]
    MOV EDX, 0
    TEST RBX, RBX
    JE L10000298c_169
    JMP L10000298c_168     ; inserted

; Entry 10000298c; block 167; address 10000333e
L10000298c_167:
    INC RSI
    CMP RSI, R8
    JNE L10000298c_170
    JMP L10000298c_171     ; inserted

; Entry 10000298c; block 168; address 100003325
L10000298c_168:
    MOV qword [RBX + 8], RCX
    MOV RDX, qword [R12 + RAX * 8]
    JMP L10000298c_169     ; inserted

; Entry 10000298c; block 169; address 10000332d
L10000298c_169:
    MOV qword [RCX], RDX
    MOV qword [RCX + 8], 0
    MOV qword [R12 + RAX * 8], RCX
    JMP L10000298c_170

; Entry 10000298c; block 170; address 1000032f7
L10000298c_170:
    MOV RCX, qword [RDI + RSI * 8]
    TEST RCX, RCX
    JE L10000298c_167
    JMP L10000298c_166     ; inserted

; Entry 10000298c; block 171; address 100003346
L10000298c_171:
    MOV RBX, R9
    JMP L10000298c_201

; Entry 10000298c; block 172; address 100003378
L10000298c_172:
    MOV dword [rel L__DATA___bss + 28], 0
    MOV RSI, qword [R13 + 48]
    LEA RDI, [rel L__TEXT___cstring_158]
    XOR EAX, EAX
    CALL _printf
    JMP L10000298c_173     ; inserted

; Entry 10000298c; block 173; address 100003394
L10000298c_173:
    JMP L10000298c_95

; Entry 10000298c; block 174; address 1000033a0
L10000298c_174:
    CALL _free
    JMP L10000298c_175     ; inserted

; Entry 10000298c; block 175; address 1000033a5
L10000298c_175:
    MOV qword [rel L__DATA___bss + 72], R15
    MOV qword [rel L__DATA___bss + 88], R12
    MOV RDI, R12
    JMP L10000298c_128     ; inserted

; Entry 10000298c; block 176; address 1000033d7
L10000298c_176:
    CMP dword [RDI + 20], ECX
    JNE L10000298c_179
    JMP L10000298c_178     ; inserted

; Entry 10000298c; block 177; address 1000033e7
L10000298c_177:
    CMP byte [rel L__DATA___bss + 104], 0
    JNE L10000298c_95
    JMP L10000298c_182     ; inserted

; Entry 10000298c; block 178; address 1000033dc
L10000298c_178:
    CMP qword [RDI + 24], RSI
    JE L10000298c_180
    JMP L10000298c_179     ; inserted

; Entry 10000298c; block 179; address 1000033e2
L10000298c_179:
    MOV RDI, qword [RDI]
    JMP L10000298c_181

; Entry 10000298c; block 180; address 10000340c
L10000298c_180:
    MOV EAX, dword [RDI + 16]
    LEA ECX, [RAX + -1]
    MOV dword [RDI + 16], ECX
    CMP EAX, 1
    JG L10000298c_90
    JMP L10000298c_185     ; inserted

; Entry 10000298c; block 181; address 1000033d2
L10000298c_181:
    TEST RDI, RDI
    JE L10000298c_177
    JMP L10000298c_176     ; inserted

; Entry 10000298c; block 182; address 1000033f4
L10000298c_182:
    MOV RAX, qword [rel L__DATA___bss + 80]
    TEST RAX, RAX
    JE L10000298c_184
    JMP L10000298c_183     ; inserted

; Entry 10000298c; block 183; address 100003400
L10000298c_183:
    MOV RDX, qword [RAX]
    MOV qword [rel L__DATA___bss + 80], RDX
    JMP L10000298c_198

; Entry 10000298c; block 184; address 100003461
L10000298c_184:
    MOV EDI, 32
    CALL _malloc
    JMP L10000298c_195     ; inserted

; Entry 10000298c; block 185; address 10000341e
L10000298c_185:
    MOV RAX, qword [RDI + 8]
    TEST RAX, RAX
    JE L10000298c_187
    JMP L10000298c_186     ; inserted

; Entry 10000298c; block 186; address 100003427
L10000298c_186:
    MOV RCX, qword [RDI]
    MOV qword [RAX], RCX
    JMP L10000298c_187     ; inserted

; Entry 10000298c; block 187; address 10000342d
L10000298c_187:
    MOV RCX, qword [RDI]
    TEST RCX, RCX
    JE L10000298c_189
    JMP L10000298c_188     ; inserted

; Entry 10000298c; block 188; address 100003435
L10000298c_188:
    MOV qword [RCX + 8], RAX
    JMP L10000298c_189     ; inserted

; Entry 10000298c; block 189; address 100003439
L10000298c_189:
    CMP qword [R15 + RBX * 8], RDI
    JNE L10000298c_191
    JMP L10000298c_190     ; inserted

; Entry 10000298c; block 190; address 10000343f
L10000298c_190:
    MOV qword [R15 + RBX * 8], RCX
    JMP L10000298c_191     ; inserted

; Entry 10000298c; block 191; address 100003443
L10000298c_191:
    DEC qword [rel L__DATA___bss + 96]
    CMP byte [rel L__DATA___bss + 104], 1
    JNE L10000298c_193
    JMP L10000298c_192     ; inserted

; Entry 10000298c; block 192; address 100003457
L10000298c_192:
    CALL _free
    JMP L10000298c_194     ; inserted

; Entry 10000298c; block 193; address 100003691
L10000298c_193:
    MOV RAX, qword [rel L__DATA___bss + 80]
    MOV qword [RDI], RAX
    MOV qword [rel L__DATA___bss + 80], RDI
    JMP L10000298c_90

; Entry 10000298c; block 194; address 10000345c
L10000298c_194:
    JMP L10000298c_90

; Entry 10000298c; block 195; address 10000346b
L10000298c_195:
    TEST RAX, RAX
    JE L10000298c_197
    JMP L10000298c_196     ; inserted

; Entry 10000298c; block 196; address 100003474
L10000298c_196:
    MOV ECX, dword [R12]
    MOV RSI, qword [R12 + 8]
    JMP L10000298c_198     ; inserted

; Entry 10000298c; block 197; address 10000370a
L10000298c_197:
    MOV byte [rel L__DATA___bss + 104], 1
    LEA RDI, [rel L__TEXT___cstring_315]
    XOR EAX, EAX
    CALL _warnx
    JMP L10000298c_246     ; inserted

; Entry 10000298c; block 198; address 10000347d
L10000298c_198:
    MOV dword [RAX + 20], ECX
    MOV qword [RAX + 24], RSI
    MOV ECX, dword [RBP + -108]
    DEC ECX
    MOV dword [RAX + 16], ECX
    INC qword [rel L__DATA___bss + 96]
    MOV RCX, qword [R15 + RBX * 8]
    MOV qword [RAX], RCX
    MOV qword [RAX + 8], 0
    MOV RCX, qword [R15 + RBX * 8]
    TEST RCX, RCX
    JE L10000298c_200
    JMP L10000298c_199     ; inserted

; Entry 10000298c; block 199; address 1000034ab
L10000298c_199:
    MOV qword [RCX + 8], RAX
    JMP L10000298c_200     ; inserted

; Entry 10000298c; block 200; address 1000034af
L10000298c_200:
    MOV qword [R15 + RBX * 8], RAX
    JMP L10000298c_95

; Entry 10000298c; block 201; address 1000034bf
L10000298c_201:
    CALL _free
    JMP L10000298c_202     ; inserted

; Entry 10000298c; block 202; address 1000034c4
L10000298c_202:
    MOV qword [rel L__DATA___bss + 32], R12
    MOV qword [rel L__DATA___bss + 48], R15
    MOV RDI, R15
    JMP L10000298c_153     ; inserted

; Entry 10000298c; block 203; address 1000034f3
L10000298c_203:
    CMP dword [RDI + 20], ECX
    JNE L10000298c_206
    JMP L10000298c_205     ; inserted

; Entry 10000298c; block 204; address 100003503
L10000298c_204:
    CMP byte [rel L__DATA___bss + 64], 0
    JNE L10000298c_103
    JMP L10000298c_209     ; inserted

; Entry 10000298c; block 205; address 1000034f8
L10000298c_205:
    CMP qword [RDI + 24], RSI
    JE L10000298c_207
    JMP L10000298c_206     ; inserted

; Entry 10000298c; block 206; address 1000034fe
L10000298c_206:
    MOV RDI, qword [RDI]
    JMP L10000298c_208

; Entry 10000298c; block 207; address 100003528
L10000298c_207:
    MOV EAX, dword [RDI + 16]
    LEA ECX, [RAX + -1]
    MOV dword [RDI + 16], ECX
    CMP EAX, 1
    JG L10000298c_95
    JMP L10000298c_212     ; inserted

; Entry 10000298c; block 208; address 1000034ee
L10000298c_208:
    TEST RDI, RDI
    JE L10000298c_204
    JMP L10000298c_203     ; inserted

; Entry 10000298c; block 209; address 100003510
L10000298c_209:
    MOV RAX, qword [rel L__DATA___bss + 40]
    TEST RAX, RAX
    JE L10000298c_211
    JMP L10000298c_210     ; inserted

; Entry 10000298c; block 210; address 10000351c
L10000298c_210:
    MOV RDX, qword [RAX]
    MOV qword [rel L__DATA___bss + 40], RDX
    JMP L10000298c_225

; Entry 10000298c; block 211; address 10000357d
L10000298c_211:
    MOV EDI, 32
    CALL _malloc
    JMP L10000298c_222     ; inserted

; Entry 10000298c; block 212; address 10000353a
L10000298c_212:
    MOV RAX, qword [RDI + 8]
    TEST RAX, RAX
    JE L10000298c_214
    JMP L10000298c_213     ; inserted

; Entry 10000298c; block 213; address 100003543
L10000298c_213:
    MOV RCX, qword [RDI]
    MOV qword [RAX], RCX
    JMP L10000298c_214     ; inserted

; Entry 10000298c; block 214; address 100003549
L10000298c_214:
    MOV RCX, qword [RDI]
    TEST RCX, RCX
    JE L10000298c_216
    JMP L10000298c_215     ; inserted

; Entry 10000298c; block 215; address 100003551
L10000298c_215:
    MOV qword [RCX + 8], RAX
    JMP L10000298c_216     ; inserted

; Entry 10000298c; block 216; address 100003555
L10000298c_216:
    CMP qword [R12 + R15 * 8], RDI
    JNE L10000298c_218
    JMP L10000298c_217     ; inserted

; Entry 10000298c; block 217; address 10000355b
L10000298c_217:
    MOV qword [R12 + R15 * 8], RCX
    JMP L10000298c_218     ; inserted

; Entry 10000298c; block 218; address 10000355f
L10000298c_218:
    DEC qword [rel L__DATA___bss + 56]
    CMP byte [rel L__DATA___bss + 64], 1
    JNE L10000298c_220
    JMP L10000298c_219     ; inserted

; Entry 10000298c; block 219; address 100003573
L10000298c_219:
    CALL _free
    JMP L10000298c_221     ; inserted

; Entry 10000298c; block 220; address 1000036a7
L10000298c_220:
    MOV RAX, qword [rel L__DATA___bss + 40]
    MOV qword [RDI], RAX
    MOV qword [rel L__DATA___bss + 40], RDI
    JMP L10000298c_95

; Entry 10000298c; block 221; address 100003578
L10000298c_221:
    JMP L10000298c_95

; Entry 10000298c; block 222; address 100003587
L10000298c_222:
    TEST RAX, RAX
    JE L10000298c_224
    JMP L10000298c_223     ; inserted

; Entry 10000298c; block 223; address 100003590
L10000298c_223:
    MOV ECX, dword [RBX]
    MOV RSI, qword [RBX + 8]
    JMP L10000298c_225     ; inserted

; Entry 10000298c; block 224; address 100003724
L10000298c_224:
    MOV byte [rel L__DATA___bss + 64], 1
    LEA RDI, [rel L__TEXT___cstring_315]
    XOR EAX, EAX
    CALL _warnx
    JMP L10000298c_247     ; inserted

; Entry 10000298c; block 225; address 100003596
L10000298c_225:
    MOV dword [RAX + 20], ECX
    MOV qword [RAX + 24], RSI
    MOVZX ECX, word [RBX + 6]
    DEC ECX
    MOV dword [RAX + 16], ECX
    INC qword [rel L__DATA___bss + 56]
    MOV RCX, qword [R12 + R15 * 8]
    MOV qword [RAX], RCX
    MOV qword [RAX + 8], 0
    MOV RCX, qword [R12 + R15 * 8]
    TEST RCX, RCX
    JE L10000298c_227
    JMP L10000298c_226     ; inserted

; Entry 10000298c; block 226; address 1000035c5
L10000298c_226:
    MOV qword [RCX + 8], RAX
    JMP L10000298c_227     ; inserted

; Entry 10000298c; block 227; address 1000035c9
L10000298c_227:
    MOV qword [R12 + R15 * 8], RAX
    JMP L10000298c_103     ; inserted

; Entry 10000298c; block 228; address 1000035da
L10000298c_228:
    MOV RAX, qword [RAX + 96]
    JMP L10000298c_230

; Entry 10000298c; block 229; address 1000035e0
L10000298c_229:
    MOV RAX, qword [RAX + 104]
    JMP L10000298c_230     ; inserted

; Entry 10000298c; block 230; address 1000035e4
L10000298c_230:
    MOV RCX, qword [rel L__DATA___bss + 8]
    CQO 
    IDIV RCX
    MOV RBX, RAX
    CMP RDX, 1
    SBB RBX, 18446744073709551615
    CMP dword [RBP + -88], 0
    JE L10000298c_232
    JMP L10000298c_231     ; inserted

; Entry 10000298c; block 231; address 100003601
L10000298c_231:
    CMP dword [rel L__DATA___bss + 4], 0
    JLE L10000298c_234
    JMP L10000298c_233     ; inserted

; Entry 10000298c; block 232; address 100003626
L10000298c_232:
    CMP word [R13 + 86], 0
    JNE L10000298c_238
    JMP L10000298c_237     ; inserted

; Entry 10000298c; block 233; address 10000360a
L10000298c_233:
    MOV RDI, RBX
    CALL L1000039df_0
    JMP L10000298c_235     ; inserted

; Entry 10000298c; block 234; address 100003630
L10000298c_234:
    IMUL RCX, RBX
    MOV RAX, RCX
    CQO 
    IDIV qword [rel L__DATA___bss + 16]
    CMP RDX, 1
    SBB RAX, 18446744073709551615
    MOV RDX, qword [R13 + 48]
    LEA RDI, [rel L__TEXT___cstring_163]
    MOV RSI, RAX
    XOR EAX, EAX
    CALL _printf
    JMP L10000298c_238     ; inserted

; Entry 10000298c; block 235; address 100003612
L10000298c_235:
    MOV RSI, qword [R13 + 48]
    LEA RDI, [rel L__TEXT___cstring_158]
    XOR EAX, EAX
    CALL _printf
    JMP L10000298c_236     ; inserted

; Entry 10000298c; block 236; address 100003624
L10000298c_236:
    JMP L10000298c_238

; Entry 10000298c; block 237; address 10000362e
L10000298c_237:
    JMP L10000298c_231

; Entry 10000298c; block 238; address 10000365d
L10000298c_238:
    MOV RAX, qword [R13 + 8]
    ADD qword [RAX + 24], RBX
    JMP L10000298c_95     ; inserted

; Entry 10000298c; block 239; address 100003672
L10000298c_239:
    MOV dword [RAX], 0
    MOV RDI, R14
    CALL _fts_read$INODE64
    JMP L10000298c_240     ; inserted

; Entry 10000298c; block 240; address 100003680
L10000298c_240:
    MOV R13, RAX
    TEST RAX, RAX
    JNE L10000298c_241
    JMP L10000298c_242     ; inserted

; Entry 10000298c; block 241; address 100002ec7
L10000298c_241:
    MOV EAX, dword [R13 + 88]
    DEC EAX
    CMP AX, 12
    JA L10000298c_84
    JMP L10000298c_83     ; inserted

; Entry 10000298c; block 242; address 10000368c
L10000298c_242:
    JMP L10000298c_243

; Entry 10000298c; block 243; address 100002e86
L10000298c_243:
    CALL ___error
    JMP L10000298c_74     ; inserted

; Entry 10000298c; block 244; address 1000036cf
L10000298c_244:
    MOV RDI, qword [rel L__DATA___bss + 88]
    MOV R15, qword [rel L__DATA___bss + 72]
    JMP L10000298c_128

; Entry 10000298c; block 245; address 1000036f7
L10000298c_245:
    MOV RDI, qword [rel L__DATA___bss + 48]
    MOV R12, qword [rel L__DATA___bss + 32]
    JMP L10000298c_153

; Entry 10000298c; block 246; address 10000371f
L10000298c_246:
    JMP L10000298c_95

; Entry 10000298c; block 247; address 100003739
L10000298c_247:
    JMP L10000298c_103

; Entry 10000298c; block 248; address 100003745
L10000298c_248:
    MOV EDI, 1
    XOR EAX, EAX
    CALL _err

; Entry 10000298c; block 249; address 100003783
L10000298c_249:
    MOV EDI, dword [RBP + -76]
    CALL _exit

; Entry 10000298c; block 250; address 100002ca2
L10000298c_250:
    MOV EDI, 1
    XOR EAX, EAX
    CALL _errx



; ---------------------
; Function: 0x1000038c0
; ---------------------
; Entry 1000038c0; block 0; address 1000038c0
L1000038c0_0:
    PUSH RBP
    MOV RBP, RSP
    MOV RAX, qword [rel ___stderrp]
    MOV RCX, qword [RAX]
    LEA RDI, [rel L__TEXT___cstring_450]
    MOV ESI, 126
    MOV EDX, 1
    CALL _fwrite
    JMP L1000038c0_1     ; inserted

; Entry 1000038c0; block 1; address 1000038e4
L1000038c0_1:
    MOV EDI, 64
    CALL _exit



; ---------------------
; Function: 0x1000038fe
; ---------------------
; Entry 1000038fe; block 0; address 1000038fe
L1000038fe_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R14
    PUSH RBX
    SUB RSP, 2176
    MOV R14, RDI
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -24], RAX
    MOV RAX, qword [RDI + 96]
    MOVZX EAX, word [RAX + 4]
    AND EAX, 61440
    CMP EAX, 16384
    JNE L1000038fe_2
    JMP L1000038fe_1     ; inserted

; Entry 1000038fe; block 1; address 100003931
L1000038fe_1:
    LEA RSI, [R14 + 104]
    LEA RDI, [rel L__TEXT___cstring_600]
    CALL _strcmp
    JMP L1000038fe_3     ; inserted

; Entry 1000038fe; block 2; address 100003945
L1000038fe_2:
    CMP byte [rel L__DATA___bss + 24], 1
    JNE L1000038fe_6
    JMP L1000038fe_5     ; inserted

; Entry 1000038fe; block 3; address 100003941
L1000038fe_3:
    TEST EAX, EAX
    JE L1000038fe_4
    JMP L1000038fe_2     ; inserted

; Entry 1000038fe; block 4; address 10000398c
L1000038fe_4:
    MOV RDI, qword [R14 + 40]
    LEA RSI, [RBP + -2192]
    CALL _statfs$INODE64
    JMP L1000038fe_14     ; inserted

; Entry 1000038fe; block 5; address 10000394e
L1000038fe_5:
    MOV RCX, qword [R14 + 96]
    MOV EAX, 1
    TEST byte [RCX + 116], 1
    JNE L1000038fe_7
    JMP L1000038fe_6     ; inserted

; Entry 1000038fe; block 6; address 10000395d
L1000038fe_6:
    MOV RBX, qword [rel L__DATA___bss + 112]
    TEST RBX, RBX
    JE L1000038fe_9
    JMP L1000038fe_8     ; inserted

; Entry 1000038fe; block 7; address 1000039be
L1000038fe_7:
    MOV RCX, qword [rel ___stack_chk_guard]
    MOV RCX, qword [RCX]
    CMP RCX, qword [RBP + -24]
    JNE L1000038fe_17
    JMP L1000038fe_16     ; inserted

; Entry 1000038fe; block 8; address 100003969
L1000038fe_8:
    ADD R14, 104
    JMP L1000038fe_13     ; inserted

; Entry 1000038fe; block 9; address 100003988
L1000038fe_9:
    XOR EAX, EAX
    JMP L1000038fe_7

; Entry 1000038fe; block 10; address 10000397a
L1000038fe_10:
    CMP EAX, 1
    JNE L1000038fe_12
    JMP L1000038fe_11     ; inserted

; Entry 1000038fe; block 11; address 10000397f
L1000038fe_11:
    MOV RBX, qword [RBX + 8]
    TEST RBX, RBX
    JNE L1000038fe_13
    JMP L1000038fe_9     ; inserted

; Entry 1000038fe; block 12; address 1000039b9
L1000038fe_12:
    MOV EAX, 1
    JMP L1000038fe_7     ; inserted

; Entry 1000038fe; block 13; address 10000396d
L1000038fe_13:
    MOV RDI, qword [RBX]
    MOV RSI, R14
    XOR EDX, EDX
    CALL _fnmatch
    JMP L1000038fe_10     ; inserted

; Entry 1000038fe; block 14; address 10000399c
L1000038fe_14:
    TEST EAX, EAX
    JS L1000038fe_2
    JMP L1000038fe_15     ; inserted

; Entry 1000038fe; block 15; address 1000039a0
L1000038fe_15:
    MOV EAX, 1719035236
    XOR EAX, dword [RBP + -2120]
    MOVZX ECX, word [RBP + -2116]
    XOR ECX, 115
    OR ECX, EAX
    JNE L1000038fe_2
    JMP L1000038fe_12     ; inserted

; Entry 1000038fe; block 16; address 1000039ce
L1000038fe_16:
    ADD RSP, 2176
    POP RBX
    POP R14
    POP RBP
    RET 

; Entry 1000038fe; block 17; address 1000039da
L1000038fe_17:
    CALL ___stack_chk_fail



; ---------------------
; Function: 0x1000039df
; ---------------------
; Entry 1000039df; block 0; address 1000039df
L1000039df_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH RBX
    PUSH RAX
    IMUL RDI, qword [rel L__DATA___bss + 8]
    MOV RDX, RDI
    SHL RDX, 9
    CMP byte [rel L__DATA___bss + 0], 0
    CMOVNE RDX, RDI
    XOR EAX, EAX
    CMP dword [rel L__DATA___bss + 4], 2
    SETE AL
    LEA R9D, [RAX * 8 * 7]
    LEA RCX, [rel L__TEXT___cstring_0]
    LEA RBX, [RBP + -13]
    MOV ESI, 5
    MOV RDI, RBX
    MOV R8D, 32
    CALL _humanize_number
    JMP L1000039df_1     ; inserted

; Entry 1000039df; block 1; address 100003a31
L1000039df_1:
    LEA RDI, [rel L__TEXT___cstring_446]
    MOV RSI, RBX
    XOR EAX, EAX
    CALL _printf
    JMP L1000039df_2     ; inserted

; Entry 1000039df; block 2; address 100003a42
L1000039df_2:
    ADD RSP, 8
    POP RBX
    POP RBP
    RET 



; ---------------------
; Function: 0x100003a49
; ---------------------
; Entry 100003a49; block 0; address 100003a49
L100003a49_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH RBX
    PUSH RAX
    JMP L100003a49_5     ; inserted

; Entry 100003a49; block 1; address 100003a5b
L100003a49_1:
    MOV RAX, qword [RBX + 8]
    MOV qword [rel L__DATA___bss + 112], RAX
    MOV RDI, qword [RBX]
    CALL _free
    JMP L100003a49_3     ; inserted

; Entry 100003a49; block 2; address 100003a78
L100003a49_2:
    ADD RSP, 8
    POP RBX
    POP RBP
    RET 

; Entry 100003a49; block 3; address 100003a6e
L100003a49_3:
    MOV RDI, RBX
    CALL _free
    JMP L100003a49_4     ; inserted

; Entry 100003a49; block 4; address 100003a76
L100003a49_4:
    JMP L100003a49_5

; Entry 100003a49; block 5; address 100003a4f
L100003a49_5:
    MOV RBX, qword [rel L__DATA___bss + 112]
    TEST RBX, RBX
    JE L100003a49_2
    JMP L100003a49_1     ; inserted




section .data
L__TEXT___cstring_0: db ``, 0
L__TEXT___cstring_1: db `+AB:HI:LPasd:cghklmnrt:x`, 0
L__TEXT___cstring_26: db `invalid argument to option B: %s`, 0
L__TEXT___cstring_59: db `invalid argument to option d: %s`, 0
L__TEXT___cstring_92: db `invalid threshold: %s`, 0
L__TEXT___cstring_114: db `vfs.nspace.prevent_materialization`, 0
L__TEXT___cstring_149: db `fts_open`, 0
L__TEXT___cstring_158: db `\t%s\n`, 0
L__TEXT___cstring_163: db `%jd\t%s\n`, 0
L__TEXT___cstring_171: db `bin/du`, 0
L__TEXT___cstring_178: db `unix2003`, 0
L__TEXT___cstring_187: db `Can't follow symlink cycle from %s to %s`, 0
L__TEXT___cstring_228: db `%s: %s`, 0
L__TEXT___cstring_235: db `Too many symlinks at %s`, 0
L__TEXT___cstring_259: db `fts_read`, 0
L__TEXT___cstring_268: db `%jd\ttotal\n`, 0
L__TEXT___cstring_279: db `si`, 0
L__TEXT___cstring_282: db `No memory for hardlink detection`, 0
L__TEXT___cstring_315: db `No more memory for tracking hard links`, 0
L__TEXT___cstring_354: db `No memory for directory hardlink detection`, 0
L__TEXT___cstring_397: db `No more memory for tracking directory hard links`, 0
L__TEXT___cstring_446: db `%4s`, 0
L__TEXT___cstring_450: db `usage: du [-Aclnx] [-H | -L | -P] [-g | -h | -k | -m] [-a | -s | -d depth] [-B blocksize] [-I mask] [-t threshold] [file ...]\n`, 0
L__TEXT___cstring_577: db `cannot allocate memory`, 0
L__TEXT___cstring_600: db `fd`, 0
L__TEXT___cstring_603: db `\ttotal`, 0

section .bss
L__DATA___bss: resb 120


section .data
L_DATASECTION:
db 02eh
db 00h


section .data
L_JUMP_TABLE_100002a5f:
dq 0x100002a61
dq 0x100002d15
dq 0x100002bd2
dq 0x100002bde
dq 0x100002d15
dq 0x100002d15
dq 0x100002c32
dq 0x100002bb7
dq 0x100002d15
dq 0x100002d15
dq 0x100002b9d
dq 0x100002c26
dq 0x100002b74
dq 0x100002bc6
dq 0x100002d15
dq 0x100002d15
dq 0x100002d15
dq 0x100002a28
dq 0x100002c4c
dq 0x100002c63
dq 0x100002d15
dq 0x100002d15
dq 0x100002d15
dq 0x100002c5a
dq 0x100002d15
dq 0x100002d15
dq 0x100002d15
dq 0x100002d15
dq 0x100002d15
dq 0x100002d15
dq 0x100002d15
L_JUMP_TABLE_100002a82:
dq 0x100002a84
dq 0x100002ac3
dq 0x100002d15
dq 0x100002d15
dq 0x100002d15
dq 0x100002d15
dq 0x100002d15
dq 0x100002ab1
dq 0x100002b07
dq 0x100002d15
dq 0x100002d15
L_JUMP_TABLE_100002ee8:
dq 0x100002eea
dq 0x100002f0f
dq 0x100002f92
dq 0x1000030d9
dq 0x100002f92
dq 0x100002f2f
dq 0x1000030d9
dq 0x100002f92
dq 0x100002f92
dq 0x1000030d9
dq 0x100002f92
dq 0x100002f92
