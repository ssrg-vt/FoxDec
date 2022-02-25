extern ___error
extern ___sprintf_chk
extern ___stack_chk_fail
extern ___strncpy_chk
extern _atof
extern _fopen
extern _fprintf
extern _free
extern _getc
extern _malloc
extern _printf
extern _realloc
extern _strcmp
extern _strerror
extern ___stack_chk_guard
extern ___stderrp
extern dyld_stub_binder


section .text

default rel

; ---------------------
; Function: 0x100000910
; ---------------------
; Entry 100000910; block 0; address 100000910
L100000910_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH RBX
    PUSH RAX
    CMP EDI, 2
    JNE L100000910_2
    JMP L100000910_1     ; inserted

; Entry 100000910; block 1; address 10000091b
L100000910_1:
    MOV RDI, qword [RSI + 8]
    CALL L100000980_0
    JMP L100000910_3     ; inserted

; Entry 100000910; block 2; address 10000094a
L100000910_2:
    MOV RAX, qword [rel ___stderrp]
    MOV RDI, qword [RAX]
    MOV RDX, qword [RSI]
    LEA RSI, [rel L__TEXT___cstring_0]
    XOR EAX, EAX
    CALL _fprintf
    JMP L100000910_7     ; inserted

; Entry 100000910; block 3; address 100000924
L100000910_3:
    XOR EBX, EBX
    TEST RAX, RAX
    JE L100000910_5
    JMP L100000910_4     ; inserted

; Entry 100000910; block 4; address 10000092b
L100000910_4:
    MULSS XMM0, dword [rel L__TEXT___const + 0]
    CVTSS2SD XMM0, XMM0
    LEA RDI, [rel L__TEXT___cstring_24]
    MOV RSI, RAX
    MOV AL, 1
    CALL _printf
    JMP L100000910_6     ; inserted

; Entry 100000910; block 5; address 10000096a
L100000910_5:
    MOV EAX, EBX
    ADD RSP, 8
    POP RBX
    POP RBP
    RET 

; Entry 100000910; block 6; address 100000948
L100000910_6:
    JMP L100000910_5

; Entry 100000910; block 7; address 100000965
L100000910_7:
    MOV EBX, 4294967295
    JMP L100000910_5     ; inserted



; ---------------------
; Function: 0x100000980
; ---------------------
; Entry 100000980; block 0; address 100000980
L100000980_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R12
    PUSH RBX
    SUB RSP, 16
    MOV RBX, RDI
    LEA RSI, [rel L__TEXT___cstring_41]
    CALL _fopen
    JMP L100000980_1     ; inserted

; Entry 100000980; block 1; address 10000099e
L100000980_1:
    TEST RAX, RAX
    JE L100000980_3
    JMP L100000980_2     ; inserted

; Entry 100000980; block 2; address 1000009a7
L100000980_2:
    MOV RDI, RAX
    CALL L100000b00_0
    JMP L100000980_4     ; inserted

; Entry 100000980; block 3; address 100000a63
L100000980_3:
    MOV RAX, qword [rel ___stderrp]
    MOV R15, qword [RAX]
    CALL ___error
    JMP L100000980_16     ; inserted

; Entry 100000980; block 4; address 1000009af
L100000980_4:
    MOV RDI, RAX
    CALL L100000ed0_0
    JMP L100000980_5     ; inserted

; Entry 100000980; block 5; address 1000009b7
L100000980_5:
    MOV RBX, RAX
    XOR R12D, R12D
    XORPS XMM0, XMM0
    LEA R14, [rel L__TEXT___cstring_63]
    LEA R15, [rel L__TEXT___cstring_71]
    JMP L100000980_9

; Entry 100000980; block 6; address 1000009d5
L100000980_6:
    MOV RAX, qword [RBX + 40]
    MOV RCX, qword [RBX + 32]
    MOV RDX, qword [RBX + 24]
    MOV RSI, qword [RBX + 16]
    MOV R8, qword [RBX]
    MOV RDI, qword [RBX + 8]
    PUSH RAX
    PUSH RCX
    PUSH RDX
    PUSH RSI
    PUSH RDI
    PUSH R8
    CALL L100001ae0_0
    JMP L100000980_8     ; inserted

; Entry 100000980; block 7; address 100000a4d
L100000980_7:
    XORPS XMM0, XMM0
    CVTSI2SS XMM0, R12D
    MOVSS XMM1, dword [RBP + -36]
    DIVSS XMM1, XMM0
    MOVAPS XMM0, XMM1
    JMP L100000980_19

; Entry 100000980; block 8; address 1000009f8
L100000980_8:
    ADD RSP, 48
    MOVSS XMM1, dword [RBP + -36]
    ADDSS XMM1, XMM0
    MOVAPS XMM0, XMM1
    INC R12D
    ADD RBX, 48
    JMP L100000980_9     ; inserted

; Entry 100000980; block 9; address 100000a0f
L100000980_9:
    MOVSS dword [RBP + -36], XMM0
    MOV EAX, dword [RBX + 8]
    CMP EAX, 6
    JNE L100000980_10
    JMP L100000980_11     ; inserted

; Entry 100000980; block 10; address 1000009d0
L100000980_10:
    CMP EAX, 11
    JE L100000980_7
    JMP L100000980_6     ; inserted

; Entry 100000980; block 11; address 100000a1c
L100000980_11:
    MOV RAX, qword [RBX]
    TEST RAX, RAX
    JE L100000980_6
    JMP L100000980_12     ; inserted

; Entry 100000980; block 12; address 100000a24
L100000980_12:
    CMP dword [RAX + 8], 9
    JNE L100000980_6
    JMP L100000980_13     ; inserted

; Entry 100000980; block 13; address 100000a2a
L100000980_13:
    MOV RDI, qword [RAX + 16]
    MOV RSI, R15
    CALL _strcmp
    JMP L100000980_14     ; inserted

; Entry 100000980; block 14; address 100000a36
L100000980_14:
    TEST EAX, EAX
    JNE L100000980_6
    JMP L100000980_15     ; inserted

; Entry 100000980; block 15; address 100000a3a
L100000980_15:
    MOV RAX, qword [RBX + 40]
    MOV R14, qword [RAX + 16]
    ADD RBX, 48
    MOVSS XMM0, dword [RBP + -36]
    JMP L100000980_9

; Entry 100000980; block 16; address 100000a72
L100000980_16:
    MOV EDI, dword [RAX]
    CALL _strerror
    JMP L100000980_17     ; inserted

; Entry 100000980; block 17; address 100000a79
L100000980_17:
    LEA RSI, [rel L__TEXT___cstring_43]
    XOR R14D, R14D
    MOV RDI, R15
    MOV RDX, RBX
    MOV RCX, RAX
    XOR EAX, EAX
    CALL _fprintf
    JMP L100000980_18     ; inserted

; Entry 100000980; block 18; address 100000a93
L100000980_18:
    XORPS XMM0, XMM0
    JMP L100000980_19     ; inserted

; Entry 100000980; block 19; address 100000a96
L100000980_19:
    MOV RAX, R14
    ADD RSP, 16
    POP RBX
    POP R12
    POP R14
    POP R15
    POP RBP
    RET 



; ---------------------
; Function: 0x100000b00
; ---------------------
; Entry 100000b00; block 0; address 100000b00
L100000b00_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 72
    MOV R14, RDI
    MOV EDI, 4096
    CALL _malloc
    JMP L100000b00_1     ; inserted

; Entry 100000b00; block 1; address 100000b1e
L100000b00_1:
    MOV qword [RBP + -64], RAX
    MOV EAX, 1
    MOV qword [RBP + -72], RAX
    MOV EBX, 11
    MOV EAX, 128
    MOV qword [RBP + -80], RAX
    XOR R13D, R13D
    MOV EAX, 128
    MOV qword [RBP + -88], RAX
    XOR EAX, EAX
    MOV qword [RBP + -104], RAX
    JMP L100000b00_7

; Entry 100000b00; block 2; address 100000b62
L100000b00_2:
    MOV R12D, EAX
    CMP EAX, 10
    JE L100000b00_4
    JMP L100000b00_3     ; inserted

; Entry 100000b00; block 3; address 100000b6e
L100000b00_3:
    LEA ECX, [R12 + -40]
    LEA EAX, [R12 + -65]
    CMP ECX, 22
    JB L100000b00_6
    JMP L100000b00_5     ; inserted

; Entry 100000b00; block 4; address 100000c07
L100000b00_4:
    INC qword [RBP + -72]
    MOV R12D, 10
    MOV R15D, 10
    CMP R15D, EBX
    JE L100000b00_20
    JMP L100000b00_21     ; inserted

; Entry 100000b00; block 5; address 100000b7d
L100000b00_5:
    CMP EAX, 57
    JA L100000b00_9
    JMP L100000b00_6     ; inserted

; Entry 100000b00; block 6; address 100000b82
L100000b00_6:
    CMP ECX, 21
    JA L100000b00_11
    JMP L100000b00_10     ; inserted

; Entry 100000b00; block 7; address 100000b5a
L100000b00_7:
    MOV RDI, R14
    CALL _getc
    JMP L100000b00_2     ; inserted

; Entry 100000b00; block 8; address 100000d51
L100000b00_8:
    MOV R15D, 11
    CMP R15D, EBX
    JE L100000b00_20
    JMP L100000b00_48     ; inserted

; Entry 100000b00; block 9; address 100000b50
L100000b00_9:
    CMP R12D, 18446744073709551615
    JE L100000b00_8
    JMP L100000b00_7     ; inserted

; Entry 100000b00; block 10; address 100000b87
L100000b00_10:
    XOR R15D, R15D
    LEA RDX, [rel L__TEXT___text + 1324]
    MOVSXD RCX, dword [RDX + RCX * 4]
    ADD RCX, RDX
    MOV RCX, QWORD PTR [L_JUMP_TABLE_100000b98 + 8*ECX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RCX; TARGETS: 100000ba5,100000d65,100000d79,100000d8d,100000be0,100000da1,100000b9a,100000db5,100000be0,100000be0,100000be0,100000be0,100000be0,100000be0,100000be0,100000be0,100000be0,100000be0,100000be0,100000be0,100000be0

; Entry 100000b00; block 11; address 100000be0
L100000b00_11:
    LEA ECX, [R12 + -48]
    CMP ECX, 10
    JB L100000b00_29
    JMP L100000b00_28     ; inserted

; Entry 100000b00; block 12; address 100000b9a
L100000b00_12:
    CMP EBX, 7
    JNE L100000b00_7
    JMP L100000b00_19     ; inserted

; Entry 100000b00; block 13; address 100000ba5
L100000b00_13:
    CMP R15D, EBX
    JNE L100000b00_21
    JMP L100000b00_20     ; inserted

; Entry 100000b00; block 14; address 100000d65
L100000b00_14:
    MOV R15D, 1
    CMP R15D, EBX
    JE L100000b00_20
    JMP L100000b00_49     ; inserted

; Entry 100000b00; block 15; address 100000d79
L100000b00_15:
    MOV R15D, 5
    CMP R15D, EBX
    JE L100000b00_20
    JMP L100000b00_50     ; inserted

; Entry 100000b00; block 16; address 100000d8d
L100000b00_16:
    MOV R15D, 3
    CMP R15D, EBX
    JE L100000b00_20
    JMP L100000b00_51     ; inserted

; Entry 100000b00; block 17; address 100000da1
L100000b00_17:
    MOV R15D, 2
    CMP R15D, EBX
    JE L100000b00_20
    JMP L100000b00_52     ; inserted

; Entry 100000b00; block 18; address 100000db5
L100000b00_18:
    MOV R15D, 4
    CMP R15D, EBX
    JE L100000b00_20
    JMP L100000b00_53     ; inserted

; Entry 100000b00; block 19; address 100000b9f
L100000b00_19:
    MOV R15D, 7
    JMP L100000b00_13     ; inserted

; Entry 100000b00; block 20; address 100000baa
L100000b00_20:
    CMP EBX, 7
    MOV RSI, qword [RBP + -80]
    JE L100000b00_23
    JMP L100000b00_22     ; inserted

; Entry 100000b00; block 21; address 100000c1c
L100000b00_21:
    CMP EBX, 9
    JNE L100000b00_32
    JMP L100000b00_31     ; inserted

; Entry 100000b00; block 22; address 100000bb3
L100000b00_22:
    CMP EBX, 9
    JNE L100000b00_24
    JMP L100000b00_23     ; inserted

; Entry 100000b00; block 23; address 100000bbc
L100000b00_23:
    CMP R13, RSI
    JNE L100000b00_26
    JMP L100000b00_25     ; inserted

; Entry 100000b00; block 24; address 100000c59
L100000b00_24:
    MOV R13D, EBX
    MOVSS XMM0, dword [rel L__TEXT___const + 4]
    JMP L100000b00_33     ; inserted

; Entry 100000b00; block 25; address 100000bc5
L100000b00_25:
    SUB RSI, 18446744073709551488
    MOV RDI, qword [RBP + -48]
    MOV qword [RBP + -80], RSI
    CALL _realloc
    JMP L100000b00_27     ; inserted

; Entry 100000b00; block 26; address 100000d38
L100000b00_26:
    MOV RAX, qword [RBP + -48]
    JMP L100000b00_47     ; inserted

; Entry 100000b00; block 27; address 100000bd6
L100000b00_27:
    JMP L100000b00_47

; Entry 100000b00; block 28; address 100000bea
L100000b00_28:
    CMP EAX, 58
    JAE L100000b00_7
    JMP L100000b00_29     ; inserted

; Entry 100000b00; block 29; address 100000bf3
L100000b00_29:
    XOR EAX, EAX
    CMP ECX, 9
    SETA AL
    LEA R15D, [RAX + RAX + 7]
    CMP R15D, EBX
    JE L100000b00_20
    JMP L100000b00_30     ; inserted

; Entry 100000b00; block 30; address 100000c05
L100000b00_30:
    JMP L100000b00_21

; Entry 100000b00; block 31; address 100000c21
L100000b00_31:
    MOV R13D, 8
    MOVSS XMM0, dword [rel L__TEXT___const + 4]
    TEST R15D, R15D
    JE L100000b00_33
    JMP L100000b00_32     ; inserted

; Entry 100000b00; block 32; address 100000c34
L100000b00_32:
    MOV R13D, EBX
    MOVSS XMM0, dword [rel L__TEXT___const + 4]
    CMP EBX, 7
    JNE L100000b00_33
    JMP L100000b00_34     ; inserted

; Entry 100000b00; block 33; address 100000c64
L100000b00_33:
    MOV RBX, qword [RBP + -104]
    MOV RSI, qword [RBP + -88]
    CMP RBX, RSI
    JNE L100000b00_37
    JMP L100000b00_36     ; inserted

; Entry 100000b00; block 34; address 100000c44
L100000b00_34:
    MOV RDI, qword [RBP + -48]
    CALL _atof
    JMP L100000b00_35     ; inserted

; Entry 100000b00; block 35; address 100000c4d
L100000b00_35:
    CVTSD2SS XMM0, XMM0
    MOV R13D, 7
    JMP L100000b00_33

; Entry 100000b00; block 36; address 100000c71
L100000b00_36:
    SUB RSI, 18446744073709551488
    MOV RDI, qword [RBP + -64]
    MOV qword [RBP + -88], RSI
    MOVSS dword [RBP + -52], XMM0
    CALL _realloc
    JMP L100000b00_38     ; inserted

; Entry 100000b00; block 37; address 100000c90
L100000b00_37:
    MOV RDX, qword [RBP + -48]
    CMP R13D, 11
    JE L100000b00_40
    JMP L100000b00_39     ; inserted

; Entry 100000b00; block 38; address 100000c87
L100000b00_38:
    MOVSS XMM0, dword [RBP + -52]
    MOV qword [RBP + -64], RAX
    JMP L100000b00_37     ; inserted

; Entry 100000b00; block 39; address 100000c9a
L100000b00_39:
    LEA RAX, [RBX + 1]
    SHL RBX, 5
    MOV RCX, qword [RBP + -64]
    MOV dword [RCX + RBX], R13D
    MOV qword [RCX + RBX + 8], RDX
    MOVSS dword [RCX + RBX + 16], XMM0
    MOV RDX, qword [RBP + -96]
    MOV qword [RCX + RBX + 24], RDX
    MOV RBX, RAX
    JMP L100000b00_40     ; inserted

; Entry 100000b00; block 40; address 100000cc1
L100000b00_40:
    CMP R15D, 7
    MOV qword [RBP + -104], RBX
    JE L100000b00_42
    JMP L100000b00_41     ; inserted

; Entry 100000b00; block 41; address 100000ccb
L100000b00_41:
    CMP R15D, 11
    JE L100000b00_44
    JMP L100000b00_43     ; inserted

; Entry 100000b00; block 42; address 100000d01
L100000b00_42:
    MOV EAX, 128
    MOV qword [RBP + -80], RAX
    MOV EDI, 128
    CALL _malloc
    JMP L100000b00_46     ; inserted

; Entry 100000b00; block 43; address 100000cd5
L100000b00_43:
    MOV EDI, 2
    CALL _malloc
    JMP L100000b00_45     ; inserted

; Entry 100000b00; block 44; address 100000ddd
L100000b00_44:
    MOV RSI, qword [RBP + -88]
    CMP RBX, RSI
    JNE L100000b00_55
    JMP L100000b00_54     ; inserted

; Entry 100000b00; block 45; address 100000cdf
L100000b00_45:
    MOV byte [RAX], R12B
    MOV R13D, 1
    MOV qword [RBP + -48], RAX
    MOV byte [RAX + R13], 0
    MOV RAX, qword [RBP + -72]
    MOV qword [RBP + -96], RAX
    MOV EBX, R15D
    JMP L100000b00_7

; Entry 100000b00; block 46; address 100000d14
L100000b00_46:
    MOV byte [RAX], R12B
    MOV EBX, 7
    MOV R13D, 1
    MOV qword [RBP + -48], RAX
    MOV byte [RAX + R13], 0
    MOV RAX, qword [RBP + -72]
    MOV qword [RBP + -96], RAX
    JMP L100000b00_7

; Entry 100000b00; block 47; address 100000d3c
L100000b00_47:
    MOV byte [RAX + R13], R12B
    INC R13
    MOV qword [RBP + -48], RAX
    MOV byte [RAX + R13], 0
    JMP L100000b00_7

; Entry 100000b00; block 48; address 100000d60
L100000b00_48:
    JMP L100000b00_21

; Entry 100000b00; block 49; address 100000d74
L100000b00_49:
    JMP L100000b00_21

; Entry 100000b00; block 50; address 100000d88
L100000b00_50:
    JMP L100000b00_21

; Entry 100000b00; block 51; address 100000d9c
L100000b00_51:
    JMP L100000b00_21

; Entry 100000b00; block 52; address 100000db0
L100000b00_52:
    JMP L100000b00_21

; Entry 100000b00; block 53; address 100000dc4
L100000b00_53:
    JMP L100000b00_21

; Entry 100000b00; block 54; address 100000de6
L100000b00_54:
    INC RSI
    MOV RDI, qword [RBP + -64]
    MOVSS dword [RBP + -52], XMM0
    CALL _realloc
    JMP L100000b00_56     ; inserted

; Entry 100000b00; block 55; address 100000e00
L100000b00_55:
    SHL RBX, 5
    MOV RCX, qword [RBP + -64]
    MOV dword [RCX + RBX], 11
    MOV qword [RCX + RBX + 8], 0
    MOVSS dword [RCX + RBX + 16], XMM0
    MOV RAX, qword [RBP + -72]
    MOV qword [RCX + RBX + 24], RAX
    MOV RAX, RCX
    ADD RSP, 72
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100000b00; block 56; address 100000df7
L100000b00_56:
    MOVSS XMM0, dword [RBP + -52]
    MOV qword [RBP + -64], RAX
    JMP L100000b00_55     ; inserted



; ---------------------
; Function: 0x100000ed0
; ---------------------
; Entry 100000ed0; block 0; address 100000ed0
L100000ed0_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 168
    MOV RBX, RDI
    MOV qword [RBP + -48], 0
    MOV EDI, 6144
    CALL _malloc
    JMP L100000ed0_1     ; inserted

; Entry 100000ed0; block 1; address 100000ef9
L100000ed0_1:
    MOV ESI, 128
    LEA R15, [RBP + -104]
    XOR EDI, EDI
    LEA R13, [RBP + -48]
    LEA R12, [RBP + -200]
    CMP RDI, RSI
    JNE L100000ed0_3
    JMP L100000ed0_2     ; inserted

; Entry 100000ed0; block 2; address 100000f14
L100000ed0_2:
    NOP word [CS:RAX + RAX]
    NOP 
    JMP L100000ed0_16     ; inserted

; Entry 100000ed0; block 3; address 100000f40
L100000ed0_3:
    MOV qword [RBP + -56], RSI
    JMP L100000ed0_5     ; inserted

; Entry 100000ed0; block 4; address 100000f33
L100000ed0_4:
    MOV RDI, R14
    JMP L100000ed0_5

; Entry 100000ed0; block 5; address 100000f44
L100000ed0_5:
    MOV qword [RBP + -120], RDI
    LEA R14, [RDI + RDI * 2]
    SHL R14, 4
    MOV qword [RBP + -64], RAX
    ADD R14, RAX
    LEA RDI, [RBP + -112]
    MOV RSI, RBX
    MOV RDX, R13
    CALL L100001440_0
    JMP L100000ed0_6     ; inserted

; Entry 100000ed0; block 6; address 100000f66
L100000ed0_6:
    NOP word [CS:RAX + RAX]
    JMP L100000ed0_12     ; inserted

; Entry 100000ed0; block 7; address 100000f85
L100000ed0_7:
    LEA RCX, [RAX + 1]
    MOV qword [RBP + -48], RCX
    SHL RAX, 5
    MOV RCX, qword [RBX + RAX + 24]
    MOV qword [RBP + -128], RCX
    MOV RCX, qword [RBX + RAX + 16]
    MOV qword [RBP + -136], RCX
    MOV RCX, qword [RBX + RAX]
    MOV RAX, qword [RBX + RAX + 8]
    MOV qword [RBP + -144], RAX
    MOV qword [RBP + -152], RCX
    MOV RDI, R12
    MOV RSI, RBX
    MOV RDX, R13
    CALL L100001440_0
    JMP L100000ed0_9     ; inserted

; Entry 100000ed0; block 8; address 100001090
L100000ed0_8:
    MOV RDI, qword [RBP + -120]
    INC RDI
    MOV RCX, qword [RBP + -72]
    MOV qword [R14 + 40], RCX
    MOV RCX, qword [RBP + -80]
    MOV qword [R14 + 32], RCX
    MOV RCX, qword [RBP + -88]
    MOV qword [R14 + 24], RCX
    MOV RCX, qword [RBP + -96]
    MOV qword [R14 + 16], RCX
    MOV RCX, qword [RBP + -112]
    MOV RDX, qword [RBP + -104]
    MOV qword [R14 + 8], RDX
    MOV qword [R14], RCX
    MOV RCX, RAX
    SHL RCX, 5
    CMP dword [RBX + RCX], 10
    JNE L100000ed0_14
    JMP L100000ed0_13     ; inserted

; Entry 100000ed0; block 9; address 100000fcb
L100000ed0_9:
    MOV EDI, 48
    CALL _malloc
    JMP L100000ed0_10     ; inserted

; Entry 100000ed0; block 10; address 100000fd5
L100000ed0_10:
    MOV RCX, qword [RBP + -112]
    MOV RDX, qword [RBP + -104]
    MOV qword [RAX], RCX
    MOV qword [RAX + 8], RDX
    MOV RCX, qword [RBP + -96]
    MOV qword [RAX + 16], RCX
    MOV RCX, qword [RBP + -88]
    MOV qword [RAX + 24], RCX
    MOV RCX, qword [RBP + -80]
    MOV qword [RAX + 32], RCX
    MOV RCX, qword [RBP + -72]
    MOV qword [RAX + 40], RCX
    MOV qword [RBP + -112], RAX
    MOV EDI, 48
    CALL _malloc
    JMP L100000ed0_11     ; inserted

; Entry 100000ed0; block 11; address 100001012
L100000ed0_11:
    MOV RCX, qword [RBP + -160]
    MOV qword [RAX + 40], RCX
    MOV RCX, qword [RBP + -168]
    MOV qword [RAX + 32], RCX
    MOV RCX, qword [RBP + -176]
    MOV qword [RAX + 24], RCX
    MOV RCX, qword [RBP + -184]
    MOV qword [RAX + 16], RCX
    MOV RCX, qword [RBP + -200]
    MOV RDX, qword [RBP + -192]
    MOV qword [RAX + 8], RDX
    MOV qword [RAX], RCX
    MOV RCX, qword [RBP + -128]
    MOV qword [R15 + 24], RCX
    MOV RCX, qword [RBP + -136]
    MOV qword [R15 + 16], RCX
    MOV RCX, qword [RBP + -152]
    MOV RDX, qword [RBP + -144]
    MOV qword [R15 + 8], RDX
    MOV qword [R15], RCX
    MOV qword [RBP + -72], RAX
    JMP L100000ed0_12

; Entry 100000ed0; block 12; address 100000f70
L100000ed0_12:
    MOV RAX, qword [RBP + -48]
    MOV RCX, RAX
    SHL RCX, 5
    CMP dword [RBX + RCX], 6
    JNE L100000ed0_8
    JMP L100000ed0_7     ; inserted

; Entry 100000ed0; block 13; address 1000010d3
L100000ed0_13:
    INC RAX
    MOV qword [RBP + -48], RAX
    MOV RAX, qword [RBP + -64]
    MOV RSI, qword [RBP + -56]
    CMP RDI, RSI
    JNE L100000ed0_3
    JMP L100000ed0_15     ; inserted

; Entry 100000ed0; block 14; address 1000010f0
L100000ed0_14:
    MOV RSI, qword [RBP + -56]
    CMP RDI, RSI
    MOV RAX, qword [RBP + -64]
    JNE L100000ed0_18
    JMP L100000ed0_17     ; inserted

; Entry 100000ed0; block 15; address 1000010eb
L100000ed0_15:
    JMP L100000ed0_16

; Entry 100000ed0; block 16; address 100000f20
L100000ed0_16:
    SUB RSI, 18446744073709551488
    MOV R14, RDI
    MOV RDI, RAX
    MOV qword [RBP + -56], RSI
    CALL _realloc
    JMP L100000ed0_4     ; inserted

; Entry 100000ed0; block 17; address 1000010fd
L100000ed0_17:
    INC RSI
    MOV RDI, RAX
    CALL _realloc
    JMP L100000ed0_18     ; inserted

; Entry 100000ed0; block 18; address 100001108
L100000ed0_18:
    ADD RSP, 168
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 



; ---------------------
; Function: 0x100001440
; ---------------------
; Entry 100001440; block 0; address 100001440
L100001440_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    PUSH RBX
    SUB RSP, 168
    MOV R15, RDX
    MOV RBX, RSI
    MOV R14, RDI
    CALL L100001940_0
    JMP L100001440_1     ; inserted

; Entry 100001440; block 1; address 100001462
L100001440_1:
    MOV RAX, qword [R15]
    MOV RCX, RAX
    SHL RCX, 5
    MOV ECX, dword [RBX + RCX]
    MOV EDX, ECX
    AND EDX, 4294967294
    CMP EDX, 4
    JNE L100001440_3
    JMP L100001440_2     ; inserted

; Entry 100001440; block 2; address 10000147d
L100001440_2:
    LEA R13, [R14 + 8]
    LEA R12, [RBP + -136]
    NOP dword [RAX + RAX]
    JMP L100001440_7     ; inserted

; Entry 100001440; block 3; address 100001582
L100001440_3:
    AND ECX, 4294967294
    CMP ECX, 2
    JNE L100001440_9
    JMP L100001440_8     ; inserted

; Entry 100001440; block 4; address 1000014cc
L100001440_4:
    MOV EDI, 48
    CALL _malloc
    JMP L100001440_5     ; inserted

; Entry 100001440; block 5; address 1000014d6
L100001440_5:
    MOV RCX, qword [R14]
    MOV RDX, qword [R14 + 8]
    MOV qword [RAX], RCX
    MOV qword [RAX + 8], RDX
    MOV RCX, qword [R14 + 16]
    MOV qword [RAX + 16], RCX
    MOV RCX, qword [R14 + 24]
    MOV qword [RAX + 24], RCX
    MOV RCX, qword [R14 + 32]
    MOV qword [RAX + 32], RCX
    MOV RCX, qword [R14 + 40]
    MOV qword [RAX + 40], RCX
    MOV qword [R14], RAX
    MOV EDI, 48
    CALL _malloc
    JMP L100001440_6     ; inserted

; Entry 100001440; block 6; address 100001511
L100001440_6:
    MOV RCX, qword [RBP + -96]
    MOV qword [RAX + 40], RCX
    MOV RCX, qword [RBP + -104]
    MOV qword [RAX + 32], RCX
    MOV RCX, qword [RBP + -112]
    MOV qword [RAX + 24], RCX
    MOV RCX, qword [RBP + -120]
    MOV qword [RAX + 16], RCX
    MOV RCX, qword [RBP + -136]
    MOV RDX, qword [RBP + -128]
    MOV qword [RAX + 8], RDX
    MOV qword [RAX], RCX
    MOV RCX, qword [RBP + -64]
    MOV qword [R13 + 24], RCX
    MOV RCX, qword [RBP + -72]
    MOV qword [R13 + 16], RCX
    MOV RCX, qword [RBP + -88]
    MOV RDX, qword [RBP + -80]
    MOV qword [R13 + 8], RDX
    MOV qword [R13], RCX
    MOV qword [R14 + 40], RAX
    MOV RAX, qword [R15]
    MOV RCX, RAX
    SHL RCX, 5
    MOV ECX, dword [RBX + RCX]
    MOV EDX, ECX
    AND EDX, 4294967294
    CMP EDX, 4
    JE L100001440_7
    JMP L100001440_3     ; inserted

; Entry 100001440; block 7; address 100001490
L100001440_7:
    LEA RCX, [RAX + 1]
    MOV qword [R15], RCX
    SHL RAX, 5
    MOV RCX, qword [RBX + RAX + 24]
    MOV qword [RBP + -64], RCX
    MOV RCX, qword [RBX + RAX + 16]
    MOV qword [RBP + -72], RCX
    MOV RCX, qword [RBX + RAX]
    MOV RAX, qword [RBX + RAX + 8]
    MOV qword [RBP + -80], RAX
    MOV qword [RBP + -88], RCX
    MOV RDI, R12
    MOV RSI, RBX
    MOV RDX, R15
    CALL L100001940_0
    JMP L100001440_4     ; inserted

; Entry 100001440; block 8; address 10000158e
L100001440_8:
    LEA R12, [RBP + -80]
    LEA RCX, [R14 + 8]
    MOV qword [RBP + -144], RCX
    LEA R13, [RBP + -136]
    JMP L100001440_13

; Entry 100001440; block 9; address 1000017d2
L100001440_9:
    MOV RAX, R14
    ADD RSP, 168
    POP RBX
    POP R12
    POP R13
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100001440; block 10; address 1000016bc
L100001440_10:
    NOP dword [RAX]
    JMP L100001440_19     ; inserted

; Entry 100001440; block 11; address 1000015ba
L100001440_11:
    MOV RCX, qword [R14]
    MOV RDX, qword [R14 + 8]
    MOV qword [RAX], RCX
    MOV qword [RAX + 8], RDX
    MOV RCX, qword [R14 + 16]
    MOV qword [RAX + 16], RCX
    MOV RCX, qword [R14 + 24]
    MOV qword [RAX + 24], RCX
    MOV RCX, qword [R14 + 32]
    MOV qword [RAX + 32], RCX
    MOV RCX, qword [R14 + 40]
    MOV qword [RAX + 40], RCX
    MOV qword [R14], RAX
    MOV EDI, 48
    CALL _malloc
    JMP L100001440_12     ; inserted

; Entry 100001440; block 12; address 1000015f5
L100001440_12:
    MOV RCX, qword [RBP + -48]
    MOV qword [RAX + 40], RCX
    MOV RCX, qword [RBP + -56]
    MOV qword [RAX + 32], RCX
    MOV RCX, qword [RBP + -64]
    MOV qword [RAX + 24], RCX
    MOV RCX, qword [RBP + -72]
    MOV qword [RAX + 16], RCX
    MOV RCX, qword [RBP + -88]
    MOV RDX, qword [RBP + -80]
    MOV qword [RAX + 8], RDX
    MOV qword [RAX], RCX
    MOV RCX, qword [RBP + -152]
    MOV RSI, qword [RBP + -144]
    MOV qword [RSI + 24], RCX
    MOV RCX, qword [RBP + -160]
    MOV qword [RSI + 16], RCX
    MOV RCX, qword [RBP + -176]
    MOV RDX, qword [RBP + -168]
    MOV qword [RSI + 8], RDX
    MOV qword [RSI], RCX
    MOV qword [R14 + 40], RAX
    MOV RAX, qword [R15]
    MOV RCX, RAX
    SHL RCX, 5
    MOV ECX, dword [RBX + RCX]
    AND ECX, 4294967294
    CMP ECX, 2
    JNE L100001440_9
    JMP L100001440_13     ; inserted

; Entry 100001440; block 13; address 100001673
L100001440_13:
    LEA RCX, [RAX + 1]
    MOV qword [R15], RCX
    SHL RAX, 5
    MOV RCX, qword [RBX + RAX + 24]
    MOV qword [RBP + -152], RCX
    MOV RCX, qword [RBX + RAX + 16]
    MOV qword [RBP + -160], RCX
    MOV RCX, qword [RBX + RAX]
    MOV RAX, qword [RBX + RAX + 8]
    MOV qword [RBP + -168], RAX
    MOV qword [RBP + -176], RCX
    LEA RDI, [RBP + -88]
    MOV RSI, RBX
    MOV RDX, R15
    CALL L100001940_0
    JMP L100001440_10     ; inserted

; Entry 100001440; block 14; address 1000015b0
L100001440_14:
    MOV EDI, 48
    CALL _malloc
    JMP L100001440_11     ; inserted

; Entry 100001440; block 15; address 1000016d9
L100001440_15:
    LEA RCX, [RAX + 1]
    MOV qword [R15], RCX
    SHL RAX, 5
    MOV RCX, qword [RBX + RAX + 24]
    MOV qword [RBP + -184], RCX
    MOV RCX, qword [RBX + RAX + 16]
    MOV qword [RBP + -192], RCX
    MOV RCX, qword [RBX + RAX]
    MOV RAX, qword [RBX + RAX + 8]
    MOV qword [RBP + -200], RAX
    MOV qword [RBP + -208], RCX
    MOV RDI, R13
    MOV RSI, RBX
    MOV RDX, R15
    CALL L100001940_0
    JMP L100001440_16     ; inserted

; Entry 100001440; block 16; address 100001721
L100001440_16:
    MOV EDI, 48
    CALL _malloc
    JMP L100001440_17     ; inserted

; Entry 100001440; block 17; address 10000172b
L100001440_17:
    MOV RCX, qword [RBP + -88]
    MOV RDX, qword [RBP + -80]
    MOV qword [RAX], RCX
    MOV qword [RAX + 8], RDX
    MOV RCX, qword [RBP + -72]
    MOV qword [RAX + 16], RCX
    MOV RCX, qword [RBP + -64]
    MOV qword [RAX + 24], RCX
    MOV RCX, qword [RBP + -56]
    MOV qword [RAX + 32], RCX
    MOV RCX, qword [RBP + -48]
    MOV qword [RAX + 40], RCX
    MOV qword [RBP + -88], RAX
    MOV EDI, 48
    CALL _malloc
    JMP L100001440_18     ; inserted

; Entry 100001440; block 18; address 100001768
L100001440_18:
    MOV RCX, qword [RBP + -96]
    MOV qword [RAX + 40], RCX
    MOV RCX, qword [RBP + -104]
    MOV qword [RAX + 32], RCX
    MOV RCX, qword [RBP + -112]
    MOV qword [RAX + 24], RCX
    MOV RCX, qword [RBP + -120]
    MOV qword [RAX + 16], RCX
    MOV RCX, qword [RBP + -136]
    MOV RDX, qword [RBP + -128]
    MOV qword [RAX + 8], RDX
    MOV qword [RAX], RCX
    MOV RCX, qword [RBP + -184]
    MOV qword [R12 + 24], RCX
    MOV RCX, qword [RBP + -192]
    MOV qword [R12 + 16], RCX
    MOV RCX, qword [RBP + -208]
    MOV RDX, qword [RBP + -200]
    MOV qword [R12 + 8], RDX
    MOV qword [R12], RCX
    MOV qword [RBP + -48], RAX
    JMP L100001440_19

; Entry 100001440; block 19; address 1000016c0
L100001440_19:
    MOV RAX, qword [R15]
    MOV RCX, RAX
    SHL RCX, 5
    MOV ECX, dword [RBX + RCX]
    AND ECX, 4294967294
    CMP ECX, 4
    JNE L100001440_14
    JMP L100001440_15     ; inserted



; ---------------------
; Function: 0x100001940
; ---------------------
; Entry 100001940; block 0; address 100001940
L100001940_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R15
    PUSH R14
    PUSH R12
    PUSH RBX
    SUB RSP, 48
    MOV R14, RDX
    MOV RBX, RDI
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    MOV qword [RBP + -40], RAX
    MOV RAX, qword [RDX]
    MOV RCX, RAX
    SHL RCX, 5
    LEA R12, [RSI + RCX]
    MOV ECX, dword [RSI + RCX]
    CMP ECX, 9
    JE L100001940_2
    JMP L100001940_1     ; inserted

; Entry 100001940; block 1; address 100001979
L100001940_1:
    MOV R15, RSI
    CMP ECX, 8
    JE L100001940_4
    JMP L100001940_3     ; inserted

; Entry 100001940; block 2; address 100001986
L100001940_2:
    INC RAX
    MOV qword [R14], RAX
    MOV qword [RBX], 0
    MOV qword [RBX + 40], 0
    MOV RAX, qword [R12]
    MOV RCX, qword [R12 + 8]
    MOV qword [RBX + 8], RAX
    MOV qword [RBX + 16], RCX
    MOV RAX, qword [R12 + 16]
    MOV qword [RBX + 24], RAX
    MOV RAX, qword [R12 + 24]
    MOV qword [RBX + 32], RAX
    JMP L100001940_14

; Entry 100001940; block 3; address 100001981
L100001940_3:
    CMP ECX, 7
    JNE L100001940_5
    JMP L100001940_2     ; inserted

; Entry 100001940; block 4; address 1000019c3
L100001940_4:
    INC RAX
    MOV qword [R14], RAX
    JMP L100001940_5     ; inserted

; Entry 100001940; block 5; address 1000019c9
L100001940_5:
    MOV RCX, RAX
    SHL RCX, 5
    CMP dword [R15 + RCX], 0
    JE L100001940_7
    JMP L100001940_6     ; inserted

; Entry 100001940; block 6; address 1000019d7
L100001940_6:
    MOV qword [RBX], 0
    MOV dword [RBX + 8], 11
    MOV RAX, qword [RBP + -68]
    MOV RCX, qword [RBP + -60]
    MOV qword [RBX + 12], RAX
    MOV qword [RBX + 20], RCX
    MOV RAX, qword [RBP + -52]
    MOV qword [RBX + 28], RAX
    MOV EAX, dword [RBP + -44]
    MOV dword [RBX + 36], EAX
    JMP L100001940_13

; Entry 100001940; block 7; address 100001a08
L100001940_7:
    INC RAX
    MOV qword [R14], RAX
    MOV RDI, RBX
    MOV RSI, R15
    MOV RDX, R14
    CALL L100001440_0
    JMP L100001940_8     ; inserted

; Entry 100001940; block 8; address 100001a1c
L100001940_8:
    MOV RAX, qword [R14]
    MOV RCX, RAX
    SHL RCX, 5
    CMP dword [R15 + RCX], 0
    JE L100001940_10
    JMP L100001940_9     ; inserted

; Entry 100001940; block 9; address 100001a2d
L100001940_9:
    INC RAX
    MOV qword [R14], RAX
    JMP L100001940_11

; Entry 100001940; block 10; address 100001a35
L100001940_10:
    MOV RAX, qword [rel ___stderrp]
    MOV RDI, qword [RAX]
    LEA RSI, [rel L__TEXT___cstring_235]
    XOR EAX, EAX
    CALL _fprintf
    JMP L100001940_11     ; inserted

; Entry 100001940; block 11; address 100001a4d
L100001940_11:
    MOV EDI, 48
    CALL _malloc
    JMP L100001940_12     ; inserted

; Entry 100001940; block 12; address 100001a57
L100001940_12:
    MOV RCX, qword [RBX + 40]
    MOV qword [RAX + 40], RCX
    MOV RCX, qword [RBX + 32]
    MOV qword [RAX + 32], RCX
    MOV RCX, qword [RBX + 24]
    MOV qword [RAX + 24], RCX
    MOV RCX, qword [RBX + 16]
    MOV qword [RAX + 16], RCX
    MOV RCX, qword [RBX]
    MOV RDX, qword [RBX + 8]
    MOV qword [RAX + 8], RDX
    MOV qword [RAX], RCX
    MOV qword [RBX], RAX
    MOV RAX, qword [R12 + 16]
    MOV qword [RBX + 24], RAX
    MOV RAX, qword [R12]
    MOV RCX, qword [R12 + 8]
    MOV qword [RBX + 16], RCX
    MOV qword [RBX + 8], RAX
    MOV RAX, qword [R12 + 24]
    MOV qword [RBX + 32], RAX
    JMP L100001940_13     ; inserted

; Entry 100001940; block 13; address 100001aab
L100001940_13:
    MOV qword [RBX + 40], 0
    JMP L100001940_14     ; inserted

; Entry 100001940; block 14; address 100001ab3
L100001940_14:
    MOV RAX, qword [rel ___stack_chk_guard]
    MOV RAX, qword [RAX]
    CMP RAX, qword [RBP + -40]
    JNE L100001940_16
    JMP L100001940_15     ; inserted

; Entry 100001940; block 15; address 100001ac3
L100001940_15:
    MOV RAX, RBX
    ADD RSP, 48
    POP RBX
    POP R12
    POP R14
    POP R15
    POP RBP
    RET 

; Entry 100001940; block 16; address 100001ad3
L100001940_16:
    CALL ___stack_chk_fail



; ---------------------
; Function: 0x100001ae0
; ---------------------
; Entry 100001ae0; block 0; address 100001ae0
L100001ae0_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH R14
    PUSH RBX
    SUB RSP, 16
    LEA R14, [RBP + 16]
    MOV RAX, qword [RBP + 16]
    TEST RAX, RAX
    JE L100001ae0_2
    JMP L100001ae0_1     ; inserted

; Entry 100001ae0; block 1; address 100001af8
L100001ae0_1:
    MOV RCX, qword [RAX + 40]
    MOV RDX, qword [RAX + 32]
    MOV RSI, qword [RAX + 24]
    MOV RDI, qword [RAX + 16]
    MOV RBX, qword [RAX]
    MOV RAX, qword [RAX + 8]
    PUSH RCX
    PUSH RDX
    PUSH RSI
    PUSH RDI
    PUSH RAX
    PUSH RBX
    CALL L100001ae0_0
    JMP L100001ae0_3     ; inserted

; Entry 100001ae0; block 2; address 100001b23
L100001ae0_2:
    MOV RAX, qword [R14 + 40]
    TEST RAX, RAX
    JE L100001ae0_5
    JMP L100001ae0_4     ; inserted

; Entry 100001ae0; block 3; address 100001b1a
L100001ae0_3:
    ADD RSP, 48
    MOVAPS XMM1, XMM0
    JMP L100001ae0_2

; Entry 100001ae0; block 4; address 100001b2c
L100001ae0_4:
    MOV RCX, qword [RAX + 40]
    MOV RDX, qword [RAX + 32]
    MOV RSI, qword [RAX + 24]
    MOV RDI, qword [RAX + 16]
    MOV RBX, qword [RAX]
    MOV RAX, qword [RAX + 8]
    PUSH RCX
    PUSH RDX
    PUSH RSI
    PUSH RDI
    PUSH RAX
    PUSH RBX
    MOVSS dword [RBP + -20], XMM1
    CALL L100001ae0_0
    JMP L100001ae0_6     ; inserted

; Entry 100001ae0; block 5; address 100001b5e
L100001ae0_5:
    MOV EAX, dword [R14 + 8]
    ADD EAX, 18446744073709551614
    CMP EAX, 6
    JA L100001ae0_8
    JMP L100001ae0_7     ; inserted

; Entry 100001ae0; block 6; address 100001b53
L100001ae0_6:
    MOVSS XMM1, dword [RBP + -20]
    ADD RSP, 48
    JMP L100001ae0_5

; Entry 100001ae0; block 7; address 100001b6e
L100001ae0_7:
    LEA RCX, [rel L__TEXT___text + 5124]
    MOVSXD RAX, dword [RCX + RAX * 4]
    ADD RAX, RCX
    MOV RAX, QWORD PTR [L_JUMP_TABLE_100001b7c + 8*EAX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 100001b7e,100001b87,100001b90,100001b99,100001ba2,100001bc1

; Entry 100001ae0; block 8; address 100001d05
L100001ae0_8:
    MOVAPS XMM0, XMM1
    ADD RSP, 16
    POP RBX
    POP R14
    POP RBP
    RET 

; Entry 100001ae0; block 9; address 100001b7e
L100001ae0_9:
    SUBSS XMM1, XMM0
    JMP L100001ae0_8

; Entry 100001ae0; block 10; address 100001b87
L100001ae0_10:
    ADDSS XMM1, XMM0
    JMP L100001ae0_8

; Entry 100001ae0; block 11; address 100001b90
L100001ae0_11:
    DIVSS XMM1, XMM0
    JMP L100001ae0_8

; Entry 100001ae0; block 12; address 100001b99
L100001ae0_12:
    MULSS XMM1, XMM0
    JMP L100001ae0_8

; Entry 100001ae0; block 13; address 100001ba2
L100001ae0_13:
    UCOMISS XMM1, XMM0
    XORPS XMM1, XMM1
    JNE L100001ae0_8
    JMP L100001ae0_15     ; inserted

; Entry 100001ae0; block 14; address 100001bc1
L100001ae0_14:
    MOVSS XMM1, dword [R14 + 24]
    JMP L100001ae0_8

; Entry 100001ae0; block 15; address 100001bae
L100001ae0_15:
    JP L100001ae0_8
    JMP L100001ae0_16     ; inserted

; Entry 100001ae0; block 16; address 100001bb4
L100001ae0_16:
    MOVSS XMM1, dword [rel L__TEXT___const + 8]
    JMP L100001ae0_8




section .data
L__TEXT___cstring_0: db `Usage: %s <input file>\n`, 0
L__TEXT___cstring_24: db `%s scored %.02f\n`, 0
L__TEXT___cstring_41: db `r`, 0
L__TEXT___cstring_43: db `Cannot open %s: %s\n`, 0
L__TEXT___cstring_63: db `Unknown`, 0
L__TEXT___cstring_71: db `name`, 0
L__TEXT___cstring_76: db `LEFT_PAREN`, 0
L__TEXT___cstring_87: db `RIGHT_PAREN`, 0
L__TEXT___cstring_99: db `MINUS`, 0
L__TEXT___cstring_105: db `PLUS`, 0
L__TEXT___cstring_110: db `DIVIDE`, 0
L__TEXT___cstring_117: db `MULTIPLY`, 0
L__TEXT___cstring_126: db `EQUALS`, 0
L__TEXT___cstring_133: db `NUMBER`, 0
L__TEXT___cstring_140: db `FUNCTION`, 0
L__TEXT___cstring_149: db `IDENTIFIER`, 0
L__TEXT___cstring_160: db `NEW_LINE`, 0
L__TEXT___cstring_169: db `TEOF`, 0
L__TEXT___cstring_174: db `Token %s on line %ld: "%s" %f`, 0
L__TEXT___cstring_204: db `NULL`, 0
L__TEXT___cstring_209: db `%f`, 0
L__TEXT___cstring_212: db `%s(%s)`, 0
L__TEXT___cstring_219: db `(%s)`, 0
L__TEXT___cstring_224: db `(%s %s %s)`, 0
L__TEXT___cstring_235: db `Expected ')' after expression`, 0
L__TEXT___cstring_265: db `sin`, 0
L__TEXT___cstring_269: db `cos`, 0
L__TEXT___cstring_273: db `Function %s is not implemented\n`, 0


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


section .data
L_JUMP_TABLE_100000b98:
dq L100000b00_13
dq L100000b00_14
dq L100000b00_15
dq L100000b00_16
dq L100000b00_11
dq L100000b00_17
dq L100000b00_12
dq L100000b00_18
dq L100000b00_11
dq L100000b00_11
dq L100000b00_11
dq L100000b00_11
dq L100000b00_11
dq L100000b00_11
dq L100000b00_11
dq L100000b00_11
dq L100000b00_11
dq L100000b00_11
dq L100000b00_11
dq L100000b00_11
dq L100000b00_11
L_JUMP_TABLE_100001b7c:
dq L100001ae0_9
dq L100001ae0_10
dq L100001ae0_11
dq L100001ae0_12
dq L100001ae0_13
dq L100001ae0_14
