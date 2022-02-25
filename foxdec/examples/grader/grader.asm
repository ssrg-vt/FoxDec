extern free@GLIBC_2.2.5
extern __errno_location@GLIBC_2.2.5
extern strncpy@GLIBC_2.2.5
extern strtod@GLIBC_2.2.5
extern printf@GLIBC_2.2.5
extern fprintf@GLIBC_2.2.5
extern malloc@GLIBC_2.2.5
extern _IO_getc@GLIBC_2.2.5
extern realloc@GLIBC_2.2.5
extern fopen@GLIBC_2.2.5
extern sprintf@GLIBC_2.2.5
extern strerror@GLIBC_2.2.5
extern stderr@GLIBC_2.2.5


section .text

default rel

global main

; --------------------------------------
; Function: __errno_location@GLIBC_2.2.5
; --------------------------------------
; Entry 830; block 0; address 830
L830_0:
    JMP __errno_location@GLIBC_2.2.5 wrt ..plt



; ----------------------------
; Function: strtod@GLIBC_2.2.5
; ----------------------------
; Entry 850; block 0; address 850
L850_0:
    JMP strtod@GLIBC_2.2.5 wrt ..plt



; ----------------------------
; Function: printf@GLIBC_2.2.5
; ----------------------------
; Entry 860; block 0; address 860
L860_0:
    JMP printf@GLIBC_2.2.5 wrt ..plt



; -----------------------------
; Function: fprintf@GLIBC_2.2.5
; -----------------------------
; Entry 870; block 0; address 870
L870_0:
    JMP fprintf@GLIBC_2.2.5 wrt ..plt



; ----------------------------
; Function: malloc@GLIBC_2.2.5
; ----------------------------
; Entry 880; block 0; address 880
L880_0:
    JMP malloc@GLIBC_2.2.5 wrt ..plt



; ------------------------------
; Function: _IO_getc@GLIBC_2.2.5
; ------------------------------
; Entry 890; block 0; address 890
L890_0:
    JMP _IO_getc@GLIBC_2.2.5 wrt ..plt



; -----------------------------
; Function: realloc@GLIBC_2.2.5
; -----------------------------
; Entry 8a0; block 0; address 8a0
L8a0_0:
    JMP realloc@GLIBC_2.2.5 wrt ..plt



; ---------------------------
; Function: fopen@GLIBC_2.2.5
; ---------------------------
; Entry 8b0; block 0; address 8b0
L8b0_0:
    JMP fopen@GLIBC_2.2.5 wrt ..plt



; ------------------------------
; Function: strerror@GLIBC_2.2.5
; ------------------------------
; Entry 8d0; block 0; address 8d0
L8d0_0:
    JMP strerror@GLIBC_2.2.5 wrt ..plt



; ---------------
; Function: 0x8f0
; ---------------
; Entry 8f0; block 0; address 8f0
L8f0_0:
main:
    SUB RSP, 8
    CMP EDI, 2
    JNE L8f0_2
    JMP L8f0_1     ; inserted

; Entry 8f0; block 1; address 8f9
L8f0_1:
    MOV RDI, qword [RSI + 8]
    CALL La80_0
    JMP L8f0_3     ; inserted

; Entry 8f0; block 2; address 92e
L8f0_2:
    MOV RDX, qword [RSI]
    MOV RDI, qword [rel stderr@GLIBC_2.2.5]
    LEA RSI, [rel L_.rodata + 4]
    XOR EAX, EAX
    CALL L870_0
    JMP L8f0_6     ; inserted

; Entry 8f0; block 3; address 902
L8f0_3:
    TEST RAX, RAX
    JE L8f0_5
    JMP L8f0_4     ; inserted

; Entry 8f0; block 4; address 907
L8f0_4:
    MULSS XMM0, dword [rel L_.rodata + 48]
    LEA RDI, [rel L_.rodata + 28]
    MOV RSI, RAX
    MOV EAX, 1
    CVTSS2SD XMM0, XMM0
    CALL L860_0
    JMP L8f0_5     ; inserted

; Entry 8f0; block 5; address 927
L8f0_5:
    XOR EAX, EAX
    JMP L8f0_7     ; inserted

; Entry 8f0; block 6; address 946
L8f0_6:
    OR EAX, 4294967295
    JMP L8f0_7

; Entry 8f0; block 7; address 929
L8f0_7:
    ADD RSP, 8
    RET 






; ---------------
; Function: 0xa80
; ---------------
; Entry a80; block 0; address a80
La80_0:
    PUSH R13
    PUSH R12
    LEA RSI, [rel L_.rodata + 60]
    PUSH RBP
    PUSH RBX
    MOV RBX, RDI
    SUB RSP, 24
    CALL L8b0_0
    JMP La80_1     ; inserted

; Entry a80; block 1; address a99
La80_1:
    TEST RAX, RAX
    JE La80_3
    JMP La80_2     ; inserted

; Entry a80; block 2; address aa2
La80_2:
    MOV RDI, RAX
    CALL Lcc0_0
    JMP La80_4     ; inserted

; Entry a80; block 3; address b80
La80_3:
    CALL L830_0
    JMP La80_17     ; inserted

; Entry a80; block 4; address aaa
La80_4:
    MOV RDI, RAX
    CALL L19d0_0
    JMP La80_5     ; inserted

; Entry a80; block 5; address ab2
La80_5:
    MOV EDX, dword [RAX + 8]
    CMP EDX, 11
    JE La80_7
    JMP La80_6     ; inserted

; Entry a80; block 6; address abe
La80_6:
    MOV RBX, RAX
    PXOR XMM1, XMM1
    LEA R12, [rel L_.rodata + 52]
    XOR EBP, EBP
    LEA R13, [rel L_.rodata + 82]
    JMP La80_9

; Entry a80; block 7; address bad
La80_7:
    PXOR XMM0, XMM0
    LEA R12, [rel L_.rodata + 52]
    MOVAPS XMM1, XMM0
    JMP La80_21

; Entry a80; block 8; address b04
La80_8:
    MOV EDX, dword [RBX + 8]
    MOVSS XMM1, dword [RSP + 60]
    ADD RSP, 48
    ADDSS XMM1, XMM0
    CMP EDX, 11
    JE La80_10
    JMP La80_9     ; inserted

; Entry a80; block 9; address b1a
La80_9:
    CMP EDX, 6
    JNE La80_11
    JMP La80_12     ; inserted

; Entry a80; block 10; address b58
La80_10:
    PXOR XMM0, XMM0
    CVTSI2SS XMM0, EBP
    JMP La80_21     ; inserted

; Entry a80; block 11; address ae0
La80_11:
    MOVSS dword [RSP + 12], XMM1
    PUSH qword [RBX + 40]
    PUSH qword [RBX + 32]
    PUSH qword [RBX + 24]
    PUSH qword [RBX + 16]
    ADD RBX, 48
    PUSH qword [RBX + -40]
    ADD EBP, 1
    PUSH qword [RBX + -48]
    CALL L1af0_0
    JMP La80_8     ; inserted

; Entry a80; block 12; address b1f
La80_12:
    MOV RDX, qword [RBX]
    TEST RDX, RDX
    JE La80_11
    JMP La80_13     ; inserted

; Entry a80; block 13; address b27
La80_13:
    CMP dword [RDX + 8], 9
    JNE La80_11
    JMP La80_14     ; inserted

; Entry a80; block 14; address b2d
La80_14:
    MOV RSI, qword [RDX + 16]
    MOV ECX, 5
    MOV RDI, R13
    REPZ CMPSB 
    JNE La80_11
    JMP La80_15     ; inserted

; Entry a80; block 15; address b3d
La80_15:
    MOV RAX, qword [RBX + 40]
    ADD RBX, 48
    MOV EDX, dword [RBX + 8]
    CMP EDX, 11
    MOV R12, qword [RAX + 16]
    JNE La80_9
    JMP La80_16     ; inserted

; Entry a80; block 16; address b51
La80_16:
    NOP dword [RAX]
    JMP La80_10     ; inserted

; Entry a80; block 17; address b85
La80_17:
    MOV EDI, dword [RAX]
    CALL L8d0_0
    JMP La80_18     ; inserted

; Entry a80; block 18; address b8c
La80_18:
    MOV RDI, qword [rel stderr@GLIBC_2.2.5]
    LEA RSI, [rel L_.rodata + 62]
    MOV RCX, RAX
    MOV RDX, RBX
    XOR EAX, EAX
    CALL L870_0
    JMP La80_19     ; inserted

; Entry a80; block 19; address ba7
La80_19:
    XOR EAX, EAX
    XOR EDX, EDX
    JMP La80_20

; Entry a80; block 20; address b6b
La80_20:
    MOV dword [RSP + 12], EDX
    MOVSS XMM0, dword [RSP + 12]
    ADD RSP, 24
    POP RBX
    POP RBP
    POP R12
    POP R13
    RET 

; Entry a80; block 21; address b60
La80_21:
    MOV RAX, R12
    DIVSS XMM1, XMM0
    MOVD EDX, XMM1
    JMP La80_20     ; inserted



; ---------------
; Function: 0xcc0
; ---------------
; Entry cc0; block 0; address cc0
Lcc0_0:
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    XOR R13D, R13D
    PUSH RBP
    PUSH RBX
    MOV R12D, 11
    XOR EBP, EBP
    SUB RSP, 72
    MOV qword [RSP + 8], RDI
    MOV EDI, 4096
    CALL L880_0
    JMP Lcc0_1     ; inserted

; Entry cc0; block 1; address ce8
Lcc0_1:
    MOV qword [RSP + 24], 1
    MOV qword [RSP + 40], RAX
    MOV qword [RSP + 48], 128
    MOV qword [RSP + 32], 128
    NOP dword [RAX + RAX]
    JMP Lcc0_8     ; inserted

; Entry cc0; block 2; address d1a
Lcc0_2:
    CMP EAX, 10
    MOV R14D, EAX
    JE Lcc0_4
    JMP Lcc0_3     ; inserted

; Entry cc0; block 3; address d26
Lcc0_3:
    LEA ECX, [RAX + -65]
    LEA EAX, [RAX + -40]
    CMP EAX, 21
    JBE Lcc0_6
    JMP Lcc0_5     ; inserted

; Entry cc0; block 4; address fb0
Lcc0_4:
    ADD qword [RSP + 24], 1
    MOV R15D, 10
    JMP Lcc0_37

; Entry cc0; block 5; address d35
Lcc0_5:
    CMP ECX, 57
    JBE Lcc0_6
    JMP Lcc0_7     ; inserted

; Entry cc0; block 6; address de0
Lcc0_6:
    CMP EAX, 21
    JA Lcc0_22
    JMP Lcc0_21     ; inserted

; Entry cc0; block 7; address d3e
Lcc0_7:
    CMP R14D, 18446744073709551615
    JNE Lcc0_8
    JMP Lcc0_9     ; inserted

; Entry cc0; block 8; address d10
Lcc0_8:
    MOV RDI, qword [RSP + 8]
    CALL L890_0
    JMP Lcc0_2     ; inserted

; Entry cc0; block 9; address d44
Lcc0_9:
    MOV R15D, 11
    NOP word [RAX + RAX]
    JMP Lcc0_37     ; inserted

; Entry cc0; block 10; address d5a
Lcc0_10:
    CMP qword [RSP + 32], RBP
    MOVSS XMM0, dword [rel L_.rodata + 352]
    JE Lcc0_13
    JMP Lcc0_12     ; inserted

; Entry cc0; block 11; address e10
Lcc0_11:
    XOR ESI, ESI
    MOV RDI, RBX
    CALL L850_0
    JMP Lcc0_31     ; inserted

; Entry cc0; block 12; address d6d
Lcc0_12:
    CMP R12D, 11
    JE Lcc0_15
    JMP Lcc0_14     ; inserted

; Entry cc0; block 13; address e30
Lcc0_13:
    SUB qword [RSP + 32], 18446744073709551488
    MOV RDI, qword [RSP + 40]
    MOVSS dword [RSP + 60], XMM0
    MOV RAX, qword [RSP + 32]
    MOV RSI, RAX
    CALL L8a0_0
    JMP Lcc0_33     ; inserted

; Entry cc0; block 14; address d77
Lcc0_14:
    MOV RAX, RBP
    MOV RSI, qword [RSP + 16]
    ADD RBP, 1
    SHL RAX, 5
    ADD RAX, qword [RSP + 40]
    CMP R15D, 11
    MOV dword [RAX], R12D
    MOV qword [RAX + 8], RBX
    MOVSS dword [RAX + 16], XMM0
    MOV qword [RAX + 24], RSI
    MOV RAX, qword [RSP + 24]
    MOV qword [RSP + 16], RAX
    JE Lcc0_17
    JMP Lcc0_16     ; inserted

; Entry cc0; block 15; address e68
Lcc0_15:
    MOV RAX, qword [RSP + 24]
    CMP R15D, 11
    MOV qword [RSP + 16], RAX
    JNE Lcc0_16
    JMP Lcc0_17     ; inserted

; Entry cc0; block 16; address db0
Lcc0_16:
    LEA EAX, [R15 + -7]
    CMP EAX, 1
    JBE Lcc0_19
    JMP Lcc0_18     ; inserted

; Entry cc0; block 17; address e7c
Lcc0_17:
    CMP RBP, qword [RSP + 32]
    JE Lcc0_36
    JMP Lcc0_35     ; inserted

; Entry cc0; block 18; address dbd
Lcc0_18:
    MOV EDI, 2
    MOV R12D, R15D
    MOV R13D, 1
    CALL L880_0
    JMP Lcc0_20     ; inserted

; Entry cc0; block 19; address fc8
Lcc0_19:
    MOV EDI, 128
    MOV R12D, R15D
    MOV R13D, 1
    CALL L880_0
    JMP Lcc0_48     ; inserted

; Entry cc0; block 20; address dd0
Lcc0_20:
    MOV byte [RAX], R14B
    MOV RBX, RAX
    JMP Lcc0_43     ; inserted

; Entry cc0; block 21; address de9
Lcc0_21:
    MOV R10D, EAX
    LEA RAX, [rel L_JUMP_TABLE_df7]
    LEA RAX, [RAX + 8*R10]
    MOV RAX, QWORD [RAX]
    ; MOV RAX, QWORD PTR [L_JUMP_TABLE_df7 + 8*EAX] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: ed8,ec8,f50,f40,f70,f30,f08,e00,f60

; Entry cc0; block 22; address f70
Lcc0_22:
    LEA EAX, [R14 + -48]
    CMP EAX, 9
    JA Lcc0_45
    JMP Lcc0_44     ; inserted

; Entry cc0; block 23; address e00
Lcc0_23:
    CMP R12D, 7
    MOV R15D, 4
    JNE Lcc0_10
    JMP Lcc0_11     ; inserted

; Entry cc0; block 24; address ec8
Lcc0_24:
    MOV R15D, 1
    JMP Lcc0_37

; Entry cc0; block 25; address ed8
Lcc0_25:
    XOR R15D, R15D
    CMP R12D, 9
    JNE Lcc0_37
    JMP Lcc0_38     ; inserted

; Entry cc0; block 26; address f08
Lcc0_26:
    CMP R12D, 7
    JNE Lcc0_8
    JMP Lcc0_40     ; inserted

; Entry cc0; block 27; address f30
Lcc0_27:
    MOV R15D, 2
    JMP Lcc0_37

; Entry cc0; block 28; address f40
Lcc0_28:
    MOV R15D, 3
    JMP Lcc0_37

; Entry cc0; block 29; address f50
Lcc0_29:
    MOV R15D, 5
    JMP Lcc0_37

; Entry cc0; block 30; address f60
Lcc0_30:
    MOV R15D, 6
    JMP Lcc0_37

; Entry cc0; block 31; address e1a
Lcc0_31:
    CVTSD2SS XMM0, XMM0
    CMP qword [RSP + 32], RBP
    JNE Lcc0_14
    JMP Lcc0_32     ; inserted

; Entry cc0; block 32; address e29
Lcc0_32:
    NOP dword [RAX]
    JMP Lcc0_13     ; inserted

; Entry cc0; block 33; address e4e
Lcc0_33:
    CMP R12D, 11
    MOV qword [RSP + 40], RAX
    MOVSS XMM0, dword [RSP + 60]
    JNE Lcc0_14
    JMP Lcc0_34     ; inserted

; Entry cc0; block 34; address e63
Lcc0_34:
    NOP dword [RAX + RAX]
    JMP Lcc0_15     ; inserted

; Entry cc0; block 35; address e87
Lcc0_35:
    SHL RBP, 5
    ADD RBP, qword [RSP + 40]
    MOV RAX, qword [RSP + 24]
    MOV dword [RBP], 11
    MOV qword [RBP + 24], RAX
    MOVSS dword [RBP + 16], XMM0
    MOV qword [RBP + 8], 0
    MOV RAX, qword [RSP + 40]
    ADD RSP, 72
    POP RBX
    POP RBP
    POP R12
    POP R13
    POP R14
    POP R15
    RET 

; Entry cc0; block 36; address 1008
Lcc0_36:
    MOV RDI, qword [RSP + 40]
    LEA RSI, [RBP + 1]
    MOVSS dword [RSP + 8], XMM0
    CALL L8a0_0
    JMP Lcc0_51     ; inserted

; Entry cc0; block 37; address d50
Lcc0_37:
    CMP R12D, 7
    JE Lcc0_11
    JMP Lcc0_10     ; inserted

; Entry cc0; block 38; address ee5
Lcc0_38:
    CMP qword [RSP + 32], RBP
    MOV R12D, 8
    MOVSS XMM0, dword [rel L_.rodata + 352]
    JNE Lcc0_14
    JMP Lcc0_39     ; inserted

; Entry cc0; block 39; address efe
Lcc0_39:
    JMP Lcc0_13

; Entry cc0; block 40; address f12
Lcc0_40:
    CMP qword [RSP + 48], R13
    JE Lcc0_42
    JMP Lcc0_41     ; inserted

; Entry cc0; block 41; address f19
Lcc0_41:
    MOV byte [RBX + R13], R14B
    ADD R13, 1
    JMP Lcc0_43

; Entry cc0; block 42; address f8f
Lcc0_42:
    SUB qword [RSP + 48], 18446744073709551488
    MOV RDI, RBX
    MOV RAX, qword [RSP + 48]
    MOV RSI, RAX
    CALL L8a0_0
    JMP Lcc0_47     ; inserted

; Entry cc0; block 43; address dd6
Lcc0_43:
    MOV byte [RBX + R13], 0
    JMP Lcc0_8

; Entry cc0; block 44; address f79
Lcc0_44:
    MOV R15D, 7
    JMP Lcc0_50     ; inserted

; Entry cc0; block 45; address ff0
Lcc0_45:
    CMP ECX, 57
    JA Lcc0_8
    JMP Lcc0_49     ; inserted

; Entry cc0; block 46; address f88
Lcc0_46:
    CMP qword [RSP + 48], R13
    JNE Lcc0_41
    JMP Lcc0_42     ; inserted

; Entry cc0; block 47; address fa5
Lcc0_47:
    MOV RBX, RAX
    JMP Lcc0_41

; Entry cc0; block 48; address fdb
Lcc0_48:
    MOV qword [RSP + 48], 128
    MOV byte [RAX], R14B
    MOV RBX, RAX
    JMP Lcc0_43

; Entry cc0; block 49; address ff9
Lcc0_49:
    MOV R15D, 9
    JMP Lcc0_50

; Entry cc0; block 50; address f7f
Lcc0_50:
    CMP R15D, R12D
    JNE Lcc0_37
    JMP Lcc0_46     ; inserted

; Entry cc0; block 51; address 101c
Lcc0_51:
    MOVSS XMM0, dword [RSP + 8]
    MOV qword [RSP + 40], RAX
    JMP Lcc0_35



; ----------------
; Function: 0x1220
; ----------------
; Entry 1220; block 0; address 1220
L1220_0:
    PUSH R13
    PUSH R12
    MOV R12, RDX
    PUSH RBP
    PUSH RBX
    MOV R13, RSI
    MOV RBX, RDI
    SUB RSP, 56
    MOV RAX, qword [RDX]
    MOV RCX, RAX
    SHL RCX, 5
    LEA RBP, [RSI + RCX]
    MOV EDX, dword [RBP]
    LEA ESI, [RDX + -7]
    AND ESI, 4294967293
    JE L1220_2
    JMP L1220_1     ; inserted

; Entry 1220; block 1; address 124c
L1220_1:
    CMP EDX, 8
    JE L1220_4
    JMP L1220_3     ; inserted

; Entry 1220; block 2; address 12a0
L1220_2:
    ADD RAX, 1
    MOV qword [RDI], 0
    MOV qword [R12], RAX
    MOV RAX, qword [RBP + 24]
    MOV ECX, dword [RBP]
    MOV RDX, qword [RBP + 8]
    MOVSS XMM0, dword [RBP + 16]
    MOV qword [RDI + 40], 0
    MOV qword [RDI + 32], RAX
    MOVSS dword [RDI + 24], XMM0
    MOV RAX, RBX
    MOV dword [RDI + 8], ECX
    MOV qword [RDI + 16], RDX
    ADD RSP, 56
    POP RBX
    POP RBP
    POP R12
    POP R13
    RET 

; Entry 1220; block 3; address 1255
L1220_3:
    TEST EDX, EDX
    JE L1220_6
    JMP L1220_5     ; inserted

; Entry 1220; block 4; address 1398
L1220_4:
    MOV EDX, dword [R13 + RCX + 32]
    ADD RAX, 1
    MOV qword [R12], RAX
    TEST EDX, EDX
    JNE L1220_5
    JMP L1220_11     ; inserted

; Entry 1220; block 5; address 125d
L1220_5:
    MOV qword [RBX], 0
    MOV dword [RBX + 8], 11
    MOV RAX, RBX
    MOV qword [RBX + 16], 0
    MOV dword [RBX + 24], 0
    MOV qword [RBX + 32], 0
    MOV qword [RBX + 40], 0
    ADD RSP, 56
    POP RBX
    POP RBP
    POP R12
    POP R13
    RET 

; Entry 1220; block 6; address 12e8
L1220_6:
    ADD RAX, 1
    MOV RDX, R12
    MOV RDI, RSP
    MOV qword [R12], RAX
    MOV RSI, R13
    CALL L15e0_0
    JMP L1220_7     ; inserted

; Entry 1220; block 7; address 12fe
L1220_7:
    MOV RAX, qword [R12]
    MOV RDX, RAX
    SHL RDX, 5
    MOV EDX, dword [R13 + RDX]
    TEST EDX, EDX
    JE L1220_9
    JMP L1220_8     ; inserted

; Entry 1220; block 8; address 1316
L1220_8:
    ADD RAX, 1
    MOV qword [R12], RAX
    JMP L1220_13     ; inserted

; Entry 1220; block 9; address 13b8
L1220_9:
    MOV RDI, qword [rel stderr@GLIBC_2.2.5]
    LEA RSI, [rel L_.rodata + 379]
    XOR EAX, EAX
    CALL L870_0
    JMP L1220_12     ; inserted

; Entry 1220; block 10; address 1328
L1220_10:
    MOV RDX, qword [RSP]
    MOV RDI, qword [RBP]
    MOV RSI, qword [RBP + 8]
    MOV RCX, qword [RBP + 16]
    MOV qword [RBX], RAX
    MOV qword [RAX], RDX
    MOV RDX, qword [RSP + 8]
    MOV qword [RBX + 8], RDI
    MOV qword [RBX + 16], RSI
    MOV qword [RBX + 24], RCX
    MOV qword [RAX + 8], RDX
    MOV RDX, qword [RSP + 16]
    MOV qword [RAX + 16], RDX
    MOV RDX, qword [RSP + 24]
    MOV qword [RAX + 24], RDX
    MOV RDX, qword [RSP + 32]
    MOV qword [RAX + 32], RDX
    MOV RDX, qword [RSP + 40]
    MOV qword [RAX + 40], RDX
    MOV RDX, qword [RBP + 24]
    MOV RAX, RBX
    MOV qword [RBX + 40], 0
    MOV qword [RBX + 32], RDX
    ADD RSP, 56
    POP RBX
    POP RBP
    POP R12
    POP R13
    RET 

; Entry 1220; block 11; address 13ad
L1220_11:
    JMP L1220_6

; Entry 1220; block 12; address 13cd
L1220_12:
    JMP L1220_13

; Entry 1220; block 13; address 131e
L1220_13:
    MOV EDI, 48
    CALL L880_0
    JMP L1220_10     ; inserted



; ----------------
; Function: 0x13e0
; ----------------
; Entry 13e0; block 0; address 13e0
L13e0_0:
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    MOV R12, RDX
    PUSH RBP
    PUSH RBX
    MOV RBP, RSI
    SUB RSP, 168
    MOV qword [RSP + 56], RDI
    LEA RDI, [RSP + 64]
    CALL L1220_0
    JMP L13e0_1     ; inserted

; Entry 13e0; block 1; address 1406
L13e0_1:
    MOV RAX, qword [R12]
    MOV RCX, qword [RSP + 64]
    MOV R13D, dword [RSP + 72]
    MOV R14, qword [RSP + 80]
    MOVSS XMM1, dword [RSP + 88]
    MOV R11, qword [RSP + 96]
    MOV RDX, RAX
    MOV RBX, qword [RSP + 104]
    SHL RDX, 5
    MOV EDX, dword [RBP + RDX]
    SUB EDX, 4
    CMP EDX, 1
    JA L13e0_3
    JMP L13e0_2     ; inserted

; Entry 13e0; block 2; address 1440
L13e0_2:
    LEA RDI, [RSP + 112]
    MOV qword [RSP + 48], RDI
    NOP word [RAX + RAX]
    JMP L13e0_7     ; inserted

; Entry 13e0; block 3; address 15c4
L13e0_3:
    MOV qword [RSP + 16], R11
    MOVAPS XMM0, XMM1
    MOV qword [RSP + 8], R14
    MOV dword [RSP + 4], R13D
    MOV R15, RCX
    JMP L13e0_8

; Entry 13e0; block 4; address 14a4
L13e0_4:
    MOV EDI, 48
    CALL L880_0
    JMP L13e0_5     ; inserted

; Entry 13e0; block 5; address 14ae
L13e0_5:
    MOV RCX, qword [RSP + 40]
    MOV R15, RAX
    MOV dword [RSP + 72], R13D
    MOVSS XMM1, dword [RSP + 36]
    MOV R11, qword [RSP + 24]
    MOVSS dword [RSP + 88], XMM1
    MOV EDI, 48
    MOV qword [RAX], RCX
    MOV RAX, qword [RSP + 72]
    MOV qword [R15 + 16], R14
    MOV qword [R15 + 32], R11
    MOV qword [R15 + 40], RBX
    MOV qword [R15 + 8], RAX
    MOV RAX, qword [RSP + 88]
    MOV qword [R15 + 24], RAX
    CALL L880_0
    JMP L13e0_6     ; inserted

; Entry 13e0; block 6; address 14f7
L13e0_6:
    MOV RBX, RAX
    MOV RAX, qword [RSP + 112]
    MOV RCX, R15
    MOVSS XMM0, dword [RSP + 32]
    MOV R13D, dword [RSP + 4]
    MOV R14, qword [RSP + 8]
    MOVAPS XMM1, XMM0
    MOV qword [RBX], RAX
    MOV RAX, qword [RSP + 120]
    MOV R11, qword [RSP + 16]
    MOV qword [RBX + 8], RAX
    MOV RAX, qword [RSP + 128]
    MOV qword [RBX + 16], RAX
    MOV RAX, qword [RSP + 136]
    MOV qword [RBX + 24], RAX
    MOV RAX, qword [RSP + 144]
    MOV qword [RBX + 32], RAX
    MOV RAX, qword [RSP + 152]
    MOV qword [RBX + 40], RAX
    MOV RAX, qword [R12]
    MOV RSI, RAX
    SHL RSI, 5
    MOV ESI, dword [RBP + RSI]
    SUB ESI, 4
    CMP ESI, 1
    JBE L13e0_7
    JMP L13e0_8     ; inserted

; Entry 13e0; block 7; address 1450
L13e0_7:
    ADD RAX, 1
    MOV RSI, RBP
    MOV qword [RSP + 40], RCX
    MOV qword [R12], RAX
    SHL RAX, 5
    MOVSS dword [RSP + 36], XMM1
    LEA RAX, [RBP + RAX-1]
    MOV qword [RSP + 24], R11
    MOV RDI, qword [RAX + 8]
    MOV EDX, dword [RAX]
    MOVSS XMM0, dword [RAX + 16]
    MOV RAX, qword [RAX + 24]
    MOVSS dword [RSP + 32], XMM0
    MOV qword [RSP + 8], RDI
    MOV RDI, qword [RSP + 48]
    MOV dword [RSP + 4], EDX
    MOV RDX, R12
    MOV qword [RSP + 16], RAX
    CALL L1220_0
    JMP L13e0_4     ; inserted

; Entry 13e0; block 8; address 1571
L13e0_8:
    MOV EAX, dword [RSP + 4]
    MOV RDX, qword [RSP + 56]
    MOVSS dword [RSP + 88], XMM0
    MOV dword [RSP + 72], EAX
    MOV RAX, qword [RSP + 72]
    MOV qword [RDX], R15
    MOV qword [RDX + 40], RBX
    MOV qword [RDX + 8], RAX
    MOV RAX, qword [RSP + 8]
    MOV qword [RDX + 16], RAX
    MOV RAX, qword [RSP + 88]
    MOV qword [RDX + 24], RAX
    MOV RAX, qword [RSP + 16]
    MOV qword [RDX + 32], RAX
    ADD RSP, 168
    MOV RAX, RDX
    POP RBX
    POP RBP
    POP R12
    POP R13
    POP R14
    POP R15
    RET 



; ----------------
; Function: 0x15e0
; ----------------
; Entry 15e0; block 0; address 15e0
L15e0_0:
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    MOV R12, RDX
    PUSH RBP
    PUSH RBX
    MOV RBP, RSI
    SUB RSP, 168
    MOV qword [RSP + 56], RDI
    LEA RDI, [RSP + 64]
    CALL L13e0_0
    JMP L15e0_1     ; inserted

; Entry 15e0; block 1; address 1606
L15e0_1:
    MOV RAX, qword [R12]
    MOV RCX, qword [RSP + 64]
    MOV R13D, dword [RSP + 72]
    MOV R14, qword [RSP + 80]
    MOVSS XMM1, dword [RSP + 88]
    MOV R11, qword [RSP + 96]
    MOV RDX, RAX
    MOV RBX, qword [RSP + 104]
    SHL RDX, 5
    MOV EDX, dword [RBP + RDX]
    SUB EDX, 2
    CMP EDX, 1
    JA L15e0_3
    JMP L15e0_2     ; inserted

; Entry 15e0; block 2; address 1640
L15e0_2:
    LEA RDI, [RSP + 112]
    MOV qword [RSP + 48], RDI
    NOP word [RAX + RAX]
    JMP L15e0_7     ; inserted

; Entry 15e0; block 3; address 17c4
L15e0_3:
    MOV qword [RSP + 16], R11
    MOVAPS XMM0, XMM1
    MOV qword [RSP + 8], R14
    MOV dword [RSP + 4], R13D
    MOV R15, RCX
    JMP L15e0_8

; Entry 15e0; block 4; address 16a4
L15e0_4:
    MOV EDI, 48
    CALL L880_0
    JMP L15e0_5     ; inserted

; Entry 15e0; block 5; address 16ae
L15e0_5:
    MOV RCX, qword [RSP + 40]
    MOV R15, RAX
    MOV dword [RSP + 72], R13D
    MOVSS XMM1, dword [RSP + 36]
    MOV R11, qword [RSP + 24]
    MOVSS dword [RSP + 88], XMM1
    MOV EDI, 48
    MOV qword [RAX], RCX
    MOV RAX, qword [RSP + 72]
    MOV qword [R15 + 16], R14
    MOV qword [R15 + 32], R11
    MOV qword [R15 + 40], RBX
    MOV qword [R15 + 8], RAX
    MOV RAX, qword [RSP + 88]
    MOV qword [R15 + 24], RAX
    CALL L880_0
    JMP L15e0_6     ; inserted

; Entry 15e0; block 6; address 16f7
L15e0_6:
    MOV RBX, RAX
    MOV RAX, qword [RSP + 112]
    MOV RCX, R15
    MOVSS XMM0, dword [RSP + 32]
    MOV R13D, dword [RSP + 4]
    MOV R14, qword [RSP + 8]
    MOVAPS XMM1, XMM0
    MOV qword [RBX], RAX
    MOV RAX, qword [RSP + 120]
    MOV R11, qword [RSP + 16]
    MOV qword [RBX + 8], RAX
    MOV RAX, qword [RSP + 128]
    MOV qword [RBX + 16], RAX
    MOV RAX, qword [RSP + 136]
    MOV qword [RBX + 24], RAX
    MOV RAX, qword [RSP + 144]
    MOV qword [RBX + 32], RAX
    MOV RAX, qword [RSP + 152]
    MOV qword [RBX + 40], RAX
    MOV RAX, qword [R12]
    MOV RSI, RAX
    SHL RSI, 5
    MOV ESI, dword [RBP + RSI]
    SUB ESI, 2
    CMP ESI, 1
    JBE L15e0_7
    JMP L15e0_8     ; inserted

; Entry 15e0; block 7; address 1650
L15e0_7:
    ADD RAX, 1
    MOV RSI, RBP
    MOV qword [RSP + 40], RCX
    MOV qword [R12], RAX
    SHL RAX, 5
    MOVSS dword [RSP + 36], XMM1
    LEA RAX, [RBP + RAX-1]
    MOV qword [RSP + 24], R11
    MOV RDI, qword [RAX + 8]
    MOV EDX, dword [RAX]
    MOVSS XMM0, dword [RAX + 16]
    MOV RAX, qword [RAX + 24]
    MOVSS dword [RSP + 32], XMM0
    MOV qword [RSP + 8], RDI
    MOV RDI, qword [RSP + 48]
    MOV dword [RSP + 4], EDX
    MOV RDX, R12
    MOV qword [RSP + 16], RAX
    CALL L13e0_0
    JMP L15e0_4     ; inserted

; Entry 15e0; block 8; address 1771
L15e0_8:
    MOV EAX, dword [RSP + 4]
    MOV RDX, qword [RSP + 56]
    MOVSS dword [RSP + 88], XMM0
    MOV dword [RSP + 72], EAX
    MOV RAX, qword [RSP + 72]
    MOV qword [RDX], R15
    MOV qword [RDX + 40], RBX
    MOV qword [RDX + 8], RAX
    MOV RAX, qword [RSP + 8]
    MOV qword [RDX + 16], RAX
    MOV RAX, qword [RSP + 88]
    MOV qword [RDX + 24], RAX
    MOV RAX, qword [RSP + 16]
    MOV qword [RDX + 32], RAX
    ADD RSP, 168
    MOV RAX, RDX
    POP RBX
    POP RBP
    POP R12
    POP R13
    POP R14
    POP R15
    RET 



; ----------------
; Function: 0x17e0
; ----------------
; Entry 17e0; block 0; address 17e0
L17e0_0:
    PUSH R15
    PUSH R14
    PUSH R13
    PUSH R12
    MOV R12, RDX
    PUSH RBP
    PUSH RBX
    MOV RBP, RSI
    SUB RSP, 168
    MOV qword [RSP + 56], RDI
    LEA RDI, [RSP + 64]
    CALL L15e0_0
    JMP L17e0_1     ; inserted

; Entry 17e0; block 1; address 1806
L17e0_1:
    MOV RAX, qword [R12]
    MOV RCX, qword [RSP + 64]
    MOV R13D, dword [RSP + 72]
    MOV R14, qword [RSP + 80]
    MOVSS XMM1, dword [RSP + 88]
    MOV R11, qword [RSP + 96]
    MOV RDX, RAX
    MOV RBX, qword [RSP + 104]
    SHL RDX, 5
    CMP dword [RBP + RDX], 6
    JNE L17e0_3
    JMP L17e0_2     ; inserted

; Entry 17e0; block 2; address 183b
L17e0_2:
    LEA RDI, [RSP + 112]
    MOV qword [RSP + 48], RDI
    NOP dword [RAX]
    JMP L17e0_7     ; inserted

; Entry 17e0; block 3; address 19b7
L17e0_3:
    MOV qword [RSP + 16], R11
    MOVAPS XMM0, XMM1
    MOV qword [RSP + 8], R14
    MOV dword [RSP + 4], R13D
    MOV R15, RCX
    JMP L17e0_8

; Entry 17e0; block 4; address 189c
L17e0_4:
    MOV EDI, 48
    CALL L880_0
    JMP L17e0_5     ; inserted

; Entry 17e0; block 5; address 18a6
L17e0_5:
    MOV RCX, qword [RSP + 40]
    MOV R15, RAX
    MOV dword [RSP + 72], R13D
    MOVSS XMM1, dword [RSP + 36]
    MOV R11, qword [RSP + 24]
    MOVSS dword [RSP + 88], XMM1
    MOV EDI, 48
    MOV qword [RAX], RCX
    MOV RAX, qword [RSP + 72]
    MOV qword [R15 + 16], R14
    MOV qword [R15 + 32], R11
    MOV qword [R15 + 40], RBX
    MOV qword [R15 + 8], RAX
    MOV RAX, qword [RSP + 88]
    MOV qword [R15 + 24], RAX
    CALL L880_0
    JMP L17e0_6     ; inserted

; Entry 17e0; block 6; address 18ef
L17e0_6:
    MOV RBX, RAX
    MOV RAX, qword [RSP + 112]
    MOV RCX, R15
    MOVSS XMM0, dword [RSP + 32]
    MOV R13D, dword [RSP + 4]
    MOV R14, qword [RSP + 8]
    MOVAPS XMM1, XMM0
    MOV qword [RBX], RAX
    MOV RAX, qword [RSP + 120]
    MOV R11, qword [RSP + 16]
    MOV qword [RBX + 8], RAX
    MOV RAX, qword [RSP + 128]
    MOV qword [RBX + 16], RAX
    MOV RAX, qword [RSP + 136]
    MOV qword [RBX + 24], RAX
    MOV RAX, qword [RSP + 144]
    MOV qword [RBX + 32], RAX
    MOV RAX, qword [RSP + 152]
    MOV qword [RBX + 40], RAX
    MOV RAX, qword [R12]
    MOV RSI, RAX
    SHL RSI, 5
    CMP dword [RBP + RSI], 6
    JE L17e0_7
    JMP L17e0_8     ; inserted

; Entry 17e0; block 7; address 1848
L17e0_7:
    ADD RAX, 1
    MOV RDI, qword [RSP + 48]
    MOV qword [RSP + 40], RCX
    MOV qword [R12], RAX
    SHL RAX, 5
    MOVSS dword [RSP + 36], XMM1
    LEA RAX, [RBP + RAX-1]
    MOV qword [RSP + 24], R11
    MOV EDX, dword [RAX]
    MOV RSI, qword [RAX + 8]
    MOVSS XMM0, dword [RAX + 16]
    MOV RAX, qword [RAX + 24]
    MOVSS dword [RSP + 32], XMM0
    MOV dword [RSP + 4], EDX
    MOV qword [RSP + 8], RSI
    MOV RDX, R12
    MOV RSI, RBP
    MOV qword [RSP + 16], RAX
    CALL L15e0_0
    JMP L17e0_4     ; inserted

; Entry 17e0; block 8; address 1964
L17e0_8:
    MOV EAX, dword [RSP + 4]
    MOV RDX, qword [RSP + 56]
    MOVSS dword [RSP + 88], XMM0
    MOV dword [RSP + 72], EAX
    MOV RAX, qword [RSP + 72]
    MOV qword [RDX], R15
    MOV qword [RDX + 40], RBX
    MOV qword [RDX + 8], RAX
    MOV RAX, qword [RSP + 8]
    MOV qword [RDX + 16], RAX
    MOV RAX, qword [RSP + 88]
    MOV qword [RDX + 24], RAX
    MOV RAX, qword [RSP + 16]
    MOV qword [RDX + 32], RAX
    ADD RSP, 168
    MOV RAX, RDX
    POP RBX
    POP RBP
    POP R12
    POP R13
    POP R14
    POP R15
    RET 



; ----------------
; Function: 0x19d0
; ----------------
; Entry 19d0; block 0; address 19d0
L19d0_0:
    PUSH R15
    PUSH R14
    XOR R15D, R15D
    PUSH R13
    PUSH R12
    MOV R12, RDI
    PUSH RBP
    PUSH RBX
    MOV EDI, 6144
    XOR EBX, EBX
    MOV R13D, 128
    SUB RSP, 88
    MOV qword [RSP + 72], 0
    CALL L880_0
    JMP L19d0_1     ; inserted

; Entry 19d0; block 1; address 19ff
L19d0_1:
    MOV RBP, RAX
    LEA RAX, [RSP + 16]
    MOV qword [RSP + 8], RAX
    LEA RAX, [RSP + 72]
    MOV qword [RSP], RAX
    JMP L19d0_5

; Entry 19d0; block 2; address 1a3c
L19d0_2:
    MOV RAX, qword [RSP + 16]
    MOV qword [RBP + RBX], RAX
    MOV RAX, qword [RSP + 24]
    MOV qword [RBP + RBX + 8], RAX
    MOV RAX, qword [RSP + 32]
    MOV qword [RBP + RBX + 16], RAX
    MOV RAX, qword [RSP + 40]
    MOV qword [RBP + RBX + 24], RAX
    MOV RAX, qword [RSP + 48]
    MOV qword [RBP + RBX + 32], RAX
    MOV RAX, qword [RSP + 56]
    MOV qword [RBP + RBX + 40], RAX
    MOV RAX, qword [RSP + 72]
    MOV RDX, RAX
    SHL RDX, 5
    CMP dword [R12 + RDX], 10
    JNE L19d0_4
    JMP L19d0_3     ; inserted

; Entry 19d0; block 3; address 1a8b
L19d0_3:
    ADD RAX, 1
    CMP R13, R14
    MOV qword [RSP + 72], RAX
    JNE L19d0_6
    JMP L19d0_7     ; inserted

; Entry 19d0; block 4; address 1ab8
L19d0_4:
    CMP R13, R14
    JE L19d0_10
    JMP L19d0_9     ; inserted

; Entry 19d0; block 5; address 1a27
L19d0_5:
    MOV RDX, qword [RSP]
    MOV RDI, qword [RSP + 8]
    MOV RSI, R12
    LEA R14, [R15 + 1]
    CALL L17e0_0
    JMP L19d0_2     ; inserted

; Entry 19d0; block 6; address 1a20
L19d0_6:
    ADD RBX, 48
    MOV R15, R14
    JMP L19d0_5     ; inserted

; Entry 19d0; block 7; address 1a99
L19d0_7:
    LEA R13, [R15 + 129]
    MOV RDI, RBP
    MOV RSI, R13
    CALL L8a0_0
    JMP L19d0_8     ; inserted

; Entry 19d0; block 8; address 1aab
L19d0_8:
    MOV RBP, RAX
    JMP L19d0_6

; Entry 19d0; block 9; address 1abd
L19d0_9:
    ADD RSP, 88
    MOV RAX, RBP
    POP RBX
    POP RBP
    POP R12
    POP R13
    POP R14
    POP R15
    RET 

; Entry 19d0; block 10; address 1ad0
L19d0_10:
    LEA RSI, [R15 + 2]
    MOV RDI, RBP
    CALL L8a0_0
    JMP L19d0_11     ; inserted

; Entry 19d0; block 11; address 1adc
L19d0_11:
    MOV RBP, RAX
    JMP L19d0_9



; ----------------
; Function: 0x1af0
; ----------------
; Entry 1af0; block 0; address 1af0
L1af0_0:
    PUSH RBP
    PUSH RBX
    SUB RSP, 24
    MOV RAX, qword [RSP + 48]
    MOV RBP, qword [RSP + 64]
    MOV RBX, qword [RSP + 88]
    TEST RAX, RAX
    JE L1af0_2
    JMP L1af0_1     ; inserted

; Entry 1af0; block 1; address 1b0a
L1af0_1:
    PUSH qword [RAX + 40]
    PUSH qword [RAX + 32]
    PUSH qword [RAX + 24]
    PUSH qword [RAX + 16]
    PUSH qword [RAX + 8]
    PUSH qword [RAX]
    CALL L1af0_0
    JMP L1af0_3     ; inserted

; Entry 1af0; block 2; address 1b27
L1af0_2:
    TEST RBX, RBX
    JE L1af0_5
    JMP L1af0_4     ; inserted

; Entry 1af0; block 3; address 1b20
L1af0_3:
    MOVAPS XMM1, XMM0
    ADD RSP, 48
    JMP L1af0_2     ; inserted

; Entry 1af0; block 4; address 1b2c
L1af0_4:
    MOVSS dword [RSP + 8], XMM1
    PUSH qword [RBX + 40]
    PUSH qword [RBX + 32]
    PUSH qword [RBX + 24]
    PUSH qword [RBX + 16]
    PUSH qword [RBX + 8]
    PUSH qword [RBX]
    CALL L1af0_0
    JMP L1af0_6     ; inserted

; Entry 1af0; block 5; address 1b58
L1af0_5:
    CMP dword [RSP + 56], 8
    JA L1af0_8
    JMP L1af0_7     ; inserted

; Entry 1af0; block 6; address 1b48
L1af0_6:
    MOVSS dword [RSP + 60], XMM0
    ADD RSP, 48
    MOVSS XMM1, dword [RSP + 8]
    JMP L1af0_5     ; inserted

; Entry 1af0; block 7; address 1b63
L1af0_7:
    MOV EDX, dword [RSP + 56]
    LEA RAX, [rel L_JUMP_TABLE_1b75]
    LEA RAX, [RAX+8*RDX]
    MOV RAX, QWORD [RAX]
    
    ;MOV RAX, QWORD PTR [L_JUMP_TABLE_1b75 + 8*DWORD PTR [RSP + 56]] ; inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table.
    JMP RAX; TARGETS: 1c40,1c34,1c50,1c60,1c78,1c90,1ca8,1cd0,1b80

; Entry 1af0; block 8; address 1c34
L1af0_8:
    ADD RSP, 24
    POP RBX
    POP RBP
    RET 

; Entry 1af0; block 9; address 1b80
L1af0_9:
    MOVZX EAX, byte [RBP]
    CMP EAX, 115
    JE L1af0_18
    JMP L1af0_17     ; inserted

; Entry 1af0; block 10; address 1c40
L1af0_10:
    ADD RSP, 24
    MOVAPS XMM0, XMM1
    POP RBX
    POP RBP
    RET 

; Entry 1af0; block 11; address 1c50
L1af0_11:
    SUBSS XMM1, dword [RSP + 12]
    ADD RSP, 24
    POP RBX
    POP RBP
    MOVAPS XMM0, XMM1
    RET 

; Entry 1af0; block 12; address 1c60
L1af0_12:
    MOVSS XMM0, dword [RSP + 12]
    ADD RSP, 24
    ADDSS XMM0, XMM1
    POP RBX
    POP RBP
    RET 

; Entry 1af0; block 13; address 1c78
L1af0_13:
    DIVSS XMM1, dword [RSP + 12]
    ADD RSP, 24
    POP RBX
    POP RBP
    MOVAPS XMM0, XMM1
    RET 

; Entry 1af0; block 14; address 1c90
L1af0_14:
    MOVSS XMM0, dword [RSP + 12]
    ADD RSP, 24
    MULSS XMM0, XMM1
    POP RBX
    POP RBP
    RET 

; Entry 1af0; block 15; address 1ca8
L1af0_15:
    UCOMISS XMM1, dword [RSP + 12]
    JP L1af0_25
    JMP L1af0_24     ; inserted

; Entry 1af0; block 16; address 1cd0
L1af0_16:
    MOVSS XMM0, dword [RSP + 72]
    ADD RSP, 24
    POP RBX
    POP RBP
    RET 

; Entry 1af0; block 17; address 1b8d
L1af0_17:
    CMP EAX, 99
    JNE L1af0_20
    JMP L1af0_19     ; inserted

; Entry 1af0; block 18; address 1ce0
L1af0_18:
    CMP byte [RBP + 1], 105
    JNE L1af0_20
    JMP L1af0_26     ; inserted

; Entry 1af0; block 19; address 1b96
L1af0_19:
    CMP byte [RBP + 1], 111
    JNE L1af0_20
    JMP L1af0_21     ; inserted

; Entry 1af0; block 20; address 1d60
L1af0_20:
    MOV RDI, qword [rel stderr@GLIBC_2.2.5]
    LEA RSI, [rel L_.rodata + 416]
    MOV RDX, RBP
    XOR EAX, EAX
    CALL L870_0
    JMP L1af0_29     ; inserted

; Entry 1af0; block 21; address 1ba0
L1af0_21:
    CMP byte [RBP + 2], 115
    JNE L1af0_20
    JMP L1af0_22     ; inserted

; Entry 1af0; block 22; address 1baa
L1af0_22:
    CMP byte [RBP + 3], 0
    JNE L1af0_20
    JMP L1af0_23     ; inserted

; Entry 1af0; block 23; address 1bb4
L1af0_23:
    MOVAPS XMM0, XMM1
    MOVAPS XMM3, XMM1
    MOVAPS XMM2, XMM1
    MULSS XMM0, XMM1
    PXOR XMM4, XMM4
    MULSS XMM3, XMM0
    MULSS XMM0, dword [rel L_.rodata + 504]
    MULSS XMM3, XMM1
    CVTSS2SD XMM4, XMM0
    MULSS XMM2, XMM3
    DIVSS XMM3, dword [rel L_.rodata + 508]
    MOVSD XMM0, qword [rel L_.rodata + 520]
    MULSS XMM2, XMM1
    SUBSD XMM0, XMM4
    CVTSS2SD XMM3, XMM3
    ADDSD XMM0, XMM3
    MOVAPS XMM3, XMM2
    MULSS XMM2, XMM1
    DIVSS XMM3, dword [rel L_.rodata + 512]
    MULSS XMM2, XMM1
    DIVSS XMM2, dword [rel L_.rodata + 516]
    CVTSS2SD XMM3, XMM3
    SUBSD XMM0, XMM3
    MOVAPD XMM3, XMM0
    PXOR XMM0, XMM0
    CVTSS2SD XMM0, XMM2
    ADDSD XMM0, XMM3
    CVTSD2SS XMM0, XMM0
    JMP L1af0_8     ; inserted

; Entry 1af0; block 24; address 1caf
L1af0_24:
    MOVSS XMM0, dword [rel L_.rodata + 484]
    JE L1af0_8
    JMP L1af0_25     ; inserted

; Entry 1af0; block 25; address 1cbd
L1af0_25:
    PXOR XMM0, XMM0
    JMP L1af0_8

; Entry 1af0; block 26; address 1ce6
L1af0_26:
    CMP byte [RBP + 2], 110
    JNE L1af0_20
    JMP L1af0_27     ; inserted

; Entry 1af0; block 27; address 1cec
L1af0_27:
    CMP byte [RBP + 3], 0
    JNE L1af0_20
    JMP L1af0_28     ; inserted

; Entry 1af0; block 28; address 1cf2
L1af0_28:
    MOVAPS XMM0, XMM1
    MOVAPS XMM3, XMM1
    MOVAPS XMM2, XMM1
    MULSS XMM0, XMM1
    MOVAPS XMM4, XMM1
    MULSS XMM0, XMM1
    MULSS XMM3, XMM0
    DIVSS XMM0, dword [rel L_.rodata + 488]
    MULSS XMM3, XMM1
    MULSS XMM2, XMM3
    DIVSS XMM3, dword [rel L_.rodata + 492]
    SUBSS XMM4, XMM0
    MOVAPS XMM0, XMM4
    MULSS XMM2, XMM1
    ADDSS XMM0, XMM3
    MOVAPS XMM3, XMM2
    MULSS XMM2, XMM1
    DIVSS XMM3, dword [rel L_.rodata + 496]
    MULSS XMM1, XMM2
    DIVSS XMM1, dword [rel L_.rodata + 500]
    SUBSS XMM0, XMM3
    ADDSS XMM0, XMM1
    JMP L1af0_8

; Entry 1af0; block 29; address 1d78
L1af0_29:
    MOVSS XMM0, dword [rel L_.rodata + 352]
    JMP L1af0_8





section .bss
L_.bss: resb 16


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
db 080h
db 030h
db 020h
db 00h
db 00h
db 00h
db 00h
db 00h

section .data
L_.rodata:
db 01h
db 00h
db 02h
db 00h
db 055h
db 073h
db 061h
db 067h
db 065h
db 03ah
db 020h
db 025h
db 073h
db 020h
db 03ch
db 069h
db 06eh
db 070h
db 075h
db 074h
db 020h
db 066h
db 069h
db 06ch
db 065h
db 03eh
db 0ah
db 00h
db 025h
db 073h
db 020h
db 073h
db 063h
db 06fh
db 072h
db 065h
db 064h
db 020h
db 025h
db 02eh
db 030h
db 032h
db 066h
db 0ah
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 0c8h
db 042h
db 055h
db 06eh
db 06bh
db 06eh
db 06fh
db 077h
db 06eh
db 00h
db 072h
db 00h
db 043h
db 061h
db 06eh
db 06eh
db 06fh
db 074h
db 020h
db 06fh
db 070h
db 065h
db 06eh
db 020h
db 025h
db 073h
db 03ah
db 020h
db 025h
db 073h
db 0ah
db 00h
db 06eh
db 061h
db 06dh
db 065h
db 00h
db 04ch
db 045h
db 046h
db 054h
db 05fh
db 050h
db 041h
db 052h
db 045h
db 04eh
db 00h
db 052h
db 049h
db 047h
db 048h
db 054h
db 05fh
db 050h
db 041h
db 052h
db 045h
db 04eh
db 00h
db 04dh
db 049h
db 04eh
db 055h
db 053h
db 00h
db 050h
db 04ch
db 055h
db 053h
db 00h
db 044h
db 049h
db 056h
db 049h
db 044h
db 045h
db 00h
db 04dh
db 055h
db 04ch
db 054h
db 049h
db 050h
db 04ch
db 059h
db 00h
db 045h
db 051h
db 055h
db 041h
db 04ch
db 053h
db 00h
db 04eh
db 055h
db 04dh
db 042h
db 045h
db 052h
db 00h
db 046h
db 055h
db 04eh
db 043h
db 054h
db 049h
db 04fh
db 04eh
db 00h
db 049h
db 044h
db 045h
db 04eh
db 054h
db 049h
db 046h
db 049h
db 045h
db 052h
db 00h
db 04eh
db 045h
db 057h
db 05fh
db 04ch
db 049h
db 04eh
db 045h
db 00h
db 054h
db 045h
db 04fh
db 046h
db 00h
db 054h
db 06fh
db 06bh
db 065h
db 06eh
db 020h
db 025h
db 073h
db 020h
db 06fh
db 06eh
db 020h
db 06ch
db 069h
db 06eh
db 065h
db 020h
db 025h
db 06ch
db 064h
db 03ah
db 020h
db 022h
db 025h
db 073h
db 022h
db 020h
db 025h
db 066h
db 00h
db 00h
db 038h
db 0edh
db 0ffh
db 0ffh
db 048h
db 0edh
db 0ffh
db 0ffh
db 058h
db 0edh
db 0ffh
db 0ffh
db 068h
db 0edh
db 0ffh
db 0ffh
db 078h
db 0edh
db 0ffh
db 0ffh
db 088h
db 0edh
db 0ffh
db 0ffh
db 098h
db 0edh
db 0ffh
db 0ffh
db 0a8h
db 0edh
db 0ffh
db 0ffh
db 0b8h
db 0edh
db 0ffh
db 0ffh
db 0c8h
db 0edh
db 0ffh
db 0ffh
db 028h
db 0edh
db 0ffh
db 0ffh
db 0f8h
db 0ech
db 0ffh
db 0ffh
db 0c0h
db 0efh
db 0ffh
db 0ffh
db 0b0h
db 0efh
db 0ffh
db 0ffh
db 038h
db 0f0h
db 0ffh
db 0ffh
db 028h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 018h
db 0f0h
db 0ffh
db 0ffh
db 0f0h
db 0efh
db 0ffh
db 0ffh
db 0e8h
db 0eeh
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 058h
db 0f0h
db 0ffh
db 0ffh
db 048h
db 0f0h
db 0ffh
db 0ffh
db 00h
db 00h
db 0c0h
db 07fh
db 04eh
db 055h
db 04ch
db 04ch
db 00h
db 025h
db 073h
db 028h
db 025h
db 073h
db 029h
db 00h
db 028h
db 025h
db 073h
db 020h
db 025h
db 073h
db 020h
db 025h
db 073h
db 029h
db 00h
db 045h
db 078h
db 070h
db 065h
db 063h
db 074h
db 065h
db 064h
db 020h
db 027h
db 029h
db 027h
db 020h
db 061h
db 066h
db 074h
db 065h
db 072h
db 020h
db 065h
db 078h
db 070h
db 072h
db 065h
db 073h
db 073h
db 069h
db 06fh
db 06eh
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 046h
db 075h
db 06eh
db 063h
db 074h
db 069h
db 06fh
db 06eh
db 020h
db 025h
db 073h
db 020h
db 069h
db 073h
db 020h
db 06eh
db 06fh
db 074h
db 020h
db 069h
db 06dh
db 070h
db 06ch
db 065h
db 06dh
db 065h
db 06eh
db 074h
db 065h
db 064h
db 0ah
db 00h
db 070h
db 0fch
db 0ffh
db 0ffh
db 064h
db 0fch
db 0ffh
db 0ffh
db 080h
db 0fch
db 0ffh
db 0ffh
db 090h
db 0fch
db 0ffh
db 0ffh
db 0a8h
db 0fch
db 0ffh
db 0ffh
db 0c0h
db 0fch
db 0ffh
db 0ffh
db 0d8h
db 0fch
db 0ffh
db 0ffh
db 00h
db 0fdh
db 0ffh
db 0ffh
db 0b0h
db 0fbh
db 0ffh
db 0ffh
db 00h
db 00h
db 080h
db 03fh
db 00h
db 00h
db 0c0h
db 040h
db 00h
db 00h
db 0f0h
db 042h
db 00h
db 080h
db 09dh
db 045h
db 00h
db 030h
db 0b1h
db 048h
db 00h
db 00h
db 00h
db 03fh
db 00h
db 00h
db 0c0h
db 041h
db 00h
db 00h
db 034h
db 044h
db 00h
db 080h
db 01dh
db 047h
db 00h
db 00h
db 00h
db 00h
db 00h
db 00h
db 0f0h
db 03fh

section .data
L_JUMP_TABLE_df7:
dq Lcc0_25
dq Lcc0_24
dq Lcc0_29
dq Lcc0_28
dq Lcc0_22
dq Lcc0_27
dq Lcc0_26
dq Lcc0_23
dq Lcc0_22
dq Lcc0_22
dq Lcc0_22
dq Lcc0_22
dq Lcc0_22
dq Lcc0_22
dq Lcc0_22
dq Lcc0_22
dq Lcc0_22
dq Lcc0_22
dq Lcc0_22
dq Lcc0_22
dq Lcc0_22
dq Lcc0_30
L_JUMP_TABLE_1b75:
dq L1af0_10
dq L1af0_8
dq L1af0_11
dq L1af0_12
dq L1af0_13
dq L1af0_14
dq L1af0_15
dq L1af0_16
dq L1af0_9
