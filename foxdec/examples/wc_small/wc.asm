extern fclose@GLIBC_2.2.5
extern printf@GLIBC_2.2.5
extern fputc@GLIBC_2.2.5
extern feof@GLIBC_2.2.5
extern _IO_getc@GLIBC_2.2.5
extern fopen@GLIBC_2.2.5
extern perror@GLIBC_2.2.5
extern vfprintf@GLIBC_2.2.5
extern exit@GLIBC_2.2.5
extern __ctype_b_loc@GLIBC_2.3
extern stderr@GLIBC_2.2.5

section .text

default rel

global main

; ----------------------------
; Function: fclose@GLIBC_2.2.5
; ----------------------------
; Entry 7c0; block 0; address 7c0
L7c0_0:
    JMP fclose@GLIBC_2.2.5 wrt ..plt



; ----------------------------
; Function: printf@GLIBC_2.2.5
; ----------------------------
; Entry 7d0; block 0; address 7d0
L7d0_0:
    JMP printf@GLIBC_2.2.5 wrt ..plt



; ---------------------------
; Function: fputc@GLIBC_2.2.5
; ---------------------------
; Entry 7e0; block 0; address 7e0
L7e0_0:
    JMP fputc@GLIBC_2.2.5 wrt ..plt



; --------------------------
; Function: feof@GLIBC_2.2.5
; --------------------------
; Entry 7f0; block 0; address 7f0
L7f0_0:
    JMP feof@GLIBC_2.2.5 wrt ..plt



; ------------------------------
; Function: _IO_getc@GLIBC_2.2.5
; ------------------------------
; Entry 800; block 0; address 800
L800_0:
    JMP _IO_getc@GLIBC_2.2.5 wrt ..plt



; ---------------------------
; Function: fopen@GLIBC_2.2.5
; ---------------------------
; Entry 810; block 0; address 810
L810_0:
    JMP fopen@GLIBC_2.2.5 wrt ..plt



; ----------------------------
; Function: perror@GLIBC_2.2.5
; ----------------------------
; Entry 820; block 0; address 820
L820_0:
    JMP perror@GLIBC_2.2.5 wrt ..plt



; ------------------------------
; Function: vfprintf@GLIBC_2.2.5
; ------------------------------
; Entry 830; block 0; address 830
L830_0:
    JMP vfprintf@GLIBC_2.2.5 wrt ..plt



; --------------------------
; Function: exit@GLIBC_2.2.5
; --------------------------
; Entry 840; block 0; address 840
L840_0:
    JMP exit@GLIBC_2.2.5 wrt ..plt



; ---------------------------------
; Function: __ctype_b_loc@GLIBC_2.3
; ---------------------------------
; Entry 850; block 0; address 850
L850_0:
    JMP __ctype_b_loc@GLIBC_2.3 wrt ..plt





; ---------------
; Function: 0x9a0
; ---------------
; Entry 9a0; block 0; address 9a0
L9a0_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 32
    MOV dword [RBP + -4], EDI
    MOV qword [RBP + -16], RSI
    MOV qword [RBP + -24], RDX
    MOV RAX, qword [rel stderr@GLIBC_2.2.5]
    MOV RDX, qword [RBP + -24]
    MOV RCX, qword [RBP + -16]
    MOV RSI, RCX
    MOV RDI, RAX
    CALL L830_0
    JMP L9a0_1     ; inserted

; Entry 9a0; block 1; address 9cd
L9a0_1:
    CMP dword [RBP + -4], 0
    JE L9a0_3
    JMP L9a0_2     ; inserted

; Entry 9a0; block 2; address 9d3
L9a0_2:
    LEA RDI, [rel L_.rodata + 4]
    CALL L820_0
    JMP L9a0_4     ; inserted

; Entry 9a0; block 3; address 9e1
L9a0_3:
    MOV RAX, qword [rel stderr@GLIBC_2.2.5]
    MOV RSI, RAX
    MOV EDI, 10
    CALL L7e0_0
    JMP L9a0_5     ; inserted

; Entry 9a0; block 4; address 9df
L9a0_4:
    JMP L9a0_5

; Entry 9a0; block 5; address 9f5
L9a0_5:
    MOV EDI, 1
    CALL L840_0



; ---------------
; Function: 0x9ff
; ---------------
; Entry 9ff; block 0; address 9ff
L9ff_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 224
    MOV qword [RBP + -216], RDI
    MOV qword [RBP + -168], RSI
    MOV qword [RBP + -160], RDX
    MOV qword [RBP + -152], RCX
    MOV qword [RBP + -144], R8
    MOV qword [RBP + -136], R9
    TEST AL, AL
    JE L9ff_2
    JMP L9ff_1     ; inserted

; Entry 9ff; block 1; address a38
L9ff_1:
    MOVAPS oword [RBP + -128], XMM0
    MOVAPS oword [RBP + -112], XMM1
    MOVAPS oword [RBP + -96], XMM2
    MOVAPS oword [RBP + -80], XMM3
    MOVAPS oword [RBP + -64], XMM4
    MOVAPS oword [RBP + -48], XMM5
    MOVAPS oword [RBP + -32], XMM6
    MOVAPS oword [RBP + -16], XMM7
    JMP L9ff_2     ; inserted

; Entry 9ff; block 2; address a58
L9ff_2:
    MOV dword [RBP + -200], 8
    MOV dword [RBP + -196], 48
    LEA RAX, [RBP + 16]
    MOV qword [RBP + -192], RAX
    LEA RAX, [RBP + -176]
    MOV qword [RBP + -184], RAX
    LEA RDX, [RBP + -200]
    MOV RAX, qword [RBP + -216]
    MOV RSI, RAX
    MOV EDI, 0
    CALL L9a0_0



; ---------------
; Function: 0xaa3
; ---------------
; Entry aa3; block 0; address aa3
Laa3_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 224
    MOV qword [RBP + -216], RDI
    MOV qword [RBP + -168], RSI
    MOV qword [RBP + -160], RDX
    MOV qword [RBP + -152], RCX
    MOV qword [RBP + -144], R8
    MOV qword [RBP + -136], R9
    TEST AL, AL
    JE Laa3_2
    JMP Laa3_1     ; inserted

; Entry aa3; block 1; address adc
Laa3_1:
    MOVAPS oword [RBP + -128], XMM0
    MOVAPS oword [RBP + -112], XMM1
    MOVAPS oword [RBP + -96], XMM2
    MOVAPS oword [RBP + -80], XMM3
    MOVAPS oword [RBP + -64], XMM4
    MOVAPS oword [RBP + -48], XMM5
    MOVAPS oword [RBP + -32], XMM6
    MOVAPS oword [RBP + -16], XMM7
    JMP Laa3_2     ; inserted

; Entry aa3; block 2; address afc
Laa3_2:
    MOV dword [RBP + -200], 8
    MOV dword [RBP + -196], 48
    LEA RAX, [RBP + 16]
    MOV qword [RBP + -192], RAX
    LEA RAX, [RBP + -176]
    MOV qword [RBP + -184], RAX
    LEA RDX, [RBP + -200]
    MOV RAX, qword [RBP + -216]
    MOV RSI, RAX
    MOV EDI, 1
    CALL L9a0_0



; ---------------
; Function: 0xb47
; ---------------
; Entry b47; block 0; address b47
Lb47_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 32
    MOV qword [RBP + -8], RDI
    MOV qword [RBP + -16], RSI
    MOV qword [RBP + -24], RDX
    MOV qword [RBP + -32], RCX
    MOV RSI, qword [RBP + -8]
    MOV RCX, qword [RBP + -16]
    MOV RDX, qword [RBP + -24]
    MOV RAX, qword [RBP + -32]
    MOV R8, RSI
    MOV RSI, RAX
    LEA RDI, [rel L_.rodata + 6]
    MOV EAX, 0
    CALL L7d0_0
    JMP Lb47_1     ; inserted

; Entry b47; block 1; address b86
Lb47_1:
    NOP 
    LEAVE 
    RET 



; ---------------
; Function: 0xb89
; ---------------
; Entry b89; block 0; address b89
Lb89_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 16
    MOV EAX, EDI
    MOV byte [RBP + -4], AL
    CALL L850_0
    JMP Lb89_1     ; inserted

; Entry b89; block 1; address b9b
Lb89_1:
    MOV RAX, qword [RAX]
    MOVZX EDX, byte [RBP + -4]
    ADD RDX, RDX
    ADD RAX, RDX
    MOVZX EAX, word [RAX]
    MOVZX EAX, AX
    AND EAX, 1024
    LEAVE 
    RET 



; ---------------
; Function: 0xbb5
; ---------------
; Entry bb5; block 0; address bb5
Lbb5_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 32
    MOV qword [RBP + -24], RDI
    MOV dword [RBP + -8], 0
    MOV RAX, qword [RBP + -24]
    MOV RDI, RAX
    CALL L7f0_0
    JMP Lbb5_1     ; inserted

; Entry bb5; block 1; address bd4
Lbb5_1:
    TEST EAX, EAX
    JE Lbb5_3
    JMP Lbb5_2     ; inserted

; Entry bb5; block 2; address bd8
Lbb5_2:
    MOV EAX, 0
    JMP Lbb5_21

; Entry bb5; block 3; address c31
Lbb5_3:
    MOV RAX, qword [RBP + -24]
    MOV RDI, RAX
    CALL L800_0
    JMP Lbb5_4     ; inserted

; Entry bb5; block 4; address c3d
Lbb5_4:
    MOV dword [RBP + -4], EAX
    CMP dword [RBP + -4], 18446744073709551615
    JNE Lbb5_9
    JMP Lbb5_10     ; inserted

; Entry bb5; block 5; address bef
Lbb5_5:
    TEST EAX, EAX
    JE Lbb5_7
    JMP Lbb5_6     ; inserted

; Entry bb5; block 6; address bf3
Lbb5_6:
    MOV RAX, qword [rel L_.bss + 40]
    ADD RAX, 1
    MOV qword [rel L_.bss + 40], RAX
    JMP Lbb5_10

; Entry bb5; block 7; address c07
Lbb5_7:
    MOV RAX, qword [rel L_.bss + 48]
    ADD RAX, 1
    MOV qword [rel L_.bss + 48], RAX
    CMP dword [RBP + -4], 10
    JNE Lbb5_3
    JMP Lbb5_8     ; inserted

; Entry bb5; block 8; address c1f
Lbb5_8:
    MOV RAX, qword [rel L_.bss + 56]
    ADD RAX, 1
    MOV qword [rel L_.bss + 56], RAX
    JMP Lbb5_3     ; inserted

; Entry bb5; block 9; address be2
Lbb5_9:
    MOV EAX, dword [RBP + -4]
    MOVZX EAX, AL
    MOV EDI, EAX
    CALL Lb89_0
    JMP Lbb5_5     ; inserted

; Entry bb5; block 10; address c46
Lbb5_10:
    JMP Lbb5_17

; Entry bb5; block 11; address c60
Lbb5_11:
    MOV RAX, qword [rel L_.bss + 56]
    ADD RAX, 1
    MOV qword [rel L_.bss + 56], RAX
    JMP Lbb5_12     ; inserted

; Entry bb5; block 12; address c72
Lbb5_12:
    MOV EAX, dword [RBP + -4]
    MOVZX EAX, AL
    MOV EDI, EAX
    CALL Lb89_0
    JMP Lbb5_13     ; inserted

; Entry bb5; block 13; address c7f
Lbb5_13:
    TEST EAX, EAX
    JE Lbb5_15
    JMP Lbb5_14     ; inserted

; Entry bb5; block 14; address c83
Lbb5_14:
    MOV RAX, qword [RBP + -24]
    MOV RDI, RAX
    CALL L800_0
    JMP Lbb5_16     ; inserted

; Entry bb5; block 15; address c9a
Lbb5_15:
    NOP 
    JMP Lbb5_20     ; inserted

; Entry bb5; block 16; address c8f
Lbb5_16:
    MOV dword [RBP + -4], EAX
    JMP Lbb5_17     ; inserted

; Entry bb5; block 17; address c92
Lbb5_17:
    CMP dword [RBP + -4], 18446744073709551615
    JNE Lbb5_18
    JMP Lbb5_19     ; inserted

; Entry bb5; block 18; address c48
Lbb5_18:
    MOV RAX, qword [rel L_.bss + 48]
    ADD RAX, 1
    MOV qword [rel L_.bss + 48], RAX
    CMP dword [RBP + -4], 10
    JNE Lbb5_12
    JMP Lbb5_11     ; inserted

; Entry bb5; block 19; address c98
Lbb5_19:
    JMP Lbb5_20

; Entry bb5; block 20; address c9b
Lbb5_20:
    CMP dword [RBP + -4], 18446744073709551615
    SETNE AL
    MOVZX EAX, AL
    JMP Lbb5_21     ; inserted

; Entry bb5; block 21; address ca5
Lbb5_21:
    LEAVE 
    RET 



; ---------------
; Function: 0xca7
; ---------------
; Entry ca7; block 0; address ca7
Lca7_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 32
    MOV qword [RBP + -24], RDI
    MOV RAX, qword [RBP + -24]
    LEA RSI, [rel L_.rodata + 25]
    MOV RDI, RAX
    CALL L810_0
    JMP Lca7_1     ; inserted

; Entry ca7; block 1; address cc6
Lca7_1:
    MOV qword [RBP + -8], RAX
    CMP qword [RBP + -8], 0
    JNE Lca7_3
    JMP Lca7_2     ; inserted

; Entry ca7; block 2; address cd1
Lca7_2:
    MOV RAX, qword [RBP + -24]
    MOV RSI, RAX
    LEA RDI, [rel L_.rodata + 27]
    MOV EAX, 0
    CALL Laa3_0

; Entry ca7; block 3; address ce9
Lca7_3:
    MOV qword [rel L_.bss + 56], 0
    MOV RAX, qword [rel L_.bss + 56]
    MOV qword [rel L_.bss + 40], RAX
    MOV RAX, qword [rel L_.bss + 40]
    MOV qword [rel L_.bss + 48], RAX
    NOP 
    JMP Lca7_5     ; inserted

; Entry ca7; block 4; address d1d
Lca7_4:
    TEST EAX, EAX
    JNE Lca7_5
    JMP Lca7_6     ; inserted

; Entry ca7; block 5; address d11
Lca7_5:
    MOV RAX, qword [RBP + -8]
    MOV RDI, RAX
    CALL Lbb5_0
    JMP Lca7_4     ; inserted

; Entry ca7; block 6; address d21
Lca7_6:
    MOV RAX, qword [RBP + -8]
    MOV RDI, RAX
    CALL L7c0_0
    JMP Lca7_7     ; inserted

; Entry ca7; block 7; address d2d
Lca7_7:
    MOV RCX, qword [rel L_.bss + 56]
    MOV RDX, qword [rel L_.bss + 40]
    MOV RSI, qword [rel L_.bss + 48]
    MOV RAX, qword [RBP + -24]
    MOV RDI, RAX
    CALL Lb47_0
    JMP Lca7_8     ; inserted

; Entry ca7; block 8; address d4e
Lca7_8:
    MOV RDX, qword [rel L_.bss + 16]
    MOV RAX, qword [rel L_.bss + 48]
    ADD RAX, RDX
    MOV qword [rel L_.bss + 16], RAX
    MOV RDX, qword [rel L_.bss + 24]
    MOV RAX, qword [rel L_.bss + 40]
    ADD RAX, RDX
    MOV qword [rel L_.bss + 24], RAX
    MOV RDX, qword [rel L_.bss + 32]
    MOV RAX, qword [rel L_.bss + 56]
    ADD RAX, RDX
    MOV qword [rel L_.bss + 32], RAX
    NOP 
    LEAVE 
    RET 



; ---------------
; Function: 0xd99
; ---------------
; Entry d99; block 0; address d99
Ld99_0:
main:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 32
    MOV dword [RBP + -20], EDI
    MOV qword [RBP + -32], RSI
    CMP dword [RBP + -20], 1
    JG Ld99_2
    JMP Ld99_1     ; inserted

; Entry d99; block 1; address dae
Ld99_1:
    LEA RDI, [rel L_.rodata + 49]
    MOV EAX, 0
    CALL L9ff_0

; Entry d99; block 2; address dbf
Ld99_2:
    MOV dword [RBP + -4], 1
    JMP Ld99_4

; Entry d99; block 3; address de7
Ld99_3:
    ADD dword [RBP + -4], 1
    JMP Ld99_4     ; inserted

; Entry d99; block 4; address deb
Ld99_4:
    MOV EAX, dword [RBP + -4]
    CMP EAX, dword [RBP + -20]
    JL Ld99_5
    JMP Ld99_6     ; inserted

; Entry d99; block 5; address dc8
Ld99_5:
    MOV EAX, dword [RBP + -4]
    CDQE 
    LEA RDX, [RAX * 8]
    MOV RAX, qword [RBP + -32]
    ADD RAX, RDX
    MOV RAX, qword [RAX]
    MOV RDI, RAX
    CALL Lca7_0
    JMP Ld99_3     ; inserted

; Entry d99; block 6; address df3
Ld99_6:
    CMP dword [RBP + -20], 2
    JLE Ld99_8
    JMP Ld99_7     ; inserted

; Entry d99; block 7; address df9
Ld99_7:
    MOV RCX, qword [rel L_.bss + 32]
    MOV RDX, qword [rel L_.bss + 24]
    MOV RAX, qword [rel L_.bss + 16]
    MOV RSI, RAX
    LEA RDI, [rel L_.rodata + 74]
    CALL Lb47_0
    JMP Ld99_8     ; inserted

; Entry d99; block 8; address e1d
Ld99_8:
    MOV EAX, 0
    LEAVE 
    RET 





section .bss
L_.bss: resb 64


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
db 070h
db 020h
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
db 020h
db 00h
db 025h
db 036h
db 06ch
db 075h
db 020h
db 025h
db 036h
db 06ch
db 075h
db 020h
db 025h
db 036h
db 06ch
db 075h
db 020h
db 025h
db 073h
db 0ah
db 00h
db 072h
db 00h
db 063h
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
db 066h
db 069h
db 06ch
db 065h
db 020h
db 060h
db 025h
db 073h
db 027h
db 00h
db 075h
db 073h
db 061h
db 067h
db 065h
db 03ah
db 020h
db 077h
db 063h
db 020h
db 046h
db 049h
db 04ch
db 045h
db 020h
db 05bh
db 046h
db 049h
db 04ch
db 045h
db 02eh
db 02eh
db 02eh
db 05dh
db 00h
db 074h
db 06fh
db 074h
db 061h
db 06ch
db 00h

section .data
