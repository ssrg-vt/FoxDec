global	_main

extern ___stack_chk_fail
extern _exit
extern _fclose
extern _feof
extern _fopen
extern _fprintf
extern _getc
extern _isalpha
extern _perror
extern _printf
extern _vfprintf
extern ___stack_chk_guard
extern ___stderrp
extern dyld_stub_binder


section .text

default rel

; ---------------------
; Function: 0x100000790
; ---------------------
; Entry 100000790; block 0; address 100000790
L100000790_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 32
    MOV qword [RBP - 8], RDI
    MOV qword [RBP - 16], RSI
    MOV qword [RBP - 24], RDX
    MOV qword [RBP - 32], RCX
    MOV RSI, qword [RBP - 32]
    MOV RDX, qword [RBP - 24]
    MOV RCX, qword [RBP - 16]
    MOV R8, qword [RBP - 8]
    LEA RDI, [rel L__TEXT___cstring_0]
    MOV AL, 0
    CALL _printf
    JMP L100000790_1     ; inserted

; Entry 100000790; block 1; address 1000007c6
L100000790_1:
    ADD RSP, 32
    POP RBP
    RET 



; ---------------------
; Function: 0x1000007d0
; ---------------------
; Entry 1000007d0; block 0; address 1000007d0
L1000007d0_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 32
    MOV qword [RBP - 16], RDI
    MOV dword [RBP - 24], 0
    MOV RDI, qword [RBP - 16]
    CALL _feof
    JMP L1000007d0_1     ; inserted

; Entry 1000007d0; block 1; address 1000007ec
L1000007d0_1:
    CMP EAX, 0
    JE L1000007d0_3
    JMP L1000007d0_2     ; inserted

; Entry 1000007d0; block 2; address 1000007f5
L1000007d0_2:
    MOV dword [RBP - 4], 0
    JMP L1000007d0_22

; Entry 1000007d0; block 3; address 100000801
L1000007d0_3:
    JMP L1000007d0_12

; Entry 1000007d0; block 4; address 10000080f
L1000007d0_4:
    MOV dword [RBP - 20], EAX
    CMP EAX, 18446744073709551615
    JE L1000007d0_6
    JMP L1000007d0_5     ; inserted

; Entry 1000007d0; block 5; address 10000081b
L1000007d0_5:
    MOV EAX, dword [RBP - 20]
    MOVZX EDI, AL
    CALL L100000910_0
    JMP L1000007d0_7     ; inserted

; Entry 1000007d0; block 6; address 10000087f
L1000007d0_6:
    JMP L1000007d0_21

; Entry 1000007d0; block 7; address 100000826
L1000007d0_7:
    CMP EAX, 0
    JE L1000007d0_9
    JMP L1000007d0_8     ; inserted

; Entry 1000007d0; block 8; address 10000082f
L1000007d0_8:
    LEA RAX, [rel L__DATA___common + 40]
    MOV RCX, qword [RAX]
    ADD RCX, 1
    MOV qword [RAX], RCX
    JMP L1000007d0_6

; Entry 1000007d0; block 9; address 100000848
L1000007d0_9:
    LEA RAX, [rel L__DATA___common + 24]
    MOV RCX, qword [RAX]
    ADD RCX, 1
    MOV qword [RAX], RCX
    CMP dword [RBP - 20], 10
    JNE L1000007d0_11
    JMP L1000007d0_10     ; inserted

; Entry 1000007d0; block 10; address 100000866
L1000007d0_10:
    LEA RAX, [rel L__DATA___common + 32]
    MOV RCX, qword [RAX]
    ADD RCX, 1
    MOV qword [RAX], RCX
    JMP L1000007d0_11     ; inserted

; Entry 1000007d0; block 11; address 10000087a
L1000007d0_11:
    JMP L1000007d0_12

; Entry 1000007d0; block 12; address 100000806
L1000007d0_12:
    MOV RDI, qword [RBP - 16]
    CALL _getc
    JMP L1000007d0_4     ; inserted

; Entry 1000007d0; block 13; address 10000088e
L1000007d0_13:
    LEA RAX, [rel L__DATA___common + 24]
    MOV RCX, qword [RAX]
    ADD RCX, 1
    MOV qword [RAX], RCX
    CMP dword [RBP - 20], 10
    JNE L1000007d0_16
    JMP L1000007d0_15     ; inserted

; Entry 1000007d0; block 14; address 1000008ef
L1000007d0_14:
    CMP dword [RBP - 20], 18446744073709551615
    SETNE AL
    AND AL, 1
    MOVZX ECX, AL
    MOV dword [RBP - 4], ECX
    JMP L1000007d0_22     ; inserted

; Entry 1000007d0; block 15; address 1000008ac
L1000007d0_15:
    LEA RAX, [rel L__DATA___common + 32]
    MOV RCX, qword [RAX]
    ADD RCX, 1
    MOV qword [RAX], RCX
    JMP L1000007d0_16     ; inserted

; Entry 1000007d0; block 16; address 1000008c0
L1000007d0_16:
    MOV EAX, dword [RBP - 20]
    MOVZX EDI, AL
    CALL L100000910_0
    JMP L1000007d0_17     ; inserted

; Entry 1000007d0; block 17; address 1000008cb
L1000007d0_17:
    CMP EAX, 0
    JNE L1000007d0_19
    JMP L1000007d0_18     ; inserted

; Entry 1000007d0; block 18; address 1000008d4
L1000007d0_18:
    JMP L1000007d0_14

; Entry 1000007d0; block 19; address 1000008d9
L1000007d0_19:
    MOV RDI, qword [RBP - 16]
    CALL _getc
    JMP L1000007d0_20     ; inserted

; Entry 1000007d0; block 20; address 1000008e7
L1000007d0_20:
    MOV dword [RBP - 20], EAX
    JMP L1000007d0_21

; Entry 1000007d0; block 21; address 100000884
L1000007d0_21:
    CMP dword [RBP - 20], 18446744073709551615
    JE L1000007d0_14
    JMP L1000007d0_13     ; inserted

; Entry 1000007d0; block 22; address 1000008fe
L1000007d0_22:
    MOV EAX, dword [RBP - 4]
    ADD RSP, 32
    POP RBP
    RET 



; ---------------------
; Function: 0x100000910
; ---------------------
; Entry 100000910; block 0; address 100000910
L100000910_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 16
    MOV byte [RBP - 1], DIL
    MOVZX EDI, byte [RBP - 1]
    CALL _isalpha
    JMP L100000910_1     ; inserted

; Entry 100000910; block 1; address 100000925
L100000910_1:
    ADD RSP, 16
    POP RBP
    RET 



; ---------------------
; Function: 0x100000930
; ---------------------
; Entry 100000930; block 0; address 100000930
L100000930_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 32
    MOV qword [RBP - 8], RDI
    MOV RDI, qword [RBP - 8]
    LEA RSI, [rel L__TEXT___cstring_19]
    CALL _fopen
    JMP L100000930_1     ; inserted

; Entry 100000930; block 1; address 10000094c
L100000930_1:
    MOV qword [RBP - 16], RAX
    CMP qword [RBP - 16], 0
    JNE L100000930_3
    JMP L100000930_2     ; inserted

; Entry 100000930; block 2; address 10000095b
L100000930_2:
    MOV RSI, qword [RBP - 8]
    LEA RDI, [rel L__TEXT___cstring_21]
    MOV AL, 0
    CALL L100000a30_0

; Entry 100000930; block 3; address 10000096d
L100000930_3:
    LEA RAX, [rel L__DATA___common + 24]
    LEA RCX, [rel L__DATA___common + 40]
    LEA RDX, [rel L__DATA___common + 32]
    MOV qword [RDX], 0
    MOV qword [RCX], 0
    MOV qword [RAX], 0
    JMP L100000930_7     ; inserted

; Entry 100000930; block 4; address 1000009a0
L100000930_4:
    CMP EAX, 0
    JE L100000930_6
    JMP L100000930_5     ; inserted

; Entry 100000930; block 5; address 1000009a9
L100000930_5:
    JMP L100000930_7

; Entry 100000930; block 6; address 1000009ae
L100000930_6:
    MOV RDI, qword [RBP - 16]
    CALL _fclose
    JMP L100000930_8     ; inserted

; Entry 100000930; block 7; address 100000997
L100000930_7:
    MOV RDI, qword [RBP - 16]
    CALL L1000007d0_0
    JMP L100000930_4     ; inserted

; Entry 100000930; block 8; address 1000009b7
L100000930_8:
    LEA RCX, [rel L__DATA___common + 32]
    LEA RDX, [rel L__DATA___common + 40]
    LEA RSI, [rel L__DATA___common + 24]
    MOV RDI, qword [RBP - 8]
    MOV RSI, qword [RSI]
    MOV RDX, qword [RDX]
    MOV RCX, qword [RCX]
    MOV dword [RBP - 20], EAX
    CALL L100000790_0
    JMP L100000930_9     ; inserted

; Entry 100000930; block 9; address 1000009e1
L100000930_9:
    LEA RCX, [rel L__DATA___common + 32]
    LEA RDX, [rel L__DATA___common + 40]
    LEA RSI, [rel L__DATA___common + 24]
    MOV RSI, qword [RSI]
    ADD RSI, qword [rel L__DATA___common + 0]
    MOV qword [rel L__DATA___common + 0], RSI
    MOV RDX, qword [RDX]
    ADD RDX, qword [rel L__DATA___common + 8]
    MOV qword [rel L__DATA___common + 8], RDX
    MOV RCX, qword [RCX]
    ADD RCX, qword [rel L__DATA___common + 16]
    MOV qword [rel L__DATA___common + 16], RCX
    ADD RSP, 32
    POP RBP
    RET 



; ---------------------
; Function: 0x100000a30
; ---------------------
; Entry 100000a30; block 0; address 100000a30
L100000a30_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH RBX
    SUB RSP, 408
    TEST AL, AL
    MOVAPS oword [RBP - 256], XMM7
    MOVAPS oword [RBP - 272], XMM6
    MOVAPS oword [RBP - 288], XMM5
    MOVAPS oword [RBP - 304], XMM4
    MOVAPS oword [RBP - 320], XMM3
    MOVAPS oword [RBP - 336], XMM2
    MOVAPS oword [RBP - 352], XMM1
    MOVAPS oword [RBP - 368], XMM0
    MOV qword [RBP - 376], R9
    MOV qword [RBP - 384], R8
    MOV qword [RBP - 392], RCX
    MOV qword [RBP - 400], RDX
    MOV qword [RBP - 408], RSI
    MOV qword [RBP - 416], RDI
    JE L100000a30_2
    JMP L100000a30_1     ; inserted

; Entry 100000a30; block 1; address 100000aa6
L100000a30_1:
    MOVAPS XMM0, oword [RBP - 368]
    MOVAPS oword [RBP - 192], XMM0
    MOVAPS XMM1, oword [RBP - 352]
    MOVAPS oword [RBP - 176], XMM1
    MOVAPS XMM2, oword [RBP - 336]
    MOVAPS oword [RBP - 160], XMM2
    MOVAPS XMM3, oword [RBP - 320]
    MOVAPS oword [RBP - 144], XMM3
    MOVAPS XMM4, oword [RBP - 304]
    MOVAPS oword [RBP - 128], XMM4
    MOVAPS XMM5, oword [RBP - 288]
    MOVAPS oword [RBP - 112], XMM5
    MOVAPS XMM6, oword [RBP - 272]
    MOVAPS oword [RBP - 96], XMM6
    MOVAPS XMM7, oword [RBP - 256]
    MOVAPS oword [RBP - 80], XMM7
    JMP L100000a30_2     ; inserted

; Entry 100000a30; block 2; address 100000b0a
L100000a30_2:
    MOV RAX, qword [RBP - 376]
    MOV qword [RBP - 200], RAX
    MOV RCX, qword [RBP - 384]
    MOV qword [RBP - 208], RCX
    MOV RDX, qword [RBP - 392]
    MOV qword [RBP - 216], RDX
    MOV RSI, qword [RBP - 400]
    MOV qword [RBP - 224], RSI
    MOV RDI, qword [RBP - 408]
    MOV qword [RBP - 232], RDI
    LEA R8, [RBP - 48]
    MOV R9, qword [rel ___stack_chk_guard]
    MOV R9, qword [R9]
    MOV qword [RBP - 16], R9
    MOV R9, qword [RBP - 416]
    MOV qword [RBP - 56], R9
    MOV R10, R8
    LEA R11, [RBP - 240]
    MOV qword [R10 + 16], R11
    LEA R11, [RBP + 16]
    MOV qword [R10 + 8], R11
    MOV dword [R10 + 4], 48
    MOV dword [R10], 8
    MOV RSI, qword [RBP - 56]
    MOV EBX, 1
    MOV EDI, EBX
    MOV RDX, R8
    CALL L100000e20_0



; ---------------------
; Function: 0x100000be0
; ---------------------
; Entry 100000be0; block 0; address 100000be0
L100000be0_0:
_main:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 32
    MOV dword [RBP - 4], 0
    MOV dword [RBP - 8], EDI
    MOV qword [RBP - 16], RSI
    CMP dword [RBP - 8], 2
    JGE L100000be0_2
    JMP L100000be0_1     ; inserted

; Entry 100000be0; block 1; address 100000c00
L100000be0_1:
    LEA RDI, [rel L__TEXT___cstring_43]
    MOV AL, 0
    CALL L100000c80_0

; Entry 100000be0; block 2; address 100000c0e
L100000be0_2:
    MOV dword [RBP - 20], 1
    JMP L100000be0_6     ; inserted

; Entry 100000be0; block 3; address 100000c21
L100000be0_3:
    MOV RAX, qword [RBP - 16]
    MOVSXD RCX, dword [RBP - 20]
    MOV RDI, qword [RAX + RCX * 8]
    CALL L100000930_0
    JMP L100000be0_5     ; inserted

; Entry 100000be0; block 4; address 100000c40
L100000be0_4:
    CMP dword [RBP - 8], 2
    JLE L100000be0_8
    JMP L100000be0_7     ; inserted

; Entry 100000be0; block 5; address 100000c32
L100000be0_5:
    MOV EAX, dword [RBP - 20]
    ADD EAX, 1
    MOV dword [RBP - 20], EAX
    JMP L100000be0_6

; Entry 100000be0; block 6; address 100000c15
L100000be0_6:
    MOV EAX, dword [RBP - 20]
    CMP EAX, dword [RBP - 8]
    JGE L100000be0_4
    JMP L100000be0_3     ; inserted

; Entry 100000be0; block 7; address 100000c4a
L100000be0_7:
    MOV RSI, qword [rel L__DATA___common + 0]
    MOV RDX, qword [rel L__DATA___common + 8]
    MOV RCX, qword [rel L__DATA___common + 16]
    LEA RDI, [rel L__TEXT___cstring_68]
    CALL L100000790_0
    JMP L100000be0_8     ; inserted

; Entry 100000be0; block 8; address 100000c6b
L100000be0_8:
    XOR EAX, EAX
    ADD RSP, 32
    POP RBP
    RET 



; ---------------------
; Function: 0x100000c80
; ---------------------
; Entry 100000c80; block 0; address 100000c80
L100000c80_0:
    PUSH RBP
    MOV RBP, RSP
    PUSH RBX
    SUB RSP, 408
    TEST AL, AL
    MOVAPS oword [RBP - 256], XMM7
    MOVAPS oword [RBP - 272], XMM6
    MOVAPS oword [RBP - 288], XMM5
    MOVAPS oword [RBP - 304], XMM4
    MOVAPS oword [RBP - 320], XMM3
    MOVAPS oword [RBP - 336], XMM2
    MOVAPS oword [RBP - 352], XMM1
    MOVAPS oword [RBP - 368], XMM0
    MOV qword [RBP - 376], R9
    MOV qword [RBP - 384], R8
    MOV qword [RBP - 392], RCX
    MOV qword [RBP - 400], RDX
    MOV qword [RBP - 408], RSI
    MOV qword [RBP - 416], RDI
    JE L100000c80_2
    JMP L100000c80_1     ; inserted

; Entry 100000c80; block 1; address 100000cf6
L100000c80_1:
    MOVAPS XMM0, oword [RBP - 368]
    MOVAPS oword [RBP - 192], XMM0
    MOVAPS XMM1, oword [RBP - 352]
    MOVAPS oword [RBP - 176], XMM1
    MOVAPS XMM2, oword [RBP - 336]
    MOVAPS oword [RBP - 160], XMM2
    MOVAPS XMM3, oword [RBP - 320]
    MOVAPS oword [RBP - 144], XMM3
    MOVAPS XMM4, oword [RBP - 304]
    MOVAPS oword [RBP - 128], XMM4
    MOVAPS XMM5, oword [RBP - 288]
    MOVAPS oword [RBP - 112], XMM5
    MOVAPS XMM6, oword [RBP - 272]
    MOVAPS oword [RBP - 96], XMM6
    MOVAPS XMM7, oword [RBP - 256]
    MOVAPS oword [RBP - 80], XMM7
    JMP L100000c80_2     ; inserted

; Entry 100000c80; block 2; address 100000d5a
L100000c80_2:
    MOV RAX, qword [RBP - 376]
    MOV qword [RBP - 200], RAX
    MOV RCX, qword [RBP - 384]
    MOV qword [RBP - 208], RCX
    MOV RDX, qword [RBP - 392]
    MOV qword [RBP - 216], RDX
    MOV RSI, qword [RBP - 400]
    MOV qword [RBP - 224], RSI
    MOV RDI, qword [RBP - 408]
    MOV qword [RBP - 232], RDI
    XOR R8D, R8D
    LEA R9, [RBP - 48]
    MOV R10, qword [rel ___stack_chk_guard]
    MOV R10, qword [R10]
    MOV qword [RBP - 16], R10
    MOV R10, qword [RBP - 416]
    MOV qword [RBP - 56], R10
    MOV R11, R9
    LEA RBX, [RBP - 240]
    MOV qword [R11 + 16], RBX
    LEA RBX, [RBP + 16]
    MOV qword [R11 + 8], RBX
    MOV dword [R11 + 4], 48
    MOV dword [R11], 8
    MOV RSI, qword [RBP - 56]
    MOV EDI, R8D
    MOV RDX, R9
    CALL L100000e20_0



; ---------------------
; Function: 0x100000e20
; ---------------------
; Entry 100000e20; block 0; address 100000e20
L100000e20_0:
    PUSH RBP
    MOV RBP, RSP
    SUB RSP, 32
    MOV RAX, qword [rel ___stderrp]
    MOV dword [RBP - 4], EDI
    MOV qword [RBP - 16], RSI
    MOV qword [RBP - 24], RDX
    MOV RDI, qword [RAX]
    MOV RSI, qword [RBP - 16]
    MOV RDX, qword [RBP - 24]
    CALL _vfprintf
    JMP L100000e20_1     ; inserted

; Entry 100000e20; block 1; address 100000e4a
L100000e20_1:
    CMP dword [RBP - 4], 0
    JE L100000e20_3
    JMP L100000e20_2     ; inserted

; Entry 100000e20; block 2; address 100000e54
L100000e20_2:
    LEA RDI, [rel L__TEXT___cstring_74]
    CALL _perror
    JMP L100000e20_4     ; inserted

; Entry 100000e20; block 3; address 100000e65
L100000e20_3:
    MOV RAX, qword [rel ___stderrp]
    MOV RDI, qword [RAX]
    LEA RSI, [rel L__TEXT___cstring_76]
    MOV AL, 0
    CALL _fprintf
    JMP L100000e20_5     ; inserted

; Entry 100000e20; block 4; address 100000e60
L100000e20_4:
    JMP L100000e20_5

; Entry 100000e20; block 5; address 100000e7d
L100000e20_5:
    MOV EDI, 1
    CALL _exit




section .data
L__TEXT___cstring_0: db `%6lu %6lu %6lu %s\n`, 0
L__TEXT___cstring_19: db `r`, 0
L__TEXT___cstring_21: db `cannot open file \`%s'`, 0
L__TEXT___cstring_43: db `usage: wc FILE [FILE...]`, 0
L__TEXT___cstring_68: db `total`, 0
L__TEXT___cstring_74: db ` `, 0
L__TEXT___cstring_76: db `\n`, 0

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


