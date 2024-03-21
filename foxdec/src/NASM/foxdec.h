#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>


// Declare a datatype for 128 bit values
typedef unsigned uint128_t __attribute__ ((mode (TI)));

// Union of various 128 bit types
union d128{
        uint128_t u;
    } ;

// Two sequentially packed bytes 
struct twobytes
{
    char lo_byte;
    char hi_byte;
} __attribute__((packed));

// Union of various 64 bit types
// Also contains accessor for 32/16/8 bits
union d64{
        uint64_t u;
        int64_t s;
        double d;
        void* ptr;
        uint32_t u32;
        uint16_t u16;
        struct twobytes bytes;
    } ;

// Union of various 32 bit types
union d32 {
        uint32_t u;
        int32_t s;
        float f;
    } ;


// Union of various 16 bit types
union d16 {
        uint16_t u;
    } ;

// Union of various 8 bit types
union d8 {
        uint8_t u;
        int8_t s;
        char c;
    } ;

// 128 bit registers
union d128 XMM0,XMM1,XMM2,XMM3,XMM4,XMM5,XMM6,XMM7,XMM8;
// 64 bit registers
union d64 RAX,RBX,RCX,RDX,RSI,RDI,RBP, RSP,R8,R9,R10,R11,R12,R13,R14,R15,FS;
// flags (including custom ones such as GF)
bool ZF,CF,SF,GF;

union d64* RAX_ptr = &RAX;

// Reading a 32-bit value from a 64/128 bit register
#define read32(val) ((union d32) ((uint32_t) (val.u32)))
// Reading a 16-bit value from a 64/128 bit register
#define read16(val) ((union d16) ((uint16_t) (val.u16)))
// Reading an 8-bit value from a 64/128 bit register
#define read8(val) ((union d8) ((uint8_t) (val.bytes.lo_byte)))

// Writing a 8-bit value to low part of a 64-bit register
#define write8(dst, val) dst.bytes.lo_byte = (uint8_t) val.u;
// Writing a 16-bit value to low part of a 64-bit register
#define write16(dst, val) dst.bytes.lo_byte = (uint16_t) val.u;
// Writing a 32-bit value to a 64/128-bit register
#define write32(dst, val) dst.u = (uint64_t) val.u;

// Execute a function call to function f


#if defined __x86_64__ || defined(_M_X64)
// TODO clobbering during call
// TODO XMM0 register return
#define CALL(f) do {\
asm volatile ("movq %[input0], %%rdi;" :  : [input0] "g" (RDI.u)  : "%rdi");\
asm volatile ("movq %[input0], %%rsi;" :  : [input0] "g" (RSI.u)  : "%rsi");\
asm volatile ("movq %[input0], %%rdx;" :  : [input0] "g" (RDX.u)  : "%rdx");\
asm volatile ("movq %[input0], %%rcx;" :  : [input0] "g" (RCX.u)  : "%rcx");\
asm volatile ("movq %[input0], %%r8;"  :  : [input0] "g" (R8.u)  : "%r8");\
asm volatile ("movq %[input0], %%r9;"  :  : [input0] "g" (R9.u)  : "%r9");\
asm volatile ("call " #f);\
asm volatile ("movq %%rax, %[output0];" : [output0] "=g" (RAX) : );\
} while(0)
#elif defined(__aarch64__) || defined(_M_ARM64)
/* Map x86-64 System V ABI to AArch64
 *
 * First push LR register, making sure SP is 16-aligned.
 * Then, map x86-64 parameter-registers to ARM ones.
 * Call the function, and pop register LR.
 * The return value register of ARM is mapped to the return register of x86-64.
 *
 * All caller-saved/temporary/sratch registers are clobbered.
 * Only callee-saved registers, as well as FP and LR are not clobbered.
 *
 * https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst#the-base-procedure-call-standard
*/
#define CALL(f) do {\
asm volatile(\
  "sub sp, sp, #0x10;"\
  "str lr, [sp];"\
  "ldr x0, [%[input0]];"\
  "ldr x1, [%[input1]];"\
  "ldr x2, [%[input2]];"\
  "ldr x3, [%[input3]];"\
  "ldr x4, [%[input4]];"\
  "ldr x5, [%[input5]];"\
  "bl " #f ";"\
  "ldr lr, [sp];"\
  "str x0, [%[ptrToRAX]];"\
  "add sp, sp, #0x10;"\
  : [output] "=g" (RAX)\
  : [input0] "r" (&RDI)\
  , [input1] "r" (&RSI)\
  , [input2] "r" (&RDX)\
  , [input3] "r" (&RCX)\
  , [input4] "r" (&R8)\
  , [input5] "r" (&R9)\
  , [ptrToRAX]"r" (&RAX)\
  , "m" (RDI.u)\
  , "m" (RDI.u)\
  , "m" (RDI.u)\
  , "m" (RDI.u)\
  , "m" (RDI.u)\
  , "m" (RDI.u)\
  : "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9"\
  , "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18"\
  );\
} while(0)
#else
#error Unknown architecture!
#endif


// 128 bit registers
union d128 XMM0,XMM1,XMM2,XMM3,XMM4,XMM5,XMM6,XMM7,XMM8;
// 64 bit registers
union d64 RAX,RBX,RCX,RDX,RSI,RDI,RBP, RSP,R8,R9,R10,R11,R12,R13,R14,R15;
// flags (including custom ones such as GF)
bool ZF,CF,SF,GF;


uint64_t Ltemp_storage_foxdec_init;
uint64_t* Ltemp_storage_foxdec = &Ltemp_storage_foxdec_init;





