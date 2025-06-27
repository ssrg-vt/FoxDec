#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <fcntl.h>
#include <unistd.h>
#include <libelf.h>
#include <gelf.h>
#include "xed-interface.h"
#include <assert.h>


void print_operand_info(const xed_decoded_inst_t* xedd) {
    const xed_inst_t* xi = xed_decoded_inst_inst(xedd);
    int n = xed_inst_noperands(xi);

    for (int i = 0; i < n; i++) {
        const xed_operand_t* op = xed_inst_operand(xi,i);
        xed_operand_enum_t op_name = xed_operand_name(op);

        if (op_name == XED_OPERAND_INVALID) {
            continue;
        }

        xed_operand_action_enum_t action = xed_operand_rw(op);
        const char* access = xed_operand_action_enum_t2str(action);
        const char* op_kind = xed_operand_enum_t2str(op_name);
        xed_operand_visibility_enum_t op_vis = xed_operand_operand_visibility(op);

        if (op_name == XED_OPERAND_BASE0 || op_name == XED_OPERAND_BASE1) {
            continue;
        }



        printf("%-10s; ", xed_operand_visibility_enum_t2str(op_vis));
        printf("%-3s", access);
        //printf("Operand[%d]: %-10s  Access: %-12s, Visibility: %s", i, op_kind, access, xed_operand_visibility_enum_t2str(op_vis));

        if (op_vis == XED_OPVIS_SUPPRESSED) {
            printf("; ");
            // Register operands
            if (xed_operand_is_register(op_name)) {
                xed_reg_enum_t reg = xed_decoded_inst_get_reg(xedd, op_name);
                printf("REG; %s", xed_reg_enum_t2str(reg));
            }
            // Memory operands (MEM0, MEM1, AGEN)
            else if (op_name == XED_OPERAND_MEM0 || op_name == XED_OPERAND_MEM1 || op_name == XED_OPERAND_AGEN) {
                int mem_idx = (op_name == XED_OPERAND_MEM1) ? 1 : 0;
                xed_reg_enum_t base = xed_decoded_inst_get_base_reg(xedd, mem_idx);
                xed_reg_enum_t index = xed_decoded_inst_get_index_reg(xedd, mem_idx);
                unsigned int scale = xed_decoded_inst_get_scale(xedd, mem_idx);
                xed_int64_t disp = xed_decoded_inst_get_memory_displacement(xedd, mem_idx);

                printf("MEM; [");

                bool first = true;
                if (base != XED_REG_INVALID) {
                    printf("%s", xed_reg_enum_t2str(base));
                    first = false;
                }
                if (index != XED_REG_INVALID) {
                    if (!first) printf(" + ");
                    printf("%s*%u", xed_reg_enum_t2str(index), scale);
                    first = false;
                }
                if (disp != 0 || first) {
                    if (!first) printf(" + ");
                    printf("0x%llx", (unsigned long long)disp);
                }

                printf("]");
            }

            // Immediate operands
            else if (op_name == XED_OPERAND_IMM0 || op_name == XED_OPERAND_IMM1) {
                xed_uint64_t imm = xed_decoded_inst_get_unsigned_immediate(xedd);
                unsigned int imm_bits = xed_decoded_inst_get_immediate_width_bits(xedd);
                printf("IMM; 0x%llx (%u bits)", (unsigned long long)imm, imm_bits);
            }
            else if (op_vis == XED_OPVIS_SUPPRESSED && op_name == XED_OPERAND_RELBR) {
                assert(false);
            }
            else {
                //printf("Operand[%d]: %-10s  Access: %-12s, Visibility: %s", i, op_kind, access, xed_operand_visibility_enum_t2str(op_vis));
            }
        }

        printf("\n");
    }
}




void disassemble_buffer(unsigned char* buffer, size_t size, xed_state_t* dstate, uint64_t vaddr_base) {
    xed_decoded_inst_t xedd;
    xed_error_enum_t xed_error;

    for (size_t offset = 0; offset < size; offset++) {
        xed_decoded_inst_zero_set_mode(&xedd, dstate);

        size_t max_len = (size - offset > 15) ? 15 : size - offset;

        xed_error = xed_decode(&xedd, buffer + offset, max_len);


        if (xed_error == XED_ERROR_NONE) {
            char buffer_str[128];

            xed_print_info_t pi;
            memset(&pi, 0, sizeof(pi));
            pi.p = &xedd;
            pi.buf = buffer_str;
            pi.blen = sizeof(buffer_str);
            pi.context = 0;
            pi.syntax = XED_SYNTAX_INTEL;
            uint64_t si = xed_decoded_inst_get_length(&xedd);
            if (xed_format_generic(&pi)) {
                unsigned int imm_width = xed_decoded_inst_get_immediate_width_bits(&xedd);
                printf("0x%llx (%lu,%d): ", (unsigned long long)(vaddr_base + offset),si,imm_width);
                printf("%s\n", buffer_str);
                print_operand_info(&xedd);
                puts("");
            } else {
                //printf("Failed to format instruction\n");
            }
        } else {
            //printf("Invalid instruction (%s)\n", xed_error_enum_t2str(xed_error));
        }
    }
}

int main(int argc, char** argv) {
    /*
    int i;
    printf("%d\n",argc);
    for(i=1;i<argc;i++)
    {
        printf("%s ",argv[i]);
    }
    puts("");
    */
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <EXECUTABLE> <SECTION>\n", argv[0]);
        fprintf(stderr, "Example: %s /usr/bin/ssh .text .plt\n", argv[0]);
        return 1;
    }

    if (elf_version(EV_CURRENT) == EV_NONE) {
        fprintf(stderr, "ELF library initialization failed\n");
        return 1;
    }

    int fd = open(argv[1], O_RDONLY);
    if (fd < 0) {
        perror("open");
        return 1;
    }

    Elf* e = elf_begin(fd, ELF_C_READ, NULL);
    if (!e) {
        fprintf(stderr, "elf_begin failed: %s\n", elf_errmsg(-1));
        close(fd);
        return 1;
    }

    GElf_Ehdr ehdr;
    if (gelf_getehdr(e, &ehdr) == NULL) {
        fprintf(stderr, "gelf_getehdr failed: %s\n", elf_errmsg(-1));
        elf_end(e);
        close(fd);
        return 1;
    }

    xed_tables_init();

    xed_state_t dstate;
    xed_state_zero(&dstate);

    if (ehdr.e_machine == EM_X86_64) {
        dstate.mmode = XED_MACHINE_MODE_LONG_64;
        dstate.stack_addr_width = XED_ADDRESS_WIDTH_64b;
    } else if (ehdr.e_machine == EM_386) {
        dstate.mmode = XED_MACHINE_MODE_LEGACY_32;
        dstate.stack_addr_width = XED_ADDRESS_WIDTH_32b;
    } else {
        fprintf(stderr, "Unsupported architecture (machine type: %u)\n", ehdr.e_machine);
        elf_end(e);
        close(fd);
        return 1;
    }

    size_t shstrndx;
    if (elf_getshdrstrndx(e, &shstrndx) != 0) {
        fprintf(stderr, "elf_getshdrstrndx failed: %s\n", elf_errmsg(-1));
        elf_end(e);
        close(fd);
        return 1;
    }

    Elf_Scn* scn = NULL;
    while ((scn = elf_nextscn(e, scn)) != NULL) {
        GElf_Shdr shdr;
        if (gelf_getshdr(scn, &shdr) != &shdr) {
            fprintf(stderr, "gelf_getshdr failed: %s\n", elf_errmsg(-1));
            continue;
        }

        const char* name = elf_strptr(e, shstrndx, shdr.sh_name);
        if (name) {
            int n;
            for (n=2;n<argc;n++) {
                if (strcmp(name, argv[n]) == 0) {
                    /*printf("Found %s text section: offset 0x%lx, vaddr 0x%lx, size %lu bytes\n",
                      argv[n],
                      (unsigned long)shdr.sh_offset,
                      (unsigned long)shdr.sh_addr,
                      (unsigned long)shdr.sh_size);
                    */
                    Elf_Data* data = elf_getdata(scn, NULL);
                    if (!data) {
                        fprintf(stderr, "elf_getdata failed for %s: %s\n", argv[n],elf_errmsg(-1));
                    }

                    uint64_t base_vaddr = (ehdr.e_type == ET_EXEC) ? shdr.sh_addr : 0;

                    disassemble_buffer((unsigned char*)data->d_buf, data->d_size, &dstate, shdr.sh_offset + base_vaddr);
                }
            }
        }
    }

    elf_end(e);
    close(fd);
    return 0;
}

