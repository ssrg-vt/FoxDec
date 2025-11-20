#include "xed-interface.h"

#include <assert.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


void print_operand_info(char* ret,int ret_idx_,const xed_decoded_inst_t* xedd) {
    const xed_inst_t* xi = xed_decoded_inst_inst(xedd);
    int n = xed_inst_noperands(xi);
    int ret_idx = ret_idx_;


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



        ret_idx += sprintf(ret+ret_idx,"%-10s; ", xed_operand_visibility_enum_t2str(op_vis));
        ret_idx += sprintf(ret+ret_idx,"%-3s", access);
        //printf("Operand[%d]: %-10s  Access: %-12s, Visibility: %s", i, op_kind, access, xed_operand_visibility_enum_t2str(op_vis));

        if (op_vis == XED_OPVIS_SUPPRESSED) {
            ret_idx += sprintf(ret+ret_idx,"; ");
            // Register operands
            if (xed_operand_is_register(op_name)) {
                xed_reg_enum_t reg = xed_decoded_inst_get_reg(xedd, op_name);
                ret_idx += sprintf(ret+ret_idx,"REG; %s", xed_reg_enum_t2str(reg));
            }
            // Memory operands (MEM0, MEM1, AGEN)
            else if (op_name == XED_OPERAND_MEM0 || op_name == XED_OPERAND_MEM1 || op_name == XED_OPERAND_AGEN) {
                int mem_idx = (op_name == XED_OPERAND_MEM1) ? 1 : 0;
                xed_reg_enum_t base = xed_decoded_inst_get_base_reg(xedd, mem_idx);
                xed_reg_enum_t index = xed_decoded_inst_get_index_reg(xedd, mem_idx);
                unsigned int scale = xed_decoded_inst_get_scale(xedd, mem_idx);
                xed_int64_t disp = xed_decoded_inst_get_memory_displacement(xedd, mem_idx);

                ret_idx += sprintf(ret+ret_idx,"MEM; [");

                bool first = true;
                if (base != XED_REG_INVALID) {
                    ret_idx += sprintf(ret+ret_idx,"%s", xed_reg_enum_t2str(base));
                    first = false;
                }
                if (index != XED_REG_INVALID) {
                    if (!first) ret_idx += sprintf(ret+ret_idx," + ");
                    ret_idx += sprintf(ret+ret_idx,"%s*%u", xed_reg_enum_t2str(index), scale);
                    first = false;
                }
                if (disp != 0 || first) {
                    if (!first) ret_idx += sprintf(ret+ret_idx," + ");
                    ret_idx += sprintf(ret+ret_idx,"0x%llx", (unsigned long long)disp);
                }

                ret_idx += sprintf(ret+ret_idx,"]");
            }

            // Immediate operands
            else if (op_name == XED_OPERAND_IMM0 || op_name == XED_OPERAND_IMM1) {
                xed_uint64_t imm = xed_decoded_inst_get_unsigned_immediate(xedd);
                unsigned int imm_bits = xed_decoded_inst_get_immediate_width_bits(xedd);
                ret_idx += sprintf(ret+ret_idx,"IMM; 0x%llx (%u bits)", (unsigned long long)imm, imm_bits);
            }
            else if (op_vis == XED_OPVIS_SUPPRESSED && op_name == XED_OPERAND_RELBR) {
                assert(false);
            }
            else {
                //printf("Operand[%d]: %-10s  Access: %-12s, Visibility: %s", i, op_kind, access, xed_operand_visibility_enum_t2str(op_vis));
            }
        }
        ret_idx += sprintf(ret+ret_idx,"\n");
    }
}



char* disassemble1_buffer(char* buffer, size_t size, unsigned long long vaddr_base) {
  char* ret = calloc(256,sizeof(char));
  int ret_idx = 0;

    xed_tables_init();

   xed_state_t dstate;
    xed_state_zero(&dstate);


    dstate.mmode = XED_MACHINE_MODE_LONG_64;
    dstate.stack_addr_width = XED_ADDRESS_WIDTH_64b;

    xed_decoded_inst_t xedd;
    xed_error_enum_t xed_error;

        xed_decoded_inst_zero_set_mode(&xedd, &dstate);
        size_t max_len = (size > 15) ? 15 : size ;

        xed_error = xed_decode(&xedd, (unsigned char*) buffer, max_len);


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
                ret_idx += sprintf(ret+ret_idx,"0x%llx (%llu,%d): ", vaddr_base,si,imm_width);
                ret_idx += sprintf(ret+ret_idx,"%s\n", buffer_str);

                print_operand_info(ret,ret_idx,&xedd);
            } else {
                //printf("Failed to format instruction\n");
            }
        } else {
            //printf("Invalid instruction (%s)\n", xed_error_enum_t2str(xed_error));
        }

  return ret;
}

void free_buffer(char* buffer) {
    free(buffer);
}

