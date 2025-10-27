#!/usr/bin/env python3

"""
reconstruct_cfi_to_asm.py

Reconstruct assembler-friendly .cfi_* directives from an ELF's .eh_frame.

This version is robust to different pyelftools shapes (rows as objects or dicts,
instructions as objects, dicts, or tuples).
"""

from __future__ import annotations
import argparse
import sys
import struct
import re
import io
from elftools.elf.elffile import ELFFile
from elftools.dwarf.callframe import CIE, FDE, CFARule

## =============================================================================
## == GENERATE =================================================================
## =============================================================================

DWARF_X86_64_REGS_ATT = {
    0: "%rax",
    1: "%rdx",
    2: "%rcx",
    3: "%rbx",
    4: "%rsi",
    5: "%rdi",
    6: "%rbp",
    7: "%rsp",
    8: "%r8",
    9: "%r9",
    10: "%r10",
    11: "%r11",
    12: "%r12",
    13: "%r13",
    14: "%r14",
    15: "%r15",
    16: "%rip",
    17: "%xmm0",
    18: "%xmm1",
    19: "%xmm2",
    20: "%xmm3",
    21: "%xmm4",
    22: "%xmm5",
    23: "%xmm6",
    24: "%xmm7",
    25: "%xmm8",
    26: "%xmm9",
    27: "%xmm10",
    28: "%xmm11",
    29: "%xmm12",
    30: "%xmm13",
    31: "%xmm14",
    32: "%xmm15",
    33: "%st(0)",
    34: "%st(1)",
    35: "%st(2)",
    36: "%st(3)",
    37: "%st(4)",
    38: "%st(5)",
    39: "%st(6)",
    40: "%st(7)",
    41: "%mm0",
    42: "%mm1",
    43: "%mm2",
    44: "%mm3",
    45: "%mm4",
    46: "%mm5",
    47: "%mm6",
    48: "%mm7",
    49: "%rflags",
    50: "%es",
    51: "%cs",
    52: "%ss",
    53: "%ds",
    54: "%fs",
    55: "%gs",
    58: "%fs_base",
    59: "%gs_base",
}

DW_CFA_OFFSET_SLOT_SIZE = -8


# === DW_EH_PE encodings ===
# https://refspecs.linuxbase.org/LSB_3.0.0/LSB-Core-generic/LSB-Core-generic/dwarfehencoding.html
DW_EH_PE_absptr = 0x00
DW_EH_PE_uleb128 = 0x01
DW_EH_PE_udata2 = 0x02
DW_EH_PE_udata4 = 0x03
DW_EH_PE_udata8 = 0x04
DW_EH_PE_sleb128 = 0x09
DW_EH_PE_sdata2 = 0x0A
DW_EH_PE_sdata4 = 0x0B
DW_EH_PE_sdata8 = 0x0C

DW_EH_PE_pcrel = 0x10
DW_EH_PE_textrel = 0x20
DW_EH_PE_datarel = 0x30
DW_EH_PE_funcrel = 0x40
DW_EH_PE_aligned = 0x50

DW_EH_PE_indirect = 0x80
DW_EH_PE_omit = 0xFF


# === Helpers ===
def encode_uleb128(value: int) -> bytes:
    """Encode unsigned LEB128."""
    if value < 0:
        raise ValueError("ULEB128 cannot encode negative")
    out = []
    while True:
        byte = value & 0x7F
        value >>= 7
        if value:
            out.append(byte | 0x80)
        else:
            out.append(byte)
            break
    return bytes(out)


def safe_get(buf, offset):
    if offset >= len(buf):
        raise ValueError(f"Buffer underrun at offset {offset}, length={len(buf)}")
    return buf[offset]


def read_uleb128(buf, offset):
    result = 0
    shift = 0
    offset1 = offset
    while True:
        b = safe_get(buf, offset1)
        offset1 += 1
        result |= (b & 0x7F) << shift
        if b & 0x80 == 0:
            break
        shift += 7
    return result, offset1, buf[offset:offset1]


def read_sleb128(buf, offset):
    result = 0
    shift = 0
    size = 64
    offset1 = offset
    while True:
        b = safe_get(buf, offset)
        offset1 += 1
        result |= (b & 0x7F) << shift
        shift += 7
        if b & 0x80 == 0:
            if (b & 0x40) and shift < size:
                result |= -(1 << shift)
            break
    return result, offset1, buf[offset:offset1]


def read_encoded(buf, offset, encoding):
    """Read a value according to DW_EH_PE encoding."""
    if encoding == DW_EH_PE_omit:
        return None, offset, []

    fmt_map = {
        DW_EH_PE_absptr: (struct.calcsize("P"), "P"),
        DW_EH_PE_udata2: (2, "<H"),
        DW_EH_PE_udata4: (4, "<I"),
        DW_EH_PE_udata8: (8, "<Q"),
        DW_EH_PE_sdata2: (2, "<h"),
        DW_EH_PE_sdata4: (4, "<i"),
        DW_EH_PE_sdata8: (8, "<q"),
    }

    form = encoding & 0x0F
    if form == DW_EH_PE_uleb128:
        val, offset1, bytes = read_uleb128(buf, offset)
    elif form == DW_EH_PE_sleb128:
        val, offset1, bytes = read_sleb128(buf, offset)
    elif form in fmt_map:
        size, fmt = fmt_map[form]
        val = struct.unpack_from(fmt, buf, offset)[0]
        offset1 = offset + size
    else:
        raise NotImplementedError(f"Encoding {encoding:#x} not supported")

    # Apply relative encodings
    app = encoding & 0x70
    if app == DW_EH_PE_pcrel:
        val = offset1 + val
    # (others like textrel, datarel, funcrel omitted for brevity)

    if encoding & DW_EH_PE_indirect:
        # Would require reading memory at 'val' (not implemented)
        pass

    return val, offset1, buf[offset:offset1]


def hex_list(x):
    return ",".join(map(hex, x))


# === CIE header parser ===
# https://refspecs.linuxfoundation.org/LSB_3.0.0/LSB-PDA/LSB-PDA/ehframechpt.html

global_typeinfo_id = 0


def parse_CIE_header(buf, out_lines):
    offset = 0
    cie_header = {}

    length, offset, bytes = read_encoded(buf, offset, DW_EH_PE_udata4)
    cie_header["length"] = length

    if length == 0xFFFFFFFF:
        extended_length, offset, bytes = read_encoded(buf, offset, DW_EH_PE_udata8)
        cie_header["extended_length"] = extended_length

    cie_id, offset, bytes = read_encoded(buf, offset, DW_EH_PE_udata4)
    cie_header["cie_id"] = cie_id

    version = safe_get(buf, offset)
    offset += 1
    cie_header["version"] = version

    augmentation_string = ""
    char = safe_get(buf, offset)
    offset += 1
    while char != 0:
        augmentation_string = augmentation_string + chr(char)
        char = safe_get(buf, offset)
        offset += 1
    cie_header["augmentation_string"] = augmentation_string

    if "eh" in augmentation_string:
        offset += 8  # TODO: assumes 64 bit arch

    code_alignment_factor, offset, bytes = read_uleb128(buf, offset)
    cie_header["code_alignment_factor"] = code_alignment_factor

    data_alignment_factor, offset, bytes = read_sleb128(buf, offset)
    cie_header["data_alignment_factor"] = data_alignment_factor

    return_address_register, offset, bytes = read_uleb128(buf, offset)
    cie_header["return_address_register"] = return_address_register

    if "z" in augmentation_string:
        augmentation_data_length, offset, bytes = read_uleb128(buf, offset)
        cie_header["augmentation_data_length"] = augmentation_data_length
        cie_header["augmentation_data_offset"] = offset
        augmentation_data = buf[offset : offset + augmentation_data_length]
        cie_header["augmentation_data"] = augmentation_data

    return cie_header


def mk_data_entry(encoding, val, bytes):
    if encoding == DW_EH_PE_uleb128:
        return "  .uleb128 " + hex(val)
    else:
        return "  .byte " + hex_list(bytes)


# === LSDA parser ===
# https://itanium-cxx-abi.github.io/cxx-abi/exceptions.pdf
def parse_and_emit_lsda(elf, loc, buf, out_lines, address, stubs):
    offset = 0
    lsda = {}

    # LPStart encoding
    lp_start_enc = safe_get(buf, offset)
    out_lines.append('.section .gcc_except_table,"a",@progbits')
    out_lines.append(".p2align 2")
    out_lines.append("@" + hex(address) + ":")
    out_lines.append("  .byte " + hex(lp_start_enc) + "\t# lp_start encoding")

    offset += 1
    if lp_start_enc != DW_EH_PE_omit:
        lp_start, offset, bytes = read_encoded(buf, offset, lp_start_enc)
        out_lines.append(mk_data_entry(lp_start_enc, lp_start, bytes) + "\t# lp_start")
        lsda["lp_start"] = lp_start

    # TType
    ttype_enc = safe_get(buf, offset)
    offset += 1
    ttype_len = 0
    out_lines.append("  .byte " + hex(ttype_enc) + "\t# ttype encoding")
    if ttype_enc != DW_EH_PE_omit:
        if ttype_enc == 0x9B:
            ttype_len, offset, bytes = read_uleb128(buf, offset)
            out_lines.append(
                "  .uleb128 .L_LSDA_"
                + hex(address)
                + "_END - .L_LSDA_"
                + hex(address)
                + "_CALLSITE_TABLE_HEADER_START \t# length = "
                + hex(ttype_len)
                + ", offset = "
                + hex(offset)
            )
            out_lines.append(
                ".L_LSDA_" + hex(address) + "_CALLSITE_TABLE_HEADER_START:"
            )
            lsda["ttype_encoding"] = ttype_enc
            lsda["ttype_len"] = ttype_len
            lsda["lsda_end"] = ttype_len + offset
        else:
            out_lines.append(
                "ERROR: do not how to deal with ttype encoding " + hex(ttype_enc)
            )

    # Call site table
    cs_enc = safe_get(buf, offset)
    offset += 1
    out_lines.append("  .byte " + hex(cs_enc) + "\t# call-site encoding")

    cs_len, offset, bytes = read_uleb128(buf, offset)
    out_lines.append(
        "  .uleb128 .L_LSDA_"
        + hex(address)
        + "_CALLSITE_TABLE_END - .L_LSDA_"
        + hex(address)
        + "_CALLSITE_TABLE_START \t# call site table length ("
        + hex(cs_len)
        + ")"
    )
    out_lines.append(".L_LSDA_" + hex(address) + "_CALLSITE_TABLE_START:")

    lsda["call_site_encoding"] = cs_enc
    lsda["call_site_table_length"] = cs_len

    if cs_enc != DW_EH_PE_uleb128:
        out_lines.append("UNKNOWN ENCODING: " + str(cs_enc))
        return

    cs_start = offset
    cs_end = offset + cs_len
    call_sites = []
    while offset < cs_end:
        start, offset, bytes = read_encoded(buf, offset, cs_enc)
        out_lines.append(
            "  .uleb128 @" + hex(start + loc) + " - @" + hex(loc) + "\t# start"
        )

        length, offset, bytes = read_encoded(buf, offset, cs_enc)
        out_lines.append(
            "  .uleb128 @"
            + hex(length + start + loc)
            + " - @"
            + hex(start + loc)
            + "\t# length"
        )

        landing_pad, offset, bytes = read_encoded(buf, offset, cs_enc)
        out_lines.append(
            "  .uleb128 @"
            + hex(landing_pad + loc)
            + " - @"
            + hex(loc)
            + "\t# landing-pad"
        )

        action, offset, bytes = read_uleb128(buf, offset)
        out_lines.append("  .uleb128 " + hex(action) + "\t\t\t# action")

        call_sites.append(
            {
                "start": start,
                "length": length,
                "landing_pad": landing_pad,
                "action": action,
            }
        )
    lsda["call_sites"] = call_sites

    max_action = 0
    for call_site in call_sites:
        if max_action < call_site["action"]:
            max_action = call_site["action"]

    out_lines.append(".L_LSDA_" + hex(address) + "_CALLSITE_TABLE_END:")

    lsda["action_records"] = []
    if max_action != 0:
        curr_action = 1
        lsda["offset_start_of_action_records"] = offset
        lsda["offset_end_of_action_records"] = offset + max_action + 1
        out_lines.append("#Action records")
        out_lines.append("#   typeinfo: indices into typeinfo table")
        out_lines.append("#   next action: relative pointer to next action record")
        while offset < lsda["offset_end_of_action_records"]:
            action_type_filter, offset, _ = read_sleb128(buf, offset)
            action_next_offset, offset, _ = read_sleb128(buf, offset)
            action_record = {}
            action_record["type_filter"] = action_type_filter
            action_record["next_offset"] = action_next_offset
            lsda["action_records"].append(action_record)

            out_lines.append("# Action record " + str(curr_action))
            out_lines.append("  .sleb128 " + str(action_type_filter) + "\t# typeinfo")
            out_lines.append(
                "  .sleb128 " + str(action_next_offset) + "\t# next action"
            )
            curr_action += 1

    max_type_index = 0
    for action_record in lsda["action_records"]:
        if max_type_index < action_record["type_filter"]:
            max_type_index = action_record["type_filter"]
    lsda["max_type_index"] = max_type_index

    lsda["types_table"] = []
    if ttype_enc != DW_EH_PE_omit:
        offset = lsda["lsda_end"] - max_type_index * 4
        out_lines.append(".p2align 2")
        out_lines.append("# typeinfo table")
        for type_index in range(max_type_index):
            curr_address = address + offset
            type_info, offset, bytes = read_encoded(buf, offset, DW_EH_PE_sdata4)
            lsda["types_table"].append(type_info + curr_address)

            # TODO: This is a quick hack
            #       I added a global typeinfo ID to make sure they are not duplicated
            global global_typeinfo_id
            label = f".L_LSDA_typeinfo_{global_typeinfo_id}"  # + str(max_type_index - type_index)
            global_typeinfo_id += 1

            out_lines.append(label + ":")
            if type_info == 0:
                out_lines.append("  .long 0")
            else:
                type_info_object_location = type_info + curr_address
                out_lines.append(
                    "  .long @"
                    + hex(type_info_object_location)
                    + " - "
                    + label
                    + " # @"
                    + hex(type_info_object_location)
                )
                # reloc = find_relocation(elf,type_info_object_location)
                # type_info_object_label = ".L" + reloc_to_name(reloc) + ".DW.stub"
                # out_lines.append( "  .long " + type_info_object_label + " - " + label + " # @" + hex(type_info_object_location))
                # stubs.append(reloc_to_name(reloc))
                ##out_lines.append( "type_info = " + hex(type_info) + ", curr_address = " + hex(curr_address)  + ", address = " + hex(address))

    out_lines.append(".L_LSDA_" + hex(address) + "_END:")
    return lsda


def reloc_to_name(reloc):
    if reloc["type"] == 8:
        return "internal_address_" + hex(reloc["addend"])

    return reloc["symbol"]


def emit_instruction(instr, out_lines):
    opname = str(instr).split()[0]
    args = instr.args
    try:
        if opname == "DW_CFA_def_cfa":
            reg, ofs = args
            reg = DWARF_X86_64_REGS_ATT[reg]
            out_lines.append(f".cfi_def_cfa {reg}, {ofs}")
        elif opname == "DW_CFA_def_cfa_offset":
            (ofs,) = args
            out_lines.append(f".cfi_def_cfa_offset {ofs}")
        elif opname == "DW_CFA_def_cfa_register":
            (reg,) = args
            reg = DWARF_X86_64_REGS_ATT[reg]
            out_lines.append(f".cfi_def_cfa_register {reg}")
        elif opname == "DW_CFA_offset":
            reg, ofs = args
            reg = DWARF_X86_64_REGS_ATT[reg]
            ofs *= DW_CFA_OFFSET_SLOT_SIZE
            out_lines.append(f".cfi_offset {reg}, {ofs}")
        elif opname == "DW_CFA_restore":
            (reg,) = args
            reg = DWARF_X86_64_REGS_ATT[reg]
            out_lines.append(f".cfi_restore {reg}")
        elif opname == "DW_CFA_undefined":
            (reg,) = args
            reg = DWARF_X86_64_REGS_ATT[reg]
            out_lines.append(f".cfi_undefined {reg}")
        elif opname == "DW_CFA_same_value":
            (reg,) = args
            reg = DWARF_X86_64_REGS_ATT[reg]
            out_lines.append(f".cfi_same_value {reg}")
        elif opname in ("DW_CFA_expression", "DW_CFA_val_expression"):
            reg, expr_bytes = args[0], args[1]
            reg = DWARF_X86_64_REGS_ATT[reg]
            pretty = disassemble_expr(bytes(expr_bytes), dwarf)
            out_lines.append(f"# .cfi_expression {reg}, {pretty}")

        elif opname == "DW_CFA_def_cfa_expression":
            out_lines.append(
                ".cfi_escape 0x0f,  "
                + hex_list(encode_uleb128(len(instr.args[0])))
                + ",  "
                + hex_list(instr.args[0])
                + " # DW_CFA_def_cfa_expression"
            )

        elif opname == "DW_CFA_restore_state":
            out_lines.append(".cfi_restore_state")
        elif opname == "DW_CFA_remember_state":
            out_lines.append(".cfi_remember_state")
        elif opname == "DW_CFA_nop":
            return
        else:
            out_lines.append("UNKNOWN DIRECTIVE: " + str(instr))
    except Exception as e:
        out_lines.append(f"# Error handling instr {opname} {args}: {e}")


def emit_personality(
    elf, entry, eh_frame_sec, offset_current_entry, out_lines, personalities
):
    sec_data = eh_frame_sec.data()
    sec_address = eh_frame_sec["sh_addr"]

    size_first_two_fields = 4  # TODO with if second optional field is there?
    cie_address = (
        offset_current_entry - entry.header.CIE_pointer + size_first_two_fields
    )
    cie_header = parse_CIE_header(bytearray(sec_data)[cie_address:], out_lines)

    # out_lines.append("offset_current_entry - CIE_pointer = " + hex(cie_address))
    # out_lines.append(str(cie_header))
    if "zP" in cie_header["augmentation_string"]:
        data = cie_header["augmentation_data"]
        if data[0] == 0x9B:
            rel_ptr, _, _ = read_encoded(data[1:], 0, DW_EH_PE_sdata4)
            ptr = (
                rel_ptr
                + sec_address
                + cie_address
                + cie_header["augmentation_data_offset"]
                + 1
            )
            reloc = find_relocation(elf, ptr)
            reloc_name = reloc_to_name(reloc)
            label = "DW.ref." + reloc_name
            out_lines.append(".cfi_personality 0x9b, " + label + " # " + hex(ptr))
            if reloc_name not in personalities:
                personalities.append(reloc_name)
            # out_lines.append(hex_list(cie_header["augmentation_data"]))
        else:
            out_lines.append(
                "DO NOT HOW TO TREAT AUGMENTATION DATA ENCODING: " + hex_list(data)
            )
    elif "P" in cie_header.augmentation_string:
        out_lines.append(
            "DO NOT HOW TO TREAT AUGMENTATION STRING: " + cie_header.augmentation_string
        )


def emit_personality_section(label, out_lines2):
    out_lines2.append(
        "  .section .data.rel.local."
        + label
        + ',"awG",@progbits,DW.ref.'
        + label
        + ",comdat"
    )
    out_lines2.append("  .align 8")
    out_lines2.append("  .type   DW.ref." + label + ", @object")
    out_lines2.append("  .size   DW.ref." + label + ", 8")
    out_lines2.append("DW.ref." + label + ":")
    out_lines2.append("  .quad " + label)
    out_lines2.append(
        '  .ident  "Lifted to symbolized assembly (GAS) and then recompiled."'
    )


def emit_fde_entry(
    elf,
    eh_frame_sec,
    offset_current_entry,
    entry,
    dwarf,
    out_lines,
    out_lines1,
    personalities,
    stubs,
):
    # if isinstance(entry, CIE):
    #    out_lines.append("CIE @ " + hex(offset_current_entry))
    #    out_lines.append(str(entry.header))
    #    out_lines.append(str(entry.instructions))
    #    out_lines.append(str(entry.structs.little_endian))
    #    out_lines.append(str(entry.structs.dwarf_format))
    #    out_lines.append(str(entry.structs.address_size))
    #    return

    if not isinstance(entry, FDE):
        return

    CIE_instructions = entry.cie.instructions
    loc0 = entry.header.initial_location
    loc = entry.header.initial_location
    if len(CIE_instructions) > 0 or (entry.lsda_pointer is not None):
        out_lines.append("@" + hex(loc) + ":")
    if entry.lsda_pointer is not None:
        emit_personality(
            elf, entry, eh_frame_sec, offset_current_entry, out_lines, personalities
        )
        out_lines.append(".cfi_lsda 0x1b, @" + hex(entry.lsda_pointer))
    for instr in CIE_instructions + entry.instructions:
        if str(instr).startswith("DW_CFA_advance_loc"):
            loc += instr.args[0]
            out_lines.append("@" + hex(loc) + ":")
        else:
            # out_lines.append(str(instr))
            emit_instruction(instr, out_lines)
    out_lines.append("")

    if not (entry.lsda_pointer is None):
        out_lines1.append(
            "#LSDA for location "
            + hex(loc0)
            + " is stored at "
            + hex(entry.lsda_pointer)
        )
        emit_gcc_except_table(elf, loc0, entry.lsda_pointer, out_lines1, stubs)
        out_lines1.append("")

    return


def emit_gcc_except_table(elf, loc, address, out_lines, stubs):
    sec = elf.get_section_by_name(".gcc_except_table")
    if sec["sh_addr"] <= address and address < sec["sh_addr"] + sec["sh_size"]:
        data = sec.data()
        offset = address - sec["sh_addr"]
        lsda = parse_and_emit_lsda(
            elf, loc, bytearray(data)[offset:], out_lines, address, stubs
        )
        # out_lines.append(str(lsda))
    else:
        out_lines.append("ERROR: could not find LSDA table")


def find_relocation(elf, address):
    for section in elf.iter_sections():
        # Only process relocation sections
        if section["sh_type"] not in ("SHT_REL", "SHT_RELA"):
            continue

        # Symbol table (if available) is referenced by sh_link
        symtable = None
        if section["sh_link"]:
            symtable = elf.get_section(section["sh_link"])

        for rel in section.iter_relocations():
            symbol_name = None
            if symtable:
                symbol = symtable.get_symbol(rel["r_info_sym"])
                if symbol:
                    symbol_name = symbol.name

            if rel["r_offset"] == address:
                reloc = {}
                reloc["offset"] = address
                reloc["type"] = rel["r_info_type"]
                reloc["addend"] = rel["r_addend"] if "r_addend" in rel.entry else 0
                reloc["symbol"] = symbol_name
                return reloc
    return None


def list_relocations(elf):
    for section in elf.iter_sections():
        # Only process relocation sections
        if section["sh_type"] not in ("SHT_REL", "SHT_RELA"):
            continue

        print(f"\nRelocation section [{section.name}]:")

        # Symbol table (if available) is referenced by sh_link
        symtable = None
        if section["sh_link"]:
            symtable = elf.get_section(section["sh_link"])

        for rel in section.iter_relocations():
            symbol_name = None
            if symtable:
                symbol = symtable.get_symbol(rel["r_info_sym"])
                if symbol:
                    symbol_name = symbol.name

            line = f"  Offset: {rel['r_offset']:08x}, " f"Type: {rel['r_info_type']}"
            if "r_addend" in rel.entry:
                line += f", Addend: {rel['r_addend']}"
            if symbol_name:
                line += f", Symbol: {symbol_name}"
            print(line)


def emit_stubs(stubs, out_lines):
    out_lines.append("  .data")
    out_lines.append("  .p2align 3")
    for stub in stubs:
        type_info_object_label = ".L" + stub + ".DW.stub"
        out_lines.append(type_info_object_label + ":")
        out_lines.append("  .quad " + stub.replace("internal_address_0x", "@0x"))


# TODO stubs are not needed, they are normal relocs in data section


def generate_patch(args):
    with open(args.elf, "rb") as f:
        elf = ELFFile(f)

        try:
            dwarf = elf.get_dwarf_info()
            setattr(dwarf, "_elf", elf)
        except Exception:
            dwarf = None

        out_lines = []
        out_lines1 = []
        out_lines2 = []
        out_lines3 = []
        personalities = []
        stubs = []

        eh_frame_sec = elf.get_section_by_name(".eh_frame")
        offset_current_entry = 0

        if not dwarf or not dwarf.has_EH_CFI():
            out_lines.append("/* No .eh_frame info present */")
        else:
            entries = dwarf.EH_CFI_entries()
            for e in entries:
                emit_fde_entry(
                    elf,
                    eh_frame_sec,
                    offset_current_entry,
                    e,
                    dwarf,
                    out_lines,
                    out_lines1,
                    personalities,
                    stubs,
                )
                if isinstance(e, FDE) or isinstance(e, CIE):
                    offset_current_entry += (
                        e.header.length + e.structs.initial_length_field_size()
                    )

        for personality in personalities:
            emit_personality_section(personality, out_lines2)
        # list_relocations (elf)

    # emit_stubs(stubs, out_lines3)

    out = ["##################"]
    out.append("# CFI directives #")
    out.append("##################")
    out.append("\n".join(out_lines))
    out.append("#####################")
    out.append("# gcc except tables #")
    out.append("#####################")
    out.append("\n".join(out_lines1))
    out.append("###########################")
    out.append("# gcc except tables (end) #")
    out.append("###########################")
    out.append("\n\n\n")
    out.append("#####################")
    out.append("# cfi personalities #")
    out.append("#####################")
    out.append("\n".join(out_lines2))
    out.append("###########################")
    out.append("# cfi personalities (end) #")
    out.append("###########################")
    out.append("\n\n\n")
    # out.append("#########")
    # out.append("# stubs #")
    # out.append("#########")
    # out.append("\n".join(out_lines3))
    # out.append("###############")
    # out.append("# stubs (end) #")
    # out.append("###############")
    output = "\n".join(out)
    if args.output:
        print("Written file: " + args.output)
        with open(args.output, "w") as of:
            of.write(output)
    else:
        print(output)


## =============================================================================
## == APPLY ====================================================================
## =============================================================================

# The spaces are important.
# I want to filter out exactly these directives, not directives that have them
# as a prefix.
REG_BLACKLIST = ("%rip",)


def _patch_directives(patch_lines, asm_lines, addrs_to_symbolize, buf):
    in_cfi_section = False

    cur_directives = []
    frontier = []

    for line in patch_lines:
        line = line.strip()

        if line.startswith("# ") and "CFI directives" in line:
            in_cfi_section = True
            continue
        if line.startswith("# ") and "CFI directives" not in line:
            in_cfi_section = False
            continue
        if not in_cfi_section or not line or line.startswith("#"):
            continue

        m = re.match(r"@0x([0-9a-fA-F]+):", line)
        if m:
            addr = int(m.group(1), 16)
            cur_directives = []
            frontier.append((addr, cur_directives))
        elif not any(reg in line for reg in REG_BLACKLIST):
            cur_directives.append(line)

    frontier.sort(key=lambda item: item[0])

    cfi_startproc_just_seen = False

    for line in asm_lines:
        m = re.search(r"(.*)# CFI ANCHOR @0x([0-9a-fA-F]+)", line)
        if m:
            addr = int(m.group(2), 16)
            while frontier:
                cfi_addr, cfi_lines = frontier[0]
                if cfi_addr > addr:
                    break

                frontier.pop(0)
                if cfi_addr == addr:
                    for cfi in cfi_lines:

                        def symbolize(m):
                            addr = int(m.group(1), 16)
                            return f".exception_table_at_{addr:X}"

                        cfi = re.sub(r"@0x([0-9a-fA-F]+)", symbolize, cfi)

                        if cfi_startproc_just_seen and cfi.strip().startswith(
                            ".cfi_def_cfa %rsp"
                        ):
                            continue

                        buf.write(f"\t{cfi}\n")

            if addr in addrs_to_symbolize:
                buf.write(f".exception_anchor_at_{addr:X}:\n")
                addrs_to_symbolize.remove(addr)

            # buf.write(line)
            buf.write(f"{m.group(1).rstrip()}\n")
        else:
            buf.write(line)

        if line.strip():
            cfi_startproc_just_seen = line.strip() == ".cfi_startproc"


def _patch_except_tables(patch_lines, buf):
    buf.write("\n\n")

    in_except_table_section = False

    for line in patch_lines:
        if line.startswith("# ") and "gcc except tables (end)" in line:
            in_except_table_section = False
            continue
        if line.startswith("# ") and "gcc except tables" in line:
            in_except_table_section = True
            continue
        if not in_except_table_section or line.startswith("#"):
            continue

        # The first line of the exception table has the raw address marker.
        # In this case, we want to replace that address marker by the correct label for this exception table.
        #
        # All other lines contain references to exception anchors in the .text section
        if line.startswith("@"):

            def symbolize(m):
                addr = int(m.group(1), 16)
                return f".exception_table_at_{addr:X}"

            line = re.sub(r"@0x([0-9a-fA-F]+)", symbolize, line)
        else:

            def symbolize(m):
                addr = int(m.group(1), 16)
                return f".exception_anchor_at_{addr:X}"

            line = re.sub(r"@0x([0-9a-fA-F]+)", symbolize, line)

        buf.write(line)


def _find_except_table_addresses_to_symbolize(patch_lines):
    addrs = set()

    in_except_table_section = False

    for line in patch_lines:
        if line.startswith("# ") and "gcc except tables" in line:
            in_except_table_section = True
            continue
        if line.startswith("# ") and "gcc except tables (end)" in line:
            in_except_table_section = False
            continue
        if not in_except_table_section or line.startswith("#"):
            continue

        matches = re.findall(r"@0x([0-9a-fA-F]+)", line)
        for addr in matches:
            addrs.add(int(addr, 16))

    return addrs


def _patch_personality(patch_lines, buf):
    buf.write("\n\n")

    in_personality_section = False

    for line in patch_lines:
        if line.startswith("# ") and "cfi personalities (end)" in line:
            in_personality_section = False
            continue
        if line.startswith("# ") and "cfi personalities" in line:
            in_personality_section = True
            continue
        if not in_personality_section or line.startswith("#"):
            continue

        buf.write(line)


def apply_patch(args):
    with open(args.input, "r") as f:
        patch_lines = f.readlines()
    with open(args.asm, "r") as f:
        orig_lines = f.readlines()

    addrs_to_symbolize = _find_except_table_addresses_to_symbolize(patch_lines)

    buf = io.StringIO()
    _patch_directives(patch_lines, orig_lines, addrs_to_symbolize, buf)
    _patch_except_tables(patch_lines, buf)
    _patch_personality(patch_lines, buf)

    with open(args.asm, "w") as f:
        f.write(buf.getvalue())


## =============================================================================
## == MAIN =====================================================================
## =============================================================================


def main():
    parser = argparse.ArgumentParser(
        prog=sys.argv[0], description="Patch .cfi_* snippets from ELF .eh_frame"
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    generate_parser = subparsers.add_parser("generate", help="Generate a patch")
    generate_parser.add_argument(
        "-o", "--output", required=True, help="Output CFI patch file"
    )
    generate_parser.add_argument("elf", help="Input EL(L)F file")

    apply_parser = subparsers.add_parser("apply", help="Apply a patch")
    apply_parser.add_argument(
        "-i", "--input", required=True, help="Input CFI patch file"
    )
    apply_parser.add_argument("asm", help="Assembly file to patch")

    args = parser.parse_args()

    if args.command == "generate":
        generate_patch(args)
    if args.command == "apply":
        apply_patch(args)


if __name__ == "__main__":
    main()
