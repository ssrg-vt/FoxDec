section "Instruction Semantics"

theory X86_InstructionSemantics
  imports State
begin



text \<open>A datatype for storing instructions. Note that we add a special kind of meta-instruction, called
      ExternalCall. A call to an external function can manually be mapped to a manually supplied state 
      transformation function. ExternalCallWithReturn happens in case of a jump to an external function:
      it also returns.\<close>
datatype I = 
    Instr string "Operand option" "Operand option" "Operand option" "64 word"
  | ExternalCall "state \<Rightarrow> state" "64 word"
  | ExternalCallWithReturn "state \<Rightarrow> state" "64 word"

text \<open>A datatype for the result of floating point comparisons.\<close>
datatype FP_Order = FP_Unordered | FP_GT | FP_LT | FP_EQ


abbreviation "instr_next i \<equiv> 
   case i of (Instr _ _ _ _ a') \<Rightarrow> a' 
| ExternalCall _ a' \<Rightarrow> a'
| ExternalCallWithReturn _ a' \<Rightarrow> a'"

locale unknowns =
  fixes unknown_addsd     :: "64 word \<Rightarrow> 64 word \<Rightarrow> 64 word"
    and unknown_subsd     :: "64 word \<Rightarrow> 64 word \<Rightarrow> 64 word"
    and unknown_mulsd     :: "64 word \<Rightarrow> 64 word \<Rightarrow> 64 word"
    and unknown_divsd     :: "64 word \<Rightarrow> 64 word \<Rightarrow> 64 word"
    and unknown_ucomisd   :: "64 word \<Rightarrow> 64 word \<Rightarrow> FP_Order"
    and unknown_semantics :: "I \<Rightarrow> state \<Rightarrow> state"
    and unknown_flags     :: "string \<Rightarrow> string \<Rightarrow> bool" 
    and unknown_computation :: "string \<Rightarrow> 256 word option \<Rightarrow> 256 word option \<Rightarrow> 256 word option \<Rightarrow> 256 word"
begin

text \<open>
  The semantics below are intended to be overapproximative and incomplete.
  This is achieved using locale ``unknowns''.
  Any place where semantics is \emph{not} modelled, it is mapped to a universally quantified uninterpreted function
  from that locale. We do not make use of @{const undefined}, since that could be used to prove that the semantics
  of two undefined behaviors are equivalent. 
  For example:
  \begin{itemize}
  \item Only a subset of instructions has semantics. In case of an unknown instruction $i$,
        the function @{term semantics} below will result in @{term "unknown_semantics i"}.
  \item Not all flags have been defined. In case a flag is read whose semantics is not defined below,
        the read will resolve to @{term "unknown_flags i f"}. Note that if the semantics of an instruction do
        not set flags, an overapproximative semantics such as below imply that the instruction indeed
        does not modify flags. In other words, if we were uncertain we would assign unknown values to flags.
  \item Not all operations have been defined. For example, floating points operations have no executable
        semantics, but are mapped to uninterpreted functions such as @{term unknown_addsd}.
  \end{itemize}
\<close>

text \<open>Generic operators \<close>

definition unop :: "('a ::len word \<Rightarrow> 'a::len word) \<Rightarrow> 
                     ('a::len word  \<Rightarrow> string \<Rightarrow> bool) \<Rightarrow>
                      Operand \<Rightarrow> state \<Rightarrow> state"
  where "unop f g op1 \<sigma> \<equiv>
          let si  = operand_size op1;
              dst = ucast (operand_read \<sigma> op1)::'a::len word in
             operand_write op1 (ucast (f dst)) (\<sigma> with [setFlags (g dst)])"

definition binop :: "('a::len word \<Rightarrow> 'a ::len word \<Rightarrow> 'a::len word) \<Rightarrow>
                     ('a::len word \<Rightarrow> 'a::len word  \<Rightarrow> string \<Rightarrow> bool) \<Rightarrow>
                      Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "binop f g op1 op2 \<sigma> \<equiv>
          let dst = ucast (operand_read \<sigma> op1)::'a::len word;
              src = ucast (operand_read \<sigma> op2)::'a::len word in
             operand_write op1 (ucast (f dst src)) (\<sigma> with [setFlags (g dst src)])"

definition unop_no_flags :: "('a ::len word \<Rightarrow> 'a::len word) \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "unop_no_flags f op1 \<sigma> \<equiv>
          let dst = ucast (operand_read \<sigma> op1)::'a::len word in
             operand_write op1 (ucast (f dst)) \<sigma>"

definition binop_flags :: "('a::len word \<Rightarrow> 'a::len word  \<Rightarrow> string \<Rightarrow> bool) \<Rightarrow>
                      Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "binop_flags g op1 op2 \<sigma> \<equiv>
          let si  = operand_size op1;
              dst = ucast (operand_read \<sigma> op1)::'a::len word;
              src = ucast (operand_read \<sigma> op2)::'a::len word in
            \<sigma> with [setFlags (g dst src)]"

definition binop_no_flags :: "('a::len word \<Rightarrow> 'a ::len word \<Rightarrow> 'a::len word) \<Rightarrow>
                      Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "binop_no_flags f op1 op2 \<sigma> \<equiv>
          let si  = operand_size op1;
              dst = ucast (operand_read \<sigma> op1)::'a::len word;
              src = ucast (operand_read \<sigma> op2)::'a::len word in
            operand_write op1 (ucast (f dst src)) \<sigma>"

definition binop_XMM :: "(64 word \<Rightarrow> 64 word \<Rightarrow> 64 word) \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "binop_XMM f op1 op2 \<sigma> \<equiv>
          let dst = ucast (operand_read \<sigma> op1)::64word;
              src = ucast (operand_read \<sigma> op2)::64word in
            operand_write op1 (ucast (overwrite 0 64 dst (f dst src))) \<sigma>"


text \<open>Moves\<close>

definition semantics_MOV :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_MOV op1 op2 \<equiv> 
           if operand_size op1 = 32 then binop_no_flags (\<lambda> v0 v1::256 word . v1) op1 op2
      else if operand_size op1 = 16 then binop_no_flags (\<lambda> v0 v1::128 word . v1) op1 op2
      else if operand_size op1 = 8  then binop_no_flags (\<lambda> v0 v1::64  word . v1) op1 op2
      else if operand_size op1 = 4  then binop_no_flags (\<lambda> v0 v1::32  word . v1) op1 op2
      else if operand_size op1 = 2  then binop_no_flags (\<lambda> v0 v1::16  word . v1) op1 op2
      else if operand_size op1 = 1  then binop_no_flags (\<lambda> v0 v1::8   word . v1) op1 op2
      else undefined"

abbreviation MOV
  where "MOV op1 op2 \<equiv> Instr ''mov'' (Some op1) (Some op2) None"

abbreviation MOVABS
  where "MOVABS op1 op2 \<equiv> Instr ''movabs'' (Some op1) (Some op2) None"

abbreviation MOVAPS
  where "MOVAPS op1 op2 \<equiv> Instr ''movaps'' (Some op1) (Some op2) None"

abbreviation MOVAPD
  where "MOVAPD op1 op2 \<equiv> Instr ''movaps'' (Some op1) (Some op2) None"

abbreviation MOVDQU
  where "MOVDQU op1 op2 \<equiv> Instr ''movdqu'' (Some op1) (Some op2) None"


definition semantics_MOVD :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_MOVD op1 op2 \<sigma> \<equiv>
          let src = ucast(operand_read \<sigma> op2)::32word in
             operand_write op1 (ucast src) \<sigma>"

abbreviation MOVD
  where "MOVD op1 op2 \<equiv> Instr ''movd'' (Some op1) (Some op2) None"

fun isXMM :: "Operand \<Rightarrow> bool"
  where "isXMM (Reg r) = (take 3 r = ''xmm'')"
  | "isXMM _ = False"

definition semantics_MOVSD :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_MOVSD op1 op2 \<sigma> \<equiv>
    if isXMM op1 \<and> isXMM op2 then
      let src = \<langle>0,64\<rangle>operand_read \<sigma> op2;
          dst = \<langle>64,128\<rangle>operand_read \<sigma> op1 in
             operand_write op1 (overwrite 0 64 dst src) \<sigma>
     else
      let src = \<langle>0,64\<rangle>operand_read \<sigma> op2 in
         operand_write op1 src \<sigma>"

abbreviation MOVSD
  where "MOVSD op1 op2 \<equiv> Instr ''movsd'' (Some op1) (Some op2) None"

abbreviation MOVQ
  where "MOVQ op1 op2 \<equiv> Instr ''movq'' (Some op1) (Some op2) None"

definition mov_zero_extension :: "('a::len) itself \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "mov_zero_extension _ op1 op2 \<sigma> \<equiv>
          let src = ucast (operand_read \<sigma> op2)::'a word in
             operand_write op1 (ucast src) \<sigma>"


definition semantics_MOVZX :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_MOVZX op1 op2 \<equiv> 
           if operand_size op1 = 32 then mov_zero_extension (TYPE(256)) op1 op2
      else if operand_size op1 = 16 then mov_zero_extension (TYPE(128)) op1 op2
      else if operand_size op1 = 8  then mov_zero_extension (TYPE(64))  op1 op2
      else if operand_size op1 = 4  then mov_zero_extension (TYPE(32))  op1 op2
      else if operand_size op1 = 2  then mov_zero_extension (TYPE(16))  op1 op2
      else if operand_size op1 = 1  then mov_zero_extension (TYPE(8))   op1 op2
      else undefined"

text \<open> lea/push/pop/call/ret/leave \<close>

definition semantics_LEA :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_LEA op1 op2 \<sigma> \<equiv>
          let si_op1 = operand_size op1 in
          case op2 of Mem si offset base index scale \<Rightarrow>
            if si_op1 = 8 then operand_write op1 (ucast (resolve_address \<sigma> offset base index scale)) \<sigma>
            else if si_op1 = 4 then operand_write op1 (ucast (ucast (resolve_address \<sigma> offset base index scale)::32 word)) \<sigma>
            else undefined (''lea'',op1,op2)"

abbreviation LEA
  where "LEA op1 op2 \<equiv> Instr ''lea'' (Some op1) (Some op2) None"

definition semantics_PUSH :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_PUSH op1 \<sigma> \<equiv>
          let src = operand_read \<sigma> op1;
              si  = operand_size op1;
              rsp = ucast (ucast(reg_read \<sigma> ''rsp'') - of_nat si :: 64 word) in
             operand_write (QWORD PTR [''rsp'']\<^sub>1) src (operand_write (Reg ''rsp'') rsp \<sigma>)"

abbreviation PUSH
  where "PUSH op1 \<equiv> Instr ''push'' (Some op1) None None"

definition semantics_POP :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_POP op1 \<sigma> \<equiv>
          let si  = operand_size op1;
              src = operand_read \<sigma> (QWORD PTR [''rsp'']\<^sub>1);
              rsp = ucast (ucast(reg_read \<sigma> ''rsp'') + of_nat si::64 word) in
             operand_write op1 src (operand_write (Reg ''rsp'') rsp \<sigma>)"

abbreviation POP
  where "POP op1 \<equiv> Instr ''pop'' (Some op1) None None"

definition semantics_CALL :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_CALL op1 \<sigma> \<equiv>
        let src = ucast (operand_read \<sigma> op1) in
           (state_update (setRip src) o semantics_PUSH (Reg ''rip'')) \<sigma>"

definition semantics_RET :: "state \<Rightarrow> state"
  where "semantics_RET \<sigma> \<equiv>
          let a   = ucast (operand_read \<sigma> (QWORD PTR [''rsp'']\<^sub>1));
              rsp = ucast (reg_read \<sigma> ''rsp'') + 8 :: 64 word in
             ((\<lambda> \<sigma> . \<sigma> with [setRip a]) o operand_write (Reg ''rsp'') (ucast rsp)) \<sigma>"

abbreviation RET
  where "RET \<equiv> Instr ''ret'' None None None"

definition semantics_LEAVE :: "state \<Rightarrow> state"
  where "semantics_LEAVE \<equiv> semantics_POP (Reg ''rbp'') o semantics_MOV (Reg ''rsp'') (Reg ''rbp'')"

abbreviation LEAVE
  where "LEAVE op1 \<equiv> Instr ''pop'' (Some op1) None None"



text \<open>Arithmetic\<close>

definition ADD_flags :: "'a::len word \<Rightarrow> 'a::len word \<Rightarrow> string \<Rightarrow> bool"
  where "ADD_flags w0 w1 flag \<equiv> case flag of
    ''zf'' \<Rightarrow> w0 + w1 = 0
  | ''cf'' \<Rightarrow> unat w0 + unat w1 \<ge> 2^(LENGTH('a))
  | ''of'' \<Rightarrow> (w0 <s 0 \<longleftrightarrow> w1 <s 0) \<and> \<not>(w0 <s 0 \<longleftrightarrow> w0+w1 <s 0)
  | ''sf'' \<Rightarrow> w0 + w1 <s 0
  | f      \<Rightarrow> unknown_flags ''ADD'' f"

definition semantics_ADD :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_ADD op1 \<equiv> 
           if operand_size op1 = 32 then binop ((+)::256 word \<Rightarrow> _ \<Rightarrow> _) ADD_flags op1
      else if operand_size op1 = 16 then binop ((+)::128 word \<Rightarrow> _ \<Rightarrow> _) ADD_flags op1
      else if operand_size op1 = 8  then binop ((+)::64  word \<Rightarrow> _ \<Rightarrow> _) ADD_flags op1
      else if operand_size op1 = 4  then binop ((+)::32  word \<Rightarrow> _ \<Rightarrow> _) ADD_flags op1
      else if operand_size op1 = 2  then binop ((+)::16  word \<Rightarrow> _ \<Rightarrow> _) ADD_flags op1
      else if operand_size op1 = 1  then binop ((+)::8   word \<Rightarrow> _ \<Rightarrow> _) ADD_flags op1
      else undefined"

abbreviation ADD
  where "ADD op1 op2 \<equiv> Instr ''add'' (Some op1) (Some op2) None"


definition INC_flags :: "256 word \<Rightarrow> ('a::len word \<Rightarrow> string \<Rightarrow> bool)"
  where "INC_flags cf w0 flag \<equiv> case flag of
    ''zf'' \<Rightarrow> w0 + 1 = 0
  | ''cf'' \<Rightarrow> cf \<noteq> 0
  | ''of'' \<Rightarrow> 0 <=s w0 \<and> w0+1 <s 0
  | ''sf'' \<Rightarrow> w0 + 1 <s 0
  | f      \<Rightarrow> unknown_flags ''INC'' f"

definition semantics_INC :: "Operand \<Rightarrow> state \<Rightarrow> state" 
  where "semantics_INC op1 \<sigma> \<equiv> 
    let cf = flag_read \<sigma> ''cf'' in
           if operand_size op1 = 32 then unop ((+) (1::256 word)) (INC_flags cf) op1 \<sigma>
      else if operand_size op1 = 16 then unop ((+) (1::128 word)) (INC_flags cf) op1 \<sigma>
      else if operand_size op1 = 8  then unop ((+) (1::64  word)) (INC_flags cf) op1 \<sigma>
      else if operand_size op1 = 4  then unop ((+) (1::32  word)) (INC_flags cf) op1 \<sigma>
      else if operand_size op1 = 2  then unop ((+) (1::16  word)) (INC_flags cf) op1 \<sigma>
      else if operand_size op1 = 1  then unop ((+) (1::8   word)) (INC_flags cf) op1 \<sigma>
      else undefined"

abbreviation INC
  where "INC op1 \<equiv> Instr ''inc'' (Some op1) None None"

definition DEC_flags :: "256 word \<Rightarrow> ('a::len word \<Rightarrow> string \<Rightarrow> bool)"
  where "DEC_flags cf w0 flag \<equiv> case flag of
    ''zf'' \<Rightarrow> w0 = 1
  | ''cf'' \<Rightarrow> cf \<noteq> 0
  | ''of'' \<Rightarrow> w0 <s 0 \<and> 0 <=s w0 - 1
  | ''sf'' \<Rightarrow> w0 - 1 <s 0
  | f      \<Rightarrow> unknown_flags ''DEC'' f"

definition semantics_DEC :: "Operand \<Rightarrow> state \<Rightarrow> state" 
  where "semantics_DEC op1 \<sigma> \<equiv> 
    let cf = flag_read \<sigma> ''cf'' in
           if operand_size op1 = 32 then unop (\<lambda> w . w - 1::256 word) (DEC_flags cf) op1 \<sigma>
      else if operand_size op1 = 16 then unop (\<lambda> w . w - 1::128 word) (DEC_flags cf) op1 \<sigma>
      else if operand_size op1 = 8  then unop (\<lambda> w . w - 1::64  word) (DEC_flags cf) op1 \<sigma>
      else if operand_size op1 = 4  then unop (\<lambda> w . w - 1::32  word) (DEC_flags cf) op1 \<sigma>
      else if operand_size op1 = 2  then unop (\<lambda> w . w - 1::16  word) (DEC_flags cf) op1 \<sigma>
      else if operand_size op1 = 1  then unop (\<lambda> w . w - 1::8   word) (DEC_flags cf) op1 \<sigma>
      else undefined"

abbreviation DEC
  where "DEC op1 \<equiv> Instr ''dec'' (Some op1) None None"

definition NEG_flags :: "('a::len word \<Rightarrow> string \<Rightarrow> bool)"
  where "NEG_flags w0 flag \<equiv> case flag of
    ''zf'' \<Rightarrow> w0 = 0
  | ''cf'' \<Rightarrow> w0 \<noteq> 0
  | ''sf'' \<Rightarrow> - w0 <s 0
  | ''of'' \<Rightarrow> msb (- w0) \<and> msb w0
  | f      \<Rightarrow> unknown_flags ''NEG'' f"


definition semantics_NEG :: "Operand \<Rightarrow> state \<Rightarrow> state" 
  where "semantics_NEG op1 \<sigma> \<equiv> 
           if operand_size op1 = 32 then unop (\<lambda> w0 . - (w0::256 word)) NEG_flags op1 \<sigma>
      else if operand_size op1 = 16 then unop (\<lambda> w0 . - (w0::128 word)) NEG_flags op1 \<sigma>
      else if operand_size op1 = 8  then unop (\<lambda> w0 . - (w0::64  word)) NEG_flags op1 \<sigma>
      else if operand_size op1 = 4  then unop (\<lambda> w0 . - (w0::32  word)) NEG_flags op1 \<sigma>
      else if operand_size op1 = 2  then unop (\<lambda> w0 . - (w0::16  word)) NEG_flags op1 \<sigma>
      else if operand_size op1 = 1  then unop (\<lambda> w0 . - (w0::8   word)) NEG_flags op1 \<sigma>
      else undefined"

abbreviation NEG
  where "NEG op1 \<equiv> Instr ''neg'' (Some op1) None None"

definition SUB_flags :: "'a::len word \<Rightarrow> 'a::len word \<Rightarrow> string  \<Rightarrow> bool"
  where "SUB_flags w0 w1 flag \<equiv> case flag of
    ''zf'' \<Rightarrow> w0 = w1
  | ''cf'' \<Rightarrow> w0 < w1
  | ''sf'' \<Rightarrow> w0 - w1 <s 0
  | ''of'' \<Rightarrow> (msb w0 \<noteq> msb w1) \<and> (msb (w0 - w1) = msb w1)
  | f      \<Rightarrow> unknown_flags ''SUB'' f"

definition semantics_SUB :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SUB op1 \<equiv> 
           if operand_size op1 = 32 then binop ((-)::256 word \<Rightarrow> _ \<Rightarrow> _) SUB_flags op1
      else if operand_size op1 = 16 then binop ((-)::128 word \<Rightarrow> _ \<Rightarrow> _) SUB_flags op1
      else if operand_size op1 = 8  then binop ((-)::64  word \<Rightarrow> _ \<Rightarrow> _) SUB_flags op1
      else if operand_size op1 = 4  then binop ((-)::32  word \<Rightarrow> _ \<Rightarrow> _) SUB_flags op1
      else if operand_size op1 = 2  then binop ((-)::16  word \<Rightarrow> _ \<Rightarrow> _) SUB_flags op1
      else if operand_size op1 = 1  then binop ((-)::8   word \<Rightarrow> _ \<Rightarrow> _) SUB_flags op1
      else undefined"

abbreviation SUB
  where "SUB op1 op2 \<equiv> Instr ''sub'' (Some op1) (Some op2) None"

definition sbb :: "'b::len word \<Rightarrow> 'a::len word \<Rightarrow> 'a word \<Rightarrow> 'a word"
  where "sbb cf dst src \<equiv> dst - (src + ucast cf)"

definition SBB_flags :: "'b::len word \<Rightarrow> 'a::len word \<Rightarrow> 'a::len word \<Rightarrow> string  \<Rightarrow> bool"
  where "SBB_flags cf dst src flag \<equiv> case flag of
    ''zf'' \<Rightarrow> sbb cf dst src = 0
  | ''cf'' \<Rightarrow> dst < src + ucast cf
  | ''sf'' \<Rightarrow> sbb cf dst src <s 0
  | ''of'' \<Rightarrow> (msb dst \<noteq> msb (src + ucast cf)) \<and> (msb (sbb cf dst src) = msb (src + ucast cf))
  | f      \<Rightarrow> unknown_flags ''SBB'' f"

definition semantics_SBB :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SBB op1 op2 \<sigma> \<equiv> 
    let cf = flag_read \<sigma> ''cf'' in
           if operand_size op1 = 32 then binop (sbb cf::256 word \<Rightarrow> _ \<Rightarrow> _) (SBB_flags cf) op1 op2 \<sigma>
      else if operand_size op1 = 16 then binop (sbb cf::128 word \<Rightarrow> _ \<Rightarrow> _) (SBB_flags cf) op1 op2 \<sigma>
      else if operand_size op1 = 8  then binop (sbb cf::64  word \<Rightarrow> _ \<Rightarrow> _) (SBB_flags cf) op1 op2 \<sigma>
      else if operand_size op1 = 4  then binop (sbb cf::32  word \<Rightarrow> _ \<Rightarrow> _) (SBB_flags cf) op1 op2 \<sigma>
      else if operand_size op1 = 2  then binop (sbb cf::16  word \<Rightarrow> _ \<Rightarrow> _) (SBB_flags cf) op1 op2 \<sigma>
      else if operand_size op1 = 1  then binop (sbb cf::8   word \<Rightarrow> _ \<Rightarrow> _) (SBB_flags cf) op1 op2 \<sigma>
      else undefined"

abbreviation SBB
  where "SBB op1 op2 \<equiv> Instr ''sbb'' (Some op1) (Some op2) None"

definition adc :: "'b::len word \<Rightarrow> 'a::len word \<Rightarrow> 'a word \<Rightarrow> 'a word"
  where "adc cf dst src \<equiv> dst + (src + ucast cf)"

definition ADC_flags :: "'b::len word \<Rightarrow> 'a::len word \<Rightarrow> 'a::len word \<Rightarrow> string  \<Rightarrow> bool"
  where "ADC_flags cf dst src flag \<equiv> case flag of
    ''zf'' \<Rightarrow> adc cf dst src = 0
  | ''cf'' \<Rightarrow> unat dst + unat src + unat cf \<ge> 2^(LENGTH('a))
  | ''of'' \<Rightarrow> (dst <s 0 \<longleftrightarrow> src + ucast cf <s 0) \<and> \<not>(dst <s 0 \<longleftrightarrow> adc cf dst src <s 0)
  | ''sf'' \<Rightarrow> adc cf dst src <s 0
  | f      \<Rightarrow> unknown_flags ''ADC'' f"


definition semantics_ADC :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_ADC op1 op2 \<sigma> \<equiv> 
    let cf = flag_read \<sigma> ''cf'' in
           if operand_size op1 = 32 then binop (adc cf::256 word \<Rightarrow> _ \<Rightarrow> _) (ADC_flags cf) op1 op2 \<sigma>
      else if operand_size op1 = 16 then binop (adc cf::128 word \<Rightarrow> _ \<Rightarrow> _) (ADC_flags cf) op1 op2 \<sigma>
      else if operand_size op1 = 8  then binop (adc cf::64  word \<Rightarrow> _ \<Rightarrow> _) (ADC_flags cf) op1 op2 \<sigma>
      else if operand_size op1 = 4  then binop (adc cf::32  word \<Rightarrow> _ \<Rightarrow> _) (ADC_flags cf) op1 op2 \<sigma>
      else if operand_size op1 = 2  then binop (adc cf::16  word \<Rightarrow> _ \<Rightarrow> _) (ADC_flags cf) op1 op2 \<sigma>
      else if operand_size op1 = 1  then binop (adc cf::8   word \<Rightarrow> _ \<Rightarrow> _) (ADC_flags cf) op1 op2 \<sigma>
      else undefined"

abbreviation ADC
  where "ADC op1 op2 \<equiv> Instr ''adc'' (Some op1) (Some op2) None"


definition write_MUL_result :: "string \<Rightarrow> string \<Rightarrow> 'a::len word \<Rightarrow> 'b::len word \<Rightarrow> _ \<Rightarrow> state \<Rightarrow> state"
  where "write_MUL_result rh rl prod_lo prod_hi flgs \<sigma> \<equiv> 
         operand_write (Reg rh) (ucast prod_hi)
            (operand_write (Reg rl) (ucast prod_lo)
              (\<sigma> with [setFlags flgs]))"

definition MUL_flags :: "'a::len word \<Rightarrow> string \<Rightarrow> bool"
  where "MUL_flags result flag \<equiv> case flag of
        ''cf'' \<Rightarrow> (\<langle>LENGTH('a) div 2,LENGTH('a)\<rangle>result) \<noteq> 0
      | ''of'' \<Rightarrow> (\<langle>LENGTH('a) div 2,LENGTH('a)\<rangle>result) \<noteq> 0
      | f      \<Rightarrow> unknown_flags ''MUL'' f"


definition IMUL_flags :: "'a::len word \<Rightarrow> string \<Rightarrow> bool"
  where "IMUL_flags result flag \<equiv> case flag of
        ''cf'' \<Rightarrow> (\<langle>LENGTH('a) div 2,LENGTH('a)\<rangle>result) \<noteq> (if bit result (LENGTH('a) div 2 - 1) then 2^(LENGTH('a) div 2)-1 else 0)
      | ''of'' \<Rightarrow> (\<langle>LENGTH('a) div 2,LENGTH('a)\<rangle>result) \<noteq> (if bit result (LENGTH('a) div 2 - 1) then 2^(LENGTH('a) div 2)-1 else 0)
      | f      \<Rightarrow> unknown_flags ''IMUL'' f"


(*
  Assumes LENGTH('a) is the size of the operands.
  Assumes LENGTH('b) is twice the size of the operands, e.g.:
    unop_MUL TYPE(8) TYPE(16) True ''al'' (Reg ''r15b'') \<sigma>
*)
definition unop_MUL :: "'a::len itself \<Rightarrow> 'b::len itself \<Rightarrow> bool \<Rightarrow> string \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "unop_MUL _ _ signd op1_reg op2 \<sigma> \<equiv>
          let cast = (if signd then scast o (ucast :: 256 word \<Rightarrow> 'a word) else ucast);
              dst  = cast (operand_read \<sigma> (Reg op1_reg))::'b::len word;
              src  = cast (operand_read \<sigma> op2)::'b::len word;
              flgs = (if signd then IMUL_flags else MUL_flags) (dst * src);
              prod_hi = \<langle>LENGTH('a),LENGTH('b)\<rangle>(dst * src);
              dst'  = ucast (operand_read \<sigma> (Reg op1_reg))::'a::len word;
              src'  = ucast (operand_read \<sigma> op2)::'a::len word;
              prod_lo = (dst' * src') in
            if LENGTH('b) = 16 then
              write_MUL_result ''ah'' op1_reg prod_lo prod_hi flgs \<sigma>
            else if LENGTH('b) = 32 then
              write_MUL_result ''dx'' op1_reg prod_lo prod_hi flgs \<sigma>
            else if LENGTH('b) = 64 then
              write_MUL_result ''edx'' op1_reg prod_lo prod_hi flgs \<sigma>
            else if LENGTH('b) = 128 then
              write_MUL_result ''rdx'' op1_reg prod_lo prod_hi flgs \<sigma>
            else
              undefined"

definition semantics_MUL :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_MUL op2 \<equiv> 
           if operand_size op2 = 8 then unop_MUL TYPE(64) TYPE(128) False ''rax'' op2
      else if operand_size op2 = 4 then unop_MUL TYPE(32) TYPE(64)  False ''eax'' op2
      else if operand_size op2 = 2 then unop_MUL TYPE(16) TYPE(32)  False ''ax''  op2
      else if operand_size op2 = 1 then unop_MUL TYPE(8)  TYPE(16)  False ''al''  op2
      else undefined"

abbreviation MUL
  where "MUL op1 \<equiv> Instr ''mul'' (Some op1) None None"

definition semantics_IMUL1 :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_IMUL1 op2 \<equiv> 
           if operand_size op2 = 8 then unop_MUL TYPE(64) TYPE(128) True ''rax'' op2
      else if operand_size op2 = 4 then unop_MUL TYPE(32) TYPE(64)  True ''eax'' op2
      else if operand_size op2 = 2 then unop_MUL TYPE(16) TYPE(32)  True ''ax''  op2
      else if operand_size op2 = 1 then unop_MUL TYPE(8)  TYPE(16)  True ''al''  op2
      else undefined"

abbreviation IMUL1
  where "IMUL1 op1 \<equiv> Instr ''imul'' (Some op1) None None"

definition ternop_IMUL :: "'a::len itself \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "ternop_IMUL _ op1 op2 op3 \<sigma> \<equiv>
          let src1 = ucast (operand_read \<sigma> op2)::'a::len word;
              src2 = ucast (operand_read \<sigma> op3)::'a::len word;
              prod = src1 * src2;
              flgs = IMUL_flags prod in
            (operand_write op1 (ucast prod) (\<sigma> with [setFlags flgs]))"

definition semantics_IMUL2 :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_IMUL2 op1 op2 \<equiv> 
           if operand_size op1 = 8 then ternop_IMUL TYPE(64) op1 op1 op2
      else if operand_size op1 = 4 then ternop_IMUL TYPE(32) op1 op1 op2
      else if operand_size op1 = 2 then ternop_IMUL TYPE(16) op1 op1 op2
      else if operand_size op1 = 1 then ternop_IMUL TYPE(8)  op1 op1 op2
      else undefined"

abbreviation IMUL2
  where "IMUL2 op1 op2 \<equiv> Instr ''imul'' (Some op1) (Some op2) None"

definition semantics_IMUL3 :: "Operand \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_IMUL3 op1 op2 op3 \<equiv> 
           if operand_size op1 = 8 then ternop_IMUL TYPE(64) op1 op2 op3
      else if operand_size op1 = 4 then ternop_IMUL TYPE(32) op1 op2 op3
      else if operand_size op1 = 2 then ternop_IMUL TYPE(16) op1 op2 op3
      else if operand_size op1 = 1 then ternop_IMUL TYPE(8)  op1 op2 op3
      else undefined"

abbreviation IMUL3
  where "IMUL3 op1 op2 op3 \<equiv> Instr ''imul'' (Some op1) (Some op2) (Some op3)"


definition unop_DIV :: "'a::len itself \<Rightarrow> 'b::len itself \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "unop_DIV _ _ op0 op1 op2 \<sigma> \<equiv>
          let src0 = ucast (ucast (operand_read \<sigma> op0)::'a::len word)::'b::len word;
              src1 = ucast (operand_read \<sigma> op1)::'a::len word;
              src2 = ucast (operand_read \<sigma> op2)::'a::len word;
              src  = word_rcat [src1,src2] ::'b :: len word;
              d    = ucast (src div src0) ::'a::len word;
              rem  = ucast (\<langle>LENGTH('a),LENGTH('a)*2\<rangle>(src div src0)) ::'a::len word;
              flgs = unknown_flags ''DIV'' in
            (operand_write op2 (ucast d) (operand_write op1 (ucast rem)
                (\<sigma> with [setFlags flgs])))"

abbreviation div64 :: "64 word \<Rightarrow> 64 word \<Rightarrow> 64 word \<Rightarrow> 64 word"
  where "div64 d a w \<equiv> ucast ((word_rcat [d, a]::128word) div ucast w)"
abbreviation div32 :: "32 word \<Rightarrow> 32 word \<Rightarrow> 32 word \<Rightarrow> 32 word"
  where "div32 d a w \<equiv> ucast ((word_rcat [d, a]::64word) div ucast w)"
abbreviation div16 :: "16 word \<Rightarrow> 16 word \<Rightarrow> 16 word \<Rightarrow> 16 word"
  where "div16 d a w \<equiv> ucast ((word_rcat [d, a]::32word) div ucast w)"
abbreviation div8 :: "8 word \<Rightarrow> 8 word \<Rightarrow> 8 word \<Rightarrow> 8 word"
  where "div8 d a w \<equiv> ucast ((word_rcat [d, a]::16word) div ucast w)"
abbreviation div_rem64 :: "64 word \<Rightarrow> 64 word \<Rightarrow> 64 word \<Rightarrow> 64 word"
  where "div_rem64 d a w \<equiv> ucast (\<langle>64,128\<rangle>(word_rcat [d, a]::128word) div ucast w)"
abbreviation div_rem32 :: "32 word \<Rightarrow> 32 word \<Rightarrow> 32 word \<Rightarrow> 32 word"
  where "div_rem32 d a w \<equiv> ucast (\<langle>32,64\<rangle>(word_rcat [d, a]::64word) div ucast w)"
abbreviation div_rem16 :: "16 word \<Rightarrow> 16 word \<Rightarrow> 16 word \<Rightarrow> 16 word"
  where "div_rem16 d a w \<equiv> ucast (\<langle>16,32\<rangle>(word_rcat [d, a]::32word) div ucast w)"
abbreviation div_rem8 :: "8 word \<Rightarrow> 8 word \<Rightarrow> 8 word \<Rightarrow> 8 word"
  where "div_rem8 d a w \<equiv> ucast (\<langle>8,16\<rangle>(word_rcat [d, a]::16word) div ucast w)"

definition semantics_DIV :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_DIV op1 \<equiv> 
           if operand_size op1 = 8 then unop_DIV TYPE(64) TYPE(128) op1 (Reg ''rdx'') (Reg ''rax'')
      else if operand_size op1 = 4 then unop_DIV TYPE(32) TYPE(64)  op1 (Reg ''edx'') (Reg ''eax'')
      else if operand_size op1 = 2 then unop_DIV TYPE(16) TYPE(32)  op1 (Reg ''dx'')  (Reg ''ax'')
      else if operand_size op1 = 1 then unop_DIV TYPE(8)  TYPE(16)  op1 (Reg ''ah'')  (Reg ''al'')
      else undefined"


definition unop_IDIV :: "'a::len itself \<Rightarrow> 'b::len itself \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "unop_IDIV _ _ op0 op1 op2 \<sigma> \<equiv>
          let src0 = ucast (ucast (operand_read \<sigma> op0)::'a::len word)::'b::len word;
              src1 = ucast (operand_read \<sigma> op1)::'a::len word;
              src2 = ucast (operand_read \<sigma> op2)::'a::len word;
              src  = word_rcat [src1,src2] ::'b :: len word;
              d    = ucast (src sdiv src0) ::'a::len word;
              rem  = ucast (\<langle>LENGTH('a),LENGTH('a)*2\<rangle>(src div src0)) ::'a::len word;
              flgs = unknown_flags ''IDIV'' in
            (operand_write op2 (ucast d) (operand_write op1 (ucast rem)
                (\<sigma> with [setFlags flgs])))"

definition semantics_IDIV :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_IDIV op1 \<equiv> 
           if operand_size op1 = 8 then unop_IDIV TYPE(64) TYPE(128) op1 (Reg ''rdx'') (Reg ''rax'')
      else if operand_size op1 = 4 then unop_IDIV TYPE(32) TYPE(64)  op1 (Reg ''edx'') (Reg ''eax'')
      else if operand_size op1 = 2 then unop_IDIV TYPE(16) TYPE(32)  op1 (Reg ''dx'')  (Reg ''ax'')
      else if operand_size op1 = 1 then unop_IDIV TYPE(8)  TYPE(16)  op1 (Reg ''ah'')  (Reg ''al'')
      else undefined"

definition SHL_flags :: "nat \<Rightarrow> ('a::len word \<Rightarrow> string \<Rightarrow> bool)"
  where "SHL_flags n dst flag \<equiv> case flag of
        ''cf'' \<Rightarrow> bit dst (LENGTH('a) - n) 
      | ''of'' \<Rightarrow> bit dst (LENGTH('a) - n - 1) \<noteq> bit dst (LENGTH('a) - n)
      | ''zf'' \<Rightarrow> (dst << n) = 0
      | ''sf'' \<Rightarrow> bit dst (LENGTH('a) - n - 1)
      | f      \<Rightarrow> unknown_flags ''SHL'' f"

definition semantics_SHL :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state" 
  where "semantics_SHL op1 op2 \<sigma> \<equiv> 
    let src = unat (operand_read \<sigma> op2) in
           if operand_size op1 = 32 then unop (\<lambda> w . w << src::256 word) (SHL_flags src) op1 \<sigma>
      else if operand_size op1 = 16 then unop (\<lambda> w . w << src::128 word) (SHL_flags src) op1 \<sigma>
      else if operand_size op1 = 8  then unop (\<lambda> w . w << src::64  word) (SHL_flags src) op1 \<sigma>
      else if operand_size op1 = 4  then unop (\<lambda> w . w << src::32  word) (SHL_flags src) op1 \<sigma>
      else if operand_size op1 = 2  then unop (\<lambda> w . w << src::16  word) (SHL_flags src) op1 \<sigma>
      else if operand_size op1 = 1  then unop (\<lambda> w . w << src::8   word) (SHL_flags src) op1 \<sigma>
      else undefined"

abbreviation SHL
  where "SHL op1 op2 \<equiv> Instr ''shl'' (Some op1) (Some op2) None"

abbreviation SAL
  where "SAL op1 op2 \<equiv> Instr ''sal'' (Some op1) (Some op2) None"

definition SHR_flags :: "nat \<Rightarrow> ('a::len word \<Rightarrow> string \<Rightarrow> bool)"
  where "SHR_flags n dst flag \<equiv> case flag of
        ''cf'' \<Rightarrow> bit dst (n - 1) 
      | ''of'' \<Rightarrow> msb dst
      | ''zf'' \<Rightarrow> (dst >> n) = 0
      | f      \<Rightarrow> unknown_flags ''SHR'' f"

definition semantics_SHR :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state" 
  where "semantics_SHR op1 op2 \<sigma> \<equiv> 
    let src = unat (operand_read \<sigma> op2) in
           if operand_size op1 = 32 then unop (\<lambda> w . w >> src::256 word) (SHR_flags src) op1 \<sigma>
      else if operand_size op1 = 16 then unop (\<lambda> w . w >> src::128 word) (SHR_flags src) op1 \<sigma>
      else if operand_size op1 = 8  then unop (\<lambda> w . w >> src::64  word) (SHR_flags src) op1 \<sigma>
      else if operand_size op1 = 4  then unop (\<lambda> w . w >> src::32  word) (SHR_flags src) op1 \<sigma>
      else if operand_size op1 = 2  then unop (\<lambda> w . w >> src::16  word) (SHR_flags src) op1 \<sigma>
      else if operand_size op1 = 1  then unop (\<lambda> w . w >> src::8   word) (SHR_flags src) op1 \<sigma>
      else undefined"

abbreviation SHR
  where "SHR op1 op2 \<equiv> Instr ''shr'' (Some op1) (Some op2) None"

definition SAR_flags :: "nat \<Rightarrow> ('a::len word \<Rightarrow> string \<Rightarrow> bool)"
  where "SAR_flags n dst flag \<equiv> case flag of
        ''cf'' \<Rightarrow> bit dst (n - 1) 
      | ''of'' \<Rightarrow> False
      | ''zf'' \<Rightarrow> (dst >>> n) = 0
      | f      \<Rightarrow> unknown_flags ''SAR'' f"


definition semantics_SAR :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state" 
  where "semantics_SAR op1 op2 \<sigma> \<equiv> 
    let src = unat (operand_read \<sigma> op2) in
           if operand_size op1 = 32 then unop (\<lambda> w . w >>> src::256 word) (SAR_flags src) op1 \<sigma>
      else if operand_size op1 = 16 then unop (\<lambda> w . w >>> src::128 word) (SAR_flags src) op1 \<sigma>
      else if operand_size op1 = 8  then unop (\<lambda> w . w >>> src::64  word) (SAR_flags src) op1 \<sigma>
      else if operand_size op1 = 4  then unop (\<lambda> w . w >>> src::32  word) (SAR_flags src) op1 \<sigma>
      else if operand_size op1 = 2  then unop (\<lambda> w . w >>> src::16  word) (SAR_flags src) op1 \<sigma>
      else if operand_size op1 = 1  then unop (\<lambda> w . w >>> src::8   word) (SAR_flags src) op1 \<sigma>
      else undefined"

abbreviation SAR
  where "SAR op1 op2 \<equiv> Instr ''sar'' (Some op1) (Some op2) None"

abbreviation shiftl_over_words :: "'a ::len word \<Rightarrow> 'a word \<Rightarrow> 'a word" (infixl \<open><<\<^sub>w\<close> 55)
  where "a <<\<^sub>w b \<equiv> a << unat b"
abbreviation shiftr_over_words :: "'a ::len word \<Rightarrow> 'a word \<Rightarrow> 'a word" (infixl \<open>>>\<^sub>w\<close> 55)
  where "a >>\<^sub>w b \<equiv> a >> unat b"
abbreviation sshiftr_over_words :: "'a ::len word \<Rightarrow> 'a word \<Rightarrow> 'a word" (infixl \<open>>>>\<^sub>w\<close> 55)
  where "a >>>\<^sub>w b \<equiv> a >>> unat b"


definition shld :: "'b::len itself \<Rightarrow> nat \<Rightarrow> 'a::len word \<Rightarrow> 'a word \<Rightarrow> 'a word"
  where "shld _ n dst src \<equiv>
    let dstsrc  = or (ucast dst << LENGTH('a)) (ucast src :: 'b word);
        shifted = \<langle>LENGTH('a),LENGTH('a)*2\<rangle>(dstsrc << n) in
      ucast shifted"

definition SHLD_flags :: "'b::len itself \<Rightarrow> nat \<Rightarrow> ('a::len word \<Rightarrow> 'a::len word \<Rightarrow> string \<Rightarrow> bool)"
  where "SHLD_flags b n src dst flag \<equiv> case flag of
        ''cf'' \<Rightarrow> bit dst (LENGTH('a) - n) 
      | ''of'' \<Rightarrow> bit dst (LENGTH('a) - n - 1) \<noteq> bit dst (LENGTH('a) - n)
      | ''zf'' \<Rightarrow> shld b n dst src = 0
      | ''sf'' \<Rightarrow> bit dst (LENGTH('a) - n - 1) \<comment> \<open>msb (shld n dst src)\<close>
      | f      \<Rightarrow> unknown_flags ''SHLD'' f"

definition semantics_SHLD :: "Operand \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SHLD op1 op2 op3 \<sigma> \<equiv> 
    let src2 = unat (operand_read \<sigma> op3) in
           if operand_size op1 = 32 then binop (shld (TYPE(512)) src2 ::256 word \<Rightarrow> _ \<Rightarrow> _) (SHLD_flags (TYPE(512)) src2) op1 op2 \<sigma>
      else if operand_size op1 = 16 then binop (shld (TYPE(256)) src2 ::128 word \<Rightarrow> _ \<Rightarrow> _) (SHLD_flags (TYPE(256)) src2) op1 op2 \<sigma>
      else if operand_size op1 = 8  then binop (shld (TYPE(128)) src2 ::64  word \<Rightarrow> _ \<Rightarrow> _) (SHLD_flags (TYPE(128)) src2) op1 op2 \<sigma>
      else if operand_size op1 = 4  then binop (shld (TYPE(64))  src2 ::32  word \<Rightarrow> _ \<Rightarrow> _) (SHLD_flags (TYPE(64))  src2) op1 op2 \<sigma>
      else if operand_size op1 = 2  then binop (shld (TYPE(32))  src2 ::16  word \<Rightarrow> _ \<Rightarrow> _) (SHLD_flags (TYPE(32))  src2) op1 op2 \<sigma>
      else if operand_size op1 = 1  then binop (shld (TYPE(16))  src2 ::8   word \<Rightarrow> _ \<Rightarrow> _) (SHLD_flags (TYPE(16))  src2) op1 op2 \<sigma>
      else undefined"



definition ROL_flags :: "nat \<Rightarrow> ('a::len word \<Rightarrow> string \<Rightarrow> bool)" (*TODO check for unaffected flags *)
  where "ROL_flags n dst flag \<equiv> case flag of
        ''cf'' \<Rightarrow> bit dst (LENGTH('a) - n) 
      | ''of'' \<Rightarrow> bit dst (LENGTH('a) - n - 1) \<noteq> bit dst (LENGTH('a) - n)
      | f      \<Rightarrow> unknown_flags ''ROL'' f"

definition semantics_ROL :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state" 
  where "semantics_ROL op1 op2 \<sigma> \<equiv> 
    let src = unat (operand_read \<sigma> op2) in
           if operand_size op1 = 32 then unop (word_rotl src::256 word\<Rightarrow>_) (ROL_flags src) op1 \<sigma>
      else if operand_size op1 = 16 then unop (word_rotl src::128 word\<Rightarrow>_) (ROL_flags src) op1 \<sigma>
      else if operand_size op1 = 8  then unop (word_rotl src::64  word\<Rightarrow>_) (ROL_flags src) op1 \<sigma>
      else if operand_size op1 = 4  then unop (word_rotl src::32  word\<Rightarrow>_) (ROL_flags src) op1 \<sigma>
      else if operand_size op1 = 2  then unop (word_rotl src::16  word\<Rightarrow>_) (ROL_flags src) op1 \<sigma>
      else if operand_size op1 = 1  then unop (word_rotl src::8   word\<Rightarrow>_) (ROL_flags src) op1 \<sigma>
      else undefined"

abbreviation ROL
  where "ROL op1 op2 \<equiv> Instr ''rol'' (Some op1) (Some op2) None"

definition ROR_flags :: "nat \<Rightarrow> ('a::len word \<Rightarrow> string \<Rightarrow> bool)"
  where "ROR_flags n dst flag \<equiv> case flag of
        ''cf'' \<Rightarrow> bit dst (n - 1) 
      | ''of'' \<Rightarrow> msb (word_rotr n dst) \<noteq> (bit (word_rotr n dst) (LENGTH('a)-2))
      | f      \<Rightarrow> unknown_flags ''ROR'' f"

definition semantics_ROR :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state" 
  where "semantics_ROR op1 op2 \<sigma> \<equiv> 
    let src = unat (operand_read \<sigma> op2) in
           if operand_size op1 = 32 then unop (word_rotr src::256 word\<Rightarrow>_) (ROR_flags src) op1 \<sigma>
      else if operand_size op1 = 16 then unop (word_rotr src::128 word\<Rightarrow>_) (ROR_flags src) op1 \<sigma>
      else if operand_size op1 = 8  then unop (word_rotr src::64  word\<Rightarrow>_) (ROR_flags src) op1 \<sigma>
      else if operand_size op1 = 4  then unop (word_rotr src::32  word\<Rightarrow>_) (ROR_flags src) op1 \<sigma>
      else if operand_size op1 = 2  then unop (word_rotr src::16  word\<Rightarrow>_) (ROR_flags src) op1 \<sigma>
      else if operand_size op1 = 1  then unop (word_rotr src::8   word\<Rightarrow>_) (ROR_flags src) op1 \<sigma>
      else undefined"

abbreviation ROR
  where "ROR op1 op2 \<equiv> Instr ''ror'' (Some op1) (Some op2) None"

text \<open> flag-related \<close>

definition semantics_CMP :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_CMP op1 \<equiv> 
           if operand_size op1 = 32 then binop_flags (SUB_flags::256 word \<Rightarrow> _ \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 16 then binop_flags (SUB_flags::128 word \<Rightarrow> _ \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 8  then binop_flags (SUB_flags::64  word \<Rightarrow> _ \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 4  then binop_flags (SUB_flags::32  word \<Rightarrow> _ \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 2  then binop_flags (SUB_flags::16  word \<Rightarrow> _ \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 1  then binop_flags (SUB_flags::8   word \<Rightarrow> _ \<Rightarrow> _ \<Rightarrow> _) op1
      else undefined"

abbreviation CMP
  where "CMP op1 op2 \<equiv> Instr ''cmp'' (Some op1) (Some op2) None"

definition logic_flags :: "('a::len word \<Rightarrow> 'a::len word \<Rightarrow> 'a::len word) \<Rightarrow> 'a::len word \<Rightarrow> 'a::len word \<Rightarrow> string  \<Rightarrow> bool"
  where "logic_flags logic_op w0 w1 flag \<equiv> case flag of
    ''zf'' \<Rightarrow> logic_op w0 w1 = 0
  | ''cf'' \<Rightarrow> False
  | ''of'' \<Rightarrow> False
  | ''sf'' \<Rightarrow> msb (logic_op w0 w1)
  | f      \<Rightarrow> unknown_flags ''logic'' f"

definition semantics_TEST :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_TEST op1 \<equiv> 
           if operand_size op1 = 32 then binop_flags (logic_flags ((and)::256 word \<Rightarrow> _ \<Rightarrow> _)) op1
      else if operand_size op1 = 16 then binop_flags (logic_flags ((and)::128 word \<Rightarrow> _ \<Rightarrow> _)) op1
      else if operand_size op1 = 8  then binop_flags (logic_flags ((and)::64  word \<Rightarrow> _ \<Rightarrow> _)) op1
      else if operand_size op1 = 4  then binop_flags (logic_flags ((and)::32  word \<Rightarrow> _ \<Rightarrow> _)) op1
      else if operand_size op1 = 2  then binop_flags (logic_flags ((and)::16  word \<Rightarrow> _ \<Rightarrow> _)) op1
      else if operand_size op1 = 1  then binop_flags (logic_flags ((and)::8   word \<Rightarrow> _ \<Rightarrow> _)) op1
      else undefined"

abbreviation TEST
  where "TEST op1 op2 \<equiv> Instr ''test'' (Some op1) (Some op2) None"

definition semantics_BT :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_BT op1 op2 \<sigma> \<equiv> \<sigma> with [setFlags (unknown_flags ''BT'')]"


text \<open> sign extension \<close>
definition mov_sign_extension :: "('a::len) itself \<Rightarrow> ('b::len) itself \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "mov_sign_extension _ _ op1 op2 \<sigma> \<equiv>
          let src = ucast (operand_read \<sigma> op2)::'b word in
             operand_write op1 (ucast (scast src::'a word)) \<sigma>"

definition semantics_MOVSXD :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_MOVSXD op1 op2 \<equiv>
      if (operand_size op1, operand_size op2) = (8,4) then
         mov_sign_extension (TYPE(64)) (TYPE(32)) op1 op2
      else if (operand_size op1, operand_size op2) = (8,2) then
         mov_sign_extension (TYPE(64)) (TYPE(16)) op1 op2
      else if (operand_size op1, operand_size op2) = (8,1) then
         mov_sign_extension (TYPE(64)) (TYPE(8)) op1 op2
      else if (operand_size op1, operand_size op2) = (4,2) then
         mov_sign_extension (TYPE(32)) (TYPE(16)) op1 op2
      else if (operand_size op1, operand_size op2) = (4,1) then
         mov_sign_extension (TYPE(32)) (TYPE(8)) op1 op2
      else if (operand_size op1, operand_size op2) = (2,1) then
         mov_sign_extension (TYPE(16)) (TYPE(8)) op1 op2
      else
        undefined"

abbreviation MOVSXD
  where "MOVSXD op1 op2 \<equiv> Instr ''movsxd'' (Some op1) (Some op2) None"

abbreviation MOVSX
  where "MOVSX op1 op2 \<equiv> Instr ''movsx'' (Some op1) (Some op2) None"

definition semantics_CDQE :: "state \<Rightarrow> state"
  where "semantics_CDQE \<equiv> semantics_MOVSXD (Reg ''rax'') (Reg ''eax'')"

abbreviation CDQE
  where "CDQE \<equiv> Instr ''cdqe'' None None None"

definition semantics_CDQ :: "state \<Rightarrow> state"
  where "semantics_CDQ \<sigma> \<equiv>
          let src = ucast (operand_read \<sigma> (Reg ''eax'')) :: 32 word in
             operand_write (Reg ''edx'') (ucast (\<langle>32,64\<rangle>(scast src::64 word))) \<sigma>"

abbreviation CDQ
  where "CDQ \<equiv> Instr ''cdq'' None None None"

definition semantics_CQO :: "state \<Rightarrow> state"
  where "semantics_CQO \<sigma> \<equiv>
          let src = ucast (operand_read \<sigma> (Reg ''rax'')) :: 64 word in
             operand_write (Reg ''rdx'') (ucast (\<langle>64,128\<rangle>(scast src::128 word))) \<sigma>"

abbreviation CQO
  where "CQO \<equiv> Instr ''cqo'' None None None"


text \<open>logic\<close>
definition semantics_AND :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_AND op1 op2 \<sigma> \<equiv> 
           if operand_size op1 = 32 then binop ((and)::256 word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((and)::256 word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 16 then binop ((and)::128 word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((and)::128 word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 8  then binop ((and)::64  word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((and)::64  word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 4  then binop ((and)::32  word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((and)::32  word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 2  then binop ((and)::16  word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((and)::16  word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 1  then binop ((and)::8   word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((and)::8   word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else undefined"

abbreviation AND'
  where "AND' op1 op2 \<equiv> Instr ''and'' (Some op1) (Some op2) None"

definition semantics_OR :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_OR op1 op2 \<sigma> \<equiv> 
           if operand_size op1 = 32 then binop ((or)::256 word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((or)::256 word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 16 then binop ((or)::128 word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((or)::128 word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 8  then binop ((or)::64  word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((or)::64  word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 4  then binop ((or)::32  word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((or)::32  word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 2  then binop ((or)::16  word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((or)::16  word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 1  then binop ((or)::8   word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((or)::8   word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else undefined"

abbreviation OR'
  where "OR' op1 op2 \<equiv> Instr ''or'' (Some op1) (Some op2) None"

definition semantics_XOR :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_XOR op1 op2 \<sigma> \<equiv> 
           if operand_size op1 = 32 then binop ((xor)::256 word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((xor)::256 word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 16 then binop ((xor)::128 word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((xor)::128 word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 8  then binop ((xor)::64  word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((xor)::64  word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 4  then binop ((xor)::32  word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((xor)::32  word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 2  then binop ((xor)::16  word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((xor)::16  word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else if operand_size op1 = 1  then binop ((xor)::8   word \<Rightarrow> _ \<Rightarrow> _) (logic_flags ((xor)::8   word \<Rightarrow> _ \<Rightarrow> _)) op1 op2 \<sigma>
      else undefined"

abbreviation XOR'
  where "XOR' op1 op2 \<equiv> Instr ''xor'' (Some op1) (Some op2) None"

definition semantics_XORPS :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_XORPS op1 \<equiv> 
           if operand_size op1 = 32 then binop_no_flags ((xor)::256 word \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 16 then binop_no_flags ((xor)::128 word \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 8  then binop_no_flags ((xor)::64  word \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 4  then binop_no_flags ((xor)::32  word \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 2  then binop_no_flags ((xor)::16  word \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 1  then binop_no_flags ((xor)::8   word \<Rightarrow> _ \<Rightarrow> _) op1
      else undefined"

abbreviation XORPS
  where "XORPS op1 op2 \<equiv> Instr ''xorps'' (Some op1) (Some op2) None"

definition semantics_XORPD :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_XORPD op1 \<equiv> 
           if operand_size op1 = 32 then binop_no_flags ((xor)::256 word \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 16 then binop_no_flags ((xor)::128 word \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 8  then binop_no_flags ((xor)::64  word \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 4  then binop_no_flags ((xor)::32  word \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 2  then binop_no_flags ((xor)::16  word \<Rightarrow> _ \<Rightarrow> _) op1
      else if operand_size op1 = 1  then binop_no_flags ((xor)::8   word \<Rightarrow> _ \<Rightarrow> _) op1
      else undefined"

abbreviation XORPD
  where "XORPD op1 op2 \<equiv> Instr ''xorps'' (Some op1) (Some op2) None"

definition semantics_NOT :: "Operand \<Rightarrow> state \<Rightarrow> state" 
  where "semantics_NOT op1 \<sigma> \<equiv> 
           if operand_size op1 = 32 then unop_no_flags (not::256 word\<Rightarrow>_) op1 \<sigma>
      else if operand_size op1 = 16 then unop_no_flags (not::128 word\<Rightarrow>_) op1 \<sigma>
      else if operand_size op1 = 8  then unop_no_flags (not::64  word\<Rightarrow>_) op1 \<sigma>
      else if operand_size op1 = 4  then unop_no_flags (not::32  word\<Rightarrow>_) op1 \<sigma>
      else if operand_size op1 = 2  then unop_no_flags (not::16  word\<Rightarrow>_) op1 \<sigma>
      else if operand_size op1 = 1  then unop_no_flags (not::8   word\<Rightarrow>_) op1 \<sigma>
      else undefined"

abbreviation NOT'
  where "NOT' op1 \<equiv> Instr ''not'' (Some op1) None None"

text \<open> jumps \<close>
datatype FlagExpr = Flag string | FE_NOT FlagExpr | FE_AND FlagExpr FlagExpr | FE_OR FlagExpr FlagExpr | FE_EQ FlagExpr FlagExpr

primrec readFlagExpr :: "FlagExpr \<Rightarrow> state \<Rightarrow> bool"
  where
    "readFlagExpr (Flag f) \<sigma> = (flag_read \<sigma> f = 1)"
  | "readFlagExpr (FE_NOT fe) \<sigma> = (\<not>readFlagExpr fe \<sigma>)"
  | "readFlagExpr (FE_AND fe0 fe1) \<sigma> = (readFlagExpr fe0 \<sigma> \<and> readFlagExpr fe1 \<sigma>)"
  | "readFlagExpr (FE_OR fe0 fe1) \<sigma> = (readFlagExpr fe0 \<sigma> \<or> readFlagExpr fe1 \<sigma>)"
  | "readFlagExpr (FE_EQ fe0 fe1) \<sigma> = (readFlagExpr fe0 \<sigma> \<longleftrightarrow> readFlagExpr fe1 \<sigma>)"

definition semantics_cond_jump :: "FlagExpr \<Rightarrow> 64 word \<Rightarrow> state \<Rightarrow> state"
  where "semantics_cond_jump fe a \<sigma> \<equiv>
          let fv = readFlagExpr fe \<sigma> in
             if fv then state_update (setRip a) \<sigma> else \<sigma>"

definition semantics_JMP :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JMP op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
            state_update (setRip a) \<sigma>"

abbreviation JMP
  where "JMP op1 \<equiv> Instr ''jmp'' (Some op1) None None"

definition semantics_JO :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JO op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (Flag ''of'') a \<sigma>"

abbreviation JO
  where "JO op1 \<equiv> Instr ''jo'' (Some op1) None None"

definition semantics_JNO :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JNO op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (FE_NOT (Flag ''of'')) a \<sigma>"

abbreviation JNO
  where "JNO op1 \<equiv> Instr ''jno'' (Some op1) None None"

definition semantics_JS :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JS op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (Flag ''sf'') a \<sigma>"

abbreviation JS
  where "JS op1 \<equiv> Instr ''js'' (Some op1) None None"

definition semantics_JNS :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JNS op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (FE_NOT (Flag ''sf'')) a \<sigma>"

abbreviation JNS
  where "JNS op1 \<equiv> Instr ''jns'' (Some op1) None None"

definition semantics_JE :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JE op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (Flag ''zf'') a \<sigma>"

abbreviation JE
  where "JE op1 \<equiv> Instr ''je'' (Some op1) None None"

abbreviation JZ
  where "JZ op1 \<equiv> Instr ''jz'' (Some op1) None None"

definition semantics_JNE :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JNE op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (FE_NOT (Flag ''zf'')) a \<sigma>"

abbreviation JNE
  where "JNE op1 \<equiv> Instr ''jne'' (Some op1) None None"

abbreviation JNZ
  where "JNZ op1 \<equiv> Instr ''jnz'' (Some op1) None None"

definition semantics_JB :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JB op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (Flag ''cf'') a \<sigma>"

abbreviation JB
  where "JB op1 \<equiv> Instr ''jb'' (Some op1) None None"

abbreviation JNAE
  where "JNAE op1 \<equiv> Instr ''jnae'' (Some op1) None None"

abbreviation JC
  where "JC op1 \<equiv> Instr ''jc'' (Some op1) None None"

definition semantics_JNB :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JNB op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (FE_NOT (Flag ''cf'')) a \<sigma>"

abbreviation JNB
  where "JNB op1 \<equiv> Instr ''jnb'' (Some op1) None None"

abbreviation JAE
  where "JAE op1 \<equiv> Instr ''jae'' (Some op1) None None"

abbreviation JNC
  where "JNC op1 \<equiv> Instr ''jnc'' (Some op1) None None"

definition semantics_JBE :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JBE op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (FE_OR (Flag ''cf'') (Flag ''zf'')) a \<sigma>"

abbreviation JBE
  where "JBE op1 \<equiv> Instr ''jbe'' (Some op1) None None"

abbreviation JNA
  where "JNA op1 \<equiv> Instr ''jna'' (Some op1) None None"

definition semantics_JA :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JA op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (FE_AND (FE_NOT (Flag ''cf'')) (FE_NOT (Flag ''zf''))) a \<sigma>"

abbreviation JA
  where "JA op1 \<equiv> Instr ''ja'' (Some op1) None None"

abbreviation JNBE
  where "JNBE op1 \<equiv> Instr ''jnbe'' (Some op1) None None"

definition semantics_JL :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JL op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (FE_NOT (FE_EQ (Flag ''sf'') (Flag ''of''))) a \<sigma>"

abbreviation JL
  where "JL op1 \<equiv> Instr ''jl'' (Some op1) None None"

abbreviation JNGE
  where "JNGE op1 \<equiv> Instr ''jnge'' (Some op1) None None"

definition semantics_JGE :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JGE op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (FE_EQ (Flag ''sf'') (Flag ''of'')) a \<sigma>"

abbreviation JGE
  where "JGE op1 \<equiv> Instr ''jge'' (Some op1) None None"

abbreviation JNL
  where "JNL op1 \<equiv> Instr ''jnl'' (Some op1) None None"

definition semantics_JLE :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JLE op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (FE_OR (Flag ''zf'') (FE_NOT (FE_EQ (Flag ''sf'') (Flag ''of'')))) a \<sigma>"

abbreviation JLE
  where "JLE op1 \<equiv> Instr ''jle'' (Some op1) None None"

abbreviation JNG
  where "JNG op1 \<equiv> Instr ''jng'' (Some op1) None None"

definition semantics_JG :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JG op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (FE_AND (FE_NOT (Flag ''zf'')) (FE_EQ (Flag ''sf'') (Flag ''of''))) a \<sigma>"

abbreviation JG
  where "JG op1 \<equiv> Instr ''jg'' (Some op1) None None"

abbreviation JNLE
  where "JNLE op1 \<equiv> Instr ''jnle'' (Some op1) None None"

definition semantics_JNP :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JNP op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump (FE_NOT (Flag ''pf'')) a \<sigma>"

abbreviation JNP
  where "JNP op1 \<equiv> Instr ''jnp'' (Some op1) None None"

definition semantics_JP :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_JP op1 \<sigma> \<equiv>
          let a = ucast (operand_read \<sigma> op1) in
             semantics_cond_jump  (Flag ''pf'') a \<sigma>"

abbreviation JP
  where "JP op1 \<equiv> Instr ''jp'' (Some op1) None None"

text \<open> setXX \<close>
definition semantics_setXX :: "FlagExpr \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_setXX fe op1 \<sigma> \<equiv>
          let fv = readFlagExpr fe \<sigma> in
             operand_write op1 (fromBool fv) \<sigma>"

abbreviation semantics_SETO :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETO \<equiv> semantics_setXX (Flag ''of'')"

abbreviation SETO
  where "SETO op1 \<equiv> Instr ''seto'' (Some op1) None None"

abbreviation semantics_SETNO :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETNO \<equiv> semantics_setXX (FE_NOT (Flag ''of''))"

abbreviation SETNO
  where "SETNO op1 \<equiv> Instr ''setno'' (Some op1) None None"

abbreviation semantics_SETS :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETS \<equiv> semantics_setXX (Flag ''sf'')"

abbreviation SETS
  where "SETS op1 \<equiv> Instr ''sets'' (Some op1) None None"

abbreviation semantics_SETNS :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETNS \<equiv> semantics_setXX (FE_NOT (Flag ''sf''))"

abbreviation SETNS
  where "SETNS op1 \<equiv> Instr ''setns'' (Some op1) None None"

abbreviation semantics_SETE :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETE \<equiv> semantics_setXX (Flag ''zf'')"

abbreviation SETE
  where "SETE op1 \<equiv> Instr ''sete'' (Some op1) None None"

abbreviation SETZ
  where "SETZ op1 \<equiv> Instr ''setz'' (Some op1) None None"

abbreviation semantics_SETNE :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETNE \<equiv> semantics_setXX (FE_NOT (Flag ''zf''))"

abbreviation SETNE
  where "SETNE op1 \<equiv> Instr ''setne'' (Some op1) None None"

abbreviation SETNZ
  where "SETNZ op1 \<equiv> Instr ''setnz'' (Some op1) None None"

abbreviation semantics_SETB :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETB \<equiv> semantics_setXX (Flag ''cf'')"

abbreviation SETB
  where "SETB op1 \<equiv> Instr ''setb'' (Some op1) None None"

abbreviation SETNAE
  where "SETNAE op1 \<equiv> Instr ''setnae'' (Some op1) None None"

abbreviation SETC
  where "SETC op1 \<equiv> Instr ''setc'' (Some op1) None None"

abbreviation semantics_SETNB :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETNB \<equiv> semantics_setXX (FE_NOT (Flag ''cf''))"

abbreviation SETNB
  where "SETNB op1 \<equiv> Instr ''setnb'' (Some op1) None None"

abbreviation SETAE
  where "SETAE op1 \<equiv> Instr ''setae'' (Some op1) None None"

abbreviation SETNC
  where "SETNC op1 \<equiv> Instr ''setnc'' (Some op1) None None"

abbreviation semantics_SETBE :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETBE \<equiv> semantics_setXX (FE_OR (Flag ''cf'') (Flag ''zf''))"

abbreviation SETBE
  where "SETBE op1 \<equiv> Instr ''setbe'' (Some op1) None None"

abbreviation SETNA
  where "SETNA op1 \<equiv> Instr ''setna'' (Some op1) None None"

abbreviation semantics_SETA :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETA \<equiv> semantics_setXX (FE_AND (FE_NOT (Flag ''cf'')) (FE_NOT (Flag ''zf'')))"

abbreviation SETA
  where "SETA op1 \<equiv> Instr ''seta'' (Some op1) None None"

abbreviation SETNBE
  where "SETNBE op1 \<equiv> Instr ''setnbe'' (Some op1) None None"

abbreviation semantics_SETL :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETL \<equiv> semantics_setXX (FE_NOT (FE_EQ (Flag ''sf'') (Flag ''of'')))"

abbreviation SETL
  where "SETL op1 \<equiv> Instr ''setl'' (Some op1) None None"

abbreviation SETNGE
  where "SETNGE op1 \<equiv> Instr ''setnge'' (Some op1) None None"

abbreviation semantics_SETGE :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETGE \<equiv> semantics_setXX (FE_EQ (Flag ''sf'') (Flag ''of''))"

abbreviation SETGE
  where "SETGE op1 \<equiv> Instr ''setge'' (Some op1) None None"

abbreviation SETNL
  where "SETNL op1 \<equiv> Instr ''setnl'' (Some op1) None None"

abbreviation semantics_SETLE :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETLE \<equiv> semantics_setXX (FE_OR (Flag ''zf'') (FE_NOT (FE_EQ (Flag ''sf'') (Flag ''of''))))"

abbreviation SETLE
  where "SETLE op1 \<equiv> Instr ''setle'' (Some op1) None None"

abbreviation SETNG
  where "SETNG op1 \<equiv> Instr ''setng'' (Some op1) None None"

abbreviation semantics_SETG :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SETG \<equiv> semantics_setXX (FE_AND (FE_NOT (Flag ''zf'')) (FE_EQ (Flag ''sf'') (Flag ''of'')))"

abbreviation SETG
  where "SETG op1 \<equiv> Instr ''setg'' (Some op1) None None"

abbreviation SETNLE
  where "SETNLE op1 \<equiv> Instr ''setnle'' (Some op1) None None"


text \<open> conditional moves \<close>

primrec cmov
  where 
    "cmov True dst src = src"
  | "cmov False dst src = dst"

definition semantics_CMOV :: "FlagExpr \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_CMOV fe op1 op2 \<sigma> \<equiv>
          let fv = readFlagExpr fe \<sigma>;
              dst = operand_read \<sigma> op1;
              src = operand_read \<sigma> op2 in
            operand_write op1 (cmov fv dst src) \<sigma>"

abbreviation "semantics_CMOVO    \<equiv> semantics_CMOV (Flag ''of'')"
abbreviation "semantics_CMOVNO   \<equiv> semantics_CMOV (FE_NOT (Flag ''of''))"
abbreviation "semantics_CMOVS    \<equiv> semantics_CMOV (Flag ''sf'')"
abbreviation "semantics_CMOVNS   \<equiv> semantics_CMOV (FE_NOT (Flag ''sf''))"
abbreviation "semantics_CMOVE    \<equiv> semantics_CMOV (Flag ''zf'')"
abbreviation "semantics_CMOVZ    \<equiv> semantics_CMOV (Flag ''zf'')"
abbreviation "semantics_CMOVNE   \<equiv> semantics_CMOV (FE_NOT (Flag ''zf''))"
abbreviation "semantics_CMOVNZ   \<equiv> semantics_CMOV (FE_NOT (Flag ''zf''))"
abbreviation "semantics_CMOVB    \<equiv> semantics_CMOV (Flag ''cf'')"
abbreviation "semantics_CMOVNAE  \<equiv> semantics_CMOV (Flag ''cf'')"
abbreviation "semantics_CMOVC    \<equiv> semantics_CMOV (Flag ''cf'')"
abbreviation "semantics_CMOVNB   \<equiv> semantics_CMOV (FE_NOT (Flag ''cf''))"
abbreviation "semantics_CMOVAE   \<equiv> semantics_CMOV (FE_NOT (Flag ''cf''))"
abbreviation "semantics_CMOVNC   \<equiv> semantics_CMOV (FE_NOT (Flag ''cf''))"
abbreviation "semantics_CMOVBE   \<equiv> semantics_CMOV (FE_OR (Flag ''cf'') (Flag ''zf''))"
abbreviation "semantics_CMOVNA   \<equiv> semantics_CMOV (FE_OR (Flag ''cf'') (Flag ''zf''))"
abbreviation "semantics_CMOVA    \<equiv> semantics_CMOV (FE_AND (FE_NOT (Flag ''cf'')) (FE_NOT (Flag ''zf'')))"
abbreviation "semantics_CMOVNBE  \<equiv> semantics_CMOV (FE_AND (FE_NOT (Flag ''cf'')) (FE_NOT (Flag ''zf'')))"
abbreviation "semantics_CMOVL    \<equiv> semantics_CMOV (FE_NOT (FE_EQ (Flag ''sf'') (Flag ''of'')))"
abbreviation "semantics_CMOVNGE  \<equiv> semantics_CMOV (FE_NOT (FE_EQ (Flag ''sf'') (Flag ''of'')))"
abbreviation "semantics_CMOVGE   \<equiv> semantics_CMOV (FE_EQ (Flag ''sf'') (Flag ''of''))"
abbreviation "semantics_CMOVNL   \<equiv> semantics_CMOV (FE_EQ (Flag ''sf'') (Flag ''of''))"
abbreviation "semantics_CMOVLE   \<equiv> semantics_CMOV (FE_OR (Flag ''zf'') (FE_NOT (FE_EQ (Flag ''sf'') (Flag ''of''))))"
abbreviation "semantics_CMOVNG   \<equiv> semantics_CMOV (FE_OR (Flag ''zf'') (FE_NOT (FE_EQ (Flag ''sf'') (Flag ''of''))))"
abbreviation "semantics_CMOVG    \<equiv> semantics_CMOV (FE_AND (FE_NOT (Flag ''zf'')) (FE_EQ (Flag ''sf'') (Flag ''of'')))"
abbreviation "semantics_CMOVNLE  \<equiv> semantics_CMOV (FE_AND (FE_NOT (Flag ''zf'')) (FE_EQ (Flag ''sf'') (Flag ''of'')))"
abbreviation "semantics_CMOVP    \<equiv> semantics_CMOV (Flag ''pf'')"
abbreviation "semantics_CMOVPE   \<equiv> semantics_CMOV (Flag ''pf'')"
abbreviation "semantics_CMOVNP   \<equiv> semantics_CMOV (FE_NOT (Flag ''pf''))"
abbreviation "semantics_CMOVPO   \<equiv> semantics_CMOV (FE_NOT (Flag ''pf''))"


text \<open>Floating Point\<close>
definition semantics_ADDSD :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_ADDSD \<equiv> binop_XMM unknown_addsd"

definition semantics_SUBSD :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_SUBSD \<equiv> binop_XMM unknown_subsd"

definition semantics_MULSD :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_MULSD \<equiv> binop_XMM unknown_mulsd"

definition semantics_DIVSD :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_DIVSD \<equiv> binop_XMM unknown_divsd"

definition UCOMISD_flags :: "64 word \<Rightarrow> 64 word \<Rightarrow> string \<Rightarrow> bool"
  where "UCOMISD_flags w0 w1 f \<equiv> 
  if f \<in> {''zf'',''pf'',''cf''} then case unknown_ucomisd w0 w1 of
      FP_Unordered \<Rightarrow> True
    | FP_GT        \<Rightarrow> False
    | FP_LT        \<Rightarrow> f = ''cf''
    | FP_EQ        \<Rightarrow> f = ''zf''
  else
    unknown_flags ''UCOMISD'' f"

definition semantics_UCOMISD :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_UCOMISD \<equiv> binop_flags UCOMISD_flags"


abbreviation ADDSD
  where "ADDSD op1 op2 \<equiv> Instr ''addsd'' (Some op1) (Some op2) None"

abbreviation SUBSD
  where "SUBSD op1 op2 \<equiv> Instr ''subsd'' (Some op1) (Some op2) None"

abbreviation MULSD
  where "MULSD op1 op2 \<equiv> Instr ''mulsd'' (Some op1) (Some op2) None"

abbreviation DIVSD
  where "DIVSD op1 op2 \<equiv> Instr ''divsd'' (Some op1) (Some op2) None"

abbreviation UCOMISD
  where "UCOMISD op1 op2 \<equiv> Instr ''ucomisd'' (Some op1) (Some op2) None"



(* SIMD *)

definition simd_32_128 :: "(32 word \<Rightarrow> 32 word \<Rightarrow> 32 word) \<Rightarrow> 128 word \<Rightarrow> 128 word \<Rightarrow> 128 word" 
  where "simd_32_128 f dst src \<equiv> 
            or ((ucast (\<langle>0,32\<rangle>(f (ucast (\<langle>96,128\<rangle>dst)) (ucast (\<langle>96,128\<rangle>src))))) << 96)
            (or ((ucast (\<langle>0,32\<rangle>(f (ucast (\<langle>64,96\<rangle>dst))  (ucast (\<langle>64,96\<rangle>src))))) << 64)  
            (or ((ucast (\<langle>0,32\<rangle>(f (ucast (\<langle>32,64\<rangle>dst))  (ucast (\<langle>32,64\<rangle>src))))) << 32) 
               (ucast (\<langle>0,32\<rangle>(f (ucast (\<langle>0,32\<rangle>dst))   (ucast (\<langle>0,32\<rangle>src)))))))"

abbreviation semantics_PADDD :: "Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_PADDD \<equiv> binop_no_flags (simd_32_128 (+))"

abbreviation PADDD
  where "PADDD op1 op2 \<equiv> Instr ''paddd'' (Some op1) (Some op2) None"

definition pshufd :: "128 word \<Rightarrow> 8 word \<Rightarrow> 128 word"
  where "pshufd src n \<equiv> or ((\<langle>0,32\<rangle>(src >> (unat (\<langle>6,8\<rangle>n)*32))) << 96) 
                        (or ((\<langle>0,32\<rangle>(src >> (unat (\<langle>4,6\<rangle>n)*32))) << 64) 
                        (or ((\<langle>0,32\<rangle>(src >> (unat (\<langle>2,4\<rangle>n)*32))) << 32) 
                            ((\<langle>0,32\<rangle>(src >> (unat (\<langle>0,2\<rangle>n)*32))))))"

lemmas pshufd_numeral[simp] = pshufd_def[of "numeral n"] for n
lemmas pshufd_0[simp] = pshufd_def[of 0]
lemmas pshufd_1[simp] = pshufd_def[of 1]

definition semantics_PSHUFD :: "Operand \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_PSHUFD op1 op2 op3 \<sigma> \<equiv> 
    let src = ucast (operand_read \<sigma> op2);
        n   = ucast (operand_read \<sigma> op3) in
      operand_write op1 (ucast (pshufd src n)) \<sigma>"

abbreviation PSHUFD
  where "PSHUFD op1 op2 op3 \<equiv> Instr ''pshufd'' op1 op2 op3"

definition semantics_PEXTRD :: "Operand \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_PEXTRD op1 op2 op3 \<sigma> \<equiv>
          let src = operand_read \<sigma> op2;
              n   = unat (operand_read \<sigma> op3) mod 4 in
             operand_write op1 (ucast ((\<langle>0,32\<rangle>(src >> n*32)))) \<sigma>"

abbreviation PEXTRD
  where "PEXTRD op1 op2 op3 \<equiv> Instr ''pextrd'' op1 op2 op3"

definition semantics_PINSRD :: "Operand \<Rightarrow> Operand \<Rightarrow> Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_PINSRD op1 op2 op3 \<sigma> \<equiv>
          let dst = ucast (operand_read \<sigma> op1)::128 word;
              src = ucast (operand_read \<sigma> op2)::128 word;
              n   = unat (operand_read \<sigma> op3) mod 4;
              m   = 0xFFFFFFFF << (n * 32) :: 128 word;
              t   = and (src << (n *32)) m in
             operand_write op1 (ucast (or (and dst (not m)) t)) \<sigma>"

abbreviation PINSRD
  where "PINSRD op1 op2 op3 \<equiv> Instr ''pinsrd'' op1 op2 op3"


(* remainder *)



definition bswap :: "32 word \<Rightarrow> 32 word"
  where "bswap w \<equiv> or ((\<langle>0,8\<rangle>w) << 24) (or ((\<langle>8,16\<rangle>w) << 16) (or ((\<langle>16,24\<rangle>w) << 8) (\<langle>24,32\<rangle>w)))"

lemmas bswap_numeral[simp] = bswap_def[of "numeral n"] for n
lemmas bswap_0[simp] = bswap_def[of 0]
lemmas bswap_1[simp] = bswap_def[of 1]

definition semantics_BSWAP :: "Operand \<Rightarrow> state \<Rightarrow> state"
  where "semantics_BSWAP \<equiv> unop_no_flags bswap"

abbreviation BSWAP
  where "BSWAP op1 \<equiv> Instr ''bswap'' op1 None None"



definition semantics_NOP :: "state \<Rightarrow> state"
  where "semantics_NOP \<equiv> id"

abbreviation NOP0
  where "NOP0 \<equiv> Instr ''nop'' None None None"

abbreviation NOP1
  where "NOP1 op1 \<equiv> Instr ''nop'' (Some op1) None None"

abbreviation NOP2
  where "NOP2 op1 op2 \<equiv> Instr ''nop'' (Some op1) (Some op2) None"

abbreviation NOP3
  where "NOP3 op1 op2 op3 \<equiv> Instr ''nop'' (Some op1) (Some op2) (Some op3)"


fun read_operand_option
  where
    "read_operand_option \<sigma> None = None"
  | "read_operand_option \<sigma> (Some op) = Some (operand_read \<sigma> op)"

fun semantics_bot_to_op1
  where "semantics_bot_to_op1 (Instr m (Some op1) op2 op3 _) \<sigma> =
      operand_write op1 (unknown_computation m (read_operand_option \<sigma> (Some op1)) (read_operand_option \<sigma> op2) (read_operand_option \<sigma> op2)) \<sigma>"

end

named_theorems semantics_simps


locale semantics = unknowns +
fixes semantics :: "I \<Rightarrow> state \<Rightarrow> state"
assumes
    [semantics_simps]: "semantics (Instr ''mov''     (Some op1) (Some op2) x si)          = semantics_MOV op1 op2"
and [semantics_simps]: "semantics (Instr ''movabs''  (Some op1) (Some op2) x si)          = semantics_MOV op1 op2"
and [semantics_simps]: "semantics (Instr ''movaps''  (Some op1) (Some op2) x si)          = semantics_MOV op1 op2"
and [semantics_simps]: "semantics (Instr ''movapd''  (Some op1) (Some op2) x si)          = semantics_MOV op1 op2"
and [semantics_simps]: "semantics (Instr ''movups''  (Some op1) (Some op2) x si)          = semantics_MOV op1 op2"
and [semantics_simps]: "semantics (Instr ''movupd''  (Some op1) (Some op2) x si)          = semantics_MOV op1 op2"
and [semantics_simps]: "semantics (Instr ''movdqa''  (Some op1) (Some op2) x si)          = semantics_MOV op1 op2"
and [semantics_simps]: "semantics (Instr ''movdqu''  (Some op1) (Some op2) x si)          = semantics_MOV op1 op2"
and [semantics_simps]: "semantics (Instr ''movd''    (Some op1) (Some op2) x si)          = semantics_MOVD op1 op2"
and [semantics_simps]: "semantics (Instr ''movzx''   (Some op1) (Some op2) x si)          = semantics_MOV op1 op2"
and [semantics_simps]: "semantics (Instr ''movsd''   (Some op1) (Some op2) x si)          = semantics_MOVSD op1 op2"
and [semantics_simps]: "semantics (Instr ''movq''    (Some op1) (Some op2) x si)          = semantics_MOVSD op1 op2"
and [semantics_simps]: "semantics (Instr ''lea''     (Some op1) (Some op2) x si)          = semantics_LEA op1 op2"
and [semantics_simps]: "semantics (Instr ''push''    (Some op1) y          x si)          = semantics_PUSH op1"
and [semantics_simps]: "semantics (Instr ''pop''     (Some op1) y          x si)          = semantics_POP op1"
and [semantics_simps]: "semantics (Instr ''ret''     z y x si)                            = semantics_RET"
and [semantics_simps]: "semantics (Instr ''call''    (Some op1) y x si)                   = semantics_CALL op1"
and [semantics_simps]: "semantics (Instr ''leave''   z y x si)                            = semantics_LEAVE"
  \<comment> \<open>arithmetic\<close>                                           
and [semantics_simps]: "semantics (Instr ''add''     (Some op1) (Some op2)  x si)         = semantics_ADD op1 op2"
and [semantics_simps]: "semantics (Instr ''inc''     (Some op1) y           x si)         = semantics_INC op1"
and [semantics_simps]: "semantics (Instr ''dec''     (Some op1) y           x si)         = semantics_DEC op1"
and [semantics_simps]: "semantics (Instr ''neg''     (Some op1) y           x si)         = semantics_NEG op1"
and [semantics_simps]: "semantics (Instr ''sub''     (Some op1) (Some op2)  x si)         = semantics_SUB op1 op2"
and [semantics_simps]: "semantics (Instr ''sbb''     (Some op1) (Some op2)  x si)         = semantics_SBB op1 op2"
and [semantics_simps]: "semantics (Instr ''adc''     (Some op1) (Some op2)  x si)         = semantics_ADC op1 op2"
and [semantics_simps]: "semantics (Instr ''mul''     (Some op1) y x si)                   = semantics_MUL op1"
and [semantics_simps]: "semantics (Instr ''imul''    (Some op1) None x si)                = semantics_IMUL1 op1" 
and [semantics_simps]: "semantics (Instr ''imul''    (Some op1) (Some op2) None si)       = semantics_IMUL2 op1 op2"
and [semantics_simps]: "semantics (Instr ''imul''    (Some op1) (Some op2) (Some op3) si) = semantics_IMUL3 op1 op2 op3"
and [semantics_simps]: "semantics (Instr ''div''     (Some op1) y x si)                   = semantics_DIV op1"
and [semantics_simps]: "semantics (Instr ''idiv''    (Some op1) y x si)                   = semantics_IDIV op1"
and [semantics_simps]: "semantics (Instr ''shl''     (Some op1) (Some op2) None si)       = semantics_SHL op1 op2"
and [semantics_simps]: "semantics (Instr ''sal''     (Some op1) (Some op2) None si)       = semantics_SHL op1 op2"
and [semantics_simps]: "semantics (Instr ''shr''     (Some op1) (Some op2) None si)       = semantics_SHR op1 op2"
and [semantics_simps]: "semantics (Instr ''sar''     (Some op1) (Some op2) None si)       = semantics_SAR op1 op2"
and [semantics_simps]: "semantics (Instr ''shld''    (Some op1) (Some op2) (Some op3) si) = semantics_SHLD op1 op2 op3"
and [semantics_simps]: "semantics (Instr ''rol''     (Some op1) (Some op2) None si)       = semantics_ROL op1 op2"
and [semantics_simps]: "semantics (Instr ''ror''     (Some op1) (Some op2) None si)       = semantics_ROR op1 op2"
  \<comment> \<open>flag-related\<close>
and [semantics_simps]: "semantics (Instr ''cmp''     (Some op1) (Some op2)  x si)         = semantics_CMP op1 op2"
and [semantics_simps]: "semantics (Instr ''test''    (Some op1) (Some op2)  x si)         = semantics_TEST op1 op2"
and [semantics_simps]: "semantics (Instr ''bt''      (Some op1) (Some op2)  x si)         = semantics_BT op1 op2"
  \<comment> \<open>sign-extension\<close>                                       
and [semantics_simps]: "semantics (Instr ''movsxd''  (Some op1) (Some op2)  x si)         = semantics_MOVSXD op1 op2"
and [semantics_simps]: "semantics (Instr ''movsx''   (Some op1) (Some op2)  x si)         = semantics_MOVSXD op1 op2"
and [semantics_simps]: "semantics (Instr ''cdqe''    (Some op1) (Some op2)  x si)         = semantics_CDQE"
and [semantics_simps]: "semantics (Instr ''cdqe''    None       None        x si)         = semantics_CDQE"
and [semantics_simps]: "semantics (Instr ''cdq''     (Some op1) (Some op2)  x si)         = semantics_CDQ"
and [semantics_simps]: "semantics (Instr ''cdq''     None       None        x si)         = semantics_CDQ"
and [semantics_simps]: "semantics (Instr ''cqo''     (Some op1) (Some op2)  x si)         = semantics_CQO"
and [semantics_simps]: "semantics (Instr ''cqo''     None       None        x si)         = semantics_CQO"
  \<comment> \<open>logic\<close>
and [semantics_simps]: "semantics (Instr ''and''     (Some op1) (Some op2)  x si)         = semantics_AND op1 op2"
and [semantics_simps]: "semantics (Instr ''or''      (Some op1) (Some op2)  x si)         = semantics_OR  op1 op2"
and [semantics_simps]: "semantics (Instr ''xor''     (Some op1) (Some op2)  x si)         = semantics_XOR op1 op2"
and [semantics_simps]: "semantics (Instr ''not''     (Some op1) y  x si)                  = semantics_NOT op1"
and [semantics_simps]: "semantics (Instr ''xorps''   (Some op1) (Some op2)  x si)         = semantics_XORPS op1 op2"
and [semantics_simps]: "semantics (Instr ''xorpd''   (Some op1) (Some op2)  x si)         = semantics_XORPD op1 op2"
and [semantics_simps]: "semantics (Instr ''andpd''   (Some op1) (Some op2) x si)          = semantics_AND op1 op2"
  \<comment> \<open>jumps\<close>
and [semantics_simps]: "semantics (Instr ''jmp''     (Some op1) None  x si)               = semantics_JMP op1"
and [semantics_simps]: "semantics (Instr ''jo''      (Some op1) None  x si)               = semantics_JO op1"
and [semantics_simps]: "semantics (Instr ''jno''     (Some op1) None  x si)               = semantics_JNO op1"
and [semantics_simps]: "semantics (Instr ''js''      (Some op1) None  x si)               = semantics_JS op1"
and [semantics_simps]: "semantics (Instr ''jns''     (Some op1) None  x si)               = semantics_JNS op1"
and [semantics_simps]: "semantics (Instr ''je''      (Some op1) None  x si)               = semantics_JE op1"
and [semantics_simps]: "semantics (Instr ''jz''      (Some op1) None  x si)               = semantics_JE op1"
and [semantics_simps]: "semantics (Instr ''jne''     (Some op1) None  x si)               = semantics_JNE op1"
and [semantics_simps]: "semantics (Instr ''jnz''     (Some op1) None  x si)               = semantics_JNE op1"
and [semantics_simps]: "semantics (Instr ''jb''      (Some op1) None  x si)               = semantics_JB op1"
and [semantics_simps]: "semantics (Instr ''jnae''    (Some op1) None  x si)               = semantics_JB op1"
and [semantics_simps]: "semantics (Instr ''jc''      (Some op1) None  x si)               = semantics_JB op1"
and [semantics_simps]: "semantics (Instr ''jnb''     (Some op1) None  x si)               = semantics_JNB op1"
and [semantics_simps]: "semantics (Instr ''jae''     (Some op1) None  x si)               = semantics_JNB op1"
and [semantics_simps]: "semantics (Instr ''jnc''     (Some op1) None  x si)               = semantics_JNB op1"
and [semantics_simps]: "semantics (Instr ''jbe''     (Some op1) None  x si)               = semantics_JBE op1"
and [semantics_simps]: "semantics (Instr ''jna''     (Some op1) None  x si)               = semantics_JBE op1"
and [semantics_simps]: "semantics (Instr ''ja''      (Some op1) None  x si)               = semantics_JA op1"
and [semantics_simps]: "semantics (Instr ''jnbe''    (Some op1) None  x si)               = semantics_JA op1"
and [semantics_simps]: "semantics (Instr ''jl''      (Some op1) None  x si)               = semantics_JL op1"
and [semantics_simps]: "semantics (Instr ''jnge''    (Some op1) None  x si)               = semantics_JL op1"
and [semantics_simps]: "semantics (Instr ''jge''     (Some op1) None  x si)               = semantics_JGE op1"
and [semantics_simps]: "semantics (Instr ''jnl''     (Some op1) None  x si)               = semantics_JGE op1"
and [semantics_simps]: "semantics (Instr ''jle''     (Some op1) None  x si)               = semantics_JLE op1"
and [semantics_simps]: "semantics (Instr ''jng''     (Some op1) None  x si)               = semantics_JLE op1"
and [semantics_simps]: "semantics (Instr ''jg''      (Some op1) None  x si)               = semantics_JG op1"
and [semantics_simps]: "semantics (Instr ''jnle''    (Some op1) None  x si)               = semantics_JG op1"
and [semantics_simps]: "semantics (Instr ''jp''      (Some op1) None  x si)               = semantics_JP op1"
and [semantics_simps]: "semantics (Instr ''jnp''     (Some op1) None  x si)               = semantics_JNP op1"
  \<comment> \<open>setXX\<close>
and [semantics_simps]: "semantics (Instr ''seto''    (Some op1) None  x si)               = semantics_SETO op1"
and [semantics_simps]: "semantics (Instr ''setno''   (Some op1) None  x si)               = semantics_SETNO op1"
and [semantics_simps]: "semantics (Instr ''sets''    (Some op1) None  x si)               = semantics_SETS op1"
and [semantics_simps]: "semantics (Instr ''setns''   (Some op1) None  x si)               = semantics_SETNS op1"
and [semantics_simps]: "semantics (Instr ''sete''    (Some op1) None  x si)               = semantics_SETE op1"
and [semantics_simps]: "semantics (Instr ''setz''    (Some op1) None  x si)               = semantics_SETE op1"
and [semantics_simps]: "semantics (Instr ''setne''   (Some op1) None  x si)               = semantics_SETNE op1"
and [semantics_simps]: "semantics (Instr ''setnz''   (Some op1) None  x si)               = semantics_SETNE op1"
and [semantics_simps]: "semantics (Instr ''setb''    (Some op1) None  x si)               = semantics_SETB op1"
and [semantics_simps]: "semantics (Instr ''setnae''  (Some op1) None  x si)               = semantics_SETB op1"
and [semantics_simps]: "semantics (Instr ''setc''    (Some op1) None  x si)               = semantics_SETB op1"
and [semantics_simps]: "semantics (Instr ''setnb''   (Some op1) None  x si)               = semantics_SETNB op1"
and [semantics_simps]: "semantics (Instr ''setae''   (Some op1) None  x si)               = semantics_SETNB op1"
and [semantics_simps]: "semantics (Instr ''setnc''   (Some op1) None  x si)               = semantics_SETNB op1"
and [semantics_simps]: "semantics (Instr ''setbe''   (Some op1) None  x si)               = semantics_SETBE op1"
and [semantics_simps]: "semantics (Instr ''setna''   (Some op1) None  x si)               = semantics_SETBE op1"
and [semantics_simps]: "semantics (Instr ''seta''    (Some op1) None  x si)               = semantics_SETA op1"
and [semantics_simps]: "semantics (Instr ''setnbe''  (Some op1) None  x si)               = semantics_SETA op1"
and [semantics_simps]: "semantics (Instr ''setl''    (Some op1) None  x si)               = semantics_SETL op1"
and [semantics_simps]: "semantics (Instr ''setnge''  (Some op1) None  x si)               = semantics_SETL op1"
and [semantics_simps]: "semantics (Instr ''setge''   (Some op1) None  x si)               = semantics_SETGE op1"
and [semantics_simps]: "semantics (Instr ''setnl''   (Some op1) None  x si)               = semantics_SETGE op1"
and [semantics_simps]: "semantics (Instr ''setle''   (Some op1) None  x si)               = semantics_SETLE op1"
and [semantics_simps]: "semantics (Instr ''setng''   (Some op1) None  x si)               = semantics_SETLE op1"
and [semantics_simps]: "semantics (Instr ''setg''    (Some op1) None  x si)               = semantics_SETG op1"
and [semantics_simps]: "semantics (Instr ''setnle''  (Some op1) None  x si)               = semantics_SETG op1"
  \<comment> \<open>conditional moves\<close>
and [semantics_simps]: "semantics (Instr ''cmovo''   (Some op1) (Some op2)  x si)         = semantics_CMOVO op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovno''  (Some op1) (Some op2)  x si)         = semantics_CMOVNO op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovs''   (Some op1) (Some op2)  x si)         = semantics_CMOVS op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovns''  (Some op1) (Some op2)  x si)         = semantics_CMOVNS op1 op2"
and [semantics_simps]: "semantics (Instr ''cmove''   (Some op1) (Some op2)  x si)         = semantics_CMOVE op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovz''   (Some op1) (Some op2)  x si)         = semantics_CMOVE op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovne''  (Some op1) (Some op2)  x si)         = semantics_CMOVNE op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovnz''  (Some op1) (Some op2)  x si)         = semantics_CMOVNE op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovb''   (Some op1) (Some op2)  x si)         = semantics_CMOVB op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovnae'' (Some op1) (Some op2)  x si)         = semantics_CMOVB op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovc''   (Some op1) (Some op2)  x si)         = semantics_CMOVB op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovnb''  (Some op1) (Some op2)  x si)         = semantics_CMOVNB op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovae''  (Some op1) (Some op2)  x si)         = semantics_CMOVNB op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovnc''  (Some op1) (Some op2)  x si)         = semantics_CMOVNB op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovbe''  (Some op1) (Some op2)  x si)         = semantics_CMOVBE op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovna''  (Some op1) (Some op2)  x si)         = semantics_CMOVBE op1 op2"
and [semantics_simps]: "semantics (Instr ''cmova''   (Some op1) (Some op2)  x si)         = semantics_CMOVA op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovnbe'' (Some op1) (Some op2)  x si)         = semantics_CMOVA op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovl''   (Some op1) (Some op2)  x si)         = semantics_CMOVL op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovnge'' (Some op1) (Some op2)  x si)         = semantics_CMOVL op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovge''  (Some op1) (Some op2)  x si)         = semantics_CMOVGE op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovnl''  (Some op1) (Some op2)  x si)         = semantics_CMOVGE op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovle''  (Some op1) (Some op2)  x si)         = semantics_CMOVLE op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovng''  (Some op1) (Some op2)  x si)         = semantics_CMOVLE op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovg''   (Some op1) (Some op2)  x si)         = semantics_CMOVG op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovnle'' (Some op1) (Some op2)  x si)         = semantics_CMOVG op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovp''   (Some op1) (Some op2)  x si)         = semantics_CMOVP op1 op2"
and [semantics_simps]: "semantics (Instr ''cmovnp''  (Some op1) (Some op2)  x si)         = semantics_CMOVNP op1 op2"
  \<comment> \<open>floating point (double)\<close>                                           
and [semantics_simps]: "semantics (Instr ''addsd''   (Some op1) (Some op2)  x si)         = semantics_ADDSD op1 op2"
and [semantics_simps]: "semantics (Instr ''subsd''   (Some op1) (Some op2)  x si)         = semantics_SUBSD op1 op2"
and [semantics_simps]: "semantics (Instr ''mulsd''   (Some op1) (Some op2)  x si)         = semantics_MULSD op1 op2"
and [semantics_simps]: "semantics (Instr ''divsd''   (Some op1) (Some op2)  x si)         = semantics_DIVSD op1 op2"
and [semantics_simps]: "semantics (Instr ''ucomisd'' (Some op1) (Some op2)  x si)         = semantics_UCOMISD op1 op2"
  \<comment> \<open>simd\<close>
and [semantics_simps]: "semantics (Instr ''paddd''   (Some op1) (Some op2) x si)          = semantics_PADDD op1 op2"
and [semantics_simps]: "semantics (Instr ''pshufd''  (Some op1) (Some op2) (Some op3) si) = semantics_PSHUFD op1 op2 op3"
and [semantics_simps]: "semantics (Instr ''pextrd''  (Some op1) (Some op2) (Some op3) si) = semantics_PEXTRD op1 op2 op3"
and [semantics_simps]: "semantics (Instr ''pinsrd''  (Some op1) (Some op2) (Some op3) si) = semantics_PINSRD op1 op2 op3"
  \<comment> \<open>remainder\<close>
and [semantics_simps]: "semantics (Instr ''nop''     z y x si)                            = semantics_NOP"
and [semantics_simps]: "semantics (Instr ''bswap''   (Some op1) y x si)                   = semantics_BSWAP op1"
  \<comment> \<open>bot_to_op1\<close>
and [semantics_simps]: "semantics (Instr ''subpd''     z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''haddpd''    z y x si) = semantics_bot_to_op1 (Instr ''haddpd''     z y x si)"
and [semantics_simps]: "semantics (Instr ''unpcklps''  z y x si) = semantics_bot_to_op1 (Instr ''unpcklps''   z y x si)"
and [semantics_simps]: "semantics (Instr ''cvtss2sd''  z y x si) = semantics_bot_to_op1 (Instr ''cvtss2sd''   z y x si)"
and [semantics_simps]: "semantics (Instr ''cvtsi2ss''  z y x si) = semantics_bot_to_op1 (Instr ''cvtsi2ss''   z y x si)"
and [semantics_simps]: "semantics (Instr ''cvtsi2sd''  z y x si) = semantics_bot_to_op1 (Instr ''cvtsi2sd''   z y x si)"
and [semantics_simps]: "semantics (Instr ''cvtsd2ss''  z y x si) = semantics_bot_to_op1 (Instr ''cvtsd2ss''   z y x si)"
and [semantics_simps]: "semantics (Instr ''cvttss2si'' z y x si) = semantics_bot_to_op1 (Instr ''cvttss2si''  z y x si)"
and [semantics_simps]: "semantics (Instr ''cvttsd2si'' z y x si) = semantics_bot_to_op1 (Instr ''cvttsd2si''  z y x si)"
and [semantics_simps]: "semantics (Instr ''cvttpd2dq'' z y x si) = semantics_bot_to_op1 (Instr ''cvttpd2dq''  z y x si)"
and [semantics_simps]: "semantics (Instr ''cvtdq2pd''  z y x si) = semantics_bot_to_op1 (Instr ''cvtdq2pd''   z y x si)"
  \<comment> \<open>TODO\<close>
and [semantics_simps]: "semantics (Instr ''paddb''     z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''paddq''     z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''psubb''     z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''psubd''     z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''psubq''     z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''pinsrb''    z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''pinsrd''    z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''pinsrq''    z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''pextrb''    z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''pextrd''    z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''pextrq''    z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''pxor''      z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''pand''      z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''pandn''     z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''por''       z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"
and [semantics_simps]: "semantics (Instr ''ptest''     z y x si) = semantics_bot_to_op1 (Instr ''subpd''      z y x si)"


\<comment> \<open>Floating point\<close>
and [semantics_simps]: "semantics (Instr ''fst''       z y x si) = semantics_bot_to_op1 (Instr ''fst''        z y x si)"
and [semantics_simps]: "semantics (Instr ''fstp''      z y x si) = semantics_bot_to_op1 (Instr ''fstp''       z y x si)"
and [semantics_simps]: "semantics (Instr ''fnstcw''    z y x si) = semantics_bot_to_op1 (Instr ''fnstcw''     z y x si)"
and [semantics_simps]: "semantics (Instr ''fstcw''     z y x si) = semantics_bot_to_op1 (Instr ''fstcw''      z y x si)"
and [semantics_simps]: "semantics (Instr ''fld''       z y x si) = id"
and [semantics_simps]: "semantics (Instr ''fld1''      z y x si) = id"
and [semantics_simps]: "semantics (Instr ''fldz''      z y x si) = id"
and [semantics_simps]: "semantics (Instr ''fild''      z y x si) = id"
and [semantics_simps]: "semantics (Instr ''fxch''      z y x si) = id"
and [semantics_simps]: "semantics (Instr ''fchs''      z y x si) = id"
and [semantics_simps]: "semantics (Instr ''fucom''     z y x si) = id"
and [semantics_simps]: "semantics (Instr ''fucomi''    z y x si) = id"
and [semantics_simps]: "semantics (Instr ''fucomip''   z y x si) = id"
and [semantics_simps]: "semantics (Instr ''fucomp''    z y x si) = id"
and [semantics_simps]: "semantics (Instr ''fucompi''   z y x si) = id"
and [semantics_simps]: "semantics (Instr ''fucompp''   z y x si) = id"
and [semantics_simps]: "semantics (Instr ''finit''     z y x si) = id"
and [semantics_simps]: "semantics (Instr ''fninit''    z y x si) = id"

begin




text \<open>A step function. In X86. the RIP register is incremented before the instruction is executed.
      This is important, e.g., when RIP is used in a jump address.\<close>
fun step :: "I \<Rightarrow> state \<Rightarrow> state" 
  where
    "step (ExternalCall f a') \<sigma> = f \<sigma>"
  | "step (ExternalCallWithReturn f a') \<sigma> = semantics_RET (f \<sigma>)"
  | "step i \<sigma> = (let \<sigma>' = \<sigma> with [setRip (instr_next i)] in
                    semantics i \<sigma>')"


text \<open>All simplification rules used during symbolic execution.\<close>



declare Let_def [semantics_simps]
declare unop_def [semantics_simps]
declare unop_no_flags_def [semantics_simps]
declare binop_def [semantics_simps]
declare binop_flags_def [semantics_simps]
declare binop_no_flags_def [semantics_simps]
declare binop_XMM_def [semantics_simps]
declare mov_sign_extension_def [semantics_simps]
declare simd_32_128_def [semantics_simps]
declare write_MUL_result_def [semantics_simps]
declare unop_MUL_def [semantics_simps]
declare ternop_IMUL_def [semantics_simps]
declare unop_DIV_def [semantics_simps]
declare unop_IDIV_def [semantics_simps]
declare sbb_def [semantics_simps]
declare adc_def [semantics_simps]
declare shld_def [semantics_simps]


declare semantics_MOV_def [semantics_simps]
declare semantics_MOVZX_def [semantics_simps]
declare semantics_MOVSD_def [semantics_simps]
declare semantics_MOVD_def [semantics_simps]
declare semantics_CMOV_def [semantics_simps]
declare semantics_LEA_def [semantics_simps]
declare semantics_PUSH_def [semantics_simps]
declare semantics_POP_def [semantics_simps]
declare semantics_RET_def [semantics_simps]
declare semantics_CALL_def [semantics_simps]
declare semantics_LEAVE_def [semantics_simps]
declare semantics_ADD_def [semantics_simps]
declare semantics_INC_def [semantics_simps]
declare semantics_DEC_def [semantics_simps]
declare semantics_NEG_def [semantics_simps]
declare semantics_SUB_def [semantics_simps]
declare semantics_SBB_def [semantics_simps]
declare semantics_ADC_def [semantics_simps]
declare semantics_MUL_def [semantics_simps]
declare semantics_IMUL1_def [semantics_simps]
declare semantics_IMUL2_def [semantics_simps]
declare semantics_IMUL3_def [semantics_simps]
declare semantics_DIV_def [semantics_simps]
declare semantics_IDIV_def [semantics_simps]
declare semantics_SHL_def [semantics_simps]
declare semantics_SHR_def [semantics_simps]
declare semantics_SAR_def [semantics_simps]
declare semantics_SHLD_def [semantics_simps]
declare semantics_ROL_def [semantics_simps]
declare semantics_ROR_def [semantics_simps]
declare semantics_CMP_def [semantics_simps]
declare semantics_BT_def [semantics_simps]
declare semantics_TEST_def [semantics_simps]
declare semantics_MOVSXD_def [semantics_simps]
declare semantics_CDQE_def [semantics_simps]
declare semantics_CDQ_def [semantics_simps]
declare semantics_CQO_def [semantics_simps]
declare semantics_AND_def [semantics_simps]
declare semantics_OR_def [semantics_simps]
declare semantics_XOR_def [semantics_simps]
declare semantics_XORPS_def [semantics_simps]
declare semantics_XORPD_def [semantics_simps]
declare semantics_NOT_def [semantics_simps]
declare semantics_cond_jump_def [semantics_simps]
declare semantics_JMP_def [semantics_simps]
declare semantics_JO_def [semantics_simps]
declare semantics_JNO_def [semantics_simps]
declare semantics_JS_def [semantics_simps]
declare semantics_JNS_def [semantics_simps]
declare semantics_JE_def [semantics_simps]
declare semantics_JNE_def [semantics_simps]
declare semantics_JB_def [semantics_simps]
declare semantics_JNB_def [semantics_simps]
declare semantics_JBE_def [semantics_simps]
declare semantics_JA_def [semantics_simps]
declare semantics_JL_def [semantics_simps]
declare semantics_JGE_def [semantics_simps]
declare semantics_JLE_def [semantics_simps]
declare semantics_JG_def [semantics_simps]
declare semantics_JP_def [semantics_simps]
declare semantics_JNP_def [semantics_simps]
declare semantics_setXX_def [semantics_simps]
declare semantics_ADDSD_def [semantics_simps]
declare semantics_SUBSD_def [semantics_simps]
declare semantics_MULSD_def [semantics_simps]
declare semantics_DIVSD_def [semantics_simps]
declare semantics_UCOMISD_def [semantics_simps]
declare semantics_NOP_def [semantics_simps]
declare semantics_BSWAP_def [semantics_simps]
declare semantics_PSHUFD_def [semantics_simps]
declare semantics_PEXTRD_def [semantics_simps]
declare semantics_PINSRD_def [semantics_simps]
declare SUB_flags_def [semantics_simps]
declare ADD_flags_def [semantics_simps]
declare INC_flags_def [semantics_simps]
declare DEC_flags_def [semantics_simps]
declare NEG_flags_def [semantics_simps]
declare MUL_flags_def [semantics_simps]
declare IMUL_flags_def [semantics_simps]
declare SHL_flags_def [semantics_simps]
declare SHR_flags_def [semantics_simps]
declare SAR_flags_def [semantics_simps]
declare SHLD_flags_def [semantics_simps]
declare logic_flags_def [semantics_simps]
declare UCOMISD_flags_def [semantics_simps]


end
end
