section "Concrete state and instructions"

theory State
  imports Main Memory
begin

text \<open>A state consists of registers, memory, flags and a rip. Some design considerations here:
\begin{itemize}
\item All register values are 256 bits. We could also distinguish 64 bits registers, 128 registers etc.
      That would increase complexity in proofs and datastructures. The cost of using 256 everywhere is
      that a goal typically will have some casted 64 bits values.
\item The instruction pointer RIP is a special 64-bit register outside of the normal register set.
\item Strings are used for registers and flags. We would prefer an enumerative datatype, however, that would be
      extremely slow since there are roughly 100 register names.
\end{itemize}
\<close>

record state =
  regs  :: "string  \<Rightarrow> 256word"
  mem   :: "64 word \<Rightarrow> 8 word"
  flags :: "string  \<Rightarrow> bool"
  rip   :: "64 word"



definition real_reg :: "string \<Rightarrow> bool \<times> string \<times> nat \<times> nat"
  where "real_reg reg \<equiv>
  case reg of
  \<comment> \<open>rip\<close>
    ''rip''   \<Rightarrow> (True,  ''rip'', 0,64)
  \<comment> \<open>rax,rbx,rcx,rdx\<close>
  | ''rax''   \<Rightarrow> (True,  ''rax'', 0,64)
  | ''eax''   \<Rightarrow> (True,  ''rax'', 0,32)
  | ''ax''    \<Rightarrow> (False, ''rax'', 0,16)
  | ''ah''    \<Rightarrow> (False, ''rax'', 8,16)
  | ''al''    \<Rightarrow> (False, ''rax'', 0,8)
  | ''rbx''   \<Rightarrow> (True,  ''rbx'', 0,64)
  | ''ebx''   \<Rightarrow> (True,  ''rbx'', 0,32)
  | ''bx''    \<Rightarrow> (False, ''rbx'', 0,16)
  | ''bh''    \<Rightarrow> (False, ''rbx'', 8,16)
  | ''bl''    \<Rightarrow> (False, ''rbx'', 0,8)
  | ''rcx''   \<Rightarrow> (True,  ''rcx'', 0,64)
  | ''ecx''   \<Rightarrow> (True,  ''rcx'', 0,32)
  | ''cx''    \<Rightarrow> (False, ''rcx'', 0,16)
  | ''ch''    \<Rightarrow> (False, ''rcx'', 8,16)
  | ''cl''    \<Rightarrow> (False, ''rcx'', 0,8)
  | ''rdx''   \<Rightarrow> (True,  ''rdx'', 0,64)
  | ''edx''   \<Rightarrow> (True,  ''rdx'', 0,32)
  | ''dx''    \<Rightarrow> (False, ''rdx'', 0,16)
  | ''dh''    \<Rightarrow> (False, ''rdx'', 8,16)
  | ''dl''    \<Rightarrow> (False, ''rdx'', 0,8)
  \<comment> \<open>RBP, RSP\<close>
  | ''rbp''   \<Rightarrow> (True,  ''rbp'', 0,64)
  | ''ebp''   \<Rightarrow> (True,  ''rbp'', 0,32)
  | ''bp''    \<Rightarrow> (False, ''rbp'', 0,16)
  | ''bpl''   \<Rightarrow> (False, ''rbp'', 0,8)
  | ''rsp''   \<Rightarrow> (True,  ''rsp'', 0,64)
  | ''esp''   \<Rightarrow> (True,  ''rsp'', 0,32)
  | ''sp''    \<Rightarrow> (False, ''rsp'', 0,16)
  | ''spl''   \<Rightarrow> (False, ''rsp'', 0,8)
  \<comment> \<open> RDI, RSI, R8 to R15\<close>
  | ''rdi''   \<Rightarrow> (True,  ''rdi'', 0,64)
  | ''edi''   \<Rightarrow> (True,  ''rdi'', 0,32)
  | ''di''    \<Rightarrow> (False, ''rdi'', 0,16)
  | ''dil''   \<Rightarrow> (False, ''rdi'', 0,8)
  | ''rsi''   \<Rightarrow> (True,  ''rsi'', 0,64)
  | ''esi''   \<Rightarrow> (True,  ''rsi'', 0,32)
  | ''si''    \<Rightarrow> (False, ''rsi'', 0,16)
  | ''sil''   \<Rightarrow> (False, ''rsi'', 0,8)
  | ''r15''   \<Rightarrow> (True,  ''r15'', 0,64)
  | ''r15d''  \<Rightarrow> (True,  ''r15'', 0,32)
  | ''r15w''  \<Rightarrow> (False, ''r15'', 0,16)
  | ''r15b''  \<Rightarrow> (False, ''r15'', 0,8)
  | ''r14''   \<Rightarrow> (True,  ''r14'', 0,64)
  | ''r14d''  \<Rightarrow> (True,  ''r14'', 0,32)
  | ''r14w''  \<Rightarrow> (False, ''r14'', 0,16)
  | ''r14b''  \<Rightarrow> (False, ''r14'', 0,8)
  | ''r13''   \<Rightarrow> (True,  ''r13'', 0,64)
  | ''r13d''  \<Rightarrow> (True,  ''r13'', 0,32)
  | ''r13w''  \<Rightarrow> (False, ''r13'', 0,16)
  | ''r13b''  \<Rightarrow> (False, ''r13'', 0,8)
  | ''r12''   \<Rightarrow> (True,  ''r12'', 0,64)
  | ''r12d''  \<Rightarrow> (True,  ''r12'', 0,32)
  | ''r12w''  \<Rightarrow> (False, ''r12'', 0,16)
  | ''r12b''  \<Rightarrow> (False, ''r12'', 0,8)
  | ''r11''   \<Rightarrow> (True,  ''r11'', 0,64)
  | ''r11d''  \<Rightarrow> (True,  ''r11'', 0,32)
  | ''r11w''  \<Rightarrow> (False, ''r11'', 0,16)
  | ''r11b''  \<Rightarrow> (False, ''r11'', 0,8)
  | ''r10''   \<Rightarrow> (True,  ''r10'', 0,64)
  | ''r10d''  \<Rightarrow> (True,  ''r10'', 0,32)
  | ''r10w''  \<Rightarrow> (False, ''r10'', 0,16)
  | ''r10b''  \<Rightarrow> (False, ''r10'', 0,8)
  | ''r9''    \<Rightarrow> (True,  ''r9'' , 0,64)
  | ''r9d''   \<Rightarrow> (True,  ''r9'' , 0,32)
  | ''r9w''   \<Rightarrow> (False, ''r9'' , 0,16)
  | ''r9b''   \<Rightarrow> (False, ''r9'' , 0,8)
  | ''r8''    \<Rightarrow> (True,  ''r8'' , 0,64)
  | ''r8d''   \<Rightarrow> (True,  ''r8'' , 0,32)
  | ''r8w''   \<Rightarrow> (False, ''r8'' , 0,16)
  | ''r8b''   \<Rightarrow> (False, ''r8'' , 0,8)
  \<comment> \<open>xmm\<close>
  | ''xmm0''  \<Rightarrow> (True, ''xmm0'' , 0,128)
  | ''xmm1''  \<Rightarrow> (True, ''xmm1'' , 0,128)
  | ''xmm2''  \<Rightarrow> (True, ''xmm2'' , 0,128)
  | ''xmm3''  \<Rightarrow> (True, ''xmm3'' , 0,128)
  | ''xmm4''  \<Rightarrow> (True, ''xmm4'' , 0,128)
  | ''xmm5''  \<Rightarrow> (True, ''xmm5'' , 0,128)
  | ''xmm6''  \<Rightarrow> (True, ''xmm6'' , 0,128)
  | ''xmm7''  \<Rightarrow> (True, ''xmm7'' , 0,128)
  | ''xmm8''  \<Rightarrow> (True, ''xmm8'' , 0,128)
  | ''xmm9''  \<Rightarrow> (True, ''xmm9'' , 0,128)
  | ''xmm10''  \<Rightarrow> (True, ''xmm10'' , 0,128)
  | ''xmm11''  \<Rightarrow> (True, ''xmm11'' , 0,128)
  | ''xmm12''  \<Rightarrow> (True, ''xmm12'' , 0,128)
  | ''xmm13''  \<Rightarrow> (True, ''xmm13'' , 0,128)
  | ''xmm14''  \<Rightarrow> (True, ''xmm14'' , 0,128)
  | ''xmm15''  \<Rightarrow> (True, ''xmm15'' , 0,128)
  "

lemma real_reg_simps:
  shows 
  \<comment> \<open>rip\<close>
"real_reg ''rip'' = (True,  ''rip'', 0,64)"
  \<comment> \<open>rax,rbx,rcx,rdx\<close>
and "real_reg ''rax'' = (True,  ''rax'', 0,64)"
and "real_reg ''eax'' = (True,  ''rax'', 0,32)"
and "real_reg ''ax''  = (False, ''rax'', 0,16)"
and "real_reg ''ah''  = (False, ''rax'', 8,16)"
and "real_reg ''al''  = (False, ''rax'', 0,8)"
and "real_reg ''rbx'' = (True,  ''rbx'', 0,64)"
and "real_reg ''ebx'' = (True,  ''rbx'', 0,32)"
and "real_reg ''bx''  = (False, ''rbx'', 0,16)"
and "real_reg ''bh''  = (False, ''rbx'', 8,16)"
and "real_reg ''bl''  = (False, ''rbx'', 0,8)"
and "real_reg ''rcx'' = (True,  ''rcx'', 0,64)"
and "real_reg ''ecx'' = (True,  ''rcx'', 0,32)"
and "real_reg ''cx''  = (False, ''rcx'', 0,16)"
and "real_reg ''ch''  = (False, ''rcx'', 8,16)"
and "real_reg ''cl''  = (False, ''rcx'', 0,8)"
and "real_reg ''rdx'' = (True,  ''rdx'', 0,64)"
and "real_reg ''edx'' = (True,  ''rdx'', 0,32)"
and "real_reg ''dx''  = (False, ''rdx'', 0,16)"
and "real_reg ''dh''  = (False, ''rdx'', 8,16)"
and "real_reg ''dl''  = (False, ''rdx'', 0,8)"
  \<comment> \<open>RBP, RSP\<close>
and "real_reg ''rbp'' = (True,  ''rbp'', 0,64)"
and "real_reg ''ebp'' = (True,  ''rbp'', 0,32)"
and "real_reg ''bp''  = (False, ''rbp'', 0,16)"
and "real_reg ''bpl'' = (False, ''rbp'', 0,8)"
and "real_reg ''rsp'' = (True,  ''rsp'', 0,64)"
and "real_reg ''esp'' = (True,  ''rsp'', 0,32)"
and "real_reg ''sp''  = (False, ''rsp'', 0,16)"
and "real_reg ''spl'' = (False, ''rsp'', 0,8)"
  \<comment> \<open> RDI, RSI, R8 to R15\<close>
and "real_reg ''rdi'' = (True,  ''rdi'', 0,64)"
and "real_reg ''edi'' = (True,  ''rdi'', 0,32)"
and "real_reg ''di''  = (False, ''rdi'', 0,16)"
and "real_reg ''dil'' = (False, ''rdi'', 0,8)"
and "real_reg ''rsi'' = (True,  ''rsi'', 0,64)"
and "real_reg ''esi'' = (True,  ''rsi'', 0,32)"
and "real_reg ''si''  = (False, ''rsi'', 0,16)"
and "real_reg ''sil'' = (False, ''rsi'', 0,8)"
and "real_reg ''r15'' = (True,  ''r15'', 0,64)"
and "real_reg ''r15d''= (True,  ''r15'', 0,32)"
and "real_reg ''r15w''= (False, ''r15'', 0,16)"
and "real_reg ''r15b''= (False, ''r15'', 0,8)"
and "real_reg ''r14'' = (True,  ''r14'', 0,64)"
and "real_reg ''r14d''= (True,  ''r14'', 0,32)"
and "real_reg ''r14w''= (False, ''r14'', 0,16)"
and "real_reg ''r14b''= (False, ''r14'', 0,8)"
and "real_reg ''r13'' = (True,  ''r13'', 0,64)"
and "real_reg ''r13d''= (True,  ''r13'', 0,32)"
and "real_reg ''r13w''= (False, ''r13'', 0,16)"
and "real_reg ''r13b''= (False, ''r13'', 0,8)"
and "real_reg ''r12'' = (True,  ''r12'', 0,64)"
and "real_reg ''r12d''= (True,  ''r12'', 0,32)"
and "real_reg ''r12w''= (False, ''r12'', 0,16)"
and "real_reg ''r12b''= (False, ''r12'', 0,8)"
and "real_reg ''r11'' = (True,  ''r11'', 0,64)"
and "real_reg ''r11d''= (True,  ''r11'', 0,32)"
and "real_reg ''r11w''= (False, ''r11'', 0,16)"
and "real_reg ''r11b''= (False, ''r11'', 0,8)"
and "real_reg ''r10'' = (True,  ''r10'', 0,64)"
and "real_reg ''r10d''= (True,  ''r10'', 0,32)"
and "real_reg ''r10w''= (False, ''r10'', 0,16)"
and "real_reg ''r10b''= (False, ''r10'', 0,8)"
and "real_reg ''r9''  = (True,  ''r9'' , 0,64)"
and "real_reg ''r9d'' = (True,  ''r9'' , 0,32)"
and "real_reg ''r9w'' = (False, ''r9'' , 0,16)"
and "real_reg ''r9b'' = (False, ''r9'' , 0,8)"
and "real_reg ''r8''  = (True,  ''r8'' , 0,64)"
and "real_reg ''r8d'' = (True,  ''r8'' , 0,32)"
and "real_reg ''r8w'' = (False, ''r8'' , 0,16)"
and "real_reg ''r8b'' = (False, ''r8'' , 0,8)"
  \<comment> \<open>xmm\<close>
and "real_reg ''xmm0''= (True, ''xmm0'' , 0,128)"
and "real_reg ''xmm1''= (True, ''xmm1'' , 0,128)"
and "real_reg ''xmm2''= (True, ''xmm2'' , 0,128)"
and "real_reg ''xmm3''= (True, ''xmm3'' , 0,128)"
and "real_reg ''xmm4''= (True, ''xmm4'' , 0,128)"
and "real_reg ''xmm5''= (True, ''xmm5'' , 0,128)"
and "real_reg ''xmm6''= (True, ''xmm6'' , 0,128)"
and "real_reg ''xmm7''= (True, ''xmm7'' , 0,128)"
and "real_reg ''xmm8''= (True, ''xmm8'' , 0,128)"
and "real_reg ''xmm9''= (True, ''xmm9'' , 0,128)"
and "real_reg ''xmm10''= (True, ''xmm10'' , 0,128)"
and "real_reg ''xmm11''= (True, ''xmm11'' , 0,128)"
and "real_reg ''xmm12''= (True, ''xmm12'' , 0,128)"
and "real_reg ''xmm13''= (True, ''xmm13'' , 0,128)"
and "real_reg ''xmm14''= (True, ''xmm14'' , 0,128)"
and "real_reg ''xmm15''= (True, ''xmm15'' , 0,128)"
  by (simp add: real_reg_def)+


text \<open>x86 has register aliassing. For example, register EAX is the lower 32 bits of register RAX.
      This function map register aliasses to the ``real'' register. For example:

      @{term "real_reg ''ah'' = (False, ''rax'', 8,16)"}.

      This means that register AH is the second byte (bits 8 to 16) of register RAX.
      The bool @{const False} indicates that writing to AH does not overwrite the remainder of RAX.
r
      @{term "real_reg ''eax'' = (True, ''rax'', 0,32)"}.

      Register EAX is the lower 4 bytes of RAX. Writing to EAX means overwritting the remainder of RAX
      with zeroes.
\<close>

definition reg_size :: "string \<Rightarrow> nat" \<comment> \<open>in bytes\<close>
  where "reg_size reg \<equiv> case real_reg reg of (_,_,l,h) \<Rightarrow> (h - l) div 8"


text\<open> We now define functions for reading and writing from state.\<close>

definition reg_read :: "state \<Rightarrow> string \<Rightarrow> 256 word"
  where "reg_read \<sigma> reg \<equiv>
      if reg = ''rip'' then ucast (rip \<sigma>) else
      if reg = '''' then 0 else \<comment> \<open>happens if no base register is used in an address\<close>
       case real_reg reg of
        (_,r,l,h) \<Rightarrow> \<langle>l,h\<rangle>(regs \<sigma> r)"


primrec fromBool :: "bool \<Rightarrow> 'a :: len word"
  where
    "fromBool True = 1"
  | "fromBool False = 0"

definition flag_read :: "state \<Rightarrow> string \<Rightarrow> 256 word"
  where "flag_read \<sigma> flag \<equiv> fromBool (flags \<sigma> flag)"

definition mem_read :: "state \<Rightarrow> 64 word \<Rightarrow> nat \<Rightarrow> 256 word"
  where "mem_read \<sigma> a si \<equiv> word_rcat (read_bytes (mem \<sigma>) a si)"


text \<open>Doing state-updates occur through a tiny deeply embedded language of state updates. This allows us
      to reason over state updates through theorems.\<close>

datatype StateUpdate =
    RegUpdate string "256 word"         \<comment> \<open>Write value to register\<close>
  | FlagUpdate "string \<Rightarrow> bool"         \<comment> \<open>Update all flags at once\<close>
  | RipUpdate "64 word"                 \<comment> \<open>Update instruction pointer with address\<close>
  | MemUpdate "64 word" nat "256 word"  \<comment> \<open>Write a number of bytes of a value to the address\<close>

primrec state_update
  where
    "state_update (RegUpdate reg val)  = (\<lambda> \<sigma> . \<sigma>\<lparr>regs := (regs \<sigma>)(reg := val)\<rparr>)"
  | "state_update (FlagUpdate  val)    = (\<lambda> \<sigma> . \<sigma>\<lparr>flags := val\<rparr>)"
  | "state_update (RipUpdate a)        = (\<lambda> \<sigma> . \<sigma>\<lparr>rip := a\<rparr>)"
  | "state_update (MemUpdate a si val) = (\<lambda> \<sigma> .
        let new = (\<lambda> a' . take_byte (unat (a' - a)) val) in
         \<sigma>\<lparr>mem := override_on (mem \<sigma>) new (region_addresses a si)\<rparr>)"

abbreviation RegUpdateSyntax ("_ :=\<^sub>r _" 30)
  where "RegUpdateSyntax reg val \<equiv> RegUpdate reg val"
abbreviation MemUpdateSyntax ("\<lbrakk>_,_\<rbrakk> :=\<^sub>m _" 30)
  where "MemUpdateSyntax a si val \<equiv> MemUpdate a si val"
abbreviation FlagUpdateSyntax ("setFlags")
  where "FlagUpdateSyntax val \<equiv> FlagUpdate val"
abbreviation RipUpdateSyntax ("setRip")
  where "RipUpdateSyntax val \<equiv> RipUpdate val"




text \<open>A datatype for operands of instructions.\<close>

datatype Operand =
    Imm "256 word"
  | Reg string
  | Flag string
  | Mem  nat    "64 word"   string    "string"      nat
      \<comment> \<open>size   offset      base-reg  index-reg    scale\<close>

abbreviation mem_op_no_offset_no_index :: "string \<Rightarrow> (64 word \<times> string \<times> string \<times> nat)" ("[_]\<^sub>1" 40)
  where "mem_op_no_offset_no_index r \<equiv> (0,r,[],0)"

abbreviation mem_op_no_index :: "64 word \<Rightarrow> string \<Rightarrow> (64 word \<times> string \<times> string \<times> nat)" ("[_ + _]\<^sub>2" 40)
  where "mem_op_no_index offset r \<equiv> (offset,r,[],0)"

abbreviation mem_op :: "64 word \<Rightarrow> string \<Rightarrow> string \<Rightarrow> nat \<Rightarrow> (64 word \<times> string \<times> string \<times> nat)" ("[_ + _ + _ * _]\<^sub>3" 40)
  where "mem_op offset r index scale\<equiv> (offset,r,index,scale)"

definition ymm_ptr ("YMMWORD PTR _")
  where "YMMWORD PTR x \<equiv> case x of (offset,base,index,scale) \<Rightarrow> Mem 32 offset base index scale"

definition xmm_ptr ("XMMWORD PTR _")
  where "XMMWORD PTR x \<equiv> case x of (offset,base,index,scale) \<Rightarrow> Mem 16 offset base index scale"

definition qword_ptr ("QWORD PTR _")
  where "QWORD PTR x \<equiv> case x of (offset,base,index,scale) \<Rightarrow> Mem 8 offset base index scale"

definition dword_ptr ("DWORD PTR _")
  where "DWORD PTR x \<equiv> case x of (offset,base,index,scale) \<Rightarrow> Mem 4 offset base index scale"

definition word_ptr ("WORD PTR _")
  where "WORD PTR x \<equiv> case x of (offset,base,index,scale) \<Rightarrow> Mem 2 offset base index scale"

definition byte_ptr ("BYTE PTR _")
  where "BYTE PTR x \<equiv> case x of (offset,base,index,scale) \<Rightarrow> Mem 1 offset base index scale"


primrec (nonexhaustive) operand_size :: "Operand \<Rightarrow> nat" \<comment> \<open>in bytes\<close>
  where
    "operand_size (Reg r) = reg_size r"
  | "operand_size (Mem si _ _ _ _) = si"
  | "operand_size (Imm _) = 8"

fun resolve_address :: "state \<Rightarrow> 64 word \<Rightarrow> char list \<Rightarrow> char list \<Rightarrow> nat \<Rightarrow> 64 word"
  where "resolve_address \<sigma> offset base index scale =
      (let i = ucast (reg_read \<sigma> index);
           b = ucast (reg_read \<sigma> base) in
         b + offset + of_nat scale*i)"

primrec operand_read :: "state \<Rightarrow> Operand \<Rightarrow> 256 word"
  where
    "operand_read \<sigma> (Imm i)  = i"
  | "operand_read \<sigma> (Reg r)  = reg_read \<sigma> r"
  | "operand_read \<sigma> (Flag f) = flag_read \<sigma> f"
  | "operand_read \<sigma> (Mem si offset base index scale) =
      (let a = resolve_address \<sigma> offset base index scale in
        mem_read \<sigma> a si
      )"


primrec state_with_updates :: "state \<Rightarrow> StateUpdate list \<Rightarrow> state" (infixl "with" 66)
  where
    "\<sigma> with [] = \<sigma>"
  | "(\<sigma> with (f#fs)) = state_update f (\<sigma> with fs)"

text \<open>Executes a write to a register in terms of the tiny deeply embedded language above.\<close>
definition reg_write
  where "reg_write reg val \<sigma> \<equiv>
      let (b,r,l,h)  = real_reg reg;
          curr_val   = reg_read \<sigma> r;
          new_val    = if b then val else overwrite l h (\<langle>0,reg_size r*8\<rangle>curr_val) val in
        \<sigma> with [r :=\<^sub>r new_val]"

primrec (nonexhaustive) operand_write :: "Operand \<Rightarrow> 256word \<Rightarrow> state \<Rightarrow> state"
  where
    "operand_write (Reg r)  v \<sigma> = reg_write r v \<sigma>"
  | "operand_write (Mem si offset base index scale) v \<sigma> =
      (let i = ucast (reg_read \<sigma> index);
           b = ucast (reg_read \<sigma> base);
           a = offset + b + of_nat scale*i in
        \<sigma> with [\<lbrakk>a,si\<rbrakk> :=\<^sub>m v]
      )"





text \<open> The following theorems simplify reading from state parts after doing updates to other state parts.\<close>

lemma regs_reg_write:
  shows "regs (\<sigma> with ((r :=\<^sub>r w)#updates)) r' = (if r=r' then w else regs (\<sigma> with updates) r')"
  by (induct updates arbitrary: \<sigma>, auto simp add: case_prod_unfold Let_def)

lemma regs_mem_write:
  shows "regs (\<sigma> with ((\<lbrakk>a,si\<rbrakk> :=\<^sub>m v)#updates)) r = regs (\<sigma> with updates) r"
  by (induct updates arbitrary: \<sigma>, auto)

lemma regs_flag_write:
  shows "regs (\<sigma> with ((setFlags v)#updates)) r = regs (\<sigma> with updates) r"
  by (induct updates arbitrary: \<sigma>, auto)

lemma regs_rip_write:
  shows "regs (\<sigma> with ((setRip a)#updates)) f = regs (\<sigma> with updates) f"
  by (auto)


lemma flag_read_reg_write:
  shows "flag_read (\<sigma> with ((r :=\<^sub>r w)#updates)) f = flag_read (\<sigma> with updates) f"
  by (induct updates arbitrary: \<sigma>, auto simp add: flag_read_def)

lemma flag_read_mem_write:
  shows "flag_read (\<sigma> with ((\<lbrakk>a,si\<rbrakk> :=\<^sub>m v)#updates)) f = flag_read (\<sigma> with updates) f"
  by (induct updates arbitrary: \<sigma>, auto simp add: flag_read_def)

lemma flag_read_flag_write:
  shows "flag_read (\<sigma> with ((setFlags v)#updates)) = fromBool o v"
  by (induct updates arbitrary: \<sigma>, auto simp add: flag_read_def)

lemma flag_read_rip_write:
  shows "flag_read (\<sigma> with ((setRip a)#updates)) f = flag_read (\<sigma> with updates) f"
  by (auto simp add: flag_read_def)

lemma mem_read_reg_write:
  shows "mem_read (\<sigma> with ((r :=\<^sub>r w)#updates)) a si = mem_read (\<sigma> with updates) a si"
  by (auto simp add: mem_read_def read_bytes_def)

lemma mem_read_flag_write:
  shows "mem_read (\<sigma> with ((setFlags v)#updates)) a si = mem_read (\<sigma> with updates) a si"
  by (auto simp add: mem_read_def read_bytes_def)

lemma mem_read_rip_write:
  shows "mem_read (\<sigma> with ((setRip a')#updates)) a si = mem_read (\<sigma> with updates) a si"
  by (auto simp add: mem_read_def read_bytes_def)

lemma mem_read_mem_write_alias:
  assumes "si' \<le> si"
      and "si \<le> 2^64"
  shows "mem_read (\<sigma> with ((\<lbrakk>a,si\<rbrakk> :=\<^sub>m v)#updates)) a si' = \<langle>0,si'*8\<rangle> v"
  using assms
  by (auto simp add: mem_read_def word_rcat_read_bytes read_bytes_override_on_enclosed[where offset=0 and offset'=0,simplified])

lemma mem_read_mem_write_alias_plus_symmetric:
  assumes "si' \<le> si"
      and "si \<le> 2^64"
    shows "mem_read (\<sigma> with ((\<lbrakk>a+b,si\<rbrakk> :=\<^sub>m v)#updates)) (b+a) si' = \<langle>0,si'*8\<rangle> v"
  using mem_read_mem_write_alias[OF assms]
  by (auto simp add:  algebra_simps)

lemma mem_read_mem_write_separate:
  assumes "separate a si a' si'"
shows "mem_read (\<sigma> with ((\<lbrakk>a,si\<rbrakk> :=\<^sub>m v)#updates)) a' si' = mem_read (\<sigma> with updates) a' si'"
  using assms
  by (auto simp add: mem_read_def read_bytes_override_on_separate)

lemma mem_read_mem_write_enclosed_minus:
  assumes "offset' \<le> offset"
    and "si' \<le> si"
    and "unat (offset - offset') + si' < 2^64"
    and "unat offset + si' \<le> si + unat offset'"
  shows "mem_read (\<sigma> with ((\<lbrakk>a - offset,si\<rbrakk> :=\<^sub>m v)#updates)) (a - offset') si' = \<langle>unat (offset - offset') * 8,unat (offset - offset') * 8 + si' * 8\<rangle>v"
  using assms
  by (auto simp add: mem_read_def read_bytes_override_on_enclosed word_rcat_read_bytes_enclosed[of "offset - offset'" si' "a - offset" v,simplified])

lemma mem_read_mem_write_enclosed_plus:
assumes "unat offset + si' \<le> si"
    and "si < 2 ^ 64"
  shows "mem_read (\<sigma> with ((\<lbrakk>a,si\<rbrakk> :=\<^sub>m v)#updates)) (offset + a) si' = \<langle>unat offset * 8,(unat offset + si') * 8\<rangle>v"
  using assms
  apply (auto simp add: mem_read_def read_bytes_override_on_enclosed_plus)
  using word_rcat_read_bytes_enclosed[of offset si' a v]
  by auto (simp add: add.commute)

lemma mem_read_mem_write_enclosed_plus2:
assumes "unat offset + si' \<le> si"
    and "si < 2 ^ 64"
  shows "mem_read (\<sigma> with ((\<lbrakk>a,si\<rbrakk> :=\<^sub>m v)#updates)) (a + offset) si' = \<langle>unat offset * 8,(unat offset + si') * 8\<rangle>v"
  using mem_read_mem_write_enclosed_plus[OF assms]
  by (auto simp add: add.commute)

lemma mem_read_mem_write_enclosed_numeral[simp]:
assumes "unat (numeral a' - numeral a::64 word) + (numeral si'::nat) \<le> numeral si"
    and "numeral a' \<ge> (numeral a::64 word)"
    and "numeral si < (2 ^ 64::nat)"
  shows "mem_read (\<sigma> with ((\<lbrakk>numeral a,numeral si\<rbrakk> :=\<^sub>m v)#updates)) (numeral a') (numeral si') = \<langle>unat (numeral a' - (numeral a::64 word)) * 8,(unat (numeral a' - (numeral a::64 word)) + (numeral si')) * 8\<rangle>v"
proof-
  have 1: "numeral a + (numeral a' - numeral a) = (numeral a'::64 word)"
    using assms(2) by (metis add.commute diff_add_cancel)
  thus ?thesis
    using mem_read_mem_write_enclosed_plus2[of "numeral a' - numeral a" "numeral si'" "numeral si" \<sigma> "numeral a" v updates,OF assms(1,3)]
    by auto
qed

lemma mem_read_mem_write_enclosed_numeral1_[simp]:
assumes "unat (numeral a' - numeral a::64 word) + (numeral si'::nat) \<le> Suc 0"
    and "numeral a' \<ge> (numeral a::64 word)"
  shows "mem_read (\<sigma> with ((\<lbrakk>numeral a,Suc 0\<rbrakk> :=\<^sub>m v)#updates)) (numeral a') (numeral si') = \<langle>unat (numeral a' - (numeral a::64 word)) * 8,(unat (numeral a' - (numeral a::64 word)) + (numeral si')) * 8\<rangle>v"
proof-
  have 1: "numeral a + (numeral a' - numeral a) = (numeral a'::64 word)"
    using assms(2) by (metis add.commute diff_add_cancel)
  thus ?thesis
    using mem_read_mem_write_enclosed_plus2[of "numeral a' - numeral a" "numeral si'" "Suc 0" \<sigma> "numeral a" v updates,OF assms(1)]
    by auto
qed

lemma mem_read_mem_write_enclosed_numeral_1[simp]:
assumes "unat (numeral a' - numeral a::64 word) + (Suc 0) \<le> numeral si"
    and "numeral a' \<ge> (numeral a::64 word)"
    and "numeral si < (2 ^ 64::nat)"
  shows "mem_read (\<sigma> with ((\<lbrakk>numeral a,numeral si\<rbrakk> :=\<^sub>m v)#updates)) (numeral a') (Suc 0) = \<langle>unat (numeral a' - (numeral a::64 word)) * 8,(unat (numeral a' - (numeral a::64 word)) + (Suc 0)) * 8\<rangle>v"
proof-
  have 1: "numeral a + (numeral a' - numeral a) = (numeral a'::64 word)"
    using assms(2) by (metis add.commute diff_add_cancel)
  thus ?thesis
    using mem_read_mem_write_enclosed_plus2[of "numeral a' - numeral a" "Suc 0" "numeral si" \<sigma> "numeral a" v updates,OF assms(1,3)]
    by auto
qed


lemma mem_read_mem_write_enclosed_numeral11[simp]:
assumes "unat (numeral a' - numeral a::64 word) + (Suc 0) \<le> Suc 0"
    and "numeral a' \<ge> (numeral a::64 word)"
  shows "mem_read (\<sigma> with ((\<lbrakk>numeral a,Suc 0\<rbrakk> :=\<^sub>m v)#updates)) (numeral a') (Suc 0) = \<langle>unat (numeral a' - (numeral a::64 word)) * 8,(unat (numeral a' - (numeral a::64 word)) + (Suc 0)) * 8\<rangle>v"
proof-
  have 1: "numeral a + (numeral a' - numeral a) = (numeral a'::64 word)"
    using assms(2) by (metis add.commute diff_add_cancel)
  thus ?thesis
    using mem_read_mem_write_enclosed_plus2[of "numeral a' - numeral a" "Suc 0" "Suc 0" \<sigma> "numeral a" v updates,OF assms(1)]
    by auto
qed


lemma rip_reg_write[simp]:
  shows "rip (\<sigma> with ((r :=\<^sub>r v)#updates)) = rip (\<sigma> with updates)"
  by (auto simp add: case_prod_unfold Let_def)

lemma rip_flag_write[simp]:
  shows "rip (\<sigma> with ((setFlags v)#updates)) = rip (\<sigma> with updates)"
  by (auto)

lemma rip_mem_write[simp]:
  shows "rip (\<sigma> with ((\<lbrakk>a,si\<rbrakk> :=\<^sub>m v)#updates)) = rip (\<sigma> with updates)"
  by (auto)

lemma rip_rip_write[simp]:
 shows "rip (\<sigma> with ((setRip a)#updates)) = a"
  by (auto)


lemma with_with:
  shows "(\<sigma> with updates) with updates' = \<sigma> with (updates' @ updates)"
by (induct updates' arbitrary: \<sigma>,auto)

lemma add_state_update_to_list:
  shows "state_update upd (\<sigma> with updates) = \<sigma> with (upd#updates)"
  by auto

text \<open>The updates performed to a state are ordered: memoery, registers, flags, rip.
      This function is basically insertion sort. Moreover, consecutive updates to the same register
      are removed.\<close>

fun insert_state_update
  where
    "insert_state_update (setRip a) (setRip a'#updates) = insert_state_update (setRip a) updates"
  | "insert_state_update (setRip a) (setFlags v#updates) = setFlags v # (insert_state_update (setRip a) updates)"
  | "insert_state_update (setRip a) ((r :=\<^sub>r v)#updates) = (r :=\<^sub>r v) # (insert_state_update (setRip a) updates)"
  | "insert_state_update (setRip a) ((\<lbrakk>a',si\<rbrakk> :=\<^sub>m v)#updates) = (\<lbrakk>a',si\<rbrakk> :=\<^sub>m v) # (insert_state_update (setRip a) updates)"

  | "insert_state_update (setFlags v) (setFlags v'#updates) = insert_state_update (setFlags v) updates"
  | "insert_state_update (setFlags v) ((r :=\<^sub>r v')#updates) = (r :=\<^sub>r v') # insert_state_update (setFlags v) updates"
  | "insert_state_update (setFlags v) ((\<lbrakk>a',si\<rbrakk> :=\<^sub>m v')#updates) = (\<lbrakk>a',si\<rbrakk> :=\<^sub>m v') # insert_state_update (setFlags v) updates"

  | "insert_state_update ((r :=\<^sub>r v)) ((r' :=\<^sub>r v')#updates) = (if r = r' then insert_state_update (r :=\<^sub>r v) updates else (r' :=\<^sub>r v')#insert_state_update (r :=\<^sub>r v) updates)"
  | "insert_state_update ((r :=\<^sub>r v)) ((\<lbrakk>a',si\<rbrakk> :=\<^sub>m v')#updates) = (\<lbrakk>a',si\<rbrakk> :=\<^sub>m v') # insert_state_update (r :=\<^sub>r v) updates"

  | "insert_state_update upd updates = upd # updates"

fun clean
  where
    "clean [] = []"
  | "clean [upd] = [upd]"
  | "clean (upd#upd'#updates) =  insert_state_update upd (clean (upd'#updates))"

lemma insert_state_update:
  shows "\<sigma> with (insert_state_update upd updates) = \<sigma> with (upd # updates)"
  by (induct updates rule: insert_state_update.induct,auto simp add: fun_upd_twist)

lemma clean_state_updates:
  shows "\<sigma> with (clean updates) = \<sigma> with updates"
  by (induct updates rule: clean.induct,auto simp add: insert_state_update)



text \<open>The set of simplification rules used during symbolic execution.\<close>
named_theorems state_simps
declare qword_ptr_def [state_simps]
declare dword_ptr_def [state_simps]
declare word_ptr_def [state_simps]
declare byte_ptr_def [state_simps]
declare reg_size_def [state_simps]
lemmas reg_write'[state_simps] = reg_write_def[unfolded Let_def]
declare real_reg_simps [state_simps]
declare reg_read_def [state_simps]
declare regs_rip_write [state_simps]
declare regs_mem_write [state_simps]
declare regs_reg_write [state_simps]
declare regs_flag_write [state_simps]
declare flag_read_reg_write [state_simps]
declare flag_read_mem_write [state_simps]
declare flag_read_rip_write [state_simps]
declare flag_read_flag_write [state_simps]
declare mem_read_reg_write [state_simps]
declare mem_read_flag_write [state_simps]
declare mem_read_rip_write [state_simps]
declare mem_read_mem_write_alias [state_simps]
declare mem_read_mem_write_alias_plus_symmetric [state_simps]
declare mem_read_mem_write_separate [state_simps]
declare mem_read_mem_write_enclosed_minus [state_simps]
declare mem_read_mem_write_enclosed_plus [state_simps]
declare mem_read_mem_write_enclosed_plus2 [state_simps]
declare with_with [state_simps]
declare add_state_update_to_list [state_simps]



declare state_with_updates.simps(2)[simp del]
declare state_update.simps[simp del]

end
