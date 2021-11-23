(*
  Base
*)
fun error_with_tracing err = 
  let val _ = tracing ("Error:" ^ err)
  in
    error_with_tracing err
  end




(* TODO: segment registers *)

datatype Operand =
           (*  size (in bytes)   segment           offset   base     index    scale *)
    Mem of (   int             * string option   * int    * string * string * int)
  | Reg of string
  | Imm of LargeInt.int

datatype Instr = Instr of (LargeInt.int * LargeInt.int * string * Operand option * Operand option * Operand option)
 | ExternalCall of (int * string * int)
 | ExternalCallWithReturn of (int * string * int)

(* PRETTY PRINTING *)
fun pp_option NONE = ""
  | pp_option (SOME s) = s

fun
  pp_mem_size 1 = "BYTE PTR"
| pp_mem_size 2 = "WORD PTR"
| pp_mem_size 4 = "DWORD PTR"
| pp_mem_size 8 = "QWORD PTR"
| pp_mem_size 16 = "XMMWORD PTR"
| pp_mem_size n = "SIZEDIR " ^ Int.toString (n*8) ^ " PTR"

fun pp_operand (Mem (si,segment,offset,base, index, scale)) =
    pp_mem_size si ^ " " ^ pp_option segment ^ ":[" ^ Int.toString offset ^ " + " ^ base ^ " + " ^ index ^ " * " ^ Int.toString scale ^ "]"
| pp_operand (Reg r) = r
| pp_operand (Imm i) = Int.toString i

fun pp_operands [] = ""
  | pp_operands (NONE::_) = ""
  | pp_operands [SOME op1] = pp_operand op1
  | pp_operands [SOME op1,NONE] = pp_operand op1
  | pp_operands (SOME op1::op2::ops) = pp_operand op1 ^ ", " ^ pp_operands (op2::ops)

fun pp_instr (Instr (a,si,m,op1,op2,op3)) =
  LargeInt.toString a ^ ": " ^ m ^ " " ^ pp_operands [op1,op2,op3] ^ "  (" ^ LargeInt.toString si ^ ")"
 | pp_instr (ExternalCall (a,f,si)) = LargeInt.toString a ^ ": " ^ "ExternalCall " ^ f ^ "  (" ^ LargeInt.toString si ^ ")"
 | pp_instr (ExternalCallWithReturn (a,f,si)) = LargeInt.toString a ^ ": " ^ "ExternalCallWithReturn " ^ f ^ "  (" ^ LargeInt.toString si ^ ")"
                     
(* PARSING *)

val intFromHexString = StringCvt.scanString (LargeInt.scan StringCvt.HEX) o Substring.string

fun intFromHexString_forced s =
    case intFromHexString s of
         SOME i => i
       | NONE => error_with_tracing ("Could not convert string '" ^ Substring.string s ^ "' to int.")

fun intFromString_forced s =
    case LargeInt.fromString (Substring.string s) of
         SOME i => i
       | NONE => error_with_tracing ("Could not convert string '" ^ Substring.string s ^ "' to int.")

fun is_whitespace c = (c = #" " orelse c = #"\t"  orelse c = #"\n")

fun trim str =
  let val (_,x) = Substring.splitl is_whitespace str
      val (y,_) = Substring.splitr is_whitespace x in
    y
  end;

val registers = [
  "rip",
  "rax", "eax", "ax", "ah", "al",
  "rbx", "ebx", "bx", "bh", "bl",
  "rcx", "ecx", "cx", "ch", "cl",
  "rdx", "edx", "dx", "dh", "dl",
  "rbp", "ebp", "bp", "bpl",
  "rsp", "esp", "sp", "spl",
  "rdi", "edi", "di", "dil",
  "rsi", "esi", "si", "sil",
  "r15", "r15d", "r15w", "r15b",
  "r14", "r14d", "r14w", "r14b",
  "r13", "r13d", "r13w", "r13b",
  "r12", "r12d", "r12w", "r12b",
  "r11", "r11d", "r11w", "r11b",
  "r10", "r10d", "r10w", "r10b",
  "r9", "r9d", "r9w", "r9b",
  "r8", "r8d", "r8w", "r8b",

  "xmm0","xmm1","xmm2","xmm3","xmm4","xmm5","xmm6","xmm7","xmm8",
  "xmm9","xmm10","xmm11","xmm12","xmm13","xmm14","xmm15"
]

fun toLowerStr str = Substring.full (String.implode (map Char.toLower (Substring.explode str)))
fun toUpperStr str = Substring.full (String.implode (map Char.toUpper (Substring.explode str)))


fun reg_size r =  
 case List.find (fn (str') => String.compare (Substring.string (toLowerStr r),str') = EQUAL)
            ["rip","rax","rbx","rcx","rdx","rbp","rsp","rdi","rsi","r15","r14","r13","r12","r11","r10","r9", "r8"]  of
   SOME _ => 64
 | NONE   =>
 case List.find (fn (str') => String.compare (Substring.string (toLowerStr r),str') = EQUAL)
            ["xmm0","xmm1","xmm2","xmm3","xmm4","xmm5","xmm6","xmm7","xmm8","xmm9","xmm10","xmm11","xmm12","xmm13","xmm14","xmm15"] of
   SOME _ => 128
 | NONE   => error_with_tracing ("Unknown register:" ^ Substring.string r)




fun is_register str = List.find (fn (str') => Substring.compare (toLowerStr str,Substring.full str') = EQUAL) registers <> NONE

fun overwrite_str "" s = s
| overwrite_str s "" = s
| overwrite_str _  s = s

fun overwrite_str_option NONE s = s
| overwrite_str_option s NONE = s
| overwrite_str_option _ s     = s

fun max x y = if x >= y then x else y

fun overwrite_Mem (Mem (si,seg,off,base,ind,sc)) (Mem (si',seg',off',base',ind',sc')) =
  if base <> "" andalso base' <> "" andalso ind' = "" then
    Mem (max si si',overwrite_str_option seg seg',max off off',base,base',1)
  else
    Mem (max si si',overwrite_str_option seg seg',max off off',overwrite_str base base',overwrite_str ind ind',max sc sc')
| overwrite_Mem _ _ = error_with_tracing "overwrite_Mem"

fun parse_operand_address_between_brackets_inner str =
  if is_register str then
    Mem (0,NONE,0,Substring.string str,"",0) (* base *)
  else
    let val tokens = map trim (Substring.tokens (fn c => c = #"*") str) in
      if length tokens = 1 then
        if Substring.isPrefix "0x" str then
          case intFromHexString str of
             SOME i => Mem (0,NONE,i,"","",0) (* offset *)
           | NONE => error_with_tracing ("Don't know how to parse operand part:" ^ Substring.string str)
        else 
          case LargeInt.fromString (Substring.string str) of
             SOME i => Mem (0,NONE,i,"","",0) (* offset *)
           | NONE => error_with_tracing ("Cannot parse operand: " ^ (Substring.string str))
      else if length tokens = 2 then
        if is_register (nth tokens 0) then
          Mem (0,NONE,0,"",Substring.string (nth tokens 0),intFromString_forced (nth tokens 1)) (* index * scale *)
        else if is_register (nth tokens 1) then
          Mem (0,NONE,0,"",Substring.string (nth tokens 1),intFromString_forced (nth tokens 0)) (* scale * index *)
        else
          error_with_tracing ("Don't know how to parse operand part:" ^ Substring.string str)
      else
        error_with_tracing ("Don't know how to parse operand part:" ^ Substring.string str)
    end

fun parse_operand_address_between_brackets_sum si segment_reg str =
  let val tokens = map trim (Substring.tokens (fn c => c = #"+") str) in
    fold (overwrite_Mem o parse_operand_address_between_brackets_inner)
         tokens
         (Mem (si,segment_reg ,0,"","",0))
  end;


fun parse_operand_address_between_brackets_sub si segment_reg str =
  let val (lhs,num) = Substring.splitl (fn c => c <> #"-") str;
      val (Mem (x0,x1,_,x3,x4,x5)) = parse_operand_address_between_brackets_sum si segment_reg lhs in
    Mem (x0,x1,0 - intFromString_forced (trim (Substring.triml 1 num)),x3,x4,x5)
   end

fun parse_operand_address_between_brackets si segment_reg str =
  let val (_,num) = Substring.splitl (fn c => c <> #"-") str in
    if Substring.isEmpty num then
      parse_operand_address_between_brackets_sum si segment_reg str
    else
      parse_operand_address_between_brackets_sub si segment_reg str
      end

fun skip_brackets str =
  let val (x,y) = Substring.splitAt (trim str,1)
      val (z,_) = Substring.splitl (fn c => c <> #"]") y in
    if Substring.compare (x,Substring.full "[") = EQUAL then
      z
    else
      error_with_tracing ("Expecting non-empty bracketed string preceded with colon or an immediate in hex-format, but got: " ^ Substring.string str)
  end;

fun parse_operand_address_bracketed si segment_reg str =
  case LargeInt.fromString (Substring.string str) of
      SOME imm => Mem (si,segment_reg,imm,"", "",0)
    | NONE => parse_operand_address_between_brackets si segment_reg (skip_brackets str)

fun tail str =
  case Substring.getc str of
      NONE => error_with_tracing ("Expecting non-empty string, but got: " ^ Substring.string str)
    | SOME (_,s) => s;

fun parse_operand_address si str =
  case Substring.splitl (fn c => c <> #":") str of
      (before_colon, after_colon) =>
          if Substring.isEmpty after_colon then
            parse_operand_address_bracketed si NONE before_colon
          else
            parse_operand_address_bracketed si (SOME (Substring.string (trim before_colon))) (tail after_colon);

fun parse_operand str' =
  let val str = trim str' in
    if Substring.isPrefix "BYTE PTR" str then
      parse_operand_address 1 (snd (Substring.splitAt (str,8)))
    else if Substring.isPrefix "WORD PTR" str then
      parse_operand_address 2 (snd (Substring.splitAt (str,8)))
    else if Substring.isPrefix "DWORD PTR" str then
      parse_operand_address 4 (snd (Substring.splitAt (str,9)))
    else if Substring.isPrefix "QWORD PTR" str then
      parse_operand_address 8 (snd (Substring.splitAt (str,9)))
    else if Substring.isPrefix "80 PTR" str then
      parse_operand_address 10 (snd (Substring.splitAt (str,6)))
    else if Substring.isPrefix "XMMWORD PTR" str then
      parse_operand_address 16 (snd (Substring.splitAt (str,11)))
    else if Substring.isPrefix "[" str then (* happens in case of a LEA instruction *)
      parse_operand_address 0 str
    else if is_register str then
      Reg (Substring.string str)
    else if Substring.isPrefix "0x" str then
      case intFromHexString (Substring.slice (str,2,NONE)) of
          NONE => error_with_tracing ("Cannot read hex number in string: " ^ (Substring.string str))
        | SOME imm => Imm imm
    else case LargeInt.fromString (Substring.string str) of
        SOME imm => Imm imm
      | NONE => error_with_tracing ("Cannot parse operand: " ^ (Substring.string str))
  end;

fun parse_operands str =
  let val tokens = map trim (Substring.tokens (fn c => c = #",") (trim str))
      val ops = map parse_operand tokens in
    case ops of
        [] => (NONE,NONE,NONE)
      | [op1] => (SOME op1,NONE,NONE)
      | [op1,op2] => (SOME op1,SOME op2,NONE)
      | [op1,op2,op3] => (SOME op1,SOME op2,SOME op3)
      | _ => error_with_tracing ("Unexpected number of operands in : " ^ Substring.string str)
  end;



fun parse_instr str =
  let val (addr,str1)   = Substring.splitl (fn c => c <> #":") str
      val a             = intFromHexString_forced (Substring.full ("0x" ^ Substring.string (trim addr)))
      val (str2,si_str) = Substring.splitr (fn c => c <> #" ") (trim (Substring.slice (str1,1,NONE)))
  in
    case LargeInt.fromString (Substring.string si_str) of
      NONE => error_with_tracing ("Could not parse intruction: " ^ Substring.string str)
    | SOME si => 
    let val (m,str3) = Substring.splitl (fn c => c <> #" ") (trim str2)
    in
       if Substring.isPrefix "ExternalCallWithReturn" m then
         ExternalCallWithReturn (a,Substring.string str3,si)
       else if Substring.isPrefix "ExternalCall" m then
         ExternalCall (a,Substring.string str3,si)
       else let val (op1,op2,op3) = parse_operands (trim str3)
            in
              Instr (a, si, Substring.string m, op1,op2,op3)
            end
    end
  end










(*
  Parse an expression using the Shunting-yard algorithm. 
  Precedence need not be taken into account as the Haskell tools produce parenthesised exprs.
  The following are the tokens:
*)

datatype ExprTokens =
    ET_Reg of substring * bool
  | ET_Imm of int
  | ET_MemRegion of bool 
  | ET_prefix of string
  | ET_binop of string
  | ET_Lpar (* left parenthesis *)
  | ET_Bot of substring

fun exprtoken_eq (ET_Reg (r0,b0)) (ET_Reg (r1,b1)) = b0 = b1 andalso Substring.compare (toLowerStr r0,toLowerStr r1) = EQUAL
| exprtoken_eq (ET_Imm i0) (ET_Imm i1) = (i0 = i1)
| exprtoken_eq (ET_prefix s0) (ET_prefix s1) = String.compare (s0,s1) = EQUAL
| exprtoken_eq (ET_binop s0) (ET_binop s1) = String.compare (s0,s1) = EQUAL
| exprtoken_eq ET_Lpar ET_Lpar = true
| exprtoken_eq (ET_MemRegion b0) (ET_MemRegion b1) = b0 = b1
| exprtoken_eq (ET_Bot s0) (ET_Bot s1) = Substring.compare (toLowerStr s0,toLowerStr s1) = EQUAL
| exprtoken_eq _ _ = false

fun show_exprtoken (ET_Reg (r,b)) = Substring.full (Substring.string r ^ (if b then "_0" else ""))
  | show_exprtoken (ET_Imm i) = Substring.full (Int.toString i)
  | show_exprtoken (ET_prefix s) = Substring.full s
  | show_exprtoken (ET_binop s)  = Substring.full ("`" ^ s ^ "`")
  | show_exprtoken ET_Lpar = Substring.full "("
  | show_exprtoken (ET_Bot f) = Substring.full (Substring.concat [Substring.full "bot(" ,f , Substring.full ")"])
  | show_exprtoken (ET_MemRegion b) = Substring.full ("[" ^ (if b then "_0" else ""))


fun parse_string_token token str =
  if Substring.isPrefix (Substring.string (toLowerStr (Substring.full token))) (toLowerStr str) then
    SOME (Substring.splitAt (str,String.size token))
  else
    NONE

fun parse_any_of_string_tokens [] _ = NONE
| parse_any_of_string_tokens (token::tokens) str = 
  case parse_string_token token str of
   NONE             => parse_any_of_string_tokens tokens str
 | SOME (token,str) => SOME (token,str)

fun parse_register str =
  case parse_any_of_string_tokens registers str of
    NONE => NONE
  | SOME (reg,str') =>
      case parse_string_token "_0" str' of
        NONE => SOME (reg,false,str')
      | SOME (_,str'') => SOME (reg,true,str'')

val prefix_operators = ["b8", "b16", "b32", "signextend", "overwrite", 
                          "not64", "not32", "not16", "not8",
                          "shl64", "shl32", "shl16", "shl8",
                          "shr64", "shr32", "shr16", "shr8",
                          "sar64", "sar32", "sar16", "sar8",
                          "div64", "div32", "div16", "div8",
                          "div_rem64", "div_rem32", "div_rem16", "div_rem8"]
fun parse_prefix_operator str = parse_any_of_string_tokens prefix_operators str

fun is_prefix_operator (ET_prefix _) = true
  | is_prefix_operator _             = false

val infix_operators = ["+64","+32","+16","+8",
                       "-64","-32","-16","-8", 
                       "*64","*32","*16","*8", 
                       "&64","&32","&16","&8", 
                       "|64","|32","|16","|8", 
                       "xor64","xor32","xor16","xor8",
                       "udiv64","udiv32","udiv16","udiv8",
                       "sdiv64","sdiv32","sdiv16","sdiv8"
                      ]
fun parse_infix_operator str = parse_any_of_string_tokens infix_operators str


fun parse_hex str =
  if Substring.isPrefix "0x" str then
    let val (hex,str') = Substring.splitl Char.isHexDigit (Substring.slice(str,2,NONE))
    in
      case intFromHexString hex of
        SOME i => SOME (i,str')
      | NONE => NONE
    end
  else
   NONE

fun parse_num str =
  let val (num,str') = Substring.splitl Char.isDigit str
  in
    if Substring.size num > 0 then 
      case LargeInt.fromString (Substring.string str) of
        SOME i => SOME (i,str')
      | NONE => NONE
    else
      NONE
  end

fun parse_bot str =
  if Substring.isPrefix "bot(" str then
    let val (f,str') = Substring.splitl (fn (c) => c <> #")") (Substring.triml 4 str)
    in
      SOME(f,Substring.triml 1 str')
    end
  else
    NONE


fun partitionUpto [] _ = ([],[])
| partitionUpto (x::xs) f =
  if f x then 
    ([],x::xs) 
  else let val (l,r) = partitionUpto xs f in
    (x::l,r)
  end



fun is_ET_Memregion (ET_MemRegion _) = true
| is_ET_Memregion _ = false

(* Shunting-yard algorithm *)
fun parse_exprtokens outQ opS str =
 let val _ = 0 in (*tracing (Substring.string str) in*)
  if Substring.compare (toLowerStr str,Substring.full "") = EQUAL then
    List.rev (outQ@opS)
  else
  (* TOKEN: prefix operator *)
  case parse_prefix_operator str of
    SOME (f,str') => parse_exprtokens outQ (ET_prefix (Substring.string f)::opS) (trim str')
  | NONE =>
  (* TOKEN: bot *)
  case parse_bot str of
    SOME (f,str') => parse_exprtokens (outQ@[ET_Bot f]) opS (trim str')
  | NONE =>
  (* TOKEN: register *)
  case parse_register str of
    SOME (reg,b,str') => parse_exprtokens (outQ@[ET_Reg (reg,b)]) opS (trim str')
  | NONE =>
  (* TOKEN: Immediate *)
  case parse_hex str of
    SOME (imm,str') => parse_exprtokens (outQ@[ET_Imm imm]) opS (trim str')
  | NONE =>
  case parse_num str of
    SOME (num,str') => parse_exprtokens (outQ@[ET_Imm num]) opS (trim str')
  | NONE =>
  (* TOKEN: infix operator *)
  case parse_infix_operator str of
    SOME (f,str') => 
      let val (toOutQ,opS') = partitionUpto opS (exprtoken_eq ET_Lpar)
      in
       parse_exprtokens (outQ@toOutQ) (ET_binop (Substring.string f)::opS') (trim str')
      end
  | NONE =>
  (* TOKEN: '(' *)
  case parse_string_token "(" str of
    SOME (_,str') => parse_exprtokens outQ (ET_Lpar :: opS) (trim str')
  | NONE =>
  (* TOKEN: '[' *)
  case parse_string_token "[" str of
    SOME (_,str') => parse_exprtokens outQ (ET_MemRegion false:: opS) (trim str')
  | NONE =>
  (* TOKEN: ')' *)
  case parse_string_token ")" str of
    SOME (_,str') => 
      let val (toOutQ,opS') = partitionUpto opS (exprtoken_eq ET_Lpar)
          val opS'' = List.tl opS'
          val (toOutQ',opS''') = if List.length opS'' > 1 andalso is_prefix_operator (List.hd opS'') then (toOutQ@[List.hd opS''],List.tl opS'') else (toOutQ,opS'')
      in
       parse_exprtokens (outQ@toOutQ') opS''' (trim str')
      end
  | NONE =>
  (* TOKEN: ']' *)
  case parse_string_token "]" str of
    SOME (_,str') => 
      let val (toOutQ,opS') = partitionUpto opS is_ET_Memregion
          val (b,str'') = case parse_string_token "_0" str' of
                            SOME (_,str'') => (true,str'') 
                          | NONE => (false,str') 
          val toOutQ' = toOutQ @ [ET_MemRegion b]
          val opS'' = List.tl opS'
      in
        parse_exprtokens (outQ@toOutQ') opS'' (trim str'')
      end
  | NONE =>
  (* TOKEN: ',' *)
  case parse_string_token "," str of
    SOME (_,str') => parse_exprtokens outQ opS (trim str')
  (* FAILURE *)
  | NONE => error_with_tracing ("Could not convert string '" ^ Substring.string str ^ "' to expr.") 
end

fun show_expr_tokens ets = Substring.concatWith " " (map show_exprtoken ets)

datatype Expr =
    E_Reg of substring * bool
  | E_Imm of int
  | E_Bot of substring
  | E_MemRegion of Expr * int * bool
  | E_App of string * Expr list

fun find str strs = List.find (fn (str') => String.compare (Substring.string (toLowerStr (Substring.full str)),str') = EQUAL) strs <> NONE

fun
  exprtokens_to_expr (ET_Reg (r,b)::ets') = (E_Reg (r,b),ets')
| exprtokens_to_expr (ET_Imm i::ets') = (E_Imm i,ets')
| exprtokens_to_expr (ET_Bot f::ets') = (E_Bot f,ets')
| exprtokens_to_expr (ET_prefix s::ets) =
   let val nargs = if find s ["b8", "b16", "b32", "not64", "not32", "not16", "not8"] then 1
                   else if find s ["shl64", "shl32", "shl16", "shl8",
                                   "shr64", "shr32", "shr16", "shr8",
                                   "sar64", "sar32", "sar16", "sar8"] then 2
                   else if find s ["signextend","overwrite", "div64", "div32", "div16", "div8",
                          "div_rem64", "div_rem32", "div_rem16", "div_rem8"] then 3
                   else error_with_tracing ("Unknown operator: " ^ s)
       val (es,ets') = exprtokens_to_exprs ets nargs
   in
     (E_App (s,List.rev es),ets')
   end
| exprtokens_to_expr (ET_binop s::ets) =
   let val (es,ets') = exprtokens_to_exprs ets 2
   in
     (E_App (s,List.rev es),ets')
   end
| exprtokens_to_expr (ET_MemRegion b::ets) =
   let val (e0,ets') = exprtokens_to_expr ets
       val (e1,ets'') = exprtokens_to_expr ets'
   in
     case e0 of
       E_Imm i => (E_MemRegion (e1,i,b),ets'')
     | _ => error_with_tracing ("Memory region with symbolic size: " ^ (Substring.string (show_exprtoken (ET_MemRegion b))))
   end
| exprtokens_to_expr (et::_) = error_with_tracing ("Unknown exprtoken:" ^ (Substring.string (show_exprtoken et)))
| exprtokens_to_expr [] = error_with_tracing ("No more exprtokens")
and
 exprtokens_to_exprs ets 1 =
   let val (e,ets') = exprtokens_to_expr ets
   in
     ([e],ets')
   end
| exprtokens_to_exprs ets n = 
   let val (e0,ets') = exprtokens_to_expr ets
       val (es,ets'') = exprtokens_to_exprs ets' (n-1)
   in
     (e0::es,ets'')
   end


fun parens str = "(" ^ str ^ ")"

fun brackets str = "[" ^ str ^ "]"


val parse_expr = fst o exprtokens_to_expr o parse_exprtokens [] []

fun show_expr (E_Reg (r,b)) = Substring.string r ^ (if b then "_0" else "")
  | show_expr (E_Imm i) = Int.toString i
  | show_expr (E_Bot f) = String.concat ["bot(", Substring.string f, ")"]
  | show_expr (E_App (s,es)) = String.concat ([s, "("] @ [String.concatWith "," (map show_expr es)] @ [")"])
  | show_expr (E_MemRegion (a,si,b))  = "[" ^ show_expr a ^ "," ^ Int.toString si ^ "]" ^ (if b then "_0" else "")

datatype Pred = 
    Equalities of (Expr * Expr) list

fun show_equality (e0,e1) = show_expr e0 ^ " = " ^ show_expr e1

fun show_predicate (Equalities eqs) = String.concatWith "; " (map show_equality eqs)

fun parse_equality str =
  let val (before_eq,after_eq)  = Substring.splitl (fn c => c <> #"=") str
      val lhs = trim before_eq
      val rhs = trim (tail after_eq)
      (*val _   = tracing (Substring.concatWith " " (map show_exprtoken (parse_exprtokens [] [] lhs)))*)
      val e0  = parse_expr lhs
      val e1  = parse_expr rhs
  in
    (e0,e1)
  end

fun parse_predicate str =
  let val strs = Substring.tokens (fn c => c = #";") (Substring.full str)
      val eqs  = map parse_equality strs
  in
    Equalities eqs
  end

fun butlast str = Substring.slice (str,0,SOME (Substring.size str-1))

fun parse_mem_region str =
  case parse_string_token "(" str of
    NONE => error_with_tracing ("Expecting (, got " ^ Substring.string str)
  | SOME (_,str0) =>
  let val str1 = butlast str0
      val (a_str,si_str) = Substring.splitr (fn c => c <> #",") str1
  in
    case LargeInt.fromString (Substring.string (trim si_str)) of
      NONE => error_with_tracing ("Expecting size after comma, got " ^ Substring.string str)      
    | SOME si =>
    let val a = parse_expr (trim (butlast a_str)) in
      (a,si)
    end
  end


fun splitAtToken str token =
  let val (pref,suff) = Substring.position token str
  in
    (pref,snd (Substring.splitAt (suff, String.size token)))
  end

fun parse_sep str =
  let val (before_X,after_X) = splitAtToken str " SEP "
      val (a0,si0) = parse_mem_region (trim before_X)
      val (a1,si1) = parse_mem_region (trim after_X)
  in
    (a0,si0,a1,si1)
  end
 

fun parse_seps str =
  if String.size str = 0 then
    []
  else let val strs = Substring.tokens (fn c => c = #";") (Substring.full str)
       in
         map (parse_sep o trim) strs
       end






fun mapI' _ f []      = []
  | mapI' i f (x::xs) = (f i x)::(mapI' (i+1) f xs)

val mapI = mapI' 0
 
fun trim_undercores str =
  let val (_,x) = Substring.splitl (fn (c) => c = #"_") (Substring.full str)
      val (y,_) = Substring.splitr (fn (c) => c = #"_") x in
    Substring.string y
  end;



fun safe_var_name f =
       if String.isPrefix "+" f then "_plus_"
  else if String.isPrefix "-" f then "_minus_"
  else if String.isPrefix "*" f then "_times_"
  else if String.isPrefix "&" f then "_and_"
  else if String.isPrefix "|" f then "_or_"
  else if String.isPrefix "xor" f then "_xor_"
  else f

fun expr_to_isa_var (E_Reg (r,true)) = Substring.string r ^ "\<^sub>0"
 | expr_to_isa_var (E_Imm i) = Int.toString i
 | expr_to_isa_var (E_MemRegion (a,si,true)) = "_M\<^sub>0_" ^ expr_to_isa_var a ^ "_" ^ Int.toString si ^ "_"
 | expr_to_isa_var (E_App (f,es)) = safe_var_name f ^ "_" ^ String.concatWith "_" (map expr_to_isa_var es)
 | expr_to_isa_var _ = error_with_tracing "expr_to_isa_var" 

fun mk_fun "+64" = ("(+)",64)
  | mk_fun "+32" = ("(+)",32)
  | mk_fun "+16" = ("(+)",16)
  | mk_fun "+8"  = ("(+)",8)
  | mk_fun "-64" = ("(-)",64)
  | mk_fun "-32" = ("(-)",32)
  | mk_fun "-16" = ("(-)",16)
  | mk_fun "-8"  = ("(-)",8)
  | mk_fun "*64" = ("(*)",64)
  | mk_fun "*32" = ("(*)",32)
  | mk_fun "*16" = ("(*)",16)
  | mk_fun "*8"  = ("(*)",8)
  | mk_fun "udiv64" = ("(div)",64)
  | mk_fun "udiv32" = ("(div)",32)
  | mk_fun "udiv16" = ("(div)",16)
  | mk_fun "udiv8"  = ("(div)",8)
  | mk_fun "sdiv64" = ("(sdiv)",64)
  | mk_fun "sdiv32" = ("(sdiv)",32)
  | mk_fun "sdiv16" = ("(sdiv)",16)
  | mk_fun "sdiv8"  = ("(sdiv)",8)
  | mk_fun "&64" = ("(AND)",64)
  | mk_fun "&32" = ("(AND)",32)
  | mk_fun "&16" = ("(AND)",66)
  | mk_fun "&8"  = ("(AND)",8)
  | mk_fun "|64" = ("(OR)",64)
  | mk_fun "|32" = ("(OR)",32)
  | mk_fun "|16" = ("(OR)",16)
  | mk_fun "|8"  = ("(OR)",8)
  | mk_fun "xor64" = ("(XOR)",64)
  | mk_fun "xor32" = ("(XOR)",32)
  | mk_fun "xor16" = ("(XOR)",16)
  | mk_fun "xor8"  = ("(XOR)",8)
  | mk_fun "div64"  = ("div64",64)
  | mk_fun "div32"  = ("div32",32)
  | mk_fun "div16"  = ("div16",16)
  | mk_fun "div8"   = ("div8",8)
  | mk_fun "div_rem64"  = ("div_rem64",64)
  | mk_fun "div_rem32"  = ("div_rem32",32)
  | mk_fun "div_rem16"  = ("div_rem16",6)
  | mk_fun "div_rem8"   = ("div_rem8",8)
  | mk_fun "shl64" = ("(<<\<^sub>w)",64)
  | mk_fun "shl32" = ("(<<\<^sub>w)",32)
  | mk_fun "shl16" = ("(<<\<^sub>w)",16)
  | mk_fun "shl8"  = ("(<<\<^sub>w)",8)
  | mk_fun "shr64" = ("(>>\<^sub>w)",64)
  | mk_fun "shr32" = ("(>>\<^sub>w)",32)
  | mk_fun "shr16" = ("(>>\<^sub>w)",16)
  | mk_fun "shr8"  = ("(>>\<^sub>w)",8)
  | mk_fun "sar64" = ("(>>>\<^sub>w)",64)
  | mk_fun "sar32" = ("(>>>\<^sub>w)",32)
  | mk_fun "sar16" = ("(>>>\<^sub>w)",16)
  | mk_fun "sar8"  = ("(>>>\<^sub>w)",8)
  | mk_fun "not64" = ("NOT",64)
  | mk_fun "not32" = ("NOT",32)
  | mk_fun "not16" = ("NOT",16)
  | mk_fun "not8"  = ("NOT",8)
  | mk_fun f = error_with_tracing ("Do not know Isabelle equivalent of: " ^ f)

fun expr_size_option (E_Reg (r,_)) = SOME (reg_size r)
 | expr_size_option  (E_MemRegion (_,si,_)) = SOME (si*8)
 | expr_size_option (E_App ("b8",[_])) = SOME 8
 | expr_size_option (E_App ("b16",[_])) = SOME 16
 | expr_size_option (E_App ("b32",[_])) = SOME 32
 | expr_size_option (E_App ("signextend",[E_Imm l,E_Imm h,e])) = SOME h
 | expr_size_option (E_App ("overwrite",[_,e1,e2])) = SOME 64 (* TODO size of operand sould be supplied *)
 | expr_size_option (E_App (f,es)) = SOME (snd (mk_fun f))
 | expr_size_option (E_Bot _) = SOME 64  (* return value in RAX *)
 | expr_size_option (E_Imm _) = NONE

fun expr_size e =
  case expr_size_option e of
    SOME si => si
  | NONE    => error_with_tracing ("expr_size" ^ show_expr e)



fun expr_to_isa_var_with_size e =
  let val isa_str = trim_undercores (expr_to_isa_var e)
      val si = expr_size e
  in
    "(" ^ isa_str ^ ":: " ^ Int.toString si ^ " word)"
  end

fun ucast str si = parens ("ucast " ^ parens str ^ " :: " ^ Int.toString si ^ " word")
fun scast str si = parens ("scast " ^ parens str ^ " :: " ^ Int.toString si ^ " word")



fun expr_to_isa_expr_str_with_size state_str si (E_Reg (r,true)) = ucast (expr_to_isa_var_with_size (E_Reg (r,true))) si
  | expr_to_isa_expr_str_with_size state_str si (E_Reg (r,false)) = ucast (ucast ("reg_read " ^ state_str ^ " ''" ^ (Substring.string (toLowerStr r)) ^ "''") (reg_size r)) si
  | expr_to_isa_expr_str_with_size state_str si (E_Imm i) = parens (Int.toString i ^ "::" ^ Int.toString si ^ "word")
  | expr_to_isa_expr_str_with_size state_str si (E_Bot f) = ucast ("bot_" ^ Substring.string f ^ "::64word") si (* return value in RAX *)
  | expr_to_isa_expr_str_with_size state_str si (E_MemRegion (a0,si0,true)) = ucast (expr_to_isa_var_with_size (E_MemRegion (a0,si0,true))) si
  | expr_to_isa_expr_str_with_size state_str si (E_MemRegion (a0,si0,false)) = ucast (ucast ("mem_read " ^ state_str ^ expr_to_isa_expr_str_with_size state_str 64 a0 ^ " "  ^ Int.toString si0) (si0*8)) si
  | expr_to_isa_expr_str_with_size state_str si (E_App ("b8" ,[e])) = ucast (ucast (expr_to_isa_expr_str state_str e) 8)  si
  | expr_to_isa_expr_str_with_size state_str si (E_App ("b16",[e])) = ucast (ucast (expr_to_isa_expr_str state_str e) 16) si
  | expr_to_isa_expr_str_with_size state_str si (E_App ("b32",[e])) = ucast (ucast (expr_to_isa_expr_str state_str e) 32) si
  | expr_to_isa_expr_str_with_size state_str si (E_App ("signextend",[E_Imm l,E_Imm h,e])) = ucast (scast (expr_to_isa_expr_str_with_size state_str l e) h) si
  | expr_to_isa_expr_str_with_size state_str si (E_App ("overwrite",[E_Imm i,e1,e2])) = parens ("overwrite " ^ String.concatWith " " ["0",Int.toString i,expr_to_isa_expr_str_with_size state_str si e1,expr_to_isa_expr_str_with_size state_str si e2 ])
  | expr_to_isa_expr_str_with_size state_str si (E_App (f,es)) = funapp_with_size f state_str si es
and
    expr_to_isa_expr_str state_str e = expr_to_isa_expr_str_with_size state_str (expr_size e) e
and
    funapp_with_size f state_str si es = 
      let val (fun_str,si') = mk_fun f
          val cast = if si <> si' then (fn str => ucast str si) else parens
      in   
        cast (fun_str ^ " " ^ String.concatWith " " (map (expr_to_isa_expr_str_with_size state_str si') es))
      end
and
    expr_to_isa_lhs_str state_str (E_Reg (r,false)) = "regs " ^ state_str ^ " ''" ^ (Substring.string (toLowerStr r)) ^ "''"
  | expr_to_isa_lhs_str state_str (E_MemRegion (a,si,false)) = parens ("mem_read " ^ state_str ^ " "  ^ expr_to_isa_expr_str_with_size state_str 64 a ^ Int.toString si)
  | expr_to_isa_lhs_str _         e = error_with_tracing ("LHS = " ^ show_expr e)

fun is_bot (E_Bot _) = true
| is_bot _ = false

fun equality_str_to_isa_equality_str state_str str = 
  let (*val _ = tracing (Substring.string str)*)
      val (lhs,rhs) = parse_equality str
      (*val _ = tracing (show_expr lhs ^ " == " ^ show_expr rhs)*)
      (*val si = expr_size lhs*)
      val rhs_str = 
        case rhs of 
            E_Imm i => Int.toString i
          | _       => ucast (expr_to_isa_expr_str state_str rhs) 256
      val isa_str = expr_to_isa_lhs_str state_str lhs ^ " = " ^ rhs_str
      (*val _ = tracing isa_str*)
  in
    isa_str
  end


fun str_to_isa_str str =
  "''" ^ Substring.string (toLowerStr (Substring.full str)) ^ "''"

fun num_to_isa_str num = if num >= 0 then Int.toString num else "(0 - " ^ (Int.toString (0 - num)) ^ ")" 

fun operand_to_isa_str (Mem (si,segment,offset,base, index, scale)) = "Mem " ^ Int.toString si ^ " " ^ num_to_isa_str offset ^ " " ^  str_to_isa_str base ^ " " ^ str_to_isa_str index ^ " " ^ Int.toString scale ^ ""
| operand_to_isa_str (Reg r) = "Reg " ^ str_to_isa_str r
| operand_to_isa_str (Imm i) = "Imm " ^ Int.toString i

fun maybe_operand_to_isa_str NONE = "None"
  | maybe_operand_to_isa_str (SOME op') = "(Some (" ^ operand_to_isa_str op' ^ "))"

fun instr_to_isa_str (Instr (a,si,m,op1,op2,op3)) =
  "Instr " ^ str_to_isa_str m ^ " "  
  ^ String.concatWith " " (map maybe_operand_to_isa_str [op1,op2,op3]) ^ " "
  ^ LargeInt.toString (a+si)
| instr_to_isa_str (ExternalCall (a,f,si)) = "ExternalCall " ^ f ^ " " ^ Int.toString (a+si)
| instr_to_isa_str (ExternalCallWithReturn (a,f,si)) = "ExternalCallWithReturn " ^ f ^ " " ^ Int.toString (a+si)


fun sep_to_isa_str (a0,si0,a1,si1) =
  "separate " ^
  String.concatWith " " 
    [parens (expr_to_isa_expr_str_with_size "\<sigma>" 64 a0), Int.toString si0, 
     parens (expr_to_isa_expr_str_with_size "\<sigma>" 64 a1), Int.toString si1
    ]

fun seps_to_isa_str seps = 
  "SEPARATIONS {" ^ String.concatWith "," (map sep_to_isa_str seps) ^ "}"




fun parse_fcs NONE = NONE
  | parse_fcs (SOME (_,str)) =
  if Substring.isPrefix "PRESERVES " (trim (Substring.full str)) then
    let val str0 = trim (Substring.slice (Substring.full str,10,NONE))
        val (f,str1) = Substring.splitl (fn c => c <> #"{") str0
    in
      if Substring.isPrefix "{" (trim str1) then
        let val strs = Substring.tokens (fn c => c = #";") (tail (butlast (trim str1)))
        in
          SOME (Substring.string (trim f),map (parse_expr o trim) strs)
        end
      else
        error_with_tracing ("Cannot parse function constraint: " ^  str)
    end
  else
    error_with_tracing ("Cannot parse function constraint: " ^  str)



fun fc_to_isa_str f e = 
  let val si = expr_size e
      val isa_str = expr_to_isa_lhs_str (parens (f ^ " \<sigma>")) e ^ " = " ^ ucast (expr_to_isa_expr_str_with_size "\<sigma>" si e) 256
  in
    isa_str
  end

fun fcs_to_isa_str NONE = NONE
| fcs_to_isa_str (SOME (f,es)) = 
    SOME (String.concatWith " \<and> " (
          (expr_to_isa_lhs_str (parens (f ^ " \<sigma>")) (E_Reg (Substring.full "rax",false)) ^ " = ucast(bot_" ^ f ^ "::64word)")
            ::(map (fc_to_isa_str f) es)))


fun mk_assumes seps_isa_strs asserts_isa_str fcs_isa_str str =
  let val strs = Substring.tokens (fn c => c = #";") (Substring.full str)
      val isa_str_assms = String.concatWith " \<and> " (map (equality_str_to_isa_equality_str "\<sigma>") strs)
      val _ = tracing ("assms = " ^ isa_str_assms)
      val fcs_assms = case fcs_isa_str of
                        NONE => ""
                      | SOME str => " \<and> " ^ str

      val seps_isa_str =  if length seps_isa_strs = 0 then "True" else String.concatWith " \<and> " seps_isa_strs
      val asserts_isa_str =  if length asserts_isa_str = 0 then "True" else String.concatWith " \<and> " asserts_isa_str
      (*val _ = tracing seps_isa_str*)
  in
    Element.Assumes [((Binding.name "assms", []), [(isa_str_assms ^ fcs_assms,[])]),
                     (*((Binding.name "instr", []), [(isa_str_instr,[])]),*)
                     ((Binding.name "seps", []), [(seps_isa_str,[])]),
                     ((Binding.name "asserts", []), [(asserts_isa_str,[])])
                    ]
  end

fun mk_concl thm_name isa_str_instruction str =
  let val strs = Substring.tokens (fn c => c = #";") (Substring.full str)
      val isa_str_concl = "let \<sigma>' = step (" ^ isa_str_instruction ^ ") \<sigma> in " ^ (String.concatWith " \<and> " (map (equality_str_to_isa_equality_str "\<sigma>'") strs))
      val _ = tracing ("Conclusion:" ^ isa_str_concl)
  in
    Element.Shows [((Binding.name thm_name, []), [(isa_str_concl,[])])]
  end


(*
  Add the command "
htriple" to the Isabelle syntax.
*)
val _ =
  Outer_Syntax.local_theory_to_proof' \<^command_keyword>\<open>htriple\<close> 
    "Hoare Triple Lemma"                              
   (Parse.text --
    Parse.$$$ "Separations" -- Parse.text -- 
    Parse.$$$ "Assertions" -- Parse.text -- 
    Parse.$$$ "Pre" -- Parse.text -- 
    Parse.$$$ "Instruction" -- Parse.text -- 
    Parse.$$$ "Post" -- Parse.text --
    (Scan.option (Parse.$$$ "FunctionConstraints" -- Parse.text))  >> 
      (fn (((((((((((thm_name,_),seps),_),asserts),_),pre),_),instr),_),post),fcs) => 
           let val instr_isa_str = instr_to_isa_str (parse_instr (Substring.full instr))
               val _ = tracing instr_isa_str

               val parsed_seps = parse_seps seps
               val _ = tracing ("#seps = " ^ Int.toString (length parsed_seps))
               val seps_strs = map sep_to_isa_str parsed_seps

               val parsed_asserts = parse_seps asserts
               val _ = tracing ("#asserts = " ^ Int.toString (length parsed_seps))
               val asserts_strs = map sep_to_isa_str parsed_asserts

               val fcs_isa_str = fcs_to_isa_str (parse_fcs fcs)
               val assms = mk_assumes seps_strs asserts_strs fcs_isa_str pre
               val concl = mk_concl thm_name instr_isa_str post
               val _ = tracing (pp_option fcs_isa_str)
           in
              Specification.theorem_cmd false "htriple" NONE (K I) Binding.empty_atts [] [Element.Fixes [], assms] concl
          end
      )
   )

fun pp_term ctxt t = Pretty.string_of (Syntax.pretty_term ctxt t)
fun pp_typ  ctxt t = Pretty.string_of (Syntax.pretty_typ ctxt t)

fun parse_term ctxt t =
  case t of
    Const (str,_)    => "Const " ^ str
  | Free  (str,typ)  => "Free(" ^ str ^ ", " ^ pp_typ @{context} typ ^ ")"
  | Var   _          => "Var " ^ pp_term ctxt t
  | Bound _          => "Bound " ^ pp_term ctxt t
  | Abs (str,typ,t') => "Abs (" ^ str ^ "," ^ pp_typ ctxt typ ^ ", (" ^ parse_term ctxt t' ^ "))" 
  | f $ x            => parse_term ctxt f ^ "$\n (" ^ parse_term ctxt x ^ ")"

fun parse_typ ctxt t =
  case t of
    Type(str,typs) => "Type(" ^ str ^ String.concat (map (parse_typ ctxt) typs) ^ ")"
  | _ => raise Fail ("Unknown type: " ^ pp_typ @{context} t)

fun count_conjuncts ctxt t = 
  case t of
    Const (str,_)    => 1 
  | Free  (str,typ)  => 1 
  | Var   _          => 1 
  | Bound _          => 1
  | Abs (str,typ,t') => 1
  | (Const ("HOL.Trueprop", _) $ t) => count_conjuncts ctxt t
  | (Const ("HOL.conj", _) $ t0 $ t1) => count_conjuncts ctxt t0 + count_conjuncts ctxt t1
  | f $ x            => 1

(* Const HOL.Trueprop$
 (Const HOL.conj$
 (Free(a, bool))$
 (Free(b, bool)))*)


(*
fun pp_term ctxt t = Pretty.string_of (Syntax.pretty_term ctxt t)
fun pp_typ  ctxt t = Pretty.string_of (Syntax.pretty_typ ctxt t)


Local_Theory.assert
Local_Theory.local_theory
Local_Theory.target_of
Proof.theorem
Proof_Context.restore_naming
Local_Theory.target_of lthy
Proof_Context.get_thm ctxt thm_name
Thm.concl_of thm 
@{context}
Proof_Context.get_thm ctxt "temp1"
Method.read
  fun mk_assume thm_name term =  ((Binding.name thm_name, []), [term]);
*)
