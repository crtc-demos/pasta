{
  open Parser
  open M6502
    
  exception BadQuoteChar
  
  let dequote s =
    let b = Buffer.create (String.length s)
    and saw_backslash = ref false in
    for i = 0 to String.length s - 1 do
      if s.[i] == '\\' then
        saw_backslash := true
      else if !saw_backslash then begin
        saw_backslash := false;
        match s.[i] with
          '\"' -> Buffer.add_char b '\"'
	| '\'' -> Buffer.add_char b '\''
	| '\\' -> Buffer.add_char b '\\'
	| 'n' -> Buffer.add_char b '\n'
	| 'r' -> Buffer.add_char b '\r'
	| 't' -> Buffer.add_char b '\t'
	| _ -> raise BadQuoteChar
	end
      else
        Buffer.add_char b s.[i]
    done;
    Buffer.contents b
    
  let tokens =
    let tokens' = Hashtbl.create 10 in
    List.iter
      (fun (str,tok) -> Hashtbl.add tokens' str tok)
      ["adc",		Adc;
       "and",		And;
       "asl",		Asl;
       "bcc",		Bcc;
       "bcs",		Bcs;
       "beq",		Beq;
       "bmi",		Bmi;
       "bne",		Bne;
       "bpl",		Bpl;
       "bvc",		Bvc;
       "bvs",		Bvs;
       "bit",		Bit;
       "brk",		Brk;
       "clc",		Clc;
       "cld",		Cld;
       "cli",		Cli;
       "clv",		Clv;
       "cmp",		Cmp;
       "cpx",		Cpx;
       "cpy",		Cpy;
       "dec",		Dec;
       "dex",		Dex;
       "dey",		Dey;
       "eor",		Eor;
       "inc",		Inc;
       "inx",		Inx;
       "iny",		Iny;
       "jmp",		Jmp;
       "jsr",		Jsr;
       "lda",		Lda;
       "ldx",		Ldx;
       "ldy",		Ldy;
       "lsr",		Lsr;
       "nop",		Nop;
       "ora",		Ora;
       "pha",		Pha;
       "php",		Php;
       "pla",		Pla;
       "plp",		Plp;
       "rol",		Rol;
       "ror",		Ror;
       "rti",		Rti;
       "rts",		Rts;
       "sbc",		Sbc;
       "sec",		Sec;
       "sed",		Sed;
       "sei",		Sei;
       "sta",		Sta;
       "stx",		Stx;
       "sty",		Sty;
       "tax",		Tax;
       "tay",		Tay;
       "txa",		Txa;
       "tya",		Tya;
       "tsx",		Tsx;
       "txs",		Txs;
       (* 65C02 *)
       "bra",		Bra;
       "phx",		Phx;
       "phy",		Phy;
       "plx",		Plx;
       "ply",		Ply;
       "stz",		Stz;
       "trb",		Trb;
       "tsb",		Tsb];
    tokens'
}

let num = ['0'-'9']+
let hexdigit = ['0'-'9'] | ['a'-'f'] | ['A'-'F']
let bindigit = '0' | '1'
let asc = ['a'-'z'] | ['A'-'Z'] | "_" | "$"
let label = asc (asc | num)*
let nonquote_char = [^'\"' '\\']
let insn = "adc" | "and" | "asl" | "bcc" | "bcs" | "beq" | "bit" | "bmi"
	 | "bne" | "bpl" | "brk" | "bvc" | "bvs" | "clc" | "cld" | "cli"
	 | "clv" | "cmp" | "cpx" | "cpy" | "dec" | "dex" | "dey" | "eor"
	 | "inc" | "inx" | "iny" | "jmp" | "jsr" | "lda" | "ldx" | "ldy"
	 | "lsr" | "nop" | "ora" | "pha" | "php" | "pla" | "plp" | "rol"
	 | "ror" | "rti" | "rts" | "sbc" | "sec" | "sed" | "sei" | "sta"
	 | "stx" | "sty" | "tax" | "tay" | "txa" | "tsx" | "txs" | "tya"
	 | "bra" | "phx" | "phy" | "plx" | "ply" | "stz" | "trb" | "tsb"

rule line = parse
    (([^'\n']*) '\n') as line	{ Some line }
  | eof				{ None }

and token = parse
    "0x" (hexdigit+ as hexnum)
  			{ NUM (Int32.of_string ("0x" ^ hexnum)) }
  | "0b" (bindigit+ as binary)
			{ NUM (Int32.of_string ("0b" ^ binary)) }
  | "$" (hexdigit+ as hexnum)
			{ NUM (Int32.of_string ("0x" ^ hexnum)) }
  | num as num		{ NUM (Int32.of_string num) }
  | "'" (([^'\''] | '\\' ['n' 'r' 't' '\\' '\'' '\"']) as ch) "'"
  			{ NUM (Int32.of_int (Char.code ((dequote ch).[0]))) }
  | insn as insn	{ INSN (Hashtbl.find tokens insn) }
  | ".byte"		{ DATA 1 }
  | ".word"		{ DATA 2 }
  | ".3byte"		{ DATA 3 }
  | ".dword"		{ DATA 4 }
  | ".asc"		{ ASCII }
  | ".dsb"		{ DSB }
  | ".macro"		{ MACRO }
  | ".mend"		{ MEND }
  | ".scope"	
  | ".("		{ SCOPE }
  | ".scend"	
  | ".)"		{ SCEND }
  | ".org"		{ ORIGIN }
  | ".alias"		{ ALIAS }
  | ".var" | ".var1"	{ VAR 1 }
  | ".var2"		{ VAR 2 }
  | ".var3"		{ VAR 3 }
  | ".var4"		{ VAR 4 }
  | ".temps"		{ TEMPS }
  | ".notemps"		{ NOTEMPS }
  | ".interf"		{ INTERF }
  | ".protect"		{ PROTECT }
  | ".context"		{ CONTEXT }
  | ".ctxend"		{ CTXEND }
  | ".include"		{ INCLUDE }
  | ".."		{ UPTO }
  | "."			{ DOT }
  | ","			{ COMMA }
  | "#"			{ HASH }
  | "+"			{ PLUS }
  | "-"			{ MINUS }
  | "*"			{ TIMES }
  | "/"			{ DIVIDE }
  | "&"			{ AND }
  | "|"			{ OR }
  | "^"			{ EOR }
  | "~"			{ NOT }
  | "<<"		{ LSHIFT }
  | ">>"		{ RSHIFT }
  | ">>>"		{ ARSHIFT }
  | "<"			{ LANGLE }
  | ">"			{ RANGLE }
  | "("			{ LBRACKET }
  | ")"			{ RBRACKET }
  | "["			{ LSQUARE }
  | "]"			{ RSQUARE }
  | "{"			{ LBRACE }
  | "}"			{ RBRACE }
  | "x"			{ X }
  | "y"			{ Y }
  | "a"			{ A }
  | '\"' (('\\' _ | nonquote_char)* as s) '\"'
			{ STRING (dequote s) }
  | label as lab	{ LABEL (lab) }
  | '%' 		{ PERCENT }
  | "@" (label as mac)	{ EXPMACRO mac }
  | "\n"
  | ";" [^'\n']* "\n"	{ incr Line.line_num;
			  EOL (Insn.SourceLine (!Line.current_file,
						pred !Line.line_num)) }
  | ":"			{ EOL (Insn.SourceLine (!Line.current_file,
						!Line.line_num)) }
  | (" "|"\t")+		{ token lexbuf }
  | eof			{ EOF }
