{
  open Parser
  open M6502
  
  let line_num = ref 1
  
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
       "txs",		Txs];
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
  | insn as insn	{ INSN (Hashtbl.find tokens insn) }
  | ".byte"		{ DATA 1 }
  | ".word"		{ DATA 2 }
  | ".3byte"		{ DATA 3 }
  | ".dword"		{ DATA 4 }
  | ".asc"		{ ASCII }
  | ".macro"		{ MACRO }
  | ".mend"		{ MEND }
  | ".scope"		{ SCOPE }
  | ".scend"		{ SCEND }
  | ".org"		{ ORIGIN }
  | ","			{ COMMA }
  | "#"			{ HASH }
  | ":"			{ COLON }
  | "+"			{ PLUS }
  | "-"			{ MINUS }
  | "<"			{ LANGLE }
  | ">"			{ RANGLE }
  | "("			{ LBRACKET }
  | ")"			{ RBRACKET }
  | "x"			{ X }
  | "y"			{ Y }
  | "a"			{ A }
  | '\"' (('\\' _ | nonquote_char)* as s) '\"'
			{ STRING (dequote s) }
  | label as lab	{ LABEL (lab) }
  | "%" label as mac	{ MACROARG (String.sub mac 1 (String.length mac - 1)) }
  | "@" label as mac	{ EXPMACRO (String.sub mac 1 (String.length mac - 1)) }
  | "\n"
  | ";" [^'\n']* "\n"	{ incr line_num; EOL }
  | (" "|"\t")+		{ token lexbuf }
  | eof			{ EOF }
