exception Error

type token = 
  | Y
  | X
  | VAR of (int)
  | UPTO
  | TIMES
  | TEMPS
  | STRING of (string)
  | SCOPE
  | SCEND
  | RSQUARE
  | RSHIFT
  | RBRACKET
  | RBRACE
  | RANGLE
  | PROTECT
  | PLUS
  | PERCENT
  | ORIGIN
  | OR
  | NUM of (int32)
  | NOTEMPS
  | NOT
  | NEQUAL
  | MINUS
  | MEND
  | MACRO
  | LTU
  | LSQUARE
  | LSHIFT
  | LEU
  | LE
  | LBRACKET
  | LBRACE
  | LANGLE
  | LABEL of (string)
  | INTERF
  | INSN of (M6502.opcode)
  | INCLUDE
  | IFNDEF
  | IFDEF
  | IF
  | HASH
  | GTU
  | GEU
  | GE
  | EXPMACRO of (string)
  | EQUAL
  | EOR
  | EOL of (Insn.srcloc)
  | EOF
  | ENDIF
  | ELSE
  | ELIF
  | DSB
  | DOT
  | DIVIDE
  | DEFINED
  | DATA of (int)
  | CTXEND
  | CONTEXT
  | COMMA
  | ASCII
  | ARSHIFT
  | AND
  | ALIAS
  | A


val insn_seq: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Insn.insn list)