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
  | RANGLE
  | PROTECT
  | PLUS
  | PERCENT
  | ORIGIN
  | OR
  | NUM of (int32)
  | NOTEMPS
  | NOT
  | MINUS
  | MEND
  | MACRO
  | LSQUARE
  | LSHIFT
  | LBRACKET
  | LANGLE
  | LABEL of (string)
  | INTERF
  | INSN of (M6502.opcode)
  | HASH
  | EXPMACRO of (string)
  | EOR
  | EOL of (Insn.srcloc)
  | EOF
  | DSB
  | DOT
  | DIVIDE
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