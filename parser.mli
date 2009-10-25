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
  | RBRACKET
  | RANGLE
  | PLUS
  | PERCENT
  | ORIGIN
  | NUM of (int32)
  | NOTEMPS
  | MINUS
  | MEND
  | MACRO
  | LSQUARE
  | LBRACKET
  | LANGLE
  | LABEL of (string)
  | INTERF
  | INSN of (M6502.opcode)
  | HASH
  | EXPMACRO of (string)
  | EOL
  | EOF
  | DSB
  | DOT
  | DIVIDE
  | DATA of (int)
  | CTXEND
  | CONTEXT
  | COMMA
  | COLON
  | ASCII
  | ALIAS
  | A


val insn_seq: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Insn.insn list)