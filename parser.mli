exception Error

type token = 
  | Y
  | X
  | TIMES
  | STRING of (string)
  | SCOPE
  | SCEND
  | RSQUARE
  | RBRACKET
  | RANGLE
  | PLUS
  | ORIGIN
  | NUM of (int32)
  | MINUS
  | MEND
  | MACROARG of (string)
  | MACRO
  | LSQUARE
  | LBRACKET
  | LANGLE
  | LABEL of (string)
  | INSN of (M6502.opcode)
  | HASH
  | EXPMACRO of (string)
  | EOL
  | EOF
  | DIVIDE
  | DATA of (int)
  | COMMA
  | COLON
  | ASCII
  | ALIAS
  | A


val insn_seq: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Insn.insn list)