exception Error

type token = 
  | Y
  | X
  | STRING of (string)
  | SCOPE
  | SCEND
  | RBRACKET
  | RANGLE
  | PLUS
  | ORIGIN
  | NUM of (int32)
  | MINUS
  | MEND
  | MACROARG of (string)
  | MACRO
  | LBRACKET
  | LANGLE
  | LABEL of (string)
  | INSN of (M6502.opcode)
  | HASH
  | EXPMACRO of (string)
  | EOL
  | EOF
  | DATA of (int)
  | COMMA
  | COLON
  | ASCII
  | A


val insn_seq: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Insn.insn list)