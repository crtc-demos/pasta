exception Error

type token = 
  | Y
  | X
  | SCOPE
  | SCEND
  | RBRACKET
  | RANGLE
  | PLUS
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
  | A


val insn_seq: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Insn.insn list)