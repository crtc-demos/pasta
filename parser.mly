%{

open Insn
open M6502

%}

%token COMMA EOL EOF HASH COLON
%token LBRACKET RBRACKET LSQUARE RSQUARE
%token PLUS MINUS TIMES DIVIDE LANGLE RANGLE PERCENT DOT
%token X Y A
%token MACRO MEND
%token SCOPE SCEND CONTEXT CTXEND
%token ORIGIN ASCII ALIAS DSB TEMPS NOTEMPS UPTO
%token <M6502.opcode> INSN
%token <string> LABEL EXPMACRO STRING
%token <int> DATA VAR
%token <int32> NUM

%nonassoc LANGLE RANGLE
%left PLUS MINUS
%left TIMES DIVIDE

%type <Expr.const_expr> num

%start <Insn.insn list> insn_seq

%%

insn_seq: EOF				{ [] }
	| i = insn EOL is = insn_seq	{ i :: is }
	| EOL is = insn_seq		{ is }
;

maybe_eol: /* nothing */
	 | EOL				{ }
;

insn: i = alu_op
    | i = label_directive
    | i = data_directive
    | i = ascii_directive
    | i = dsb_directive
    | i = alias_directive
    | i = origin_directive
    | i = var_directive
    | i = temps_directive
    | i = notemps_directive
    | i = expand_macro
    | i = macro
    | i = scope
    | i = context			{ i }
;

macro: MACRO l = LABEL args = list(arg) EOL m = macro_seq MEND
					{ Macrodef (l, args, m) }
;

macro_seq: ml = list(minsn)		{ ml }
;

(* a, x, y are too useful to be forbidden as arguments...  *)
arg: l = LABEL				{ l }
   | A					{ "a" }
   | X					{ "x" }
   | Y					{ "y" }
;

minsn: i = alu_op EOL
     | i = label_directive maybe_eol
     | i = expand_macro EOL		{ i }
;

scope: SCOPE EOL is = insns_in_scope SCEND
					{ Scope (Env.new_env (), is) }
;

insns_in_scope: /* nothing */		{ [] }
	      | i = insn EOL is = insns_in_scope
	      				{ i :: is }
	      | EOL is = insns_in_scope	{ is }
;

context: CONTEXT l = LABEL EOL is = insns_in_scope CTXEND
					{ Context (Env.new_env (), l, is) }
;

alu_op: op = INSN a = am_immediate
      | op = INSN a = am_num
      | op = INSN a = am_num_x
      | op = INSN a = am_num_y
      | op = INSN a = am_indirect
      | op = INSN a = am_x_indirect
      | op = INSN a = am_indirect_y
      | op = INSN a = am_accumulator
      | op = INSN a = am_implied	{ Raw_insn (op, a) }
;

am_immediate: HASH n = num		{ Raw_immediate n }
;

am_num: n = num				{ Raw_num n }
;

am_num_x: n = num COMMA X		{ Raw_num_x n }
;

am_num_y: n = num COMMA Y		{ Raw_num_y n }
;

am_indirect: LBRACKET n = num RBRACKET	{ Raw_indirect n }
;

am_x_indirect: LBRACKET n = num COMMA X RBRACKET
					{ Raw_x_indirect n }
;

am_indirect_y: LBRACKET n = num RBRACKET COMMA Y
					{ Raw_indirect_y n }
;

am_accumulator: A			{ Raw_accumulator }
;

am_implied:				{ Raw_implied }
;

label_directive: lab = LABEL COLON	{ Label lab }
;

data_directive: sz = DATA n = separated_list(COMMA, num)
					{ Data (sz, n) }
;

ascii_directive: ASCII n = separated_list(COMMA, ascitem)
					{ Ascii n }
;

ascitem: n = num			{ AscChar n }
       | s = STRING			{ AscString s }
;

dsb_directive: DSB c = num COMMA bval = num
					{ DataBlock (c, bval) }
;

alias_directive: ALIAS l = LABEL n = num
					{ Alias (l, n) }
;

origin_directive: ORIGIN n = num	{ Origin n }
;

var_directive: sz = VAR vl = separated_list(COMMA, LABEL)
					{ DeclVars (sz, vl) }
;

temps_directive: TEMPS tl = separated_list(COMMA, tempspec)
					{ Temps tl }
;

tempspec: t = num			{ OneTemp t }
	| st = num UPTO en = num	{ TempRange (st, en) }
;

notemps_directive: NOTEMPS ll = separated_list(COMMA, LABEL)
					{ NoTemps ll }
;

expand_macro: m = EXPMACRO al = separated_list(COMMA, param)
					{ Expmacro (m, al) }
;

param: n = num				{ n }
;

num: n = NUM				{ Expr.Int n }
   | a = num PLUS b = num		{ Expr.Plus (a, b) }
   | a = num MINUS b = num		{ Expr.Minus (a, b) }
   | a = num TIMES b = num		{ Expr.Times (a, b) }
   | a = num DIVIDE b = num		{ Expr.Divide (a, b) }
   | MINUS a = num			{ Expr.Uminus a }
   | LANGLE a = num			{ Expr.LoByte a }
   | RANGLE a = num			{ Expr.HiByte a }
   | lab = LABEL			{ Expr.ExLabel lab }
   | v = var_ref			{ v }
   | LSQUARE n = num RSQUARE		{ n }
;

%inline var_ref: PERCENT vl = separated_list(DOT, LABEL)
					{ Expr.VarRef vl }
;

%%

