open Insn
open M6502

exception UnknownInstruction

let single_arg = function
    [| x |] -> x
  | _ -> failwith "Expected single argument"

let rec output_data fh size data =
  match size with
    0 -> ()
  | n ->
      output_byte fh ((Int32.to_int data) land 255);
      output_data fh (pred n) (Int32.shift_right_logical data 8)

let output_addrmode fh env vpc addrmode args =
  match addrmode with
    Accumulator | Implied -> ()
  | Absolute | Absolute_X | Absolute_Y ->
      output_data fh 2 (single_arg args)
  | Immediate | Indirect | X_Indirect | Indirect_Y | Zeropage | Zeropage_X
  | Zeropage_Y ->
      output_data fh 1 (single_arg args)
  | Relative ->
      let target = single_arg args in
      let offset = Int32.sub target (Int32.of_int vpc) in
      let offset' = Int32.sub offset 2l in
      output_data fh 1 offset'
  | Synth_lbra -> failwith "Synthetic long branches not implemented"

let rec emit_insns fh start_vpc env insns =
  List.fold_right
    (fun insn vpc ->
      match insn with
        Insn (op, addrmode, args) ->
	  let opcode = M6502.insn_opcode op addrmode in
	  output_byte fh opcode;
          output_addrmode fh env vpc addrmode args;
	  vpc + Layout.insn_size env insn
      | Data (size, cexplist) ->
          List.iter
	    (fun cexp ->
	      let d32 = Expr.eval ~env cexp in
	      output_data fh size d32)
	    cexplist;
	  vpc + Layout.insn_size env insn
      | Ascii al ->
	  List.iter
	    (function
	        AscChar c ->
		  let d32 = Expr.eval ~env c in
		  output_data fh 1 d32
	      | AscString s ->
		  for i = 0 to String.length s - 1 do
		    let code = Char.code s.[i] in
		    output_data fh 1 (Int32.of_int code)
		  done)
	    al;
	  vpc + Layout.insn_size env insn
      | DataBlock (numexp, bvalexp) ->
          let num = Int32.to_int (Expr.eval ~env numexp)
	  and bval = Expr.eval ~env bvalexp in
          for i = 0 to num - 1 do
	    output_data fh 1 bval
	  done;
	  vpc + num
      | Label _
      | DeclVars _
      | Interf _
      | Protect _ -> vpc
      | Alias _ -> failwith "Can't output alias"
      | Scope (inner_env, insns) -> failwith "Can't output scope"
      | Raw_insn _ -> failwith "Can't output raw insn"
      | Macrodef _ -> failwith "Can't output macro definition"
      | Expmacro _ -> failwith "Can't output macro instantiation"
      | Origin _ -> failwith "Can't output origin"
      | NoTemps _ -> failwith "Can't output notemps directive"
      | Temps _ -> failwith "Can't output temps directive"
      | Context _ -> failwith "Can't output context")
    insns
    start_vpc

let encode_prog start_vpc env prog outfile =
  let fo = open_out_bin outfile in
  let last_vpc = emit_insns fo start_vpc env prog in
  close_out fo;
  last_vpc
