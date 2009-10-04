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

let encode_prog start_vpc env prog =
  let fo = open_out_bin "a.out" in
  let last_vpc = List.fold_right
    (fun insn vpc ->
      begin match insn with
        Insn (op, addrmode, args) ->
	  let opcode = M6502.insn_opcode op addrmode in
	  output_byte fo opcode;
          output_addrmode fo env vpc addrmode args
      | Data (size, cexp) ->
          let d32 = Expr.eval ~env cexp in
	  output_data fo size d32
      | Label _ -> ()
      | Raw_insn _ -> failwith "Can't output raw insn"
      | Macrodef _ -> failwith "Can't output macro definition"
      | Expmacro _ -> failwith "Can't output macro instantiation"
      end;
      vpc + Layout.insn_size insn)
    prog
    start_vpc in
  close_out fo;
  last_vpc
