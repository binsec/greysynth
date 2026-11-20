(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2026-2027                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  You can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* mba_equiv - Check equivalence of two MBA expressions
 *
 * Usage:
 *   mba_equiv -obf -string "<expr>" -gt -string "<expr>"
 *   mba_equiv -obf -cil <file> <func> -gt -cil <file> <func>
 *   mba_equiv -obf -cil <file> <func> -gt -string "<expr>"
 *   mba_equiv -obf -string "<expr>" -gt -cil <file> <func>
 *
 * Equivalence mode:
 *   -smt          Use SMT solver (default)
 *   -sample N     Evaluate both expressions on N random inputs and compare
 *)

type mode = Smt | Sample of int

(* Expression source type *)
type expr_source =
  | StringExpr of string
  | CilExpr of string * string (* file, func *)

(* Load expression from source *)
let load_expr default_size = function
  | StringExpr s -> String_to_sexpr.parse ?default_size s
  | CilExpr (file, func) -> Cil_to_sexpr.load_from_file file func

(* Read expression string from file *)
let read_expr_from_file (file : string) : string =
  try
    let ic = open_in file in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    String.trim content
  with Sys_error msg ->
    Format.eprintf "Error reading file %s: %s\n" file msg;
    exit 1

(* Main *)
let () =
  let obf_source : expr_source option ref = ref None in
  let gt_source : expr_source option ref = ref None in
  let timeout = ref 10 in
  let default_size = ref None in
  let current_target = ref None in
  let mode = ref Smt in

  let usage_msg =
    "Usage:\n\
    \  mba_equiv -obf -string \"<expr>\" -gt -string \"<expr>\"\n\
    \  mba_equiv -obf -string-file <file> -gt -string-file <file>\n\
    \  mba_equiv -obf -cil <file> <func> -gt -cil <file> <func>\n\
    \  mba_equiv -obf -cil <file> <func> -gt -string \"<expr>\"\n\
    \  mba_equiv -obf -cil <file> <func> -gt -string-file <file>\n\n\
     Equivalence mode (default: -smt):\n\
    \  -smt              Use SMT solver\n\
    \  -sample <N>       Evaluate both expressions on N random inputs and compare\n\n\
     Options:"
  in

  let set_string_expr s =
    match !current_target with
    | Some `Obf -> obf_source := Some (StringExpr s)
    | Some `Gt -> gt_source := Some (StringExpr s)
    | None ->
        Format.eprintf "Error: -string must come after -obf or -gt\n";
        exit 1
  in

  let set_string_file_expr file =
    let content = read_expr_from_file file in
    set_string_expr content
  in

  let set_cil_expr file func =
    match !current_target with
    | Some `Obf -> obf_source := Some (CilExpr (file, func))
    | Some `Gt -> gt_source := Some (CilExpr (file, func))
    | None ->
        Format.eprintf "Error: -cil must come after -obf or -gt\n";
        exit 1
  in

  let spec =
    [
      ( "-obf",
        Arg.Unit (fun () -> current_target := Some `Obf),
        " Specify obfuscated expression (followed by -string or -cil)" );
      ( "-gt",
        Arg.Unit (fun () -> current_target := Some `Gt),
        " Specify ground truth expression (followed by -string or -cil)" );
      ( "-string",
        Arg.String set_string_expr,
        "<expr> Parse string expression (DBA syntax)" );
      ( "-cil",
        Arg.Tuple
          [
            Arg.String
              (fun file ->
                let func_ref = ref "" in
                Arg.current := !Arg.current;
                func_ref := file);
            Arg.String
              (fun func ->
                let file = Sys.argv.(!Arg.current - 1) in
                set_cil_expr file func);
          ],
        "<file> <func> Parse C file and extract function's return expression" );
      ( "-smt",
        Arg.Unit (fun () -> mode := Smt),
        " Use SMT solver for equivalence checking (default)" );
      ( "-sample",
        Arg.Int (fun n -> mode := Sample n),
        "<N> Evaluate on N random inputs and compare outputs" );
      ( "-timeout",
        Arg.Int (fun t -> timeout := t),
        "<seconds> SMT solver timeout (default: 10)" );
    ]
  in

  (* Custom argument parsing to handle the stateful -obf/-gt followed by -string/-cil *)
  let argv = Sys.argv in
  let argc = Array.length argv in
  let i = ref 1 in

  while !i < argc do
    let arg = argv.(!i) in
    match arg with
    | "-obf" ->
        current_target := Some `Obf;
        incr i
    | "-gt" ->
        current_target := Some `Gt;
        incr i
    | "-string" ->
        if !i + 1 >= argc then (
          Format.eprintf "Error: -string requires an argument\n";
          exit 1);
        incr i;
        set_string_expr argv.(!i);
        incr i
    | "-string-file" ->
        if !i + 1 >= argc then (
          Format.eprintf "Error: -string-file requires an argument\n";
          exit 1);
        incr i;
        set_string_file_expr argv.(!i);
        incr i
    | "-cil" ->
        if !i + 2 >= argc then (
          Format.eprintf
            "Error: -cil requires two arguments (file and function)\n";
          exit 1);
        incr i;
        let file = argv.(!i) in
        incr i;
        let func = argv.(!i) in
        set_cil_expr file func;
        incr i
    | "-smt" ->
        mode := Smt;
        incr i
    | "-sample" ->
        if !i + 1 >= argc then (
          Format.eprintf "Error: -sample requires an argument\n";
          exit 1);
        incr i;
        mode := Sample (int_of_string argv.(!i));
        incr i
    | "-timeout" ->
        if !i + 1 >= argc then (
          Format.eprintf "Error: -timeout requires an argument\n";
          exit 1);
        incr i;
        timeout := int_of_string argv.(!i);
        incr i
    | "-size" ->
        if !i + 1 >= argc then (
          Format.eprintf "Error: -size requires an argument\n";
          exit 1);
        incr i;
        default_size := Some (int_of_string argv.(!i));
        incr i
    | "-help" | "--help" ->
        Arg.usage spec usage_msg;
        exit 0
    | _ ->
        Format.eprintf "Error: Unknown argument: %s\n" arg;
        Arg.usage spec usage_msg;
        exit 1
  done;

  (* Validate that we have both expressions *)
  if Option.is_none !obf_source then (
    Format.eprintf "Error: Missing -obf expression\n";
    Arg.usage spec usage_msg;
    exit 1);

  if Option.is_none !gt_source then (
    Format.eprintf "Error: Missing -gt expression\n";
    Arg.usage spec usage_msg;
    exit 1);

  try
    let sexpr_obf_raw = load_expr !default_size (Option.get !obf_source) in
    let sexpr_gt_raw = load_expr !default_size (Option.get !gt_source) in

    (* ── Align variable sizes ──────────────────────────────────────────
       When the same variable name appears in both expressions with different
       sizes (e.g. x0<8> in obf vs x0<32> in gt), rewrite the expression
       with the LARGER variable to use uext(diff, Var(name, small_size)).
       This ensures the SMT solver uses a single variable per name. *)
    let collect_vars expr =
      let tbl = Hashtbl.create 8 in
      let rec walk = function
        | Sexpr.Expr.Var v -> Hashtbl.replace tbl v.name v.size
        | Sexpr.Expr.Unary r -> walk r.x
        | Sexpr.Expr.Binary r -> walk r.x; walk r.y
        | Sexpr.Expr.Ite r -> walk r.c; walk r.t; walk r.e
        | _ -> ()
      in
      walk expr; tbl
    in
    let rec rewrite_var expr name old_sz new_sz =
      match expr with
      | Sexpr.Expr.Var v when v.name = name && v.size = old_sz ->
          Sexpr.Expr.unary (Sexpr.Expr.Uext (old_sz - new_sz))
            (Sexpr.Expr.var name new_sz name)
      | Sexpr.Expr.Unary r ->
          Sexpr.Expr.unary r.f (rewrite_var r.x name old_sz new_sz)
      | Sexpr.Expr.Binary r ->
          Sexpr.Expr.binary r.f
            (rewrite_var r.x name old_sz new_sz)
            (rewrite_var r.y name old_sz new_sz)
      | Sexpr.Expr.Ite r ->
          Sexpr.Expr.ite
            (rewrite_var r.c name old_sz new_sz)
            (rewrite_var r.t name old_sz new_sz)
            (rewrite_var r.e name old_sz new_sz)
      | other -> other
    in
    let vars_obf = collect_vars sexpr_obf_raw in
    let vars_gt = collect_vars sexpr_gt_raw in
    let sexpr_obf, sexpr_gt =
      Hashtbl.fold (fun name sz_obf (e_obf, e_gt) ->
        match Hashtbl.find_opt vars_gt name with
        | Some sz_gt when sz_obf <> sz_gt ->
            let small = min sz_obf sz_gt in
            if sz_obf > small then
              (rewrite_var e_obf name sz_obf small, e_gt)
            else
              (e_obf, rewrite_var e_gt name sz_gt small)
        | _ -> (e_obf, e_gt)
      ) vars_obf (sexpr_obf_raw, sexpr_gt_raw)
    in

    let size_obf = Greysynth_utils.Utils.size_of_expr sexpr_obf in
    let size_gt = Greysynth_utils.Utils.size_of_expr sexpr_gt in
    let expr_obf_str = Greysynth_utils.Utils.pp sexpr_obf in
    let expr_gt_str = Greysynth_utils.Utils.pp sexpr_gt in

    let equiv_str, time, counter_example_json =
      match !mode with
      | Smt ->
          let result, t, counter_example =
            Greysynth_utils.Utils.are_equivalent sexpr_obf sexpr_gt (Some !timeout)
              ~show_counter_example:true
          in
          let s =
            match result with
            | Greysynth_utils.Utils.Yes -> "yes"
            | Greysynth_utils.Utils.No -> "no"
            | _ -> "ukn"
          in
          let ce_json =
            match counter_example with
            | Some ce ->
                `Assoc
                  (List.map
                     (fun (var, value) -> (var, `String (Z.to_string value)))
                     ce)
            | None -> `Null
          in
          (s, t, ce_json)
      | Sample n ->
          let ev = Sample_evaluator.make n in
          let t0 = Unix.gettimeofday () in
          let equiv = ref true in
          (try
             for sid = 0 to n - 1 do
               let v_obf = ev.eval sexpr_obf sid in
               let v_gt = ev.eval sexpr_gt sid in
               if not (Bitvector.equal v_obf v_gt) then (
                 equiv := false;
                 raise Exit)
             done
           with
          | Exit -> ()
          | Division_by_zero -> ());
          let t = Unix.gettimeofday () -. t0 in
          let s = if !equiv then "yes" else "no" in
          (s, t, `Null)
    in

    let json =
      `Assoc
        [
          ("equiv", `String equiv_str);
          ("equiv_time", `Float time);
          ( "obf",
            `Assoc
              [ ("expression", `String expr_obf_str); ("size", `Int size_obf) ]
          );
          ( "gt",
            `Assoc
              [ ("expression", `String expr_gt_str); ("size", `Int size_gt) ] );
          ("counter_example", counter_example_json);
        ]
    in
    Format.printf "%s\n" (Yojson.Basic.pretty_to_string json)
  with
  | Failure msg ->
      Format.eprintf "Error: %s\n" msg;
      exit 1
  | e ->
      Format.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1
