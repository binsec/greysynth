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

(* bin_extract.ml - Extract MBA expressions from binaries using Binsec SSE
 *
 * This tool extracts Sexpr.Expr.t from binaries using the same SSE script format
 * as xyntia's sampler, applies MBA-compatible simplifications (reducing sign
 * extend/restrict operations), and outputs in mba_info JSON format.
 *
 * Usage:
 *   bin_extract <binary> <sse_script> [options]
 *
 * Output (JSON):
 *   For each extracted output (register or memory):
 *   {
 *     "name": "rax",
 *     "size": 42,
 *     "output_bitsize": 64,
 *     "expression": "x<64> + y<64>",
 *     "expression_simplified": "(x + y)",
 *     "classification": "linear",
 *     "gamba_friendly": true
 *   }
 *)

open Binary_to_expr

(* Check if expression is GAMBA-compatible (same as mba_info) *)
let rec is_gamba_compatible (e : Sexpr.Expr.t) : bool =
  match e with
  | Sexpr.Expr.Var _ | Sexpr.Expr.Cst _ -> true
  | Sexpr.Expr.Unary { f; x; _ } -> (
      match f with
      | Sexpr.Expr.Sext _ | Sexpr.Expr.Uext _ | Sexpr.Expr.Restrict _ ->
          false (* Extensions and restrictions not allowed *)
      | _ -> is_gamba_compatible x)
  | Sexpr.Expr.Binary { f; x; y; _ } -> (
      match f with
      | Sexpr.Expr.Udiv | Sexpr.Expr.Sdiv | Sexpr.Expr.Umod | Sexpr.Expr.Smod ->
          false (* Division and modulo not allowed *)
      | Sexpr.Expr.Lsr | Sexpr.Expr.Asr -> false (* Right shifts not allowed *)
      | _ -> is_gamba_compatible x && is_gamba_compatible y)
  | Sexpr.Expr.Load _ -> true
  | Sexpr.Expr.Ite { c; t; e; _ } ->
      is_gamba_compatible c && is_gamba_compatible t && is_gamba_compatible e

(* Apply MBA-compatible simplification - removes extensions and restrictions *)
let mba_simplify (expr : Sexpr.Expr.t) : Sexpr.Expr.t =
  (* Step 1: Apply Binsec's simplifier first *)
  let simplified = Greysynth_utils.Utils.binsec_simplify expr in

  (* Step 2: Recursively remove all Sext/Uext/Restrict operations *)
  let rec strip_extensions e =
    match e with
    | Sexpr.Expr.Var _ | Sexpr.Expr.Cst _ -> e
    | Sexpr.Expr.Unary { f; x; _ } -> (
        let x' = strip_extensions x in
        match f with
        | Sexpr.Expr.Sext _ | Sexpr.Expr.Uext _ | Sexpr.Expr.Restrict _ ->
            (* Strip the extension/restriction, keep the inner expression *)
            x'
        | _ -> Sexpr.Expr.unary f x')
    | Sexpr.Expr.Binary { f; x; y; _ } ->
        let x' = strip_extensions x in
        let y' = strip_extensions y in
        Sexpr.Expr.binary f x' y'
    | Sexpr.Expr.Ite { c; t; e; _ } ->
        let c' = strip_extensions c in
        let t' = strip_extensions t in
        let e' = strip_extensions e in
        Sexpr.Expr.ite c' t' e'
    | Sexpr.Expr.Load _ ->
        (* Keep Load nodes - they should have been resolved during extraction *)
        (* TODO: Proper Load resolution requires access to memory state during extraction *)
        e
  in

  (* Step 3: Strip extensions, then simplify again *)
  let no_ext = strip_extensions simplified in
  Greysynth_utils.Utils.binsec_simplify no_ext

(* Check if expression contains Load operations *)
let rec contains_load (expr : Sexpr.Expr.t) : bool =
  match expr with
  | Sexpr.Expr.Load _ -> true
  | Sexpr.Expr.Var _ | Sexpr.Expr.Cst _ -> false
  | Sexpr.Expr.Unary { x; _ } -> contains_load x
  | Sexpr.Expr.Binary { x; y; _ } -> contains_load x || contains_load y
  | Sexpr.Expr.Ite { c; t; e; _ } ->
      contains_load c || contains_load t || contains_load e

(* Convert an expression to JSON in mba_info format *)
let expr_to_mba_json (name : string) (expr : Sexpr.Expr.t) : Yojson.Basic.t =
  try
    let size = Greysynth_utils.Utils.size_of_expr expr in
    let output_bitsize = Sexpr.Expr.sizeof expr in

    let expr_with_size =
      try Greysynth_utils.Utils.pp ~show_size:true expr
      with e ->
        Printf.eprintf "Warning: Failed to pretty-print expression for %s: %s\n"
          name (Printexc.to_string e);
        Format.asprintf "%a" Term.pp expr
    in

    (* Only classify if expression doesn't contain Load operations *)
    let classification_str, classification_error =
      if contains_load expr then
        ("unknown", Some "Expression contains Load (memory) operations")
      else
        try
          let classification = Mba_classifier.classify expr in
          (Mba_classifier.to_string classification, None)
        with e -> ("error", Some (Printexc.to_string e))
    in

    (* Apply MBA-compatible simplification *)
    let simplified =
      try mba_simplify expr
      with e ->
        Printf.eprintf "Warning: Simplification failed for %s: %s\n" name
          (Printexc.to_string e);
        expr
    in

    let simplified_str =
      try Greysynth_utils.Utils.pp_no_ext ~show_size:false simplified
      with e ->
        Printf.eprintf
          "Warning: Failed to pretty-print simplified expression for %s: %s\n"
          name (Printexc.to_string e);
        Format.asprintf "%a" Term.pp simplified
    in

    let base_fields =
      [
        ("name", `String name);
        ("size", `Int size);
        ("output_bitsize", `Int output_bitsize);
        ("expression", `String expr_with_size);
        ("expression_simplified", `String simplified_str);
        ("classification", `String classification_str);
        ("gamba_friendly", `Bool (is_gamba_compatible simplified));
      ]
    in

    let fields_with_error =
      match classification_error with
      | Some err -> base_fields @ [ ("classification_note", `String err) ]
      | None -> base_fields
    in

    `Assoc fields_with_error
  with e ->
    (* Last resort error handling *)
    Printf.eprintf "Error processing expression %s: %s\n" name
      (Printexc.to_string e);
    `Assoc [ ("name", `String name); ("error", `String (Printexc.to_string e)) ]

(* Convert memory expression to JSON *)
let memory_expr_to_json (addr_expr : Sexpr.Expr.t) (value_expr : Sexpr.Expr.t) :
    Yojson.Basic.t =
  let addr_str = Format.asprintf "%a" Term.pp addr_expr in
  let name = Format.sprintf "mem[%s]" addr_str in
  expr_to_mba_json name value_expr

(* Main *)
let () =
  let binary_path = ref "" in
  let sse_script = ref "" in
  let output_format = ref "json" in
  let verbose = ref false in
  let output_file = ref None in
  let only_registers = ref false in
  let only_memory = ref false in
  let filter_reg = ref None in
  let only_final_version = ref true in
  (* Default: only show final version *)
  let only_symbolic = ref false in
  (* Only show x0, x1, etc. not rdi, rsi *)

  let anon_args = ref [] in
  let add_anon s = anon_args := s :: !anon_args in

  let usage_msg =
    "bin_extract - Extract MBA expressions from binaries\n\n\
     Usage:\n\
    \  bin_extract <binary> <sse_script> [options]\n\n\
     Options:"
  in

  let specs =
    [
      ( "-o",
        Arg.String (fun s -> output_file := Some s),
        "<file> Output to file instead of stdout" );
      ( "-format",
        Arg.Symbol ([ "json"; "json-compact" ], fun s -> output_format := s),
        " Output format (json, json-compact)" );
      ("-v", Arg.Set verbose, " Verbose output (show extraction progress)");
      ( "--registers-only",
        Arg.Set only_registers,
        " Only extract register expressions" );
      ("--memory-only", Arg.Set only_memory, " Only extract memory expressions");
      ( "-r",
        Arg.String (fun s -> filter_reg := Some s),
        "<reg> Only extract specific register (e.g., rax, eax)" );
      ( "--all-versions",
        Arg.Clear only_final_version,
        " Show all SSA versions (eax!1, eax!2, etc.), not just final" );
      ( "--only-symbolic",
        Arg.Set only_symbolic,
        " Only show symbolic variables (x0, x1, etc.), filter out rdi, rsi, \
         rbp, rsp" );
    ]
  in

  Arg.parse specs add_anon usage_msg;

  (* Process anonymous arguments *)
  (match List.rev !anon_args with
  | [ bin; script ] ->
      binary_path := bin;
      sse_script := script
  | _ ->
      Printf.eprintf "Error: Expected <binary> and <sse_script> arguments\n\n";
      Arg.usage specs usage_msg;
      exit 1);

  try
    (* Validate inputs *)
    if !binary_path = "" || !sse_script = "" then (
      Printf.eprintf "Error: Binary and SSE script required\n\n";
      Arg.usage specs usage_msg;
      exit 1);

    if not (Sys.file_exists !binary_path) then (
      Printf.eprintf "Error: Binary not found: %s\n" !binary_path;
      exit 1);

    if not (Sys.file_exists !sse_script) then (
      Printf.eprintf "Error: SSE script not found: %s\n" !sse_script;
      exit 1);

    if !verbose then Printf.eprintf "Extracting from binary: %s\n" !binary_path;

    (* Run extraction using Binsec *)
    let result = run_extraction !binary_path !sse_script in

    if !verbose then
      Printf.eprintf
        "Extracted %d register expressions and %d memory expressions\n"
        (List.length result.expressions)
        (List.length result.memory_expressions);

    (* Helper: extract base name and version from versioned register (e.g., "eax!2" -> ("eax", 2)) *)
    let parse_reg_version name =
      match String.split_on_char '!' name with
      | [ base ] -> (base, 0)
      | [ base; ver ] -> (base, int_of_string ver)
      | _ -> (name, 0)
    in

    (* Helper: check if register is symbolic variable (x0, x1, etc.) *)
    let is_symbolic_var name =
      let base_name, _ = parse_reg_version name in
      String.length base_name > 0
      && base_name.[0] = 'x'
      && String.length base_name > 1
      &&
      try
        ignore
          (int_of_string (String.sub base_name 1 (String.length base_name - 1)));
        true
      with _ -> false
    in

    (* Helper: match register name with or without version suffix (e.g., eax or eax!1) *)
    let reg_matches filter_name actual_name =
      let base_name, _ = parse_reg_version actual_name in
      base_name = filter_name
    in

    (* Filter to keep only final versions of each register *)
    let keep_final_versions exprs =
      let tbl = Hashtbl.create 16 in
      (* For each register, keep the one with highest version number *)
      List.iter
        (fun (name, expr) ->
          let base, version = parse_reg_version name in
          match Hashtbl.find_opt tbl base with
          | None -> Hashtbl.add tbl base (name, version, expr)
          | Some (_, old_version, _) when version > old_version ->
              Hashtbl.replace tbl base (name, version, expr)
          | _ -> ())
        exprs;
      Hashtbl.fold (fun _ (name, _, expr) acc -> (name, expr) :: acc) tbl []
    in

    (* Filter expressions based on options *)
    let reg_exprs =
      if !only_memory then []
      else
        let exprs = result.expressions in
        (* Apply symbolic-only filter if requested *)
        let exprs =
          if !only_symbolic then
            List.filter (fun (name, _) -> is_symbolic_var name) exprs
          else exprs
        in
        (* Apply register name filter if specified *)
        let exprs =
          match !filter_reg with
          | Some reg_name ->
              let filtered =
                List.filter (fun (name, _) -> reg_matches reg_name name) exprs
              in
              if filtered = [] then (
                (* No matches found - show available registers *)
                Printf.eprintf "Warning: Register '%s' not found.\n" reg_name;
                Printf.eprintf "Available registers:\n";
                List.iter
                  (fun (name, _) ->
                    let marker =
                      if is_symbolic_var name then " (symbolic)" else ""
                    in
                    Printf.eprintf "  - %s%s\n" name marker)
                  result.expressions;
                if result.expressions = [] then
                  Printf.eprintf
                    "  (none - all registers were constant or unchanged)\n");
              filtered
          | None -> exprs
        in
        (* Apply final-version filter if requested *)
        if !only_final_version then keep_final_versions exprs else exprs
    in

    (* When filtering by register, automatically exclude memory unless explicitly requested *)
    let mem_exprs =
      if !only_registers then []
      else if Option.is_some !filter_reg && not !only_memory then
        (* When filtering by register, don't include memory unless user wants both *)
        []
      else result.memory_expressions
    in

    (* Check if we have any output *)
    if reg_exprs = [] && mem_exprs = [] then (
      Printf.eprintf "Error: No expressions to output.\n";
      if Option.is_some !filter_reg then
        Printf.eprintf
          "The filtered register was not found or had no symbolic value.\n";
      exit 1);

    (* Convert to JSON *)
    let reg_json =
      List.map (fun (name, expr) -> expr_to_mba_json name expr) reg_exprs
    in

    let mem_json =
      List.map (fun (addr, value) -> memory_expr_to_json addr value) mem_exprs
    in

    let all_outputs = reg_json @ mem_json in

    let output_json =
      `Assoc
        [
          ("binary", `String !binary_path);
          ("sse_script", `String !sse_script);
          ("num_outputs", `Int (List.length all_outputs));
          ("outputs", `List all_outputs);
        ]
    in

    (* Format output *)
    let json_str =
      match !output_format with
      | "json-compact" -> Yojson.Basic.to_string output_json
      | _ -> Yojson.Basic.pretty_to_string output_json
    in

    (* Write output *)
    (match !output_file with
    | Some file ->
        let oc = open_out file in
        output_string oc json_str;
        output_string oc "\n";
        close_out oc;
        if !verbose then Printf.eprintf "Output written to: %s\n" file
    | None -> Printf.printf "%s\n" json_str);

    if !verbose then Printf.eprintf "Extraction complete!\n"
  with
  | Extraction_error msg ->
      Printf.eprintf "Extraction error: %s\n" msg;
      exit 1
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
      Printexc.print_backtrace stderr;
      exit 1
