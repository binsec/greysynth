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

(* mba_info - Returns information about an MBA expression
 *
 * Usage:
 *   mba_info -string "<expression>"
 *   mba_info -cil <c_file> <function_name>
 *   mba_info -raw-c <c_file> <function_name>
 *
 * Output (JSON):
 *   - size: expression size (AST node count)
 *   - output_bitsize: output bitwidth of expression
 *   - expression: expression as a string (with size annotations)
 *   - classification: MBA classification (bitwise, linear, polynomial, non-polynomial, non-mba)
 *   - expr_gamba_friendly: (cil mode only) cleaned return expression string for GAMBA, or null
 *
 * -cil:   Pure CIL parsing (goes through Binsec smart constructors, may produce restrict/uext)
 * -raw-c: Extracts raw return string from C, parses it via String_to_sexpr (no smart constructors)
 *)

(* Check if expression is GAMBA-compatible *)
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

(* Main *)
let () =
  let string_expr = ref None in
  let cil_file = ref None in
  let cil_func = ref None in
  let raw_c_file = ref None in
  let raw_c_func = ref None in
  let default_size = ref None in

  let usage_msg =
    "Usage:\n\
    \  mba_info -string \"<expression>\"\n\
    \  mba_info -string-file <file>\n\
    \  mba_info -cil <c_file> <function_name>\n\
    \  mba_info -raw-c <c_file> <function_name>\n\n\
     Options:"
  in

  let spec =
    [
      ( "-string",
        Arg.String (fun s -> string_expr := Some s),
        "<expr> Parse string expression (DBA syntax with size annotations, \
         e.g. \"x<64> + y<64>\")" );
      ( "-string-file",
        Arg.String
          (fun file ->
            try
              let ic = open_in file in
              let len = in_channel_length ic in
              let content = really_input_string ic len in
              close_in ic;
              let content = String.trim content in
              string_expr := Some content
            with Sys_error msg ->
              Format.eprintf "Error reading file %s: %s\n" file msg;
              exit 1),
        "<file> Read string expression from file" );
      ( "-cil",
        Arg.Tuple
          [
            Arg.String (fun s -> cil_file := Some s);
            Arg.String (fun s -> cil_func := Some s);
          ],
        "<file> <func> Parse C file via CIL (Binsec smart constructors)" );
      ( "-raw-c",
        Arg.Tuple
          [
            Arg.String (fun s -> raw_c_file := Some s);
            Arg.String (fun s -> raw_c_func := Some s);
          ],
        "<file> <func> Extract raw return expression from C, parse via \
         String_to_sexpr (no smart constructors)" );
      ( "-size",
        Arg.Int (fun s -> default_size := Some s),
        "<bits> Default bit size for variables without annotations" );
    ]
  in

  Arg.parse spec (fun _ -> ()) usage_msg;

  let has_string = Option.is_some !string_expr in
  let has_cil = Option.is_some !cil_file && Option.is_some !cil_func in
  let has_raw_c = Option.is_some !raw_c_file && Option.is_some !raw_c_func in

  let mode_count =
    (if has_string then 1 else 0)
    + (if has_cil then 1 else 0)
    + if has_raw_c then 1 else 0
  in

  if mode_count = 0 then (
    Arg.usage spec usage_msg;
    exit 1);

  if mode_count > 1 then (
    Format.eprintf "Error: Specify exactly one of -string, -cil, or -raw-c\n";
    exit 1);

  try
    if has_raw_c then
      (* -raw-c mode: extract raw return string from C, output as-is (no Sexpr.Expr.t) *)
      let file = Option.get !raw_c_file in
      let func = Option.get !raw_c_func in
      match Cil_to_sexpr.extract_gamba_friendly file func with
      | None ->
          failwith
            (Printf.sprintf
               "Could not extract GAMBA-friendly expression from %s:%s" file
               func)
      | Some raw_str ->
          let json =
            `Assoc
              [
                ("expression", `String raw_str); ("gamba_friendly", `Bool true);
              ]
          in
          Format.printf "%s\n" (Yojson.Basic.pretty_to_string json)
    else
      let sexpr =
        if has_string then
          String_to_sexpr.parse ?default_size:!default_size
            (Option.get !string_expr)
        else
          Cil_to_sexpr.load_from_file (Option.get !cil_file)
            (Option.get !cil_func)
      in

      let size = Greysynth_utils.Utils.size_of_expr sexpr in
      let output_bitsize = Sexpr.Expr.sizeof sexpr in
      let expr_with_size = Greysynth_utils.Utils.pp ~show_size:true sexpr in
      let classification = Mba_classifier.classify sexpr in
      let classification_str = Mba_classifier.to_string classification in

      (* In CIL mode, also extract the raw GAMBA-friendly string *)
      let gamba_friendly_str =
        if has_cil then
          Cil_to_sexpr.extract_gamba_friendly (Option.get !cil_file)
            (Option.get !cil_func)
        else None
      in

      let simplified = Greysynth_utils.Utils.binsec_simplify sexpr in
      let simplified_str =
        Greysynth_utils.Utils.pp_no_ext ~show_size:false simplified
      in

      let json =
        `Assoc
          ([
             ("size", `Int size);
             ("output_bitsize", `Int output_bitsize);
             ("expression", `String expr_with_size);
             ("expression_simplified", `String simplified_str);
             ("classification", `String classification_str);
             ("gamba_friendly", `Bool (is_gamba_compatible sexpr));
           ]
          @
          match gamba_friendly_str with
          | Some s -> [ ("expr_gamba_friendly", `String s) ]
          | None -> [])
      in
      Format.printf "%s\n" (Yojson.Basic.pretty_to_string json)
  with
  | Failure msg ->
      Format.eprintf "Error: %s\n" msg;
      exit 1
  | e ->
      Format.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1
