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

(* cli_input.ml - Command-line loading of an obfuscated expression and its
 * optional ground truth, shared by the simplifier CLIs (greysynth,
 * hardcoded_rules_simplifier) so they accept the same inputs:
 *
 *   -obf <src> [-gt <src>] [-size <n>] [positional args...]
 *
 *   <src> = -string "<expr>"      expression string (string_to_sexpr syntax)
 *         | -string-file <file>   same, read from a file
 *         | -cil <file> <func>    return expression of a C function
 *
 *   -size sets the default bit width of unsized string expressions.
 *)

type source =
  | String_src of string
  | Cil_src of string * string (* file, func *)

type t = {
  obf : Sexpr.Expr.t;
  gt : Sexpr.Expr.t option;
  positional : string list; (* remaining non-option arguments, in order *)
}

let load default_size = function
  | String_src s -> String_to_sexpr.parse ?default_size s
  | Cil_src (file, func) -> Cil_to_sexpr.load_from_file file func

(* Only used to print the option list in usage messages. *)
let spec =
  [
    ("-obf", Arg.Unit ignore, " Obfuscated expression (followed by a source)");
    ("-gt", Arg.Unit ignore, " Ground truth expression (followed by a source)");
    ("-string", Arg.String ignore, "<expr> Expression string (DBA syntax)");
    ("-string-file", Arg.String ignore, "<file> Read expression string from file");
    ( "-cil",
      Arg.Tuple [ Arg.String ignore; Arg.String ignore ],
      "<file> <func> Return expression of a C function" );
    ("-size", Arg.Int ignore, "<n> Default bit width for unsized expressions");
  ]

let fail_usage usage fmt =
  Format.kfprintf
    (fun ppf ->
      Format.pp_print_flush ppf ();
      Arg.usage spec usage;
      exit 1)
    Format.err_formatter fmt

(* Parse [Sys.argv]; on bad usage print [usage] and exit. *)
let parse_command_line ~(usage : string) : t =
  let obf = ref None and gt = ref None in
  let default_size = ref None in
  let target = ref None in
  let positional = ref [] in
  let set_source src =
    let set name slot =
      if Option.is_some !slot then
        fail_usage usage "Error: %s given more than one expression\n" name
      else slot := Some src
    in
    match !target with
    | Some `Obf -> set "-obf" obf
    | Some `Gt -> set "-gt" gt
    | None -> fail_usage usage "Error: a source must come after -obf or -gt\n"
  in
  let argv = Sys.argv in
  let argc = Array.length argv in
  let arg i name =
    if i >= argc then fail_usage usage "Error: %s requires an argument\n" name
    else argv.(i)
  in
  let i = ref 1 in
  while !i < argc do
    match argv.(!i) with
    | "-obf" ->
        target := Some `Obf;
        incr i
    | "-gt" ->
        target := Some `Gt;
        incr i
    | "-string" ->
        set_source (String_src (arg (!i + 1) "-string"));
        i := !i + 2
    | "-string-file" ->
        let file = arg (!i + 1) "-string-file" in
        (try
           let content = In_channel.with_open_text file In_channel.input_all in
           set_source (String_src (String.trim content))
         with Sys_error msg ->
           Format.eprintf "Error reading file %s: %s\n" file msg;
           exit 1);
        i := !i + 2
    | "-cil" ->
        let file = arg (!i + 1) "-cil" in
        let func = arg (!i + 2) "-cil" in
        set_source (Cil_src (file, func));
        i := !i + 3
    | "-size" ->
        let n = arg (!i + 1) "-size" in
        (match int_of_string_opt n with
        | Some n -> default_size := Some n
        | None -> fail_usage usage "Error: -size expects an integer, got %s\n" n);
        i := !i + 2
    | "-help" | "--help" ->
        Arg.usage spec usage;
        exit 0
    | s when String.length s > 0 && s.[0] = '-' ->
        fail_usage usage "Error: Unknown option: %s\n" s
    | s ->
        positional := s :: !positional;
        incr i
  done;
  match !obf with
  | None -> fail_usage usage "Error: -obf expression is required\n"
  | Some obf_src -> (
      try
        {
          obf = load !default_size obf_src;
          gt = Option.map (load !default_size) !gt;
          positional = List.rev !positional;
        }
      with Failure msg ->
        Format.eprintf "Error: %s\n" msg;
        exit 1)
