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

(* binary_to_expr.ml - Extract Sexpr.Expr.t from binary files using Binsec
 *
 * This module provides a Binsec plugin that extracts symbolic expressions
 * from binaries during symbolic execution. It hooks directly into Binsec's
 * SSE engine to capture the symbolic state at specified points.
 *)

open Types

(* ========================================================================== *)
(* Global state for extracted expressions *)
(* ========================================================================== *)

(* Storage for extracted expressions - populated by the plugin at runtime *)
let extracted_expressions : (string * Sexpr.Expr.t) list ref = ref []
let extracted_memory : (Sexpr.Expr.t * Sexpr.Expr.t) list ref = ref []
let extraction_done : bool ref = ref false
let extraction_error : string option ref = ref None

(* Result of extraction *)
type extraction_result = {
  expressions: (string * Sexpr.Expr.t) list;
  memory_expressions: (Sexpr.Expr.t * Sexpr.Expr.t) list;
}

exception Extraction_error of string

(* ========================================================================== *)
(* Custom AST types for the "extract" instruction *)
(* ========================================================================== *)

type Ast.Instr.t +=
  | Extract of string list  (* List of register names to extract, or empty for all *)

type Ir.builtin +=
  | BuiltinExtract of Dba.LValue.t list

type Ast.t +=
  | SetExtractOutput of string  (* Directory or "stdout" *)

(* ========================================================================== *)
(* CLI module for plugin configuration *)
(* ========================================================================== *)

include Cli.Make (struct
  let name = "BinaryToExpr"
  let shortname = "bin2expr"
end)

module OutputDir = Builder.String_option (struct
  let name = "out"
  let doc = "Set output directory for extracted expressions"
end)

(* ========================================================================== *)
(* Binsec Plugin Implementation *)
(* ========================================================================== *)

let () =
  Exec.register_plugin
    (module struct
      let name = "binary_to_expr"

      (* Grammar extension to add "extract" instruction *)
      let grammar_extension = [
        Dyp.Add_rules [
          (* extract - extract all outputs *)
          (("fallthrough",
            [
              Dyp.Regexp (RE_String "extract");
            ],
            "default_priority",
            []),
           fun _ _ -> (Libparser.Syntax.Instr (Extract []), []));

          (* extract <reg1>, <reg2>, ... - extract specific registers *)
          (("fallthrough",
            [
              Dyp.Regexp (RE_String "extract");
              Dyp.Non_ter ("lval", No_priority);
            ],
            "default_priority",
            []),
           fun _ -> function
             | [_; Libparser.Syntax.Loc _loc] ->
                 (Libparser.Syntax.Instr (Extract []), [])
             | _ -> assert false);
        ];
      ]

      let instruction_printer = Some (fun fmt instr ->
        match instr with
        | Extract [] ->
            Format.fprintf fmt "extract";
            true
        | Extract regs ->
            Format.fprintf fmt "extract %s" (String.concat ", " regs);
            true
        | _ -> false)

      let declaration_printer = None

      (* Main extension that hooks into the symbolic execution *)
      let extension :
          type a b.
          (module EXPLORATION_STATISTICS) ->
          (module Path.S with type t = a) ->
          (module STATE with type t = b) ->
          (module Exec.EXTENSION with type path = a and type state = b) option =
       fun _stats path state ->
         let module MS = (val state) in
         match is_enabled (), MS.Value.kind, MS.getter Senv.VisibleMemory, MS.getter Senv.VisibleSymbols with
         | true, Senv.Term, Some get_vmem, Some get_vsymb -> Some (module struct
               module P = (val path)
               module S = MS

               type path = P.t
               and state = S.t

               let initialization_callback = None

               let instruction_callback = Some (fun inst _env ->
                 match inst with
                 | Extract _regs ->
                     let lvals = [] in
                     [Ir.Builtin (BuiltinExtract lvals)]
                 | _ -> [])

               let declaration_callback = Some (fun decl _env _path state ->
                 match decl with
                 | SetExtractOutput _dir ->
                     Some state
                 | _ -> None)

               let process_callback = None

               (* Helper to get all register expressions from state *)
               let get_register_outputs state =
                 let symbols = get_vsymb state in
                 Dba_types.Var.Map.fold
                   (fun (var : Dba.Var.t) (expr : Sexpr.Expr.t) acc ->
                     (* Filter out temporaries and flags *)
                     match var.info with
                     | Temp | Flag -> acc
                     | _ ->
                         (* Also filter expressions that are just reads of the same var *)
                         (match expr with
                          | Var { label; _ } when String.equal var.name label -> acc
                          | _ -> (var.name, expr) :: acc))
                   symbols []

               (* Helper to get memory expressions *)
               let get_memory_outputs state =
                 let rec extract_memory (mem : Sexpr.Memory.t) acc =
                   match mem with
                   | Layer { over; addr; store; _ } ->
                       let acc = Sexpr.Store.fold_term
                         (fun offset expr cumul ->
                           let word_size = Kernel_options.Machine.word_size () in
                           let load_addr = Sexpr.Expr.add addr
                             (Sexpr.Expr.constant (Bitvector.create offset word_size)) in
                           (load_addr, expr) :: cumul)
                         acc store
                       in
                       extract_memory over acc
                   | Symbol _ | Root -> acc
                 in
                 extract_memory (get_vmem state) []

               (* The extract builtin - captures symbolic state *)
               let do_extract _lvals _addr _path _depth state : (S.t, status) Result.t =
                 (* Extract all register expressions *)
                 let reg_exprs = get_register_outputs state in
                 extracted_expressions := reg_exprs;

                 (* Extract memory expressions *)
                 let mem_exprs = get_memory_outputs state in
                 extracted_memory := mem_exprs;

                 extraction_done := true;

                 (* Log what we extracted *)
                 Logger.info "Extracted %d register expressions and %d memory expressions"
                   (List.length reg_exprs) (List.length mem_exprs);

                 (* Continue execution (or halt if we only want extraction) *)
                 Ok state

               let builtin_callback = Some (function
                 | BuiltinExtract lvals -> Some (do_extract lvals)
                 | _ -> None)

               let builtin_printer = Some (fun fmt builtin ->
                 match builtin with
                 | BuiltinExtract [] ->
                     Format.fprintf fmt "extract()";
                     true
                 | BuiltinExtract _ ->
                     Format.fprintf fmt "extract(...)";
                     true
                 | _ -> false)

               let at_exit_callback = Some (fun () ->
                 if not !extraction_done then begin
                   Logger.warning "Extraction was not triggered during execution"
                 end)
             end)
         | _ -> None
    end : Exec.PLUGIN)

(* ========================================================================== *)
(* High-level API *)
(* ========================================================================== *)

let reset () =
  extracted_expressions := [];
  extracted_memory := [];
  extraction_done := false;
  extraction_error := None

let run_extraction
    ?(target_addr : int option)
    (binary_path : string)
    (sse_script : string)
    : extraction_result =

  let _ = target_addr in  (* TODO: use for targeted extraction *)

  reset ();

  (* Check files exist *)
  if not (Sys.file_exists binary_path) then
    raise (Extraction_error (Printf.sprintf "Binary not found: %s" binary_path));

  if not (Sys.file_exists sse_script) then
    raise (Extraction_error (Printf.sprintf "SSE script not found: %s" sse_script));

  (* Enable this plugin and the SSE subsystem *)
  enable ();
  Options.enable ();

  (* Configure Binsec *)
  Kernel_options.ExecFile.set binary_path;
  Kernel_options.Machine.set Machine.amd64;
  Options.ScriptFiles.set [sse_script];

  (* Run Binsec's SSE *)
  begin
    try
      Sse.run ()
    with e ->
      extraction_error := Some (Printexc.to_string e);
      raise (Extraction_error (Printf.sprintf "Binsec execution failed: %s"
        (Printexc.to_string e)))
  end;

  (* Return results *)
  if not !extraction_done then
    raise (Extraction_error
      "Extraction point was not reached. Make sure your SSE script includes 'extract'");

  {
    expressions = !extracted_expressions;
    memory_expressions = !extracted_memory;
  }
