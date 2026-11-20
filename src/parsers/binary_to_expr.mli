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

(* binary_to_expr.mli - Interface for extracting Sexpr.Expr.t from binaries *)

(** Result of extraction containing register and memory expressions *)
type extraction_result = {
  expressions: (string * Sexpr.Expr.t) list;  (** (register_name, expression) pairs *)
  memory_expressions: (Sexpr.Expr.t * Sexpr.Expr.t) list;  (** (address, value) pairs *)
}

(** Exception raised when extraction fails *)
exception Extraction_error of string

(** [run_extraction ?target_addr binary_path sse_script] extracts symbolic expressions
    from a binary file using Binsec SSE (Symbolic Scripting Engine).

    @param target_addr Optional target address for extraction
    @param binary_path Path to the binary executable
    @param sse_script Path to the SSE script file
    @return extraction_result containing all extracted expressions
    @raise Extraction_error if binary or script not found, or execution fails *)
val run_extraction :
  ?target_addr:int ->
  string ->
  string ->
  extraction_result
