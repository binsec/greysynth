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

(** Return expression of a C function, as a [Sexpr.Expr.t] (via CIL) or as
    cleaned C text for GAMBA. *)

val load_from_file : string -> string -> Sexpr.Expr.t
(** [load_from_file file func] parses [file] with CIL and converts the
    expression returned by [func]. C integer types give the widths; casts
    become extensions/restrictions.
    @raise Failure if the file or the function does not exist. *)

val extract_gamba_friendly : string -> string -> string option
(** [extract_gamba_friendly file func] returns the raw C text of [func]'s
    return expression, cleaned (literal suffixes, signedness-only casts and
    outer parentheses removed), if it only uses what GAMBA accepts (no
    casts, division, modulo or right shift); [None] otherwise. *)

val set_pointer_size : int -> unit
(** Width in bits given to pointer-typed values (default 64). *)

val get_pointer_size : unit -> int
