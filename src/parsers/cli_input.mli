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

(** Command-line loading of an obfuscated expression and its optional ground
    truth, shared by the simplifier CLIs (greysynth,
    hardcoded_rules_simplifier) so that they accept the same inputs:

    {v -obf <src> [-gt <src>] [-size <n>] [positional args...] v}

    where [<src>] is one of
    - [-string "<expr>"]: expression string ({!String_to_sexpr} syntax);
    - [-string-file <file>]: the same, read from a file;
    - [-cil <file> <func>]: return expression of a C function.

    [-size] sets the default bit width of unsized string expressions. *)

type t = {
  obf : Sexpr.Expr.t;
  gt : Sexpr.Expr.t option;
  positional : string list;  (** non-option arguments, in order *)
}

val parse_command_line : usage:string -> t
(** Parse [Sys.argv] and load the expressions. On bad usage, prints the error
    and [usage] followed by the option list, then exits; on an expression
    that fails to load, prints the error and exits. *)

val fail_usage : string -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail_usage usage fmt ...] prints the formatted error, then [usage] and
    the option list, and exits with code 1. For a CLI's own checks on
    [positional]. *)
