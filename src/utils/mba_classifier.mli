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

(* mba_classifier.mli - MBA expression classification interface *)

(* MBA Classification types *)
type mba_class =
  | Bitwise        (* Pure bitwise: &, |, ^, ~ *)
  | Bitwise_const  (* Bitwise operations with constants only *)
  | Linear         (* Sum of (const * bitwise_op) *)
  | Linear_const   (* Linear terms with constants only *)
  | Polynomial     (* Sum of (const * product_of_bitwise_ops) *)
  | Polynomial_const (* Polynomial terms with constants only *)
  | Non_polynomial (* MBA operations (+, -, *, &, |, ^, ~, <<) but not polynomial *)
  | Non_mba        (* Contains non-MBA operations: div, mod, >>, etc. *)

(* Classify an MBA expression *)
val classify : Sexpr.Expr.t -> mba_class

(* Convert classification to string *)
val to_string : mba_class -> string

(* Convert classification to colored string for terminal *)
val to_colored_string : mba_class -> string

(* Helper predicates for classification *)
val is_bitwise : Sexpr.Expr.t -> bool
val is_mba_operations : Sexpr.Expr.t -> bool
