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

(** Parse expression strings into [Sexpr.Expr.t].

    Operators, by increasing precedence (C-like):
    + [|]
    + [^]
    + [&]
    + [+], [-]
    + [<<], [>>] (logical), [>>s] (arithmetic), [rol], [ror], [++] (concat)
    + [*], [/], [/s], [%], [%s] ([s]: signed)
    + [**] (power by a constant, right associative, unrolled into products)
    + unary [~], [-], [uext(n)], [sext(n)] (extend {e by} [n] bits),
      [restrict(lo,hi)]

    Variables ([x], [var1], [_tmp]) and constants may carry a width:
    [x<32>], [5<8>]. Operand widths are reconciled by extension. *)

val parse : ?default_size:int -> string -> Sexpr.Expr.t
(** [default_size] (default 64) is the width of unsized variables and
    constants.
    @raise Failure ["Parse error: ..."] on invalid input. *)
