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

(** Sample evaluator: evaluates expressions concretely on a fixed bank of
    input samples, so that expressions can be compared by behaviour.

    Sample [i] gives each variable one value (0, 1, -1, max and min signed for
    the first five samples, random afterwards), drawn once and cached: every
    expression evaluated by the same evaluator sees the same inputs. The
    vector of outputs over all samples is therefore a semantic signature, and
    two expressions with the same {!outhash} are probably equivalent.

    It is a separate library because GreySynth is not its only user:
    - GreySynth: OES rewrites nodes sharing an outhash; synthesis samples
      subtrees for the backends;
    - mba_equiv [-sample]: compares two expressions on the same inputs;
    - mba_noiser: rejects noise that is semantically constant;
    - hardcoded_rules_simplifier: asserts each rewrite keeps the outhash. *)

type outhash = int

module Fingerprint : sig
  type t
  (** The value of an expression on each sample (sample id -> value). *)
end

type t = {
  eval : Sexpr.Expr.t -> int -> Bitvector.t;
      (** [eval e i]: value of [e] on sample [i] (cached).
          @raise Division_by_zero *)
  eval_all : Sexpr.Expr.t -> unit;
      (** Evaluate [e] on every sample, filling the cache. *)
  reset : int -> unit;  (** Forget the cached values of sample [i]. *)
  node_to_fingerprint : Sexpr.Expr.t -> Fingerprint.t;
      (** Values on the first samples that do not divide by zero. *)
  node_to_outhash : Sexpr.Expr.t -> outhash;
      (** Hash of the values on all samples, skipping any sample on which
          some expression divided by zero. *)
  is_node_constant : Sexpr.Expr.t -> Bitvector.t option;
      (** [Some v] if [e] evaluates to [v] on every sample. Reads the cache:
          call [eval_all] first. *)
  eval_label :
    Sexpr.Expr.t -> int -> (Sexpr.Expr.t, Sexpr.Expr.t) Hashtbl.t -> Bitvector.t;
      (** [eval_label e i labels]: value of [e] on sample [i], where each
          subterm bound in [labels] (e.g. a fresh variable standing for a cut
          subtree) takes the value of the term it maps to. *)
}

val make : int -> t
(** [make n]: a fresh evaluator over [n] samples, with its own inputs. *)
