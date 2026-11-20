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

(* Sample evaluator (what and why: sample_evaluator.mli). Values are cached
   per sample, keyed by node hash. *)

open Libterm.Sexpr
module IntSet = Set.Make (Int)

type outhash = int

let const_max_retries = 100

module Fingerprint = struct
  module IdMap = Map.Make (Int)

  type t = Bitvector.t IdMap.t
end

type t = {
  eval : Expr.t -> int -> Bitvector.t;
  eval_all : Expr.t -> unit;
  reset : int -> unit;
  node_to_fingerprint : Expr.t -> Fingerprint.t;
  node_to_outhash : Expr.t -> outhash;
  is_node_constant : Expr.t -> Bitvector.t option;
  eval_label :
    Expr.t -> int -> (Sexpr.Expr.t, Sexpr.Expr.t) Hashtbl.t -> Bitvector.t;
}

let get_random_value size sample_id nb_samples =
  let max_signed = (1 lsl (size - 1)) - 1 in
  let min_signed = -(1 lsl (size - 1)) in

  match sample_id with
  | 0 -> Bitvector.of_int ~size 0
  | 1 -> Bitvector.of_int ~size 1
  | 2 -> Bitvector.of_int ~size (-1)
  | 3 -> Bitvector.of_int ~size max_signed
  | 4 -> Bitvector.of_int ~size min_signed
  | _ ->
      let threshold = nb_samples / 10 in
      if sample_id <= 4 + threshold then Bitvector.rand size
      else
        let r = Random.int 101 - 50 in
        Bitvector.of_int ~size r

let make nb_samples : t =
  let forbidden_sample_ids = ref IntSet.empty in

  let nb_models = ref nb_samples in

  (* hash node to Bitvector value *)
  let nodes_val : (int, Bitvector.t) Hashtbl.t array =
    Array.init (10 * !nb_models) (fun _ -> Hashtbl.create 32)
  in

  let rec eval (node : Sexpr.Expr.t) (sample_id : int) : Bitvector.t =
    let hash = Hashtbl.hash node in

    match Hashtbl.find_opt nodes_val.(sample_id) hash with
    | Some result -> result
    | None ->
        let result =
          match node with
          | Expr.Cst n -> n
          | Expr.Var v -> get_random_value v.size sample_id !nb_models
          | Expr.Unary { f; x; _ } -> Term.Bv.unary f (eval x sample_id)
          | Expr.Binary { f; x; y; _ } -> (
              let x_term = eval x sample_id in
              let y_term = eval y sample_id in
              match f with
              | Sexpr.Expr.Lsl | Sexpr.Expr.Lsr | Sexpr.Expr.Asr
              | Sexpr.Expr.Rol | Sexpr.Expr.Ror ->
                  let mask = Term.Bv.of_int ~size:(Bitvector.size_of y_term) 63 in
                  let masked_y = Term.Bv.binary Sexpr.Expr.And y_term mask in
                  Term.Bv.binary f x_term masked_y
              | _ -> Term.Bv.binary f x_term y_term)
          | Expr.Ite { c; t; e; _ } ->
              (* condition is a 1-bit truth value; evaluate only the taken
                 branch (avoids spurious division-by-zero in the other one) *)
              if Bitvector.is_zero (eval c sample_id) then eval e sample_id
              else eval t sample_id
          | _ -> failwith "Not implemented"
        in
        Hashtbl.add nodes_val.(sample_id) hash result;
        result
  in

  let rec eval_label ?(debug = false) (node : Sexpr.Expr.t) (sample_id : int)
      (relations : (Sexpr.Expr.t, Sexpr.Expr.t) Hashtbl.t) : Bitvector.t =
    (* Format.eprintf "Entering node: %a@." Term.pp node; *)
    match Hashtbl.find_opt relations node with
    | Some labeled_node ->
        let ret_val = eval labeled_node sample_id in
        (* Format.eprintf "Node is a label %a mapped to %a with value %s@." Term.pp
          node Term.pp labeled_node
          (Bitvector.to_string ret_val); *)
        ret_val
    | None ->
        (* Format.eprintf "Evaluating node %a@." Term.pp node; *)
        let result =
          match node with
          | Expr.Unary { f; x; _ } ->
              Term.Bv.unary f (eval_label ~debug x sample_id relations)
          | Expr.Binary { f; x; y; _ } ->
              Term.Bv.binary f
                (eval_label ~debug x sample_id relations)
                (eval_label ~debug y sample_id relations)
          | Expr.Cst n -> n
          | Expr.Ite { c; t; e; _ } ->
              if Bitvector.is_zero (eval_label ~debug c sample_id relations) then
                eval_label ~debug e sample_id relations
              else eval_label ~debug t sample_id relations
          | _ ->
              failwith
                (Printf.sprintf "Not implemented %s"
                   (Greysynth_utils.Utils.pp node))
        in
        result
  in

  let reset sample_id = Hashtbl.clear nodes_val.(sample_id) in

  let node_to_fingerprint (expr : Sexpr.Expr.t) : Fingerprint.t =
    let samples = ref Fingerprint.IdMap.empty in
    let current_sid = ref 0 in
    let count = ref 0 in

    while !count < !nb_models do
      try
        let value = eval expr !current_sid in
        samples := Fingerprint.IdMap.add !current_sid value !samples;
        incr count;
        incr current_sid
      with Division_by_zero ->
        incr current_sid;
        (* If the difference between tried IDs and successful IDs exceeds retries *)
        if !current_sid - !count > const_max_retries then
          failwith "Too many division by zero errors"
    done;

    !samples
  in

  let node_to_outhash (expr : Expr.t) : int =
    let fails = ref 0 in
    let rec aux sample_id =
      if IntSet.mem sample_id !forbidden_sample_ids then (
        incr fails;
        aux (sample_id + 1))
      else if sample_id - !fails >= !nb_models then []
      else if sample_id >= !nb_models * 10 then
        failwith "Too many sampling retries!"
      else
        try
          let value = eval expr sample_id in
          value :: aux (sample_id + 1)
        with Division_by_zero ->
          incr fails;
          forbidden_sample_ids := IntSet.add sample_id !forbidden_sample_ids;
          aux (sample_id + 1)
    in

    let out_array = aux 0 |> Array.of_list in
    Hashtbl.hash (Array.to_list out_array)
  in

  let eval_all expr =
    for sample_id = 0 to !nb_models - 1 do
      let _ = eval expr sample_id in
      ()
    done
  in

  let is_node_constant : Expr.t -> Bitvector.t option =
   fun expr ->
    let hash = Hashtbl.hash expr in
    let values = ref [] in
    (try
       for sample_id = 0 to !nb_models - 1 do
         if not (IntSet.mem sample_id !forbidden_sample_ids) then
           values := Hashtbl.find nodes_val.(sample_id) hash :: !values
       done
     with Not_found -> values := []);
    match !values with
    | [] -> None
    | first :: rest ->
        if List.for_all (fun x -> Bitvector.equal x first) rest then
          Some first
        else None
  in

  {
    eval;
    eval_all;
    reset;
    node_to_outhash;
    node_to_fingerprint;
    is_node_constant;
    eval_label;
  }
