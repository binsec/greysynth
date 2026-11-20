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

(* MBA Classification types *)
type mba_class =
  | Bitwise (* Pure bitwise: &, |, ^, ~ *)
  | Bitwise_const (* Pure bitwise: &, |, ^, ~ *)
  | Linear (* Sum of (const * bitwise_op) *)
  | Linear_const (* Sum of (const * bitwise_op) *)
  | Polynomial (* Sum of (const * product_of_bitwise_ops) *)
  | Polynomial_const (* Sum of (const * product_of_bitwise_ops) *)
  | Non_polynomial
    (* MBA operations (+, -, *, &, |, ^, ~, <<) but not polynomial *)
  | Non_mba (* Contains non-MBA operations: div, mod, >>, etc. *)

let rec is_bitwise (e : Sexpr.Expr.t) : bool =
  match e with
  | Sexpr.Expr.Var _ -> true
  | Sexpr.Expr.Cst _ -> false
  | Sexpr.Expr.Unary { f = Sexpr.Expr.Not; x; _ } -> is_bitwise x
  | Sexpr.Expr.Binary { f; x; y; _ } -> (
      match f with
      | Sexpr.Expr.And | Sexpr.Expr.Or | Sexpr.Expr.Xor ->
          is_bitwise x && is_bitwise y
      | _ -> false)
  | _ -> false

let rec is_bitwise_const (e : Sexpr.Expr.t) : bool =
  match e with
  | Sexpr.Expr.Var _ -> true
  | Sexpr.Expr.Cst _ -> true
  | Sexpr.Expr.Unary { f = Sexpr.Expr.Not; x; _ } -> is_bitwise_const x
  | Sexpr.Expr.Binary { f; x; y; _ } -> (
      match f with
      | Sexpr.Expr.And | Sexpr.Expr.Or | Sexpr.Expr.Xor ->
          is_bitwise_const x && is_bitwise_const y
      | _ -> false)
  | _ -> false

(* Check if expression is a constant *)
let is_constant (e : Sexpr.Expr.t) : bool =
  match e with Sexpr.Expr.Cst _ -> true | _ -> false

let is_linear_term (e : Sexpr.Expr.t) : bool =
  match e with
  (* Base cases *)
  | _ when is_bitwise e -> true
  | Sexpr.Expr.Cst _ -> true
  (* const * bitwise is linear term *)
  | Sexpr.Expr.Binary { f = Sexpr.Expr.Mul; x; y; _ } ->
      (is_constant x && is_bitwise y) || (is_bitwise x && is_constant y)
  | _ -> false

let is_linear_const_term (e : Sexpr.Expr.t) : bool =
  match e with
  (* Base cases *)
  | _ when is_bitwise_const e -> true
  | Sexpr.Expr.Binary { f = Sexpr.Expr.Mul; x; y; _ } ->
      (is_constant x && is_bitwise_const y)
      || (is_bitwise_const x && is_constant y)
  | _ -> false

let rec is_polynomial_term (e : Sexpr.Expr.t) : bool =
  match e with
  | _ when is_linear_term e -> true
  | Sexpr.Expr.Binary { f = Sexpr.Expr.Mul; x; y; _ } ->
      is_polynomial_term x && is_polynomial_term y
  | _ -> false

let rec is_polynomial_const_term (e : Sexpr.Expr.t) : bool =
  match e with
  | _ when is_linear_const_term e -> true
  | Sexpr.Expr.Binary { f = Sexpr.Expr.Mul; x; y; _ } ->
      is_polynomial_const_term x && is_polynomial_const_term y
  | _ -> false

let rec is_sum_of pred (e : Sexpr.Expr.t) : bool =
  match e with
  | _ when pred e -> true
  | Sexpr.Expr.Binary { f = Sexpr.Expr.Plus; x; y; _ }
  | Sexpr.Expr.Binary { f = Sexpr.Expr.Minus; x; y; _ } ->
      is_sum_of pred x && is_sum_of pred y
  | Sexpr.Expr.Unary { f = Sexpr.Expr.Minus; x; _ } -> is_sum_of pred x
  | _ -> false

(* Check if expression contains only MBA operations: +, -, *, &, |, ^, ~, << *)
let rec is_mba_operations (e : Sexpr.Expr.t) : bool =
  match e with
  | Sexpr.Expr.Var _ | Sexpr.Expr.Cst _ -> true
  | Sexpr.Expr.Unary { f; x; _ } -> (
      match f with
      | Sexpr.Expr.Minus | Sexpr.Expr.Not -> is_mba_operations x
      | Sexpr.Expr.Sext _ | Sexpr.Expr.Uext _ | Sexpr.Expr.Restrict _ -> false)
  | Sexpr.Expr.Binary { f; x; y; _ } -> (
      match f with
      | Sexpr.Expr.Plus | Sexpr.Expr.Minus | Sexpr.Expr.Mul | Sexpr.Expr.And
      | Sexpr.Expr.Or | Sexpr.Expr.Xor | Sexpr.Expr.Lsl ->
          (* Left shift is MBA operation *)
          is_mba_operations x && is_mba_operations y
      | Sexpr.Expr.Udiv | Sexpr.Expr.Sdiv | Sexpr.Expr.Umod | Sexpr.Expr.Smod
      | Sexpr.Expr.Lsr | Sexpr.Expr.Asr ->
          (* These are NOT MBA operations *)
          false
      | _ -> false)
  | Sexpr.Expr.Load _ -> failwith "Load not supported in MBA classification"
  | Sexpr.Expr.Ite _ -> failwith "ITE not supported in MBA classification"

(* Main classification function *)
let classify (e : Sexpr.Expr.t) : mba_class =
  if is_bitwise e then Bitwise
  else if is_bitwise_const e then Bitwise_const
  else if is_sum_of is_linear_term e then Linear
  else if is_sum_of is_linear_const_term e then Linear_const
  else if is_sum_of is_polynomial_term e then Polynomial
  else if is_sum_of is_polynomial_const_term e then Polynomial_const
  else if is_mba_operations e then Non_polynomial
  else Non_mba

(* Convert classification to string *)
let to_string = function
  | Bitwise -> "bitwise"
  | Bitwise_const -> "bitwise-const"
  | Linear -> "linear"
  | Linear_const -> "linear-const"
  | Polynomial -> "polynomial"
  | Polynomial_const -> "polynomial-const"
  | Non_polynomial -> "non-polynomial"
  | Non_mba -> "non-mba"

(* Convert classification to colored string for terminal *)
let to_colored_string = function
  | Bitwise -> "\027[35mbitwise\027[0m" (* magenta *)
  | Bitwise_const -> "\027[34mbitwise-const\027[0m" (* blue *)
  | Linear -> "\027[32mlinear\027[0m" (* green *)
  | Linear_const -> "\027[32mlinear-const\027[0m" (* green *)
  | Polynomial -> "\027[33mpolynomial\027[0m" (* yellow *)
  | Polynomial_const -> "\027[33mpolynomial-const\027[0m" (* yellow *)
  | Non_polynomial -> "\027[36mnon-polynomial\027[0m" (* cyan *)
  | Non_mba -> "\027[31mnon-mba\027[0m" (* red *)
