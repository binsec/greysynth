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

(* hardcoded_rules_simplifier - Rewrite known (Tigress) MBA patterns with
 * hand-written rules, to a fixpoint. Standalone baseline to compare with
 * greysynth; takes the same expression inputs (see Cli_input):
 *
 *   hardcoded_rules_simplifier -obf <src> [-gt <src>] [-size n] [config.json]
 *
 * config.json is optional; only its "equiv_timeout" is read. Prints a JSON
 * report whose fields match greysynth's (obf_sz, syn_expr, syn_sz, equiv, ...).
 *)

open Sexpr.Expr
open Sample_evaluator
let evaluator = Sample_evaluator.make 100
let match_counter = ref 0
let rule_counter = Hashtbl.create 23


let counter_add tbl key =
  let count = match Hashtbl.find_opt tbl key with
    | None -> 0
    | Some v -> v
  in
  Hashtbl.replace tbl key (count + 1)

let rec size_of_expr e =
  match e with
  | Sexpr.Expr.Unary r -> 
          
          let subexpr = r.x in
          1 + size_of_expr subexpr
  | Sexpr.Expr.Binary r -> 
          
          let subexpr1 = r.x in
          let subexpr2 = r.y in
          1 + size_of_expr subexpr2 + size_of_expr subexpr1
  | Sexpr.Expr.Ite r ->
          
          let subexpr1 = r.c in
          let subexpr2 = r.t in
          let subexpr3 = r.e in
          1 + size_of_expr subexpr3 + size_of_expr subexpr2 + size_of_expr subexpr1
  | Sexpr.Expr.Var _ -> 1
  | Sexpr.Expr.Load _ -> 1
  | Sexpr.Expr.Cst _ -> 1


let check_rule (id: string) (x_list : Sexpr.Expr.t list) (y_list : Sexpr.Expr.t list) (cst_list : (Bitvector.t * int) list) (original: Sexpr.Expr.t) (new_shape: Sexpr.Expr.t) : Sexpr.Expr.t =
  (* Check that all x in x_list have the same hash *)
  let x_all_same = match x_list with
    | [] -> true
    | hd :: tl ->
        let first_hash = Sexpr.Expr.hash hd in
        List.for_all (fun x -> Sexpr.Expr.hash x = first_hash) tl
  in

  (* Check that all y in y_list have the same hash *)
  let y_all_same = match y_list with
    | [] -> true
    | hd :: tl ->
        let first_hash = Sexpr.Expr.hash hd in
        List.for_all (fun y -> Sexpr.Expr.hash y = first_hash) tl
  in

  (* Check that for all (c, integer) in cst_list, c == Bitvector.create integer (size_of c) *)
  let cst_valid = List.for_all (fun (c, integer) ->
    let size_of_c = Bitvector.size_of c in
    let expected_bv = Bitvector.create (Z.of_int integer) size_of_c in
    Bitvector.equal c expected_bv
  ) cst_list
  in

  let min_x = match x_list with
    | [] -> Sexpr.Expr.var "x" 1 "x"  (* default value *)
    | x :: _ -> x
  in
  let min_y = match y_list with
    | [] -> Sexpr.Expr.var "y" 1 "y"  (* default value *)
    | y :: _ -> y
  in


  let rec replace node =
    match node with
    | Sexpr.Expr.Unary r -> Sexpr.Expr.unary r.f (replace r.x)
    | Sexpr.Expr.Binary r -> Sexpr.Expr.binary r.f (replace r.x) (replace r.y)
    | Sexpr.Expr.Var r -> if String.equal r.name "x" then min_x else if String.equal r.name "y" then min_y else failwith "Wrong new shape format"
    | _ -> node
  in

  if x_all_same && y_all_same && cst_valid then
    begin
      Format.eprintf "\n\nRule matched: %s\n" id;

      Format.eprintf "[SIMPLIFICATION] Size of all x:";
      List.iter (fun x -> Format.eprintf " %d" (size_of_expr x)) x_list;
      Format.eprintf "\n";
      Format.eprintf "[SIMPLIFICATION] Size of all y:";
      List.iter (fun y -> Format.eprintf " %d" (size_of_expr y)) y_list;
      Format.eprintf "\n";

      counter_add rule_counter id;
      match_counter := !match_counter + 1;
      let new_expr = replace new_shape in
      let () = Format.eprintf "[SIMPLIFICATION] Size before: %d, size after: %d\n" (size_of_expr original) (size_of_expr new_expr) in
      new_expr
    end 
  else original


let parse expr =
    (* Shape placeholders, replaced by the matched operands in [check_rule].
       Not 1-bit: Binsec's smart constructors would fold a 1-bit x + y into
       x ^ y (and x * y into x & y), corrupting the shapes. *)
    let dummy_x = Sexpr.Expr.var "x" 64 "x" in
    let dummy_y = Sexpr.Expr.var "y" 64 "y" in

    let shape_plus = Sexpr.Expr.binary Sexpr.Expr.Plus dummy_x dummy_y in
    let shape_or = Sexpr.Expr.binary Sexpr.Expr.Or dummy_x dummy_y in
    let shape_and = Sexpr.Expr.binary Sexpr.Expr.And dummy_x dummy_y in
    let shape_xor = Sexpr.Expr.binary Sexpr.Expr.Xor dummy_x dummy_y in
    let shape_not = Sexpr.Expr.unary Sexpr.Expr.Not dummy_x in
    let shape_minus = Sexpr.Expr.binary Sexpr.Expr.Minus dummy_x dummy_y in
    let shape_mul = Sexpr.Expr.binary Sexpr.Expr.Mul dummy_x dummy_y in


    match expr with
    (* REWRITE RULES FOR + *)
    | Binary{
      f = Minus;
      x = Binary {
        f = Lsl;
        x = Binary {
          f = Or;
          x = x1;
          y = y1;
          _;
        };
        y = Cst c;
        _;
      };
      y = Binary {
        f = Xor;
        x = x2;
        y = y2;
        _;
      };
      _
    } -> check_rule "plus_1" [x1; x2] [y1;y2] [(c,1)] expr shape_plus

    | Binary {
      f = Minus;
      x = Binary{
        f = Plus;
        x = Binary{
          f = Or;
          x = x1;
          y = y1;
          _;
        };
        y = Binary{
          f = Or;
          x = x2;
          y = y2;
          _;
        };
        _;
      };
      y = Binary{
        f = Xor;
        x = x3;
        y = y3;
        _
      };
      _;
    } -> check_rule "plus_2" [x1; x2; x3] [y1;y2; y3] [] expr shape_plus

    | Binary {
      f = Plus;
      x = Binary{
        f = Plus;
        x = Binary {
          f = Xor;
          x = x1;
          y = Unary{ f = Not; x = y1; _ };
          _;
        };
        y = Binary{
          f = Lsl;
          x = Binary{
            f = Or;
            x = x2;
            y = y2;
            _;
          };
          y = Cst c1;
          _;
        };
        _;
      };
      y = Cst c2;
      _;
    } -> check_rule "plus_3" [x1; x2] [y1;y2] [(c1, 1); (c2, 2)] expr shape_plus

    | Binary {
      f = Plus;
      x = Binary { f = Xor; x = x1; y = y1; _; };
      y = Binary {
        f = Lsl;
        x = Binary { f = And; x = x2; y = y2; _; };
        y = Cst c1 ;
        _;
      };
      _;
    } -> check_rule "plus_4" [x1; x2] [y1;y2] [(c1, 1)] expr shape_plus

    | Binary {
      f = Plus;
      x = Binary{ f = Xor; x = x1; y = y1; _; };
      y = Binary{
        f = Plus;
        x = Binary{ f = And; x = x2; y = y2; _; };
        y = Binary{ f = And; x = x3; y = y3; _; };
        _;
      };
      _;
    } -> check_rule "plus_5" [x1; x2; x3] [y1;y2; y3] [] expr shape_plus

    | Binary {
      f = Plus;
      x = Binary{
        f = Plus;
        x = Binary{ f = Xor; x = x1; y = Unary{ f = Not; x = y1; _ }; _; };
        y = Binary{
          f = Plus;
          x = Binary{ f = Or; x = x2; y = y2; _; };
          y = Binary{ f = Or; x = x3; y = y3; _; };
          _;
        };
        _;
      };
      y = Cst c1;
      _;
    } -> check_rule "plus_6" [x1; x2; x3] [y1;y2; y3] [(c1,1)] expr shape_plus

    | Binary {
      f = Minus;
      x = Binary{
        f = Minus;
        x = x1;
        y = Unary{ f = Not; x = y1; _;};
        _;
      };
      y = Cst c1;
      _
    } -> check_rule "plus_7" [x1] [y1] [(c1,1)] expr shape_plus
    
    | Binary{
      f = Plus;
      x = Binary{ f = Or; x = x1; y = y1; _};
      y = Binary{ f = And; x = x2; y = y2; _};
      _
    } -> check_rule "plus_8" [x1; x2] [y1; y2] [] expr shape_plus




  

    (* REWRITE RULES FOR OR *)
    | Binary{
      f = Plus;
      x = Binary{
        f = And;
        x = x1;
        y = Unary{ f = Not; x = y1; _};
        _
      };
      y = y2;
      _
    } -> check_rule "or_1" [x1] [y1;y2] [] expr shape_or


    | Binary{
      f = Plus;
      x = Binary{
        f = Plus;
        x = Binary{ f = Plus; x = x1; y = y1; _};
        y = Cst c1;
        _
      };
      y = Binary{
        f = Or;
        x = Binary{ f = Minus; x = Unary{ f = Minus; x = x2; _}; y = Cst c2; _};
        y = Binary{ f = Minus; x = Unary{ f = Minus; x = y2; _}; y = Cst c3; _};
        _
      };
      _
    } -> check_rule "or_2" [x1; x2] [y1;y2] [(c1,1); (c2, 1); (c3, 1)] expr shape_or




    (* REWRITE RULES FOR AND *)
    | Binary {
      f = Minus;
      x = Binary{
        f = Or;
        x = Unary{ f = Not; x = x1; _};
        y = y1;
        _
      };
      y = Unary { f = Not; x = x2; _};
      _
    } -> check_rule "and_3" [x1; x2] [y1] [] expr shape_and

    | Binary {
      f = Plus;
      x = Binary{
        f = Plus;
        x = Binary{
          f = Or;
          x = Unary{ f = Not; x = x1; _};
          y = y1;
          _
        };
        y = x2;
        _
      };
      y = Cst c1;
      _
    } -> check_rule "and_4" [x1; x2] [y1] [(c1, 1)] expr shape_and




  (* REWRITE RULES FOR XOR *)
  | Binary {
    f = Minus;
    x = Binary{
      f = Minus;
      x = Binary{ f = Minus; x = x1; y = y1; _};
      y = Binary{
        f = Plus;
        x = Binary{
          f = Or;
          x = x2;
          y = Unary{ f = Not; x = y2; _ };
          _
        };
        y = Binary{
          f = Or;
          x = x3;
          y = Unary{ f = Not; x = y3; _};
          _
        };
        _
      };
      _
    };
    y = Cst c1;
    _
  } -> check_rule "xor_1" [x1; x2; x3] [y1; y2; y3] [(c1, 2)] expr shape_xor

  | Binary {
    f = Minus;
    x = Binary{ f = Or; x = x1; y = y1; _};
    y = Binary{ f = And; x = x2; y = y2; _};
    _
  } -> check_rule "xor_2" [x1; x2] [y1; y2] [] expr shape_xor

  | Binary {
    f = Minus;
    x = Binary{
      f = Minus;
      x = Binary{ f = Minus; x = x1; y = y1; _};
      y = Binary{
        f = Lsl;
        x = Binary{
          f = Or;
          x = x2;
          y = Unary{ f = Not; x = y2; _};
          _
        };
        y = Cst c1;
        _
      };
      _
    };
    y = Cst c2;
    _
  } -> check_rule "xor_3" [x1; x2] [y1; y2] [(c1, 1); (c2, 2)] expr shape_xor




  (* REWRITE RULES FOR NOT *)
  | Binary {
    f = Minus;
    x = Unary{ f = Minus; x = x1; _};
    y = Cst c1;
    _
  } -> check_rule "not_1" [x1] [] [(c1, 1)] expr shape_not




  (* REWRITE RULES FOR MINUS *)
  | Binary{
    f = Minus;
    x = Binary{
      f = And;
      x = x1;
      y = Unary{f = Not; x = y1; _};
      _
    };
    y = Binary{
      f = And;
      x = Unary{f = Not; x = x2; _};
      y = y2;
      _
    };
    _
  } -> check_rule "minus_1" [x1; x2] [y1; y2] [] expr shape_minus

  | Binary{
    f = Minus;
    x = Binary{
      f = Xor;
      x = x1;
      y = y1;
      _
    };
    y = Binary{
      f = Plus;
      x = Binary{
        f = And;
        x = Unary{ f = Not; x = x2; _};
        y = y2;
        _
      };
      y = Binary{
        f = And;
        x = Unary{ f = Not; x = x3; _};
        y = y3;
        _
      };
      _
    };
    _
  } -> check_rule "minus_2" [x1; x2; x3] [y1; y2; y3] [] expr shape_minus

  | Binary{
    f = Plus;
    x = Binary{
      f = Plus;
      x = x1;
      y = Unary{ f = Not; x = y1; _};
      _
    };
    y = Cst c1;
    _
  } -> check_rule "minus_3" [x1] [y1] [(c1, 1)] expr shape_minus

  | Binary{
    f = Minus;
    x = Binary{
      f = Plus;
      x = Binary{
        f = And;
        x = x1;
        y = Unary{ f = Not; x = y1; _};
        _
      };
      y = Binary{
        f = And;
        x = x2;
        y = Unary{ f = Not; x = y2; _};
        _
      };
      _
    };
    y = Binary{
      f = Xor;
      x = x3;
      y = y3;
      _
    };
    _
  } -> check_rule "minus_4" [x1; x2; x3] [y1; y2; y3] [] expr shape_minus

  | Binary{
    f = Minus;
    x = Binary{ f = Xor; x = x1; y = y1; _};
    y = Binary{
      f = Lsl;
      x = Binary{
        f = And;
        x = Unary{ f = Not; x = x2; _};
        y = y2;
        _
      };
      y = Cst c1;
      _
    };
    _
  } -> check_rule "minus_5" [x1; x2] [y1; y2] [(c1, 1)] expr shape_minus

  | Binary{
    f = Minus;
    x = Binary{
      f = Lsl;
      x = Binary{
        f = And;
        x = x1;
        y = Unary{ f = Not; x = y1; _};
        _
      };
      y = Cst c1;
      _
    };
    y = Binary{ f = Xor; x = x2; y = y2; _};
    _
  } -> check_rule "minus_6" [x1; x2] [y1; y2] [(c1, 1)] expr shape_minus



  (* REWRITE RULES FOR MUL *)

  | Binary{
    f = Plus;
    x = Binary{
      f = Mul;
      x = Binary{
        f = And;
        x = x1;
        y = y1;
        _
      };
      y = Binary{
        f = Or;
        x = x2;
        y = y2;
        _
      };
      _
    };
    y = Binary{
      f = Mul;
      x =Binary{
        f = And;
        x = x3;
        y = Unary{f=Not; x=y3;_};
        _
      } ;
      y = Binary{
        f=And;
        x=Unary{f=Not;x=x4;_};
        y=y4;
        _
      };
      _
    };
    _
  } -> check_rule "mul_1" [x1; x2; x3; x4] [y1; y2; y3; y4] [] expr shape_mul


  | _ -> expr

let one_pass expr =
  let _ = evaluator.eval_all expr in
  let rec aux expr =
    let outhash = evaluator.node_to_outhash expr in

    let res = parse expr in

    let outhash_after = evaluator.node_to_outhash res in
    assert (outhash = outhash_after);

    match res with
    | Sexpr.Expr.Unary r -> Sexpr.Expr.unary r.f (aux r.x)
    | Sexpr.Expr.Binary r -> Sexpr.Expr.binary r.f (aux r.x) (aux r.y)
    | _ -> res
  in
  let res = aux expr in
  res


let simplify expr =
  let start_time = Unix.gettimeofday () in

  let rec fix expr =
    let new_expr = one_pass expr in
    let () = Format.eprintf "[Hardcoded Rules Simplifier] Size before: %d, size after: %d\n" (size_of_expr expr) (size_of_expr new_expr) in
    if new_expr = expr then expr
    else fix new_expr in
  let res = fix expr in

  let end_time = Unix.gettimeofday () in
  let processing_time = end_time -. start_time in

  Format.eprintf "[Hardcoded Rules Simplifier] Processing time: %.6f seconds\n" processing_time;
  flush stdout;
  flush stderr;
  res

let usage_msg =
  "Usage:\n\
  \  hardcoded_rules_simplifier -obf -string \"<expr>\" [-gt -string \"<expr>\"] [config.json]\n\
  \  hardcoded_rules_simplifier -obf -cil <file> <func> [-gt -cil <file> <func>] [config.json]\n\n\
   Options:"

let equiv_timeout_of_config = function
  | None -> 5
  | Some file -> (
      match Yojson.Basic.Util.member "equiv_timeout" (Yojson.Basic.from_file file) with
      | `Int t -> t
      | _ -> 5)

let () =
  let input = Cli_input.parse_command_line ~usage:usage_msg in
  let config =
    match input.positional with
    | [] -> None
    | [ config ] -> Some config
    | _ -> Cli_input.fail_usage usage_msg "Error: Too many anonymous arguments\n"
  in
  let equiv_timeout = equiv_timeout_of_config config in
  let module U = Greysynth_utils.Utils in
  let obf = input.obf in
  let start_time = Unix.gettimeofday () in
  let simplified = simplify obf in
  let time = Unix.gettimeofday () -. start_time in

  (* Check the result against the ground truth (falling back to the
     obfuscated input), or against the input alone when there is no gt. *)
  let equiv, equiv_time =
    match input.gt with
    | Some gt -> U.equiv_gt_then_obf simplified gt obf (Some equiv_timeout)
    | None ->
        let res, t, _ = U.are_equivalent simplified obf (Some equiv_timeout) in
        (res, t)
  in

  let json =
    `Assoc
      ([
         ("obf_sz", `Int (U.size_of_expr obf));
         ("obf_expr", `String (U.pp obf));
         ("syn_expr", `String (U.pp simplified));
         ("syn_sz", `Int (U.size_of_expr simplified));
         ("equiv", `String (U.equiv_to_str equiv));
         ("equiv_time", `Float equiv_time);
         ("time", `Float time);
       ]
      @
      match input.gt with
      | Some gt ->
          [ ("gt_sz", `Int (U.size_of_expr gt)); ("gt_expr", `String (U.pp gt)) ]
      | None -> [])
  in
  Format.printf "%s\n" (Yojson.Basic.pretty_to_string json)