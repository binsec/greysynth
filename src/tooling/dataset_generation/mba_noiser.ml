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

(* mba_noiser - Noise an (obfuscated, ground-truth) expression pair by
 * substituting every original variable with a random MBA expression over a
 * FRESH pool of "noise" variables (a, b, c, ...).
 *
 * The SAME substitution is applied to both inputs, so the noised pair stays
 * mutually equivalent (noised_obf == noised_gt). It is deliberately NOT
 * equivalent to the originals: it is now a function of the fresh variables.
 *
 *   with  x -> (a|b)+c,  y -> (a^c)-b :
 *     gt   x+y           ->  ((a|b)+c) + ((a^c)-b)
 *     obf  (x|y)+(x&y)   ->  (same substitution applied)
 *
 * Usage (same input methods as mba_info, but takes an obf AND a gt):
 *   mba_noiser -obf <src> -gt <src> [-nvars 3] [-depth 2] [-seed 42] [-size b]
 *   where <src> = -string "<expr>" | -string-file <file> | -cil <file> <func>
 *
 * Output (JSON):
 *   - seed, noise_variables
 *   - obf_noised           { size, expression }
 *   - gt_noised            { size, expression }
 *   - variable_replacement { x -> expr(a,b,c) }
 *)

type expr_source =
  | StringExpr of string
  | CilExpr of string * string (* file, func *)

let load_expr default_size = function
  | StringExpr s -> String_to_sexpr.parse ?default_size s
  | CilExpr (file, func) -> Cil_to_sexpr.load_from_file file func

let pp = Greysynth_utils.Utils.pp ~show_size:true
let size_of = Greysynth_utils.Utils.size_of_expr

(* ── noise building blocks ─────────────────────────────────────── *)

let noise_ops =
  [|
    Sexpr.Expr.Plus;
    Sexpr.Expr.Minus;
    Sexpr.Expr.Xor;
    Sexpr.Expr.And;
    Sexpr.Expr.Or;
  |]

(* random small MBA expression over [pool] (leaves are pool variables, internal
   nodes are random MBA ops), bounded by [depth]. Deliberately NOT value
   preserving. *)
let rec gen_noise pool depth =
  let leaf () = pool.(Random.int (Array.length pool)) in
  if depth <= 0 || Random.int 3 = 0 then leaf ()
  else
    let op = noise_ops.(Random.int (Array.length noise_ops)) in
    let l = gen_noise pool (depth - 1) in
    (* avoid identical operands (x^x, x|x, x&x, x-x, ...) by construction *)
    let rec distinct_right tries =
      let r = gen_noise pool (depth - 1) in
      if tries > 0 && pp r = pp l then distinct_right (tries - 1) else r
    in
    Sexpr.Expr.binary op l (distinct_right 8)

(* true if any binary node has structurally-identical operands (x^x, x|x, x&x,
   x-x, x+x, ...) anywhere in [e] *)
let rec has_trivial_pattern e =
  match e with
  | Sexpr.Expr.Binary r ->
      pp r.x = pp r.y || has_trivial_pattern r.x || has_trivial_pattern r.y
  | Sexpr.Expr.Unary r -> has_trivial_pattern r.x
  | Sexpr.Expr.Ite r ->
      has_trivial_pattern r.c || has_trivial_pattern r.t
      || has_trivial_pattern r.e
  | _ -> false

(* is [e] semantically constant (same value on all sampled inputs)? catches a
   literal constant AND expressions that only *evaluate* to a constant, such as
   (a-b) & (a-a) == 0, which binsec does not fold syntactically. *)
let is_constant ev e =
  ev.Sample_evaluator.eval_all e;
  match ev.Sample_evaluator.is_node_constant e with Some _ -> true | None -> false

(* number of distinct variable names occurring in [e] *)
let distinct_var_count e =
  let seen = Hashtbl.create 8 in
  let rec aux = function
    | Sexpr.Expr.Var v -> Hashtbl.replace seen v.name ()
    | Sexpr.Expr.Unary r -> aux r.x
    | Sexpr.Expr.Binary r ->
        aux r.x;
        aux r.y
    | Sexpr.Expr.Ite r ->
        aux r.c;
        aux r.t;
        aux r.e
    | _ -> ()
  in
  aux e;
  Hashtbl.length seen

(* Generate a noise expression over [pool] (a set of fresh variables private to
   this replacement) that (a) is not a bare leaf, (b) is not semantically
   constant, and (c) uses ALL of [pool]'s variables. Retries, with a guaranteed
   fallback that chains every pool variable. *)
let gen_replacement ev pool depth =
  let k = Array.length pool in
  let acceptable e =
    (match e with Sexpr.Expr.Var _ | Sexpr.Expr.Cst _ -> false | _ -> true)
    && distinct_var_count e = k
    && (not (has_trivial_pattern e))
    && not (is_constant ev e)
  in
  let rec loop tries =
    let e = gen_noise pool depth in
    if acceptable e then e
    else if tries > 0 then loop (tries - 1)
    else
      (* fallback: fold all pool vars with random ops -> uses all, non-constant *)
      Array.fold_left
        (fun acc v ->
          Sexpr.Expr.binary noise_ops.(Random.int (Array.length noise_ops)) acc v)
        pool.(0)
        (Array.sub pool 1 (k - 1))
  in
  loop 50

(* collect distinct variables (name -> size) occurring in [e] *)
let collect_vars tbl e =
  let rec aux node =
    match node with
    | Sexpr.Expr.Var v ->
        if not (Hashtbl.mem tbl v.name) then Hashtbl.replace tbl v.name v.size
    | Sexpr.Expr.Unary r -> aux r.x
    | Sexpr.Expr.Binary r ->
        aux r.x;
        aux r.y
    | Sexpr.Expr.Ite r ->
        aux r.c;
        aux r.t;
        aux r.e
    | _ -> ()
  in
  aux e

(* replace every original variable by its noise expression (single pass: the
   inserted noise, over fresh variables, is not itself re-substituted) *)
let substitute noise_map e =
  let rec aux node =
    match node with
    | Sexpr.Expr.Var v -> (
        match Hashtbl.find_opt noise_map v.name with Some h -> h | None -> node)
    | Sexpr.Expr.Unary r -> Sexpr.Expr.unary r.f (aux r.x)
    | Sexpr.Expr.Binary r -> Sexpr.Expr.binary r.f (aux r.x) (aux r.y)
    | Sexpr.Expr.Ite r -> Sexpr.Expr.ite (aux r.c) (aux r.t) (aux r.e)
    | _ -> node
  in
  aux e

(* ── CLI ───────────────────────────────────────────────────────── *)

let () =
  let obf_src = ref None in
  let gt_src = ref None in
  let current = ref `Obf in
  let default_size = ref None in
  let seed = ref None in
  let nvars = ref None in
  let depth = ref 2 in
  let cil_pending = ref "" in

  let set_src s =
    match !current with `Obf -> obf_src := Some s | `Gt -> gt_src := Some s
  in

  let usage_msg =
    "Usage:\n\
    \  mba_noiser -obf <src> -gt <src> [-nvars 3] [-depth 2] [-seed 42]\n\
    \  where <src> is one of: -string \"<expr>\" | -string-file <file> | -cil \
     <file> <func>\n\n\
     Options:"
  in
  let spec =
    [
      ( "-obf",
        Arg.Unit (fun () -> current := `Obf),
        " select the obfuscated expression as the target of the next source" );
      ( "-gt",
        Arg.Unit (fun () -> current := `Gt),
        " select the ground-truth expression as the target of the next source"
      );
      ( "-string",
        Arg.String (fun s -> set_src (StringExpr s)),
        "<expr> parse a string expression (DBA syntax with size annotations)" );
      ( "-string-file",
        Arg.String
          (fun file ->
            try
              let ic = open_in file in
              let len = in_channel_length ic in
              let content = really_input_string ic len in
              close_in ic;
              set_src (StringExpr (String.trim content))
            with Sys_error msg ->
              Format.eprintf "Error reading file %s: %s\n" file msg;
              exit 1),
        "<file> read a string expression from a file" );
      ( "-cil",
        Arg.Tuple
          [
            Arg.String (fun f -> cil_pending := f);
            Arg.String (fun func -> set_src (CilExpr (!cil_pending, func)));
          ],
        "<file> <func> parse a C file via CIL" );
      ( "-nvars",
        Arg.Int (fun n -> nvars := Some n),
        "<n> fresh noise variables per replacement (default: random 2 or 3)" );
      ( "-depth",
        Arg.Int (fun n -> depth := n),
        "<d> max depth of each noise expression (default 2)" );
      ( "-size",
        Arg.Int (fun n -> default_size := Some n),
        "<bits> default bit size for unannotated variables" );
      ( "-seed",
        Arg.Int (fun n -> seed := Some n),
        "<n> RNG seed (default: random each run; the chosen seed is printed for \
         replay)" );
    ]
  in
  Arg.parse spec (fun _ -> ()) usage_msg;

  (match (!obf_src, !gt_src) with
  | Some _, Some _ -> ()
  | _ ->
      Format.eprintf "Error: both -obf and -gt expressions are required\n";
      Arg.usage spec usage_msg;
      exit 1);

  Random.self_init ();
  let actual_seed =
    match !seed with Some s -> s | None -> Random.int 1_000_000_000
  in
  Random.init actual_seed;

  try
    let obf = load_expr !default_size (Option.get !obf_src) in
    let gt = load_expr !default_size (Option.get !gt_src) in

    (* original variables (union over obf and gt) *)
    let vars_tbl = Hashtbl.create 16 in
    collect_vars vars_tbl obf;
    collect_vars vars_tbl gt;
    let vars =
      Hashtbl.fold (fun name sz acc -> (name, sz) :: acc) vars_tbl []
      |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    in

    (* fresh noise variables share one width (that of the substituted vars) *)
    (* one fresh noise pool per distinct variable width (8/16/32/64/...); when
       several widths are present, names are suffixed by width to stay distinct
       (a32, b8, ...); with a single width the plain a, b, c are used. *)
    (match vars with [] -> failwith "no variables to replace" | _ -> ());

    (* Each replacement gets its OWN disjoint set of fresh noise variables (no
       sharing across replacements), 2-3 of them, at the width of the variable
       being replaced. Names are allocated globally (a, b, c, d, ...) so they
       never clash across replacements or widths. *)
    let ev = Sample_evaluator.make 16 in
    let counter = ref 0 in
    let fresh_name () =
      let i = !counter in
      incr counter;
      if i < 26 then String.make 1 (Char.chr (Char.code 'a' + i))
      else Printf.sprintf "n%d" i
    in
    let all_noise_names = ref [] in
    let noise_map = Hashtbl.create 16 in
    List.iter
      (fun (name, w) ->
        let k = match !nvars with Some n -> n | None -> 2 + Random.int 2 in
        let pool =
          Array.init k (fun _ ->
              let n = fresh_name () in
              all_noise_names := n :: !all_noise_names;
              Sexpr.Expr.var n w n)
        in
        Hashtbl.replace noise_map name (gen_replacement ev pool !depth))
      vars;
    let noise_names = List.rev !all_noise_names in

    let obf_noised = substitute noise_map obf in
    let gt_noised = substitute noise_map gt in

    let variable_replacement =
      `Assoc
        (List.map
           (fun (name, _) ->
             (name, `String (pp (Hashtbl.find noise_map name))))
           vars)
    in

    let json =
      `Assoc
        [
          ("seed", `Int actual_seed);
          ( "noise_variables",
            `List (List.map (fun n -> `String n) noise_names) );
          ( "obf_noised",
            `Assoc
              [
                ("size", `Int (size_of obf_noised));
                ("expression", `String (pp obf_noised));
              ] );
          ( "gt_noised",
            `Assoc
              [
                ("size", `Int (size_of gt_noised));
                ("expression", `String (pp gt_noised));
              ] );
          ("variable_replacement", variable_replacement);
        ]
    in
    Format.printf "%s\n" (Yojson.Basic.pretty_to_string json)
  with
  | Failure msg ->
      Format.eprintf "Error: %s\n" msg;
      exit 1
  | e ->
      Format.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1
