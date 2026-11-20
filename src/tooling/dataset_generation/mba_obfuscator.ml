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

(* mba_obf.ml - Obfuscate MBA sub-expressions via MBA-Obfuscator docker container
 *
 * Usage:
 *   mba_obf -string "<expression>"
 *   mba_obf -string-file <file>
 *   mba_obf -cil <c_file> <function_name>
 *   mba_obf -flag <p|l|np_zero|np_recur|np_replace>   (default: np_zero)
 *
 * Strategy (top-down skeleton):
 *   Starting from the root, greedily build a linear MBA skeleton by including
 *   all linear-MBA-compatible nodes (Plus, Minus, And, Or, Xor, Not,
 *   Mul-by-const, Var, Cst) and stubbing anything else as a slot variable.
 *   Call MBA-Obfuscator on the skeleton, then recursively apply the same
 *   algorithm on each stubbed child.
 *
 * Output: same JSON as mba_info, on the obfuscated expression.
 *)

let docker_image = "mba_obfuscator:v1"

let obfuscate_script =
  match Sys.getenv_opt "MBA_OBFUSCATOR_SCRIPT" with
  | Some s -> s
  | None ->
      "/home/MBA-Obfuscator/mba_obfuscator/mba_obfuscator/obfuscate_external.py"

(* Resolve a working python interpreter for the obfuscator script.
   Tries: $MBA_OBFUSCATOR_PYTHON, pyenv py36 venv, python3, python. *)
let python_bin =
  let candidates =
    (match Sys.getenv_opt "MBA_OBFUSCATOR_PYTHON" with
    | Some p -> [ p ]
    | None -> [])
    @ [
        "/home/opam/.pyenv/versions/3.6.15/envs/py36/bin/python";
        "python3";
        "python";
      ]
  in
  List.find_opt
    (fun p -> Sys.command (Printf.sprintf "%s --version >/dev/null 2>&1" p) = 0)
    candidates

(* If true, call the script directly instead of docker *)
let use_direct_python = Sys.file_exists obfuscate_script && python_bin <> None
let mba_var_names = [| "x"; "y"; "z"; "t" |]

(* ------------------------------------------------------------------ *)
(* Basic helpers                                                        *)
(* ------------------------------------------------------------------ *)

(* A linear MBA is a linear combination of bitwise terms:
 *   linear  ::= cst | -linear | linear+linear | linear-linear | cst*bitwise | bitwise
 *   bitwise ::= var | ~bitwise | bitwise&bitwise | bitwise|bitwise | bitwise^bitwise
 * Constants are only valid at the linear level; arithmetic is not valid inside bitwise. *)
[@@@warning "-32"]

let rec is_linear_mba (e : Sexpr.Expr.t) : bool = is_linear e

and is_linear (e : Sexpr.Expr.t) : bool =
  match e with
  | Sexpr.Expr.Cst _ -> true
  | Sexpr.Expr.Unary { f = Sexpr.Expr.Minus; x; _ } -> is_linear x
  | Sexpr.Expr.Binary { f = Sexpr.Expr.Plus; x; y; _ } ->
      is_linear x && is_linear y
  | Sexpr.Expr.Binary { f = Sexpr.Expr.Minus; x; y; _ } ->
      is_linear x && is_linear y
  | Sexpr.Expr.Binary { f = Sexpr.Expr.Mul; x = Sexpr.Expr.Cst _; y; _ } ->
      is_bitwise y
  | Sexpr.Expr.Binary { f = Sexpr.Expr.Mul; x; y = Sexpr.Expr.Cst _; _ } ->
      is_bitwise x
  | _ -> is_bitwise e

and is_bitwise (e : Sexpr.Expr.t) : bool =
  match e with
  | Sexpr.Expr.Var _ -> true
  | Sexpr.Expr.Unary { f = Sexpr.Expr.Not; x; _ } -> is_bitwise x
  | Sexpr.Expr.Binary { f = Sexpr.Expr.And; x; y; _ } ->
      is_bitwise x && is_bitwise y
  | Sexpr.Expr.Binary { f = Sexpr.Expr.Or; x; y; _ } ->
      is_bitwise x && is_bitwise y
  | Sexpr.Expr.Binary { f = Sexpr.Expr.Xor; x; y; _ } ->
      is_bitwise x && is_bitwise y
  | _ -> false

[@@@warning "+32"]

type slot = VarSlot of string * int | StubSlot of Sexpr.Expr.t

exception Too_many_slots

(* Build the largest linear MBA skeleton rooted at [e], stubbing non-linear
 * sub-expressions as anonymous slot variables (max 4: x, y, z, t).
 * Returns None if >4 slots needed, or if the skeleton degenerates to a single
 * bare slot (whole expression is one var/stub — nothing to obfuscate here). *)
let get_largest_linear_prefix (e : Sexpr.Expr.t) : (string * slot array) option
    =
  let n = ref 0 in
  let slots = Array.make 4 (StubSlot e) in
  let var_idx : (string, int) Hashtbl.t = Hashtbl.create 4 in

  let alloc_var name size =
    match Hashtbl.find_opt var_idx name with
    | Some i -> mba_var_names.(i)
    | None ->
        if !n >= 4 then raise Too_many_slots;
        let i = !n in
        slots.(i) <- VarSlot (name, size);
        Hashtbl.add var_idx name i;
        incr n;
        mba_var_names.(i)
  in

  let stub_idx : (Sexpr.Expr.t, int) Hashtbl.t = Hashtbl.create 4 in

  let alloc_stub sub_e =
    match Hashtbl.find_opt stub_idx sub_e with
    | Some i -> mba_var_names.(i)
    | None ->
        if !n >= 4 then raise Too_many_slots;
        let i = !n in
        slots.(i) <- StubSlot sub_e;
        Hashtbl.add stub_idx sub_e i;
        incr n;
        mba_var_names.(i)
  in

  (* Join two signed term-strings without creating double signs.
   * If [rhs] already starts with '-', concatenate directly (e.g. "x" ^ "-2" = "x-2");
   * otherwise insert a '+' separator (e.g. "x" ^ "+" ^ "2" = "x+2"). *)
  let join_terms lhs rhs =
    if String.length rhs > 0 && rhs.[0] = '-' then lhs ^ rhs
    else lhs ^ "+" ^ rhs
  in

  (* Build the string representation of a linear expression for MBA-Obfuscator.
   * [neg] propagates negation downward so we never emit parentheses at the
   * linear level (which expression_2_term cannot parse). *)
  let rec build_term ~neg e =
    match e with
    | Sexpr.Expr.Cst bv ->
        let v = Bitvector.signed_of bv in
        Z.to_string (if neg then Z.neg v else v)
    | Sexpr.Expr.Unary { f = Sexpr.Expr.Minus; x; _ } ->
        build_term ~neg:(not neg) x
    | Sexpr.Expr.Binary { f = Sexpr.Expr.Plus; x; y; _ } ->
        join_terms (build_term ~neg x) (build_term ~neg y)
    | Sexpr.Expr.Binary { f = Sexpr.Expr.Minus; x; y; _ } ->
        join_terms (build_term ~neg x) (build_term ~neg:(not neg) y)
    | Sexpr.Expr.Binary { f = Sexpr.Expr.Mul; x = Sexpr.Expr.Cst bv; y; _ } ->
        let v = Bitvector.signed_of bv in
        Printf.sprintf "%s*(%s)"
          (Z.to_string (if neg then Z.neg v else v))
          (build_bitwise y)
    | Sexpr.Expr.Binary { f = Sexpr.Expr.Mul; x; y = Sexpr.Expr.Cst bv; _ } ->
        let v = Bitvector.signed_of bv in
        Printf.sprintf "%s*(%s)"
          (Z.to_string (if neg then Z.neg v else v))
          (build_bitwise x)
    | _ ->
        let bits = build_bitwise e in
        if neg then "-" ^ bits else bits
  and build_bitwise e =
    match e with
    | Sexpr.Expr.Var v -> alloc_var v.name v.size
    | Sexpr.Expr.Unary { f = Sexpr.Expr.Not; x; _ } ->
        Printf.sprintf "(~%s)" (build_bitwise x)
    | Sexpr.Expr.Binary { f = Sexpr.Expr.And; x; y; _ } ->
        Printf.sprintf "(%s&%s)" (build_bitwise x) (build_bitwise y)
    | Sexpr.Expr.Binary { f = Sexpr.Expr.Or; x; y; _ } ->
        Printf.sprintf "(%s|%s)" (build_bitwise x) (build_bitwise y)
    | Sexpr.Expr.Binary { f = Sexpr.Expr.Xor; x; y; _ } ->
        Printf.sprintf "(%s^%s)" (build_bitwise x) (build_bitwise y)
    | _ -> alloc_stub e
  in

  match try Some (build_term ~neg:false e) with Too_many_slots -> None with
  | None -> None
  | Some raw_skel ->
      (* Strip outer parens only if they form a matched pair wrapping the whole
       * skeleton. build_bitwise wraps every binary op in parens, so a root
       * bitwise op produces "(x&y)" which the MBA-Obfuscator parser rejects.
       * Naive end-character check is wrong: "(x&y)+(x|y)" starts with '(' and
       * ends with ')' but those parens are NOT matched — stripping would yield
       * the invalid "x&y)+(x|y". Walk to find the close of the first '(' and
       * only strip if it lands exactly at the last character. *)
      let skel =
        let len = String.length raw_skel in
        if len >= 2 && raw_skel.[0] = '(' then (
          let depth = ref 1 in
          let i = ref 1 in
          while !i < len && !depth > 0 do
            (if raw_skel.[!i] = '(' then incr depth
             else if raw_skel.[!i] = ')' then decr depth);
            incr i
          done;
          if !depth = 0 && !i = len then String.sub raw_skel 1 (len - 2)
          else raw_skel)
        else raw_skel
      in
      (* Trivial: no slots (pure constant) or single bare slot (entire expr
       * mapped to one var/stub) — nothing useful to obfuscate at this level. *)
      if !n = 0 || (!n = 1 && skel = mba_var_names.(0)) then None
      else Some (skel, Array.sub slots 0 !n)

(* Apply g to each direct child of e and reconstruct the node. *)
let map_children g (e : Sexpr.Expr.t) : Sexpr.Expr.t =
  match e with
  | Sexpr.Expr.Var _ | Sexpr.Expr.Cst _ -> e
  | Sexpr.Expr.Unary { f = op; x; _ } -> Sexpr.Expr.unary op (g x)
  | Sexpr.Expr.Binary { f = op; x; y; _ } -> Sexpr.Expr.binary op (g x) (g y)
  | _ -> e

(* Substitute slot variables (x/y/z/t) in [obf_expr] with [obfuscated_slots.(i)].
 * Variables not in mba_var_names (fresh vars introduced by the obfuscator) are kept. *)
let subst_slots (obf_expr : Sexpr.Expr.t)
    (obfuscated_slots : Sexpr.Expr.t array) : Sexpr.Expr.t =
  let n = Array.length mba_var_names in
  let rec subst e =
    match e with
    | Sexpr.Expr.Var v -> (
        let rec find i =
          if i >= n then None
          else if mba_var_names.(i) = v.name then Some i
          else find (i + 1)
        in
        match find 0 with
        | None -> e
        | Some i ->
            if i >= Array.length obfuscated_slots then e
            else obfuscated_slots.(i))
    | _ -> map_children subst e
  in
  subst obf_expr

(* Timeout in seconds for each MBA-Obfuscator invocation *)
let obfuscator_timeout =
  match Sys.getenv_opt "MBA_OBFUSCATOR_TIMEOUT" with
  | Some s -> ( try int_of_string s with _ -> 60)
  | None -> 60

(* Call MBA-Obfuscator on [skel], return the obfuscated string or None on failure.
   Uses direct python3 call if the script exists locally, otherwise falls back to docker.
   Kills the process after [obfuscator_timeout] seconds. *)
let run_cmd cmd =
  Format.eprintf "[mba_obf] Running: %s\n%!" cmd;
  let ic = Unix.open_process_in cmd in
  let result =
    try
      let line = input_line ic in
      if String.length line = 0 then None else Some line
    with End_of_file -> None
  in
  (try ignore (Unix.close_process_in ic) with _ -> ());
  result

let call_docker ~flag skel =
  if use_direct_python then
    let cmd =
      Printf.sprintf "timeout %d %s %s '%s' -flag %s 2>/dev/null"
        obfuscator_timeout (Option.get python_bin) obfuscate_script skel flag
    in
    run_cmd cmd
  else
    let docker_args =
      Printf.sprintf "docker run --rm %s python %s '%s' -flag %s 2>/dev/null"
        docker_image obfuscate_script skel flag
    in
    let cmd = Printf.sprintf "timeout %d %s" obfuscator_timeout docker_args in
    match run_cmd cmd with
    | Some _ as r -> r
    | None ->
        let sudo_cmd =
          Printf.sprintf "timeout %d sudo %s" obfuscator_timeout docker_args
        in
        Format.eprintf "[mba_obf] Retrying with sudo\n%!";
        run_cmd sudo_cmd

let rec obfuscate ~flag (e : Sexpr.Expr.t) : Sexpr.Expr.t =
  match get_largest_linear_prefix e with
  | None -> map_children (obfuscate ~flag) e
  | Some (skel, slots) -> (
      let bitwidth = Sexpr.Expr.sizeof e in
      Format.eprintf "[mba_obf] Skeleton (%d slot(s)): %s\n%!"
        (Array.length slots) skel;
      match call_docker ~flag skel with
      | None ->
          Format.eprintf
            "[mba_obf] Docker returned nothing, recursing into children\n%!";
          map_children (obfuscate ~flag) e
      | Some result_str -> (
          Format.eprintf "[mba_obf] Result (truncated): %s...\n%!"
            (String.sub result_str 0 (min 80 (String.length result_str)));
          let obfuscated_slots =
            Array.map
              (fun slot ->
                match slot with
                | VarSlot (name, size) -> Sexpr.Expr.var name size name
                | StubSlot sub_e -> obfuscate ~flag sub_e)
              slots
          in
          try
            let obf_expr =
              String_to_sexpr.parse ~default_size:bitwidth result_str
            in
            subst_slots obf_expr obfuscated_slots
          with _ ->
            Format.eprintf
              "[mba_obf] Failed to parse docker output, recursing\n%!";
            map_children (obfuscate ~flag) e))

(* ------------------------------------------------------------------ *)
(* Main                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  let string_expr = ref None in
  let cil_file = ref None in
  let cil_func = ref None in
  let default_size = ref None in
  let flag = ref "np_zero" in

  let usage_msg =
    "Usage:\n\
    \  mba_obf -string \"<expression>\"\n\
    \  mba_obf -string-file <file>\n\
    \  mba_obf -cil <c_file> <function_name>\n\n\
     Options:"
  in

  let spec =
    [
      ( "-string",
        Arg.String (fun s -> string_expr := Some s),
        "<expr> Parse string expression" );
      ( "-string-file",
        Arg.String
          (fun file ->
            try
              let ic = open_in file in
              let len = in_channel_length ic in
              let content = really_input_string ic len in
              close_in ic;
              string_expr := Some (String.trim content)
            with Sys_error msg ->
              Format.eprintf "Error reading file %s: %s\n" file msg;
              exit 1),
        "<file> Read expression from file" );
      ( "-cil",
        Arg.Tuple
          [
            Arg.String (fun s -> cil_file := Some s);
            Arg.String (fun s -> cil_func := Some s);
          ],
        "<file> <func> Extract return expression from C function" );
      ( "-size",
        Arg.Int (fun s -> default_size := Some s),
        "<bits> Default bit size for variables without annotations" );
      ( "-flag",
        Arg.String (fun s -> flag := s),
        "<flag> MBA-Obfuscator flag: p, l, np_zero, np_recur, np_replace \
         (default: np_zero)" );
    ]
  in

  Arg.parse spec (fun _ -> ()) usage_msg;

  let has_string = Option.is_some !string_expr in
  let has_cil = Option.is_some !cil_file && Option.is_some !cil_func in

  if (not has_string) && not has_cil then (
    Arg.usage spec usage_msg;
    exit 1);
  if has_string && has_cil then (
    Format.eprintf "Error: cannot specify both -string and -cil\n";
    exit 1);

  try
    let sexpr =
      if has_string then
        String_to_sexpr.parse ?default_size:!default_size
          (Option.get !string_expr)
      else
        Cil_to_sexpr.load_from_file (Option.get !cil_file)
          (Option.get !cil_func)
    in

    let obf_sexpr = obfuscate ~flag:!flag sexpr in

    let size = Greysynth_utils.Utils.size_of_expr obf_sexpr in
    let output_bitsize = Sexpr.Expr.sizeof obf_sexpr in
    let expr_with_size = Greysynth_utils.Utils.pp ~show_size:true obf_sexpr in
    let expression_no_ext =
      Greysynth_utils.Utils.pp_no_ext ~show_size:false obf_sexpr
    in
    let classification = Mba_classifier.classify obf_sexpr in
    let classification_str = Mba_classifier.to_string classification in

    let equiv, _, _ =
      Greysynth_utils.Utils.are_equivalent sexpr obf_sexpr (Some 10)
    in
    let equiv_str =
      match equiv with
      | Greysynth_utils.Utils.Yes -> "yes"
      | Greysynth_utils.Utils.No -> "no"
      | Greysynth_utils.Utils.Unknown -> "unknown"
      | Greysynth_utils.Utils.Timeout -> "timeout"
      | Greysynth_utils.Utils.Disabled -> "disabled"
    in

    let json =
      `Assoc
        [
          ("size", `Int size);
          ("output_bitsize", `Int output_bitsize);
          ("expression", `String expr_with_size);
          ("expression_no_ext", `String expression_no_ext);
          ("classification", `String classification_str);
          ("equiv", `String equiv_str);
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
