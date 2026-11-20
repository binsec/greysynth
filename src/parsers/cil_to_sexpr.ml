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

(* cil_to_sexpr.ml - Convert CIL expressions to Sexpr.Expr.t *)

open GoblintCil

(* Configurable pointer size (in bits). Default: 64 for x86-64 *)
let pointer_size = ref 64
let set_pointer_size size = pointer_size := size
let get_pointer_size () = !pointer_size

(* Get the bit size of a CIL integer kind *)
let bitsize_of_ikind (ik : Cil.ikind) : int =
  match ik with
  | IChar | ISChar | IUChar -> 8
  | IShort | IUShort -> 16
  | IInt | IUInt -> 32
  | ILong | IULong -> 64
  | ILongLong | IULongLong -> 64
  | IBool -> 1
  | IInt128 | IUInt128 -> 128

let bitsize_of_type (typ : Cil.typ) : int =
  match Cil.unrollType typ with
  | TInt (ik, _) -> bitsize_of_ikind ik
  | TPtr _ -> !pointer_size
  | _ -> !pointer_size

let is_signed_ikind (ik : Cil.ikind) : bool =
  match ik with
  | IChar | ISChar | IShort | IInt | ILong | ILongLong | IInt128 -> true
  | IUChar | IUShort | IUInt | IULong | IULongLong | IUInt128 | IBool -> false

(* Adjust expression size for explicit casts *)
let adjust_size ~is_signed (e : Sexpr.Expr.t) (target_size : int) : Sexpr.Expr.t
    =
  let current_size = Sexpr.Expr.sizeof e in
  if current_size = target_size then e
  else if current_size > target_size then
    Sexpr.Expr.unary (Sexpr.Expr.Restrict { lo = 0; hi = target_size - 1 }) e
  else
    let ext_amount = target_size - current_size in
    if is_signed then Sexpr.Expr.unary (Sexpr.Expr.Sext ext_amount) e
    else Sexpr.Expr.unary (Sexpr.Expr.Uext ext_amount) e

(* Create a constant bitvector of a specific size, masking the value to fit *)
let make_constant (value : Z.t) (size : int) : Sexpr.Expr.t =
  let mask = Z.sub (Z.shift_left Z.one size) Z.one in
  let masked = Z.logand value mask in
  let bv = Bitvector.create masked size in
  Sexpr.Expr.constant bv

(* Get the bit size of a Sexpr expression *)
let sexpr_sizeof (expr : Sexpr.Expr.t) : int =
  Sexpr.Expr.sizeof expr

(* Equalize sizes of two Sexpr expressions by extending the smaller one *)
let equalize_sizes ~is_signed (e1 : Sexpr.Expr.t) (e2 : Sexpr.Expr.t) : Sexpr.Expr.t * Sexpr.Expr.t =
  let size1 = sexpr_sizeof e1 in
  let size2 = sexpr_sizeof e2 in
  if size1 = size2 then
    (e1, e2)
  else if size1 < size2 then
    let ext_amount = size2 - size1 in
    if is_signed then
      (Sexpr.Expr.unary (Sexpr.Expr.Sext ext_amount) e1, e2)
    else
      (Sexpr.Expr.unary (Sexpr.Expr.Uext ext_amount) e1, e2)
  else
    let ext_amount = size1 - size2 in
    if is_signed then
      (e1, Sexpr.Expr.unary (Sexpr.Expr.Sext ext_amount) e2)
    else
      (e1, Sexpr.Expr.unary (Sexpr.Expr.Uext ext_amount) e2)

(* Convert CIL expression to Sexpr.Expr.t
   Trusts CIL to insert explicit CastE nodes for all implicit conversions *)
let rec convert (cexpr : Cil.exp) : Sexpr.Expr.t =
  match cexpr with
  | Const (CInt (i, ik, _)) ->
      let size = bitsize_of_ikind ik in
      make_constant i size
  | Lval (Var vi, NoOffset) ->
      let size = bitsize_of_type vi.vtype in
      Sexpr.Expr.var vi.vname size vi.vname
  | CastE (target_typ, e) ->
      let inner = convert e in
      let target_size = bitsize_of_type target_typ in
      let is_signed =
        match Cil.unrollType (Cil.typeOf e) with
        | TInt (ik, _) -> is_signed_ikind ik
        | _ -> false
      in
      adjust_size ~is_signed inner target_size
  | UnOp (u, e, _res_typ) ->
      let inner = convert e in
      (match u with
      | Neg -> Sexpr.Expr.unary Sexpr.Expr.Minus inner
      | BNot -> Sexpr.Expr.unary Sexpr.Expr.Not inner
      | LNot ->
          let size = Sexpr.Expr.sizeof inner in
          let neg = Sexpr.Expr.unary Sexpr.Expr.Minus inner in
          let or_expr = Sexpr.Expr.binary Sexpr.Expr.Or inner neg in
          let shift_amt = make_constant (Z.of_int (size - 1)) size in
          let shifted = Sexpr.Expr.binary Sexpr.Expr.Lsr or_expr shift_amt in
          let one = make_constant Z.one size in
          Sexpr.Expr.binary Sexpr.Expr.Xor shifted one)
  | BinOp (b, e1, e2, res_typ) ->
      let sexpr_e1 = convert e1 in
      let sexpr_e2 = convert e2 in
      let is_result_signed =
        match Cil.unrollType res_typ with
        | TInt (ik, _) -> is_signed_ikind ik
        | _ -> false
      in
      let (sexpr_e1', sexpr_e2') = equalize_sizes ~is_signed:is_result_signed sexpr_e1 sexpr_e2 in
      let sexpr_op =
        match b with
        | PlusA | PlusPI | IndexPI -> Sexpr.Expr.Plus
        | MinusA | MinusPI | MinusPP -> Sexpr.Expr.Minus
        | Mult -> Sexpr.Expr.Mul
        | Div -> if is_result_signed then Sexpr.Expr.Sdiv else Sexpr.Expr.Udiv
        | Mod -> if is_result_signed then Sexpr.Expr.Smod else Sexpr.Expr.Umod
        | Shiftlt -> Sexpr.Expr.Lsl
        | Shiftrt -> if is_result_signed then Sexpr.Expr.Asr else Sexpr.Expr.Lsr
        | BAnd | LAnd -> Sexpr.Expr.And
        | BXor -> Sexpr.Expr.Xor
        | BOr | LOr -> Sexpr.Expr.Or
        | _ -> failwith "Unsupported binary operator"
      in
      Sexpr.Expr.binary sexpr_op sexpr_e1' sexpr_e2'
  | _ -> failwith "Unsupported CIL expression type"

let get_ret_expr (stmts : Cil.stmt list) : Cil.exp =
  let rec find_return = function
    | [] -> failwith "No return statement found"
    | stmt :: rest -> (
        match stmt.skind with
        | Return (Some exp, _, _) -> exp
        | Return (None, _, _) -> failwith "Return statement has no expression"
        | Block b -> ( try find_return b.bstmts with _ -> find_return rest)
        | If (_, tb, fb, _, _) -> (
            try find_return tb.bstmts
            with _ -> ( try find_return fb.bstmts with _ -> find_return rest))
        | _ -> find_return rest)
  in
  find_return stmts

(* Find a function by name in the CIL file *)
let find_function (cil_file : Cil.file) (func_name : string) : Cil.fundec option
    =
  let result = ref None in
  let visitor =
    object
      inherit nopCilVisitor

      method! vfunc fd =
        if fd.svar.vname = func_name then (
          result := Some fd;
          SkipChildren)
        else DoChildren
    end
  in
  ignore (visitCilFile visitor cil_file);
  !result

(* CIL initialization state *)
let cil_initialized = ref false

(* --- Raw string extraction for GAMBA-friendly expressions --- *)

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_alnum c = is_alpha c || is_digit c

(* Find substring in string starting from position *)
let find_substring s sub start =
  let slen = String.length s in
  let sublen = String.length sub in
  if sublen = 0 then Some start
  else
    let rec search i =
      if i + sublen > slen then None
      else if String.sub s i sublen = sub then Some i
      else search (i + 1)
    in
    search start

(* Extract the return expression as a raw string from a C source *)
let extract_return_string (file_contents : string) (func_name : string) : string option =
  let len = String.length file_contents in
  let func_name_len = String.length func_name in
  (* Find the LAST occurrence of func_name followed by '(' — the definition, not a declaration *)
  let rec find_last_func pos best =
    match find_substring file_contents func_name pos with
    | None -> best
    | Some i ->
      let before_ok = i = 0 || let c = file_contents.[i - 1] in not (is_alnum c || c = '_') in
      let after_pos = i + func_name_len in
      let after_ok = after_pos < len &&
        let rec skip_ws j = if j < len && (file_contents.[j] = ' ' || file_contents.[j] = '\t' || file_contents.[j] = '\n') then skip_ws (j+1) else j in
        let j = skip_ws after_pos in
        j < len && file_contents.[j] = '('
      in
      let best' = if before_ok && after_ok then Some i else best in
      find_last_func (i + 1) best'
  in
  match find_last_func 0 None with
  | None -> None
  | Some func_pos ->
    let rec find_brace i =
      if i >= len then None
      else if file_contents.[i] = '{' then Some i
      else find_brace (i + 1)
    in
    (match find_brace (func_pos + func_name_len) with
    | None -> None
    | Some brace_start ->
      let rec find_body depth pos =
        if pos >= len then None
        else match file_contents.[pos] with
          | '{' -> find_body (depth + 1) (pos + 1)
          | '}' ->
            if depth = 1 then Some pos
            else find_body (depth - 1) (pos + 1)
          | _ -> find_body depth (pos + 1)
      in
      match find_body 1 (brace_start + 1) with
      | None -> None
      | Some body_end ->
        let body = String.sub file_contents (brace_start + 1) (body_end - brace_start - 1) in
        let body_len = String.length body in
        (* Find the LAST "return" keyword in the body *)
        let rec find_last_return pos best =
          match find_substring body "return" pos with
          | None -> best
          | Some i ->
            let before_ok = i = 0 || let c = body.[i - 1] in not (is_alnum c || c = '_') in
            let after_pos = i + 6 in
            let after_ok = after_pos < body_len &&
              let c = body.[after_pos] in c = ' ' || c = '\t' || c = '\n' || c = '(' in
            let best' = if before_ok && after_ok then Some i else best in
            find_last_return (i + 1) best'
        in
        match find_last_return 0 None with
        | None -> None
        | Some ret_pos ->
          let rec skip_ws i = if i < body_len && (body.[i] = ' ' || body.[i] = '\t' || body.[i] = '\n') then skip_ws (i+1) else i in
          let expr_start = skip_ws (ret_pos + 6) in
          let rec find_semi depth i =
            if i >= body_len then None
            else match body.[i] with
              | '(' -> find_semi (depth + 1) (i + 1)
              | ')' -> find_semi (depth - 1) (i + 1)
              | ';' when depth = 0 -> Some i
              | _ -> find_semi depth (i + 1)
          in
          match find_semi 0 expr_start with
          | None -> None
          | Some semi_pos ->
            let expr = String.trim (String.sub body expr_start (semi_pos - expr_start)) in
            Some expr)

(* Strip C integer literal suffixes: 1126379749U -> 1126379749, 0x432330e5UL -> 0x432330e5 *)
let strip_c_suffixes (expr : string) : string =
  let len = String.length expr in
  let buf = Buffer.create len in
  let rec scan i =
    if i >= len then ()
    else if expr.[i] = '0' && i + 1 < len && (expr.[i+1] = 'x' || expr.[i+1] = 'X') then begin
      Buffer.add_char buf '0';
      Buffer.add_char buf expr.[i+1];
      let j = ref (i + 2) in
      while !j < len && is_hex expr.[!j] do
        Buffer.add_char buf expr.[!j];
        incr j
      done;
      let k = skip_int_suffix expr !j len in
      scan k
    end
    else if is_digit expr.[i] then begin
      let j = ref i in
      while !j < len && is_digit expr.[!j] do
        Buffer.add_char buf expr.[!j];
        incr j
      done;
      let k = skip_int_suffix expr !j len in
      scan k
    end
    else begin
      Buffer.add_char buf expr.[i];
      scan (i + 1)
    end
  and skip_int_suffix s pos slen =
    let p = ref pos in
    let had_u = !p < slen && (s.[!p] = 'U' || s.[!p] = 'u') in
    if had_u then incr p;
    if !p < slen && (s.[!p] = 'L' || s.[!p] = 'l') then begin
      incr p;
      if !p < slen && (s.[!p] = 'L' || s.[!p] = 'l') then incr p
    end;
    if (not had_u) && !p < slen && (s.[!p] = 'U' || s.[!p] = 'u') then incr p;
    if !p < slen && (is_alnum s.[!p] || s.[!p] = '_') then pos
    else !p
  in
  scan 0;
  Buffer.contents buf

(* Strip balanced outer parentheses *)
let strip_outer_parens (s : string) : string =
  let s = String.trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '(' && s.[len - 1] = ')' then
    let inner = String.sub s 1 (len - 2) in
    let rec check depth i =
      if i >= String.length inner then depth = 0
      else match inner.[i] with
        | '(' -> check (depth + 1) (i + 1)
        | ')' -> if depth = 0 then false else check (depth - 1) (i + 1)
        | _ -> check depth (i + 1)
    in
    if check 0 0 then inner else s
  else s

(* Check if a cleaned expression fits the GAMBA grammar:
   variables (x0, x1, ...), decimal/hex constants, and operators + - * & | ^ ~ << ( )
   No casts, no division, no modulo, no right shift. *)
let is_gamba_friendly_string (expr : string) : bool =
  let len = String.length expr in
  (* Reject type casts: (unsigned ...), (signed ...), (int), etc. *)
  let has_cast =
    let keywords = ["unsigned"; "signed"; "int"; "long"; "short"; "char"] in
    let rec check i =
      if i >= len then false
      else if expr.[i] = '(' then
        let j = ref (i + 1) in
        while !j < len && (expr.[!j] = ' ' || expr.[!j] = '\t') do incr j done;
        List.exists (fun kw ->
          let kwlen = String.length kw in
          !j + kwlen <= len &&
          String.sub expr !j kwlen = kw &&
          (!j + kwlen >= len || not (is_alnum expr.[!j + kwlen] || expr.[!j + kwlen] = '_'))
        ) keywords || check (i + 1)
      else check (i + 1)
    in
    check 0
  in
  if has_cast then false
  else
    let has_div = String.contains expr '/' in
    if has_div then false
    else
      let has_mod = String.contains expr '%' in
      if has_mod then false
      else
        let rec has_rshift i =
          if i + 1 >= len then false
          else if expr.[i] = '>' && expr.[i+1] = '>' then true
          else has_rshift (i + 1)
        in
        not (has_rshift 0)

(* Strip a leading signedness-only cast: (unsigned int)expr -> expr, (signed)expr -> expr, etc.
   Only strips casts that change signedness but NOT bit size (unsigned, signed, unsigned int, etc.).
   Does NOT strip size-changing casts like (uint64_t) on a 32-bit value. *)
let strip_signedness_cast (s : string) : string =
  let s = String.trim s in
  let len = String.length s in
  if len < 3 || s.[0] <> '(' then s
  else
    (* Find matching closing paren *)
    let rec find_close depth i =
      if i >= len then None
      else match s.[i] with
        | '(' -> find_close (depth + 1) (i + 1)
        | ')' -> if depth = 1 then Some i else find_close (depth - 1) (i + 1)
        | _ -> find_close depth (i + 1)
    in
    match find_close 1 1 with
    | None -> s
    | Some close_pos ->
      let cast_content = String.trim (String.sub s 1 (close_pos - 1)) in
      (* Only allow pure signedness casts: combinations of "unsigned", "signed", "int",
         "long", "short", "char" — these don't change bit width *)
      let words = String.split_on_char ' ' cast_content in
      let words = List.filter (fun w -> String.length w > 0) words in
      let sign_words = ["unsigned"; "signed"; "int"; "long"; "short"; "char"] in
      let all_sign = List.for_all (fun w -> List.mem w sign_words) words in
      if all_sign && words <> [] then
        let rest = String.sub s (close_pos + 1) (len - close_pos - 1) in
        String.trim rest
      else s

(* Try to extract a GAMBA-friendly expression string from a C file.
   Returns Some cleaned_string if the return expression is GAMBA-compatible,
   None otherwise. *)
let extract_gamba_friendly (file : string) (func_name : string) : string option =
  let file_contents = In_channel.with_open_text file In_channel.input_all in
  match extract_return_string file_contents func_name with
  | None -> None
  | Some raw_expr ->
    let cleaned = strip_c_suffixes raw_expr in
    let cleaned = strip_outer_parens cleaned in
    let cleaned = strip_signedness_cast cleaned in
    let cleaned = strip_outer_parens cleaned in
    if is_gamba_friendly_string cleaned then Some cleaned
    else None

(* Load an expression from a C file and function name *)
let load_from_file (file : string) (func : string) : Sexpr.Expr.t =
  if not !cil_initialized then (
    Cil.initCIL ();
    cil_initialized := true);
  if not (Sys.file_exists file) then
    failwith (Printf.sprintf "File %s does not exist" file);
  let cil_file_parsed = Frontc.parse file () in
  match find_function cil_file_parsed func with
  | None -> failwith (Printf.sprintf "Function '%s' not found in %s" func file)
  | Some fundec ->
    let cil_expr = get_ret_expr fundec.sbody.bstmts in
    convert cil_expr
