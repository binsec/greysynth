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

(* string_to_sexpr.ml - Convert string expressions to Sexpr.Expr.t
 * Implements a grammar-based lexer and parser for expression syntax
 *
 * Grammar (strict C precedence, lowest to highest):
 * 1. | (bitwise OR)
 * 2. ^ (bitwise XOR)
 * 3. & (bitwise AND)
 * 4. +, - (addition, subtraction)
 * 5. <<, >>, >>s, rol, ror, ++ (shifts, rotates, concatenation)
 * 6. *, /, /s, %, %s (multiplication, division, modulo)
 * 7. ** (power - right associative, unrolls during parsing)
 * 8. ~, -, uext(n), sext(n), restrict(lo,hi) (unary operators)
 *
 * - Variables: x, var1, _tmp with optional size: x<64>
 * - Default variable size: 64 bits (or provided as parameter)
 *)

(** Token types *)
type token =
  | INT of Z.t * int option       (* numeric constant and optional size *)
  | VAR of string * int option    (* variable name and optional size *)
  | LPAREN                        (* ( *)
  | RPAREN                        (* ) *)
  | COMMA                         (* , *)
  | PLUS                          (* + *)
  | MINUS                         (* - *)
  | MUL                           (* * *)
  | DIV                           (* / *)
  | DIVS                          (* /s *)
  | MOD                           (* % *)
  | MODS                          (* %s *)
  | LSHIFT                        (* << *)
  | LSRU                          (* >> *)
  | LSRS                          (* >>s *)
  | LROTATE                       (* rol *)
  | RROTATE                       (* ror *)
  | AND                           (* & *)
  | OR                            (* | *)
  | XOR                           (* ^ *)
  | NOT                           (* ~ and ! *)
  | POWER                         (* ** *)
  | CONCAT                        (* ++ *)
  | UEXT of int                   (* uext(n) *)
  | SEXT of int                   (* sext(n) *)
  | RESTRICT of int * int         (* restrict(lo,hi) *)
  | EQ                            (* = (also ==) *)
  | QUESTION                      (* ? *)
  | COLON                         (* : *)
  | EOF

(** Expression AST *)
type expr =
  | Const of Z.t * int            (* constant with resolved size *)
  | Var of string * int           (* variable with resolved size *)
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Uext of int * expr
  | Sext of int * expr
  | Restrict of int * int * expr  (* restrict(lo, hi, expr) *)
  | Power of expr * int           (* power with constant exponent *)
  | Ite of expr * expr * expr     (* condition ? then : else *)

and binop =
  | Add | Sub | Mul | Udiv | Sdiv | Umod | Smod
  | Lsl | Lsr | Asr | Rol | Ror
  | And | Or | Xor | Concat | Eq

and unop =
  | Not | Minus

(** Lexer *)
module Lexer = struct
  type lexbuf = {
    mutable input: string;
    mutable pos: int;
    mutable default_size: int;
  }

  let create input default_size = {
    input;
    pos = 0;
    default_size;
  }

  let peek buf =
    if buf.pos < String.length buf.input then
      Some buf.input.[buf.pos]
    else
      None

  let advance buf =
    buf.pos <- buf.pos + 1

  let skip_whitespace buf =
    while match peek buf with
      | Some (' ' | '\t' | '\n' | '\r') -> advance buf; true
      | _ -> false
    do () done

  let read_number buf =
    let start = buf.pos in
    let is_hex = peek buf = Some '0' && buf.pos + 1 < String.length buf.input &&
                 (buf.input.[buf.pos + 1] = 'x' || buf.input.[buf.pos + 1] = 'X') in
    if is_hex then (
      advance buf; advance buf;  (* skip 0x *)
      while match peek buf with
        | Some ('0'..'9' | 'a'..'f' | 'A'..'F') -> advance buf; true
        | _ -> false
      do () done
    ) else (
      while match peek buf with
        | Some ('0'..'9') -> advance buf; true
        | _ -> false
      do () done
    );
    let num_str = String.sub buf.input start (buf.pos - start) in
    Z.of_string num_str

  let is_multibyte_start c = Char.code c > 127

  let read_ident buf =
    let start = buf.pos in
    while match peek buf with
      | Some ('a'..'z' | 'A'..'Z' | '0'..'9' | '_') -> advance buf; true
      | Some c when is_multibyte_start c -> advance buf; true
      | _ -> false
    do () done;
    String.sub buf.input start (buf.pos - start)

  let read_size buf =
    (* Read <64> format *)
    if peek buf = Some '<' then (
      advance buf;
      let size_str = read_ident buf in
      if peek buf = Some '>' then (
        advance buf;
        (try Some (int_of_string size_str) with _ -> None)
      ) else
        None
    ) else
      None

  let next buf : token =
    skip_whitespace buf;
    match peek buf with
    | None -> EOF
    | Some '(' -> advance buf; LPAREN
    | Some ')' -> advance buf; RPAREN
    | Some ',' -> advance buf; COMMA
    | Some '?' -> advance buf; QUESTION
    | Some ':' -> advance buf; COLON
    | Some '=' ->
        advance buf;
        if peek buf = Some '=' then advance buf;  (* accept == as = *)
        EQ
    | Some '+' ->
        advance buf;
        if peek buf = Some '+' then (
          advance buf;
          CONCAT
        ) else
          PLUS
    | Some '-' ->
        advance buf;
        if peek buf = Some '>' then
          advance buf;
        MINUS
    | Some '*' ->
        advance buf;
        if peek buf = Some '*' then (
          advance buf;
          POWER
        ) else
          MUL
    | Some '/' ->
        advance buf;
        if peek buf = Some 's' then (
          advance buf;
          DIVS
        ) else
          DIV
    | Some '%' ->
        advance buf;
        if peek buf = Some 's' then (
          advance buf;
          MODS
        ) else
          MOD
    | Some '<' ->
        advance buf;
        if peek buf = Some '<' then (
          advance buf;
          LSHIFT
        ) else (
          (* Could be the start of a size annotation, backtrack *)
          buf.pos <- buf.pos - 1;
          raise (Failure "Unexpected < token")
        )
    | Some '>' ->
        advance buf;
        if peek buf = Some '>' then (
          advance buf;
          if peek buf = Some 's' then (
            advance buf;
            LSRS
          ) else
            LSRU
        ) else (
          buf.pos <- buf.pos - 1;
          raise (Failure "Unexpected > token")
        )
    | Some '&' -> advance buf; AND
    | Some '|' -> advance buf; OR
    | Some '^' -> advance buf; XOR
    | Some '~' -> advance buf; NOT
    | Some '!' -> advance buf; NOT
    | Some ('0'..'9') ->
        let num = read_number buf in
        let size = read_size buf in
        INT (num, size)
    | Some c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
                  || is_multibyte_start c ->
        let ident = read_ident buf in
        (match ident with
        | "rol" -> LROTATE
        | "ror" -> RROTATE
        | "uext" ->
            if peek buf = Some '(' then (
              advance buf;
              let size = read_number buf in
              if peek buf = Some ')' then advance buf;
              let int_size = Z.to_int size in
              UEXT int_size
            ) else
              raise (Failure "uext requires (...)")
        | "sext" ->
            if peek buf = Some '(' then (
              advance buf;
              let size = read_number buf in
              if peek buf = Some ')' then advance buf;
              SEXT (Z.to_int size)
            ) else
              raise (Failure "sext requires (...)")
        | "restrict" ->
            if peek buf = Some '(' then (
              advance buf;
              let lo = read_number buf in
              if peek buf = Some ',' then advance buf;
              let hi = read_number buf in
              if peek buf = Some ')' then advance buf;
              RESTRICT (Z.to_int lo, Z.to_int hi)
            ) else
              raise (Failure "restrict requires (...)")
        | _ ->
            let size = read_size buf in
            VAR (ident, size))
    | Some c -> raise (Failure (Printf.sprintf "Unexpected character: %c" c))
end

(** Parser with operator precedence *)
module Parser = struct
  type pstate = {
    mutable current: token;
    mutable lexbuf: Lexer.lexbuf;
  }

  let create input default_size =
    let lexbuf = Lexer.create input default_size in
    {
      current = Lexer.next lexbuf;
      lexbuf;
    }

  let peek state = state.current

  let advance state =
    state.current <- Lexer.next state.lexbuf

  let resolve_size state _var_name size_opt =
    match size_opt with
    | Some sz -> sz
    | None -> state.lexbuf.Lexer.default_size

  (* Forward declarations - C precedence (low to high): | < ^ < & < << >> < +/- < * / % *)
  let rec parse_expr state = parse_ternary state
  and parse_ternary state =
    let cond = parse_equality state in
    match peek state with
    | QUESTION ->
        advance state;
        let then_e = parse_expr state in
        (match peek state with
        | COLON -> advance state
        | _ -> raise (Failure "Expected ':' in ternary expression"));
        let else_e = parse_ternary state in
        Ite (cond, then_e, else_e)
    | _ -> cond
  and parse_equality state =
    let left = parse_or state in
    let rec loop left =
      match peek state with
      | EQ ->
          advance state;
          let right = parse_or state in
          loop (BinOp (Eq, left, right))
      | _ -> left
    in
    loop left
  and parse_or state =
    let left = parse_xor state in
    let rec loop left =
      match peek state with
      | OR ->
          advance state;
          let right = parse_xor state in
          loop (BinOp (Or, left, right))
      | _ -> left
    in
    loop left

  and parse_xor state =
    let left = parse_and state in
    let rec loop left =
      match peek state with
      | XOR ->
          advance state;
          let right = parse_and state in
          loop (BinOp (Xor, left, right))
      | _ -> left
    in
    loop left

  and parse_and state =
    let left = parse_shift state in
    let rec loop left =
      match peek state with
      | AND ->
          advance state;
          let right = parse_shift state in
          loop (BinOp (And, left, right))
      | _ -> left
    in
    loop left

  and parse_shift state =
    let left = parse_additive state in
    let rec loop left =
      match peek state with
      | LSHIFT ->
          advance state;
          let right = parse_additive state in
          loop (BinOp (Lsl, left, right))
      | LSRU ->
          advance state;
          let right = parse_additive state in
          loop (BinOp (Lsr, left, right))
      | LSRS ->
          advance state;
          let right = parse_additive state in
          loop (BinOp (Asr, left, right))
      | LROTATE ->
          advance state;
          let right = parse_additive state in
          loop (BinOp (Rol, left, right))
      | RROTATE ->
          advance state;
          let right = parse_additive state in
          loop (BinOp (Ror, left, right))
      | CONCAT ->
          advance state;
          let right = parse_additive state in
          loop (BinOp (Concat, left, right))
      | _ -> left
    in
    loop left

  and parse_additive state =
    let left = parse_multiplicative state in
    let rec loop left =
      match peek state with
      | PLUS ->
          advance state;
          let right = parse_multiplicative state in
          loop (BinOp (Add, left, right))
      | MINUS ->
          advance state;
          let right = parse_multiplicative state in
          loop (BinOp (Sub, left, right))
      | _ -> left
    in
    loop left

  and parse_multiplicative state =
    let left = parse_power state in
    let rec loop left =
      match peek state with
      | MUL ->
          advance state;
          let right = parse_power state in
          loop (BinOp (Mul, left, right))
      | DIV ->
          advance state;
          let right = parse_power state in
          loop (BinOp (Udiv, left, right))
      | DIVS ->
          advance state;
          let right = parse_power state in
          loop (BinOp (Sdiv, left, right))
      | MOD ->
          advance state;
          let right = parse_power state in
          loop (BinOp (Umod, left, right))
      | MODS ->
          advance state;
          let right = parse_power state in
          loop (BinOp (Smod, left, right))
      | _ -> left
    in
    loop left

  and parse_power state =
    let left = parse_unary state in
    match peek state with
    | POWER ->
        advance state;
        let right = parse_unary state in
        (* Special case: 2**expr → 1 << expr *)
        (match left with
        | Const (base_z, base_sz) when Z.equal base_z (Z.of_int 2) ->
            BinOp (Lsl, Const (Z.one, base_sz), right)
        | _ ->
            (* General case: exponent must be a constant *)
            (match right with
            | Const (exp_z, _) ->
                let exp = Z.to_int exp_z in
                if exp < 0 then
                  raise (Failure "Power exponent must be non-negative")
                else if exp = 0 then
                  Const (Z.one, 1)
                else if exp = 1 then
                  left
                else
                  let rec unroll n acc =
                    if n = 1 then acc
                    else unroll (n - 1) (BinOp (Mul, acc, left))
                  in
                  unroll exp left
            | _ -> raise (Failure "Power exponent must be a constant")))
    | _ -> left

  and parse_unary state =
    match peek state with
    | NOT ->
        advance state;
        let expr = parse_unary state in
        UnOp (Not, expr)
    | MINUS ->
        advance state;
        let expr = parse_unary state in
        UnOp (Minus, expr)
    | UEXT size ->
        advance state;
        let expr = parse_unary_with_ext_size state (Some size) in
        Uext (size, expr)
    | SEXT size ->
        advance state;
        let expr = parse_unary_with_ext_size state (Some size) in
        Sext (size, expr)
    | RESTRICT (lo, hi) ->
        advance state;
        let expr = parse_unary state in
        Restrict (lo, hi, expr)
    | _ -> parse_postfix state

  and parse_unary_with_ext_size state ext_size =
    match peek state with
    | NOT ->
        advance state;
        let expr = parse_unary_with_ext_size state ext_size in
        UnOp (Not, expr)
    | MINUS ->
        advance state;
        let expr = parse_unary_with_ext_size state ext_size in
        UnOp (Minus, expr)
    | _ -> parse_postfix_with_ext_size state ext_size

  and parse_postfix_with_ext_size state ext_size =
    parse_primary_with_ext_size state ext_size

  and parse_primary_with_ext_size state ext_size =
    match peek state with
    | INT (num, size_opt) ->
        advance state;
        let resolved_size =
          match size_opt with
          | Some sz -> sz
          | None ->
              (* If this constant is operand of uext/sext, use that size as default *)
              (match ext_size with
              | Some sz -> sz
              | None -> state.lexbuf.Lexer.default_size)
        in
        Const (num, resolved_size)
    | VAR (name, size_opt) ->
        advance state;
        let resolved_size = resolve_size state name size_opt in
        Var (name, resolved_size)
    | LPAREN ->
        advance state;
        let expr = parse_expr state in
        (match peek state with
        | RPAREN -> advance state; expr
        | _ -> raise (Failure "Expected )"))
    | _ -> raise (Failure "Expected expression")

  and parse_postfix state = parse_primary state

  and parse_primary state =
    match peek state with
    | INT (num, size_opt) ->
        advance state;
        let resolved_size =
          match size_opt with
          | Some sz -> sz
          | None -> state.lexbuf.Lexer.default_size  (* Use default size for unsized constants *)
        in
        Const (num, resolved_size)
    | VAR (name, size_opt) ->
        advance state;
        let resolved_size = resolve_size state name size_opt in
        Var (name, resolved_size)
    | LPAREN ->
        advance state;
        let expr = parse_expr state in
        (match peek state with
        | RPAREN -> advance state; expr
        | _ -> raise (Failure "Expected )"))
    | _ -> raise (Failure "Expected expression")

  let parse input default_size =
    let state = create input default_size in
    let expr = parse_expr state in
    if peek state = EOF then
      expr
    else
      raise (Failure "Unexpected tokens after expression")
end

(** Check if a value fits in a given bit width *)
let fits_in_bits (value : Z.t) (bits : int) : bool =
  let max_val = Z.sub (Z.shift_left Z.one bits) Z.one in
  Z.leq Z.zero value && Z.leq value max_val

(** Get the size of an expression *)
let rec get_expr_size (expr : expr) : int =
  match expr with
  | Const (_, sz) -> sz
  | Var (_, sz) -> sz
  | BinOp (Concat, e1, e2) -> get_expr_size e1 + get_expr_size e2  (* Concat: sum of sizes *)
  | BinOp (Eq, _, _) -> 1  (* comparison yields a 1-bit truth value *)
  | BinOp (_, e1, _) -> get_expr_size e1  (* Other binary ops: first operand size *)
  | UnOp (_, e) -> get_expr_size e
  | Uext (n, e) -> get_expr_size e + n
  | Sext (n, e) -> get_expr_size e + n
  | Restrict (lo, hi, _) -> hi - lo + 1
  | Power (e, _) -> get_expr_size e  (* Power result size = base size *)
  | Ite (_, t, _) -> get_expr_size t  (* result size = branch size *)

(** Normalize constant sizes in the AST by adjusting them to match operand sizes *)
let rec normalize_sizes (expr : expr) : expr =
  match expr with
  | Const _ -> expr
  | Var _ -> expr
  | BinOp (op, e1, e2) ->
      let e1' = normalize_sizes e1 in
      let e2' = normalize_sizes e2 in

      let size1 = get_expr_size e1' in
      let size2 = get_expr_size e2' in

      (* Adjust constant sizes to match the other operand's size when possible *)
      let e1'' =
        match e1' with
        | Const (z, const_sz) when const_sz <> size2 && fits_in_bits z size2 ->
            Const (z, size2)
        | _ -> e1'
      in
      let e2'' =
        match e2' with
        | Const (z, const_sz) when const_sz <> size1 && fits_in_bits z size1 ->
            Const (z, size1)
        | _ -> e2'
      in
      BinOp (op, e1'', e2'')
  | UnOp (op, e) ->
      UnOp (op, normalize_sizes e)
  | Uext (n, e) ->
      Uext (n, normalize_sizes e)
  | Sext (n, e) ->
      Sext (n, normalize_sizes e)
  | Restrict (lo, hi, e) ->
      Restrict (lo, hi, normalize_sizes e)
  | Power (e, n) ->
      Power (normalize_sizes e, n)
  | Ite (c, t, e) ->
      Ite (normalize_sizes c, normalize_sizes t, normalize_sizes e)

(** Get Sexpr expression size *)
let sexpr_sizeof (expr : Sexpr.Expr.t) : int =
  Sexpr.Expr.sizeof expr

(** Equalize sizes of two Sexpr expressions by extending the smaller one *)
let equalize_sizes_sexpr (e1 : Sexpr.Expr.t) (e2 : Sexpr.Expr.t) : Sexpr.Expr.t * Sexpr.Expr.t =
  let size1 = sexpr_sizeof e1 in
  let size2 = sexpr_sizeof e2 in
  if size1 = size2 then
    (e1, e2)
  else if size1 < size2 then
    let ext_amount = size2 - size1 in
    (Sexpr.Expr.unary (Sexpr.Expr.Uext ext_amount) e1, e2)
  else
    let ext_amount = size1 - size2 in
    (e1, Sexpr.Expr.unary (Sexpr.Expr.Uext ext_amount) e2)

(** Convert AST to Sexpr.Expr.t *)
let rec ast_to_sexpr (expr : expr) : Sexpr.Expr.t =
  match expr with
  | Const (z, size) ->
      let bv = Bitvector.create z size in
      Sexpr.Expr.constant bv
  | Var (name, size) ->
      Sexpr.Expr.var name size name
  | BinOp (op, e1, e2) ->
      let se1 = ast_to_sexpr e1 in
      let se2 = ast_to_sexpr e2 in
      (match op with
      | Concat ->
          (* Concat does NOT equalize sizes - result is sum of sizes *)
          Sexpr.Expr.binary Sexpr.Expr.Concat se1 se2
      | _ ->
          (* Equalize sizes for all other binary operations *)
          let (se1', se2') = equalize_sizes_sexpr se1 se2 in
          (match op with
          | Add -> Sexpr.Expr.binary Sexpr.Expr.Plus se1' se2'
          | Sub -> Sexpr.Expr.binary Sexpr.Expr.Minus se1' se2'
          | Mul -> Sexpr.Expr.binary Sexpr.Expr.Mul se1' se2'
          | Udiv -> Sexpr.Expr.binary Sexpr.Expr.Udiv se1' se2'
          | Sdiv -> Sexpr.Expr.binary Sexpr.Expr.Sdiv se1' se2'
          | Umod -> Sexpr.Expr.binary Sexpr.Expr.Umod se1' se2'
          | Smod -> Sexpr.Expr.binary Sexpr.Expr.Smod se1' se2'
          | Lsl -> Sexpr.Expr.binary Sexpr.Expr.Lsl se1' se2'
          | Lsr -> Sexpr.Expr.binary Sexpr.Expr.Lsr se1' se2'
          | Asr -> Sexpr.Expr.binary Sexpr.Expr.Asr se1' se2'
          | Rol -> Sexpr.Expr.binary Sexpr.Expr.Rol se1' se2'
          | Ror -> Sexpr.Expr.binary Sexpr.Expr.Ror se1' se2'
          | And -> Sexpr.Expr.binary Sexpr.Expr.And se1' se2'
          | Or -> Sexpr.Expr.binary Sexpr.Expr.Or se1' se2'
          | Xor -> Sexpr.Expr.binary Sexpr.Expr.Xor se1' se2'
          | Eq -> Sexpr.Expr.binary Sexpr.Expr.Eq se1' se2'
          | Concat -> assert false (* handled above *)))
  | UnOp (op, e) ->
      let se = ast_to_sexpr e in
      (match op with
      | Not -> Sexpr.Expr.lognot se
      | Minus -> Sexpr.Expr.uminus se)
  | Uext (ext_size, e) ->
      let se = ast_to_sexpr e in
      (* uext(n) means extend BY n bits, not TO n bits *)
      Sexpr.Expr.unary (Sexpr.Expr.Uext ext_size) se
  | Sext (ext_size, e) ->
      let se = ast_to_sexpr e in
      (* sext(n) means extend BY n bits, not TO n bits *)
      Sexpr.Expr.unary (Sexpr.Expr.Sext ext_size) se
  | Restrict (lo, hi, e) ->
      let se = ast_to_sexpr e in
      Sexpr.Expr.restrict ~lo ~hi se
  | Power _ ->
      raise (Failure "Power should have been unrolled during parsing")
  | Ite (c, t, e) ->
      let sc = ast_to_sexpr c in
      let st = ast_to_sexpr t in
      let se = ast_to_sexpr e in
      (* the condition must be a 1-bit truth value; if it isn't a comparison
         already, reduce it to (c <> 0) *)
      let sc =
        if sexpr_sizeof sc = 1 then sc
        else
          Sexpr.Expr.binary Sexpr.Expr.Diff sc
            (Sexpr.Expr.constant (Bitvector.create Z.zero (sexpr_sizeof sc)))
      in
      let se1', se2' = equalize_sizes_sexpr st se in
      Sexpr.Expr.ite sc se1' se2'

(** Main parse function *)
let parse ?(default_size=64) (s : string) : Sexpr.Expr.t =
  try
    let ast = Parser.parse s default_size in
    let normalized_ast = normalize_sizes ast in
    ast_to_sexpr normalized_ast
  with Failure msg ->
    raise (Failure ("Parse error: " ^ msg))
