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

let byte_size = (Basic_types.Constants.bytesize :> int)

type size = int
type 'a interval = 'a Interval.t = { lo : 'a; hi : 'a }
type endianness = Machine.endianness = LittleEndian | BigEndian

let pp_endiannesss ppf = function
  | LittleEndian -> Format.pp_print_char ppf 'L'
  | BigEndian -> Format.pp_print_char ppf 'B'

type unary = U
and binary = B

type _ operator =
  | Not : unary operator
  | Sext : size -> unary operator
  | Uext : size -> unary operator
  | Restrict : int interval -> unary operator
  | Plus : binary operator
  | Minus : _ operator
  | Mul : binary operator
  | Udiv : binary operator (* Corresponds to *)
  | Umod : binary operator (* the truncated division *)
  | Sdiv : binary operator (* of C99 and most *)
  | Smod : binary operator (* processors *)
  | Or : binary operator
  | And : binary operator
  | Xor : binary operator
  | Concat : binary operator
  | Lsl : binary operator
  | Lsr : binary operator
  | Asr : binary operator
  | Rol : binary operator
  | Ror : binary operator
  | Eq : binary operator
  | Diff : binary operator
  | Ule : binary operator
  | Ult : binary operator
  | Uge : binary operator
  | Ugt : binary operator
  | Sle : binary operator
  | Slt : binary operator
  | Sge : binary operator
  | Sgt : binary operator

module Op = struct
  type 'a t = 'a operator

  external to_int : 'a t -> int = "%identity"

  let equal : type a b. a t -> b t -> bool =
   fun t t' ->
    match (t, t') with
    | ( ( Not | Plus | Minus | Mul | Udiv | Umod | Sdiv | Smod | Or | And | Xor
        | Concat | Lsl | Lsr | Asr | Rol | Ror | Eq | Diff | Ule | Ult | Uge
        | Ugt | Sle | Slt | Sge | Sgt ),
        ( Not | Plus | Minus | Mul | Udiv | Umod | Sdiv | Smod | Or | And | Xor
        | Concat | Lsl | Lsr | Asr | Rol | Ror | Eq | Diff | Ule | Ult | Uge
        | Ugt | Sle | Slt | Sge | Sgt ) ) ->
        to_int t = to_int t'
    | Sext n, Sext n' | Uext n, Uext n' -> n = n'
    | Restrict { hi; lo }, Restrict { hi = hi'; lo = lo' } ->
        hi = hi' && lo = lo'
    | ( ( Not | Sext _ | Uext _ | Restrict _ | Plus | Minus | Mul | Udiv | Umod
        | Sdiv | Smod | Or | And | Xor | Concat | Lsl | Lsr | Asr | Rol | Ror
        | Eq | Diff | Ule | Ult | Uge | Ugt | Sle | Slt | Sge | Sgt ),
        ( Not | Sext _ | Uext _ | Restrict _ | Plus | Minus | Mul | Udiv | Umod
        | Sdiv | Smod | Or | And | Xor | Concat | Lsl | Lsr | Asr | Rol | Ror
        | Eq | Diff | Ule | Ult | Uge | Ugt | Sle | Slt | Sge | Sgt ) ) ->
        false

  let compare : type a b. a t -> b t -> int =
   fun t t' ->
    match (t, t') with
    | ( ( Not | Plus | Minus | Mul | Udiv | Umod | Sdiv | Smod | Or | And | Xor
        | Concat | Lsl | Lsr | Asr | Rol | Ror | Eq | Diff | Ule | Ult | Uge
        | Ugt | Sle | Slt | Sge | Sgt ),
        ( Not | Plus | Minus | Mul | Udiv | Umod | Sdiv | Smod | Or | And | Xor
        | Concat | Lsl | Lsr | Asr | Rol | Ror | Eq | Diff | Ule | Ult | Uge
        | Ugt | Sle | Slt | Sge | Sgt ) ) ->
        to_int t - to_int t'
    | Sext n, Sext n' | Uext n, Uext n' -> n - n'
    | Restrict { hi; lo }, Restrict { hi = hi'; lo = lo' } ->
        let d = hi - hi' in
        if d = 0 then lo - lo' else d
    | ( ( Not | Plus | Minus | Mul | Udiv | Umod | Sdiv | Smod | Or | And | Xor
        | Concat | Lsl | Lsr | Asr | Rol | Ror | Eq | Diff | Ule | Ult | Uge
        | Ugt | Sle | Slt | Sge | Sgt ),
        Sext _ ) ->
        -1
    | ( Sext _,
        ( Not | Plus | Minus | Mul | Udiv | Umod | Sdiv | Smod | Or | And | Xor
        | Concat | Lsl | Lsr | Asr | Rol | Ror | Eq | Diff | Ule | Ult | Uge
        | Ugt | Sle | Slt | Sge | Sgt ) ) ->
        1
    | ( ( Not | Plus | Minus | Mul | Udiv | Umod | Sdiv | Smod | Or | And | Xor
        | Concat | Lsl | Lsr | Asr | Rol | Ror | Eq | Diff | Ule | Ult | Uge
        | Ugt | Sle | Slt | Sge | Sgt | Sext _ ),
        Uext _ ) ->
        -1
    | ( Uext _,
        ( Not | Plus | Minus | Mul | Udiv | Umod | Sdiv | Smod | Or | And | Xor
        | Concat | Lsl | Lsr | Asr | Rol | Ror | Eq | Diff | Ule | Ult | Uge
        | Ugt | Sle | Slt | Sge | Sgt | Sext _ ) ) ->
        1
    | ( ( Not | Plus | Minus | Mul | Udiv | Umod | Sdiv | Smod | Or | And | Xor
        | Concat | Lsl | Lsr | Asr | Rol | Ror | Eq | Diff | Ule | Ult | Uge
        | Ugt | Sle | Slt | Sge | Sgt | Sext _ | Uext _ ),
        Restrict _ ) ->
        -1
    | ( Restrict _,
        ( Not | Plus | Minus | Mul | Udiv | Umod | Sdiv | Smod | Or | And | Xor
        | Concat | Lsl | Lsr | Asr | Rol | Ror | Eq | Diff | Ule | Ult | Uge
        | Ugt | Sle | Slt | Sge | Sgt | Sext _ | Uext _ ) ) ->
        1

  let hash : type a. a t -> int =
   fun t ->
    match t with
    | Sext _ | Uext _ | Restrict _ -> Hashtbl.hash t
    | Not | Plus | Minus | Mul | Udiv | Umod | Sdiv | Smod | Or | And | Xor
    | Concat | Lsl | Lsr | Asr | Rol | Ror | Eq | Diff | Ule | Ult | Uge | Ugt
    | Sle | Slt | Sge | Sgt ->
        to_int t

  let pp : type a. Format.formatter -> a operator -> unit =
   fun ppf -> function
    | Minus -> Format.pp_print_char ppf '-'
    | Not -> Format.pp_print_char ppf '!'
    | Sext n -> Format.fprintf ppf "sext +%d" n
    | Uext n -> Format.fprintf ppf "uext +%d" n
    | Restrict { lo; hi } ->
        if lo = hi then Format.fprintf ppf "select %d" lo
        else Format.fprintf ppf "select <%d .. %d>" hi lo
    | Plus -> Format.pp_print_char ppf '+'
    | Mul -> Format.pp_print_char ppf '*'
    | Udiv -> Format.pp_print_string ppf "udiv"
    | Sdiv -> Format.pp_print_string ppf "sdiv"
    | Umod -> Format.pp_print_string ppf "umod"
    | Smod -> Format.pp_print_string ppf "smod"
    | Or -> Format.pp_print_string ppf "or"
    | And -> Format.pp_print_string ppf "and"
    | Xor -> Format.pp_print_string ppf "xor"
    | Concat -> Format.pp_print_string ppf "::"
    | Lsl -> Format.pp_print_string ppf "lsl"
    | Lsr -> Format.pp_print_string ppf "lsr"
    | Asr -> Format.pp_print_string ppf "asr"
    | Rol -> Format.pp_print_string ppf "rol"
    | Ror -> Format.pp_print_string ppf "ror"
    | Eq -> Format.pp_print_char ppf '='
    | Diff -> Format.pp_print_string ppf "<>"
    | Ule -> Format.pp_print_string ppf "ule"
    | Ult -> Format.pp_print_string ppf "ult"
    | Uge -> Format.pp_print_string ppf "uge"
    | Ugt -> Format.pp_print_string ppf "ugt"
    | Sle -> Format.pp_print_string ppf "sle"
    | Slt -> Format.pp_print_string ppf "slt"
    | Sge -> Format.pp_print_string ppf "sge"
    | Sgt -> Format.pp_print_string ppf "sgt"
end

type (_, 'a, 'b) t =
  | Var : {
      hash : int;
      size : size;
      name : string;
      label : 'a;
    }
      -> ([< `Var | `Loc | `Exp ], 'a, _) t
  | Load : {
      hash : int;
      len : size;
      dir : endianness;
      mutable addr : ([ `Exp ], 'a, 'b) t;
      label : 'b;
    }
      -> ([< `Mem | `Loc | `Exp ], 'a, 'b) t
  | Cst : Bitvector.t -> ([< `Cst | `Exp ], _, _) t
  | Unary : {
      hash : int;
      size : size;
      f : unary operator;
      mutable x : ([ `Exp ], 'a, 'b) t;
    }
      -> ([< `Unary | `Exp ], 'a, 'b) t
  | Binary : {
      hash : int;
      size : size;
      f : binary operator;
      mutable x : ([ `Exp ], 'a, 'b) t;
      mutable y : ([ `Exp ], 'a, 'b) t;
    }
      -> ([< `Binary | `Exp ], 'a, 'b) t
  | Ite : {
      hash : int;
      size : size;
      mutable c : ([ `Exp ], 'a, 'b) t;
      mutable t : ([ `Exp ], 'a, 'b) t;
      mutable e : ([ `Exp ], 'a, 'b) t;
    }
      -> ([< `Ite | `Exp ], 'a, 'b) t

let rec pp : type k. Format.formatter -> (k, 'a, 'b) t -> unit =
 fun ppf -> function
  | Var { name; size; _ } -> Format.fprintf ppf "%s<%d>" name size
  | Load { len; dir; addr; _ } ->
      Format.fprintf ppf "%@[%a]%d%a" pp addr len pp_endiannesss dir
  | Cst bv -> Bitvector.pp_hex_or_bin ppf bv
  | Unary { f; x; _ } -> Format.fprintf ppf "@[(%a %a)@]" Op.pp f pp x
  | Binary { f; x; y; _ } ->
      Format.fprintf ppf "@[(%a %a %a)@]" Op.pp f pp x pp y
  | Ite { c; t; e; _ } -> Format.fprintf ppf "@[(%a ? %a : %a)@]" pp c pp t pp e

let to_string t = Format.asprintf "%a" pp t
let abort t = raise (Invalid_argument (to_string t))

let hash : type k. (k, _, _) t -> int = function
  | Cst bv -> Bitvector.hash bv
  | Load { hash; _ } -> hash
  | Var { hash; _ } -> hash
  | Unary { hash; _ } -> hash
  | Binary { hash; _ } -> hash
  | Ite { hash; _ } -> hash

let sizeof : type k. (k, _, _) t -> int = function
  | Cst bv -> Bitvector.size_of bv
  | Load { len; _ } -> byte_size * len
  | Var { size; _ } -> size
  | Unary { size; _ } -> size
  | Binary { size; _ } -> size
  | Ite { size; _ } -> size

type ('a, 'b) any = Term : (_, 'a, 'b) t -> ('a, 'b) any [@@unboxed]

let to_exp t =
  match Term t with
  | Term (Var _ as v) -> v
  | Term (Load _ as l) -> l
  | Term (Cst _ as c) -> c
  | Term (Unary _ as u) -> u
  | Term (Binary _ as b) -> b
  | Term (Ite _ as i) -> i

let to_var t = match Term t with Term (Var _ as v) -> Some v | _ -> None
let to_var_exn t = match Term t with Term (Var _ as v) -> v | _ -> abort t

let to_loc t =
  match Term t with
  | Term (Var _ as v) -> Some v
  | Term (Load _ as l) -> Some l
  | _ -> None

let to_loc_exn t =
  match Term t with
  | Term (Var _ as v) -> v
  | Term (Load _ as l) -> l
  | _ -> abort t

let to_mem t = match Term t with Term (Load _ as l) -> Some l | _ -> None
let to_mem_exn t = match Term t with Term (Load _ as l) -> l | _ -> abort t
let to_cst t = match Term t with Term (Cst _ as c) -> Some c | _ -> None
let to_cst_exn t = match Term t with Term (Cst _ as c) -> c | _ -> abort t

module Bv = struct
  include Bitvector

  let unary f x =
    match f with
    | Not -> lognot x
    | Minus -> neg x
    | Uext n -> extend x (size_of x + n)
    | Sext n -> extend_signed x (size_of x + n)
    | Restrict it -> extract x it

  let binary f x y =
    let n = size_of x in
    if f <> Concat && n <> size_of y then
      abort (Binary { f; x = Cst x; y = Cst y; size = n; hash = 0 })
    else
      match f with
      | Plus -> add x y
      | Minus -> sub x y
      | Mul -> mul x y
      | Udiv -> udiv x y
      | Umod -> umod x y
      | Sdiv -> sdiv x y
      | Smod -> smod x y
      | Or -> logor x y
      | And -> logand x y
      | Xor -> logxor x y
      | Eq -> of_bool (equal x y)
      | Diff -> of_bool (diff x y)
      | Ule -> of_bool (ule x y)
      | Ult -> of_bool (ult x y)
      | Uge -> of_bool (uge x y)
      | Ugt -> of_bool (ugt x y)
      | Sle -> of_bool (sle x y)
      | Slt -> of_bool (slt x y)
      | Sge -> of_bool (sge x y)
      | Sgt -> of_bool (sgt x y)
      | Lsl -> shift_left x (to_uint y)
      | Lsr -> shift_right x (to_uint y)
      | Asr -> shift_right_signed x (to_uint y)
      | Rol -> rotate_left x (to_uint y)
      | Ror -> rotate_right x (to_uint y)
      | Concat -> append x y

  let extract ~lo ~hi x = extract x { lo; hi }
end

module type S = sig
  type a
  and b

  type nonrec size = size
  type nonrec 'a interval = 'a interval = { lo : 'a; hi : 'a }
  type nonrec endianness = endianness = LittleEndian | BigEndian

  type 'a op = 'a operator =
    | Not : unary op
    | Sext : size -> unary op
    | Uext : size -> unary op
    | Restrict : int interval -> unary op
    | Plus : binary op
    | Minus : _ op
    | Mul : binary op
    | Udiv : binary op (* Corresponds to *)
    | Umod : binary op (* the truncated division *)
    | Sdiv : binary op (* of C99 and most *)
    | Smod : binary op (* processors *)
    | Or : binary op
    | And : binary op
    | Xor : binary op
    | Concat : binary op
    | Lsl : binary op
    | Lsr : binary op
    | Asr : binary op
    | Rol : binary op
    | Ror : binary op
    | Eq : binary op
    | Diff : binary op
    | Ule : binary op
    | Ult : binary op
    | Uge : binary op
    | Ugt : binary op
    | Sle : binary op
    | Slt : binary op
    | Sge : binary op
    | Sgt : binary op

  type ('k, 'a, 'b) term = ('k, 'a, 'b) t = private
    | Var : {
        hash : int;
        size : size;
        name : string;
        label : 'a;
      }
        -> ([< `Var | `Loc | `Exp ], 'a, _) term
    | Load : {
        hash : int;
        len : size;
        dir : endianness;
        mutable addr : ([ `Exp ], 'a, 'b) term;
        label : 'b;
      }
        -> ([< `Mem | `Loc | `Exp ], 'a, 'b) term
    | Cst : Bitvector.t -> ([< `Cst | `Exp ], _, _) term
    | Unary : {
        hash : int;
        size : size;
        f : unary operator;
        mutable x : ([ `Exp ], 'a, 'b) term;
      }
        -> ([< `Unary | `Exp ], 'a, 'b) term
    | Binary : {
        hash : int;
        size : size;
        f : binary operator;
        mutable x : ([ `Exp ], 'a, 'b) term;
        mutable y : ([ `Exp ], 'a, 'b) term;
      }
        -> ([< `Binary | `Exp ], 'a, 'b) term
    | Ite : {
        hash : int;
        size : size;
        mutable c : ([ `Exp ], 'a, 'b) term;
        mutable t : ([ `Exp ], 'a, 'b) term;
        mutable e : ([ `Exp ], 'a, 'b) term;
      }
        -> ([< `Ite | `Exp ], 'a, 'b) term

  type t = ([ `Exp ], a, b) term

  (** {2 Smart constructors} *)

  val var : string -> size -> a -> t
  (** [var name bitsize label] *)

  val load : size -> endianness -> t -> b -> t
  (** [load nbytes endianness addr label] *)

  val constant : Bitvector.t -> t
  (** [constant bv] creates a constant expression from the bitvector [bv].
  *)

  val unary : unary op -> t -> t
  (** [unary f x] creates a unary application of [f] on [x].
  *)

  val binary : binary op -> t -> t -> t
  (** [binary f x y] creates a binary application of [f] on [x] and [y].
  *)

  val ite : t -> t -> t -> t
  (** [ite c t e] creates an if-then-else expression [c] ? [t] : [e].
  *)

  val uminus : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val smod : t -> t -> t
  val umod : t -> t -> t
  val udiv : t -> t -> t
  val sdiv : t -> t -> t
  val append : t -> t -> t
  val equal : t -> t -> t
  val diff : t -> t -> t
  val ule : t -> t -> t
  val uge : t -> t -> t
  val ult : t -> t -> t
  val ugt : t -> t -> t
  val sle : t -> t -> t
  val sge : t -> t -> t
  val slt : t -> t -> t
  val sgt : t -> t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val lognot : t -> t
  val logxor : t -> t -> t
  val shift_left : t -> t -> t
  val shift_right : t -> t -> t

  val shift_right_signed : t -> t -> t
  (** [shift_(left|right) e q] shifts expression [e] by quantity [q], padding
      with zeroes *)

  val rotate_left : t -> t -> t

  val rotate_right : t -> t -> t
  (** [rotate_(left|right) e q] rotates expression [e] by quantity [q] *)

  val sext : size -> t -> t
  (** [sext sz e] performs a signed extension of expression [e] to size [sz] *)

  val uext : size -> t -> t
  (** [uext sz e] performs an unsigned extension expression [e] to size [sz] *)

  val restrict : lo:int -> hi:int -> t -> t
  (** [restrict lo hi e] creates [Dba.ExprUnary(Restrict(lo, hi), e)] if
      [hi >= lo && lo >=0] .
  *)

  val bit_restrict : int -> t -> t
  (** [bit_restrict o e] is [restrict o o e] *)

  (** {3 Specific constants }*)

  val zeros : int -> t
  (** [zeros n] creates a constant expression of value 0 with length [n] *)

  val ones : int -> t
  (** [ones n] creates a constant expression of value 1 with length [n].
      I.e.; it has (n - 1) zeros in binary.
  *)

  val one : t
  val zero : t
  val addi : t -> int -> t
  val addz : t -> Z.t -> t

  (** {4 Utils} **)

  val hash : t -> int
  (** [hash t] returns the hash of [t] in constant time.
  *)

  val is_equal : t -> t -> bool
  val compare : t -> t -> int

  val sizeof : t -> size
  (** [sizeof t] returns the bit size of [t] in constant time.
  *)

  val map :
    (string -> int -> 'a -> t) ->
    (int -> Machine.endianness -> t -> 'b -> t) ->
    (_, 'a, 'b) term ->
    t

  (** {2 Raw constructors} *)
  val _unary : unary op -> t -> t
  (** [_unary f x] creates a unary application of [f] on [x].
  *)

  val _binary : binary op -> t -> t -> t
  (** [_binary f x y] creates a binary application of [f] on [x] and [y].
  *)

  val _ite : t -> t -> t -> t
  (** [_ite c t e] creates an if-then-else expression [c] ? [t] : [e].
  *)
end

module Make (A : Sigs.HASHABLE) (B : Sigs.HASHABLE) :
  S with type a := A.t and type b := B.t = struct
  type nonrec size = size
  type nonrec 'a interval = 'a interval = { lo : 'a; hi : 'a }
  type nonrec endianness = endianness = LittleEndian | BigEndian

  type 'a op = 'a operator =
    | Not : unary op
    | Sext : size -> unary op
    | Uext : size -> unary op
    | Restrict : int interval -> unary op
    | Plus : binary op
    | Minus : _ op
    | Mul : binary op
    | Udiv : binary op (* Corresponds to *)
    | Umod : binary op (* the truncated division *)
    | Sdiv : binary op (* of C99 and most *)
    | Smod : binary op (* processors *)
    | Or : binary op
    | And : binary op
    | Xor : binary op
    | Concat : binary op
    | Lsl : binary op
    | Lsr : binary op
    | Asr : binary op
    | Rol : binary op
    | Ror : binary op
    | Eq : binary op
    | Diff : binary op
    | Ule : binary op
    | Ult : binary op
    | Uge : binary op
    | Ugt : binary op
    | Sle : binary op
    | Slt : binary op
    | Sge : binary op
    | Sgt : binary op

  type ('k, 'a, 'b) term = ('k, 'a, 'b) t =
    | Var : {
        hash : int;
        size : size;
        name : string;
        label : 'a;
      }
        -> ([< `Var | `Loc | `Exp ], 'a, _) term
    | Load : {
        hash : int;
        len : size;
        dir : endianness;
        mutable addr : ([ `Exp ], 'a, 'b) term;
        label : 'b;
      }
        -> ([< `Mem | `Loc | `Exp ], 'a, 'b) term
    | Cst : Bitvector.t -> ([< `Cst | `Exp ], _, _) term
    | Unary : {
        hash : int;
        size : size;
        f : unary operator;
        mutable x : ([ `Exp ], 'a, 'b) term;
      }
        -> ([< `Unary | `Exp ], 'a, 'b) term
    | Binary : {
        hash : int;
        size : size;
        f : binary operator;
        mutable x : ([ `Exp ], 'a, 'b) term;
        mutable y : ([ `Exp ], 'a, 'b) term;
      }
        -> ([< `Binary | `Exp ], 'a, 'b) term
    | Ite : {
        hash : int;
        size : size;
        mutable c : ([ `Exp ], 'a, 'b) term;
        mutable t : ([ `Exp ], 'a, 'b) term;
        mutable e : ([ `Exp ], 'a, 'b) term;
      }
        -> ([< `Ite | `Exp ], 'a, 'b) term

  type t = ([ `Exp ], A.t, B.t) term

  let hash = hash

  external ( <! ) : 'a -> 'a -> bool = "cstubs_hashcons_older" [@@noalloc]

  let set_load_addr : ([ `Mem ], A.t, B.t) term -> t -> unit =
   fun (Load r) e -> r.addr <- e

  and set_unary_x : ([ `Unary ], A.t, B.t) term -> t -> unit =
   fun (Unary r) e -> r.x <- e

  and set_binary_x : ([ `Binary ], A.t, B.t) term -> t -> unit =
   fun (Binary r) e -> r.x <- e

  and set_binary_y : ([ `Binary ], A.t, B.t) term -> t -> unit =
   fun (Binary r) e -> r.y <- e

  and set_ite_c : ([ `Ite ], A.t, B.t) term -> t -> unit =
   fun (Ite r) e -> r.c <- e

  and set_ite_t : ([ `Ite ], A.t, B.t) term -> t -> unit =
   fun (Ite r) e -> r.t <- e

  and set_ite_e : ([ `Ite ], A.t, B.t) term -> t -> unit =
   fun (Ite r) e -> r.e <- e

  let is_equal =
    let rec is_equal_match t t' =
      match (t, t') with
      | Cst bv, Cst bv' -> Bv.equal bv bv'
      | Var r, Var r' ->
          r.hash = r'.hash && r.size = r'.size
          && String.equal r.name r'.name
          && A.equal r.label r'.label
      | (Load r as l), (Load r' as l') ->
          r.hash = r'.hash && r.len = r'.len && r.dir = r'.dir
          && is_equal_unify r.addr r'.addr set_load_addr l l'
          && B.equal r.label r'.label
      | (Unary r as u), (Unary r' as u') ->
          r.hash = r'.hash && r.f = r'.f
          && is_equal_unify r.x r'.x set_unary_x u u'
      | (Binary r as b), (Binary r' as b') ->
          r.hash = r'.hash && r.f = r'.f
          && is_equal_unify r.x r'.x set_binary_x b b'
          && is_equal_unify r.y r'.y set_binary_y b b'
      | (Ite r as i), (Ite r' as i') ->
          r.hash = r'.hash
          && is_equal_unify r.c r'.c set_ite_c i i'
          && is_equal_unify r.t r'.t set_ite_t i i'
          && is_equal_unify r.e r'.e set_ite_e i i'
      | _, _ -> false
    and is_equal_unify :
        type a.
        t ->
        t ->
        ((a, A.t, B.t) term -> t -> unit) ->
        (a, A.t, B.t) term ->
        (a, A.t, B.t) term ->
        bool =
     fun t t' f p p' ->
      t == t'
      || is_equal_match t t'
         &&
         (if t <! t' then f p' t else f p t';
          true)
    in
    fun t t' -> t == t' || is_equal_match t t'

  let compare =
    let rec compare_match t t' =
      match (t, t') with
      | Cst bv, Cst bv' -> Bv.compare bv bv'
      | Cst _, Load _ -> -1
      | Cst _, Unary _ -> -1
      | Cst _, Binary _ -> -1
      | Cst _, Ite _ -> -1
      | Cst _, Var _ -> -1
      | Load _, Cst _ -> 1
      | (Load r as l), (Load r' as l') ->
          let d = r.len - r'.len in
          if d <> 0 then d
          else
            let d = compare r.dir r'.dir in
            if d <> 0 then d
            else
              let d = compare_unify r.addr r'.addr set_load_addr l l' in
              if d <> 0 then d else B.compare r.label r'.label
      | Load _, Unary _ -> -1
      | Load _, Binary _ -> -1
      | Load _, Ite _ -> -1
      | Load _, Var _ -> -1
      | Unary _, Cst _ -> 1
      | Unary _, Load _ -> 1
      | (Unary r as u), (Unary r' as u') ->
          let d = Op.compare r.f r'.f in
          if d <> 0 then d
          else
            let d = r.size - r'.size in
            if d <> 0 then d else compare_unify r.x r'.x set_unary_x u u'
      | Unary _, Binary _ -> -1
      | Unary _, Ite _ -> -1
      | Unary _, Var _ -> -1
      | Binary _, Cst _ -> 1
      | Binary _, Load _ -> 1
      | Binary _, Unary _ -> 1
      | (Binary r as b), (Binary r' as b') ->
          let d = Op.compare r.f r'.f in
          if d <> 0 then d
          else
            let d = r.size - r'.size in
            if d <> 0 then d
            else
              let d = r.hash - r'.hash in
              if d <> 0 then d
              else
                let d = compare_unify r.x r'.x set_binary_x b b' in
                if d <> 0 then d else compare_unify r.y r'.y set_binary_y b b'
      | Binary _, Ite _ -> -1
      | Binary _, Var _ -> -1
      | Ite _, Cst _ -> 1
      | Ite _, Load _ -> 1
      | Ite _, Unary _ -> 1
      | Ite _, Binary _ -> 1
      | (Ite r as i), (Ite r' as i') ->
          let d = r.size - r'.size in
          if d <> 0 then d
          else
            let d = r.hash - r'.hash in
            if d <> 0 then d
            else
              let d = compare_unify r.c r'.c set_ite_c i i' in
              if d <> 0 then d
              else
                let d = compare_unify r.t r'.t set_ite_t i i' in
                if d <> 0 then d else compare_unify r.e r'.e set_ite_e i i'
      | Ite _, Var _ -> -1
      | Var _, Cst _ -> 1
      | Var _, Load _ -> 1
      | Var _, Unary _ -> 1
      | Var _, Binary _ -> 1
      | Var _, Ite _ -> 1
      | Var r, Var r' ->
          let d = r.size - r'.size in
          if d <> 0 then d
          else
            let d = String.compare r.name r'.name in
            if d <> 0 then d else A.compare r.label r'.label
    and compare_unify :
        type a.
        t ->
        t ->
        ((a, A.t, B.t) term -> t -> unit) ->
        (a, A.t, B.t) term ->
        (a, A.t, B.t) term ->
        int =
     fun t t' f p p' ->
      if t == t' then 0
      else
        let d = compare_match t t' in
        if d = 0 then if t <! t' then f p' t else f p t';
        d
    in
    fun t t' -> if t == t' then 0 else compare_match t t'


  let sizeof = sizeof

  let var name size label =
    Var
      {
        name;
        size;
        label;
        hash =
          Hash.(return @@ fold_int (fold_string (seed 0x88206212) name) size);
      }

  let load len dir addr label =
    Load
      {
        len;
        dir;
        addr;
        label;
        hash =
          Hash.(return @@ fold_int (fold_int (seed 0xc4dba348) len) (hash addr));
      }

  let constant bv = Cst bv

  let mk_unary f x =
    let size =
      match f with
      | Uext n | Sext n -> n + sizeof x
      | Restrict { lo; hi } -> hi - lo + 1
      | Not | Minus -> sizeof x
    in
    Unary
      {
        f;
        x;
        size;
        hash =
          Hash.(
            return
            @@ fold_int (fold_int (seed 0xec9576a) (Hashtbl.hash f)) (hash x));
      }

  let mk_binary f x y =
    let size =
      match f with
      | Eq | Diff | Ule | Ult | Uge | Ugt | Sle | Slt | Sge | Sgt -> 1
      | Concat -> sizeof x + sizeof y
      | _ -> sizeof x
    in
    Binary
      {
        f;
        x;
        y;
        size;
        hash =
          Hash.(
            return
            @@ fold_int
                 (fold_int
                    (fold_int (seed 0x4b8498a0) (Hashtbl.hash f))
                    (hash x))
                 (hash y));
      }

  let mk_ite c t e =
    Ite
      {
        c;
        t;
        e;
        size = sizeof t;
        hash =
          Hash.(
            return
            @@ fold_int
                 (fold_int (fold_int (seed 0x8bfe92b2) (hash c)) (hash t))
                 (hash e));
      }

  let zeros n = Cst (Bv.zeros n)
  let ones n = Cst (Bv.ones n)
  let one = Cst Bv.one
  let zero = Cst Bv.zero

  let  unary f x =
    match (f, x) with
    (* safety pattern guard *)
    (* TODO: move outside of the rec pattern if the rewriter is trusted *)
    | _, _ -> mk_unary f x

  and binary f x y _ =
    match (f, x, y) with
    | _, _, _ -> mk_binary f x y

  and ite c t e =
    match (c, t, e) with
    | _, _, _ -> mk_ite c t e

  let binary f x y =
    let sx = sizeof x and sy = sizeof y in
    if f <> Concat && sx <> sy then abort @@ mk_binary f x y;
    binary f x y sx

  let lognot t = unary Not t
  let uminus t = unary Minus t
  let sext n t = unary (Sext (n - sizeof t)) t
  let uext n t = unary (Uext (n - sizeof t)) t
  let restrict ~lo ~hi t = unary (Restrict { lo; hi }) t
  let bit_restrict i t = restrict ~lo:i ~hi:i t
  let add t t' = binary Plus t t'
  let sub t t' = binary Minus t t'
  let mul t t' = binary Mul t t'
  let smod t t' = binary Smod t t'
  let umod t t' = binary Umod t t'
  let udiv t t' = binary Udiv t t'
  let sdiv t t' = binary Sdiv t t'
  let logor t t' = binary Or t t'
  let logxor t t' = binary Xor t t'
  let logand t t' = binary And t t'
  let equal t t' = binary Eq t t'
  let diff t t' = binary Diff t t'
  let ule t t' = binary Ule t t'
  let sle t t' = binary Sle t t'
  let ult t t' = binary Ult t t'
  let slt t t' = binary Slt t t'
  let uge t t' = binary Uge t t'
  let sge t t' = binary Sge t t'
  let ugt t t' = binary Ugt t t'
  let sgt t t' = binary Sgt t t'
  let append t t' = binary Concat t t'
  let shift_left t t' = binary Lsl t t'
  let shift_right t t' = binary Lsr t t'
  let shift_right_signed t t' = binary Asr t t'
  let rotate_left t t' = binary Rol t t'
  let rotate_right t t' = binary Ror t t'
  let addi x y = binary Plus x (constant (Bv.of_int ~size:(sizeof x) y))
  let addz x y = binary Plus x (constant (Bv.create y (sizeof x)))

  let rec map :
      type k a b.
      (string -> int -> a -> t) ->
      (int -> Machine.endianness -> t -> b -> t) ->
      (k, a, b) term ->
      t =
   fun a b t ->
    match Term t with
    | Term (Var { name; size; label; _ }) -> a name size label
    | Term (Load { len; dir; addr; label; _ }) -> b len dir (map a b addr) label
    | Term (Cst _ as c) -> c
    | Term (Unary { f; x; _ }) -> unary f (map a b x)
    | Term (Binary { f; x; y; _ }) -> binary f (map a b x) (map a b y)
    | Term (Ite { c; t; e; _ }) -> ite (map a b c) (map a b t) (map a b e)

  let _unary = mk_unary
  let _binary = mk_binary
  let _ite = mk_ite
end
