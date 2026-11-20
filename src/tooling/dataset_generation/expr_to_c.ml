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

(* expr_to_c.ml - Generate a C program from expression descriptions
 *
 * Reads a JSON description of an expression (or an array of them) and
 * generates one C file per expression under the output directory.
 *
 * Input JSON format (single object or array):
 *   {
 *     "name":         "target_xx",          (* function name; output file is name.c *)
 *     "expr":         "x<64> + y<64>",      (* expression string, same syntax as string_to_sexpr *)
 *     "input_signs":  {"x":"unsigned","y":"signed"},  (* optional: per-variable signedness *)
 *     "output_sign":  "signed"              (* optional: signedness of return type, default "unsigned" *)
 *   }
 *
 * Usage:
 *   expr_to_c -outdir <dir> [input.json]
 *   expr_to_c -outdir <dir>                        (reads JSON from stdin)
 *   expr_to_c -outdir <dir> -string "x<64>+y<64>" -name target_xx [-signed x,y]
 *)

(* ===== Variable extraction (mirrors c_extractor.ml) ===== *)

let rec extract_vars (e : Sexpr.Expr.t) : (string * int) list =
  match e with
  | Sexpr.Expr.Var v -> [ (v.name, v.size) ]
  | Sexpr.Expr.Cst _ -> []
  | Sexpr.Expr.Unary { x; _ } -> extract_vars x
  | Sexpr.Expr.Binary { x; y; _ } -> extract_vars x @ extract_vars y
  | Sexpr.Expr.Load _ -> []
  | Sexpr.Expr.Ite { c; t; e; _ } ->
      extract_vars c @ extract_vars t @ extract_vars e

let unique_vars (vars : (string * int) list) : (string * int) list =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun (name, _) ->
      if Hashtbl.mem seen name then false
      else (
        Hashtbl.add seen name ();
        true))
    vars
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

(* ===== C type generation ===== *)

let size_to_c_type ~(signed : bool) (size : int) : string =
  if signed then
    if size <= 8 then "signed char"
    else if size <= 16 then "short"
    else if size <= 32 then "int"
    else if size <= 64 then "long long"
    else "__int128"
  else if size <= 8 then "unsigned char"
  else if size <= 16 then "unsigned short"
  else if size <= 32 then "unsigned int"
  else if size <= 64 then "unsigned long long"
  else "__uint128_t"

(* ===== C expression generation ===== *)

(* Convert Sexpr.Expr.t to a C expression string.
   Handles Asr and Lsr correctly via explicit casts. *)
let rec sexpr_to_c (e : Sexpr.Expr.t) : string =
  match e with
  | Sexpr.Expr.Var v -> v.name
  | Sexpr.Expr.Cst bv ->
      let value = Bitvector.value_of bv in
      let size = Bitvector.size_of bv in
      if size <= 64 then
        let hex_digits = (size + 3) / 4 in
        let hex_str = Z.format "%x" value in
        let padded =
          String.make (max 0 (hex_digits - String.length hex_str)) '0' ^ hex_str
        in
        Printf.sprintf "0x%sULL" padded
      else
        let hex_str = Z.format "%x" value in
        let padded =
          String.make (max 0 (32 - String.length hex_str)) '0' ^ hex_str
        in
        Printf.sprintf "(__uint128_t)0x%sULL" padded
  | Sexpr.Expr.Unary { f; x; _ } -> (
      let inner = sexpr_to_c x in
      match f with
      | Sexpr.Expr.Minus -> Printf.sprintf "(-%s)" inner
      | Sexpr.Expr.Not -> Printf.sprintf "(~%s)" inner
      | Sexpr.Expr.Uext n ->
          let target_size = Sexpr.Expr.sizeof x + n in
          let cast = size_to_c_type ~signed:false target_size in
          Printf.sprintf "((%s)%s)" cast inner
      | Sexpr.Expr.Sext n ->
          let src_size = Sexpr.Expr.sizeof x in
          let target_size = src_size + n in
          let signed_src = size_to_c_type ~signed:true src_size in
          let cast = size_to_c_type ~signed:true target_size in
          Printf.sprintf "((%s)((%s)%s))" cast signed_src inner
      | Sexpr.Expr.Restrict { lo; hi } ->
          let width = hi - lo + 1 in
          let mask = Z.sub (Z.shift_left Z.one width) Z.one in
          let hex_str = Z.format "%x" mask in
          let padded =
            String.make (max 0 (16 - String.length hex_str)) '0' ^ hex_str
          in
          if lo = 0 then Printf.sprintf "(%s & 0x%sULL)" inner padded
          else Printf.sprintf "((%s >> %d) & 0x%sULL)" inner lo padded)
  | Sexpr.Expr.Binary { f; x; y; _ } -> (
      let left = sexpr_to_c x in
      let right = sexpr_to_c y in
      match f with
      (* Shifts: explicit cast ensures correct behavior regardless of variable type *)
      | Sexpr.Expr.Lsr ->
          let sz = Sexpr.Expr.sizeof x in
          let uc = size_to_c_type ~signed:false sz in
          Printf.sprintf "((%s)(%s) >> %s)" uc left right
      | Sexpr.Expr.Asr ->
          let sz = Sexpr.Expr.sizeof x in
          let sc = size_to_c_type ~signed:true sz in
          Printf.sprintf "((%s)(%s) >> %s)" sc left right
      | Sexpr.Expr.Rol ->
          let sz = Sexpr.Expr.sizeof x in
          Printf.sprintf "ROL%d(%s, %s)" sz left right
      | Sexpr.Expr.Ror ->
          let sz = Sexpr.Expr.sizeof x in
          Printf.sprintf "ROR%d(%s, %s)" sz left right
      | Sexpr.Expr.Concat ->
          let lsz = Sexpr.Expr.sizeof x in
          let rsz = Sexpr.Expr.sizeof y in
          let total = lsz + rsz in
          let uc = size_to_c_type ~signed:false total in
          Printf.sprintf "(((%s)(%s) << %d) | (%s)(%s))" uc left rsz uc right
      | _ ->
          let op =
            match f with
            | Sexpr.Expr.Plus -> "+"
            | Sexpr.Expr.Minus -> "-"
            | Sexpr.Expr.Mul -> "*"
            | Sexpr.Expr.Udiv -> "/"
            | Sexpr.Expr.Sdiv -> "/"
            | Sexpr.Expr.Umod -> "%"
            | Sexpr.Expr.Smod -> "%"
            | Sexpr.Expr.Or -> "|"
            | Sexpr.Expr.And -> "&"
            | Sexpr.Expr.Xor -> "^"
            | Sexpr.Expr.Lsl -> "<<"
            | Sexpr.Expr.Eq -> "=="
            | Sexpr.Expr.Diff -> "!="
            | Sexpr.Expr.Ule | Sexpr.Expr.Sle -> "<="
            | Sexpr.Expr.Ult | Sexpr.Expr.Slt -> "<"
            | Sexpr.Expr.Uge | Sexpr.Expr.Sge -> ">="
            | Sexpr.Expr.Ugt | Sexpr.Expr.Sgt -> ">"
            | Sexpr.Expr.Lsr | Sexpr.Expr.Asr | Sexpr.Expr.Rol | Sexpr.Expr.Ror
            | Sexpr.Expr.Concat ->
                assert false (* handled above *)
          in
          Printf.sprintf "(%s %s %s)" left op right)
  | Sexpr.Expr.Load _ -> failwith "Load is not directly expressible in C"
  | Sexpr.Expr.Ite { c; t; e; _ } ->
      Printf.sprintf "(%s ? %s : %s)" (sexpr_to_c c) (sexpr_to_c t)
        (sexpr_to_c e)

(* ===== Expression descriptor ===== *)

type expr_desc = {
  name : string;
  expr_str : string;
  input_signs : (string, bool) Hashtbl.t; (* true = signed *)
  output_signed : bool;
}

(* ===== JSON parsing ===== *)

let parse_signs (json : Yojson.Basic.t) : (string, bool) Hashtbl.t =
  let tbl = Hashtbl.create 8 in
  (match json with
  | `Assoc pairs ->
      List.iter
        (fun (var, v) ->
          let s =
            match v with
            | `String "signed" -> true
            | `String "unsigned" -> false
            | _ ->
                Format.eprintf
                  "Warning: unknown sign '%s' for '%s', defaulting to unsigned\n"
                  (Yojson.Basic.to_string v) var;
                false
          in
          Hashtbl.replace tbl var s)
        pairs
  | _ -> ());
  tbl

let parse_one (json : Yojson.Basic.t) : expr_desc =
  let open Yojson.Basic.Util in
  let name = json |> member "name" |> to_string in
  let expr_str = json |> member "expr" |> to_string in
  let input_signs =
    match json |> member "input_signs" with
    | `Null -> Hashtbl.create 0
    | j -> parse_signs j
  in
  let output_signed =
    match json |> member "output_sign" with
    | `String "signed" -> true
    | _ -> false
  in
  { name; expr_str; input_signs; output_signed }

let parse_json (json : Yojson.Basic.t) : expr_desc list =
  match json with
  | `List items -> List.map parse_one items
  | `Assoc _ -> [ parse_one json ]
  | _ -> failwith "Expected a JSON object or array of objects"

(* ===== C file generation ===== *)

(* Collect sizes of ROL/ROR operands so we can emit the right macros *)
let rot_macro_sizes (e : Sexpr.Expr.t) : int list =
  let sizes = Hashtbl.create 4 in
  let rec scan expr =
    match expr with
    | Sexpr.Expr.Binary { f = Sexpr.Expr.Rol; x; _ }
    | Sexpr.Expr.Binary { f = Sexpr.Expr.Ror; x; _ } ->
        Hashtbl.replace sizes (Sexpr.Expr.sizeof x) ();
        scan x
    | Sexpr.Expr.Unary { x; _ } -> scan x
    | Sexpr.Expr.Binary { x; y; _ } ->
        scan x;
        scan y
    | Sexpr.Expr.Ite { c; t; e; _ } ->
        scan c;
        scan t;
        scan e
    | _ -> ()
  in
  scan e;
  Hashtbl.fold (fun sz () acc -> sz :: acc) sizes [] |> List.sort compare

let emit_c_file (outdir : string) (sexpr : Sexpr.Expr.t) (desc : expr_desc) :
    unit =
  let path = Filename.concat outdir (desc.name ^ ".c") in
  let oc =
    try open_out path
    with Sys_error msg ->
      Format.eprintf "Error creating '%s': %s\n" path msg;
      exit 1
  in
  let fprintf = Printf.fprintf in

  (* Header: use native C types so GoblintCIL can parse without preprocessing *)

  (* ROL/ROR macros if needed *)
  let rot_sizes = rot_macro_sizes sexpr in
  if rot_sizes <> [] then (
    fprintf oc "\n/* Rotation macros */\n";
    List.iter
      (fun sz ->
        let t = size_to_c_type ~signed:false sz in
        fprintf oc
          "#define ROL%d(x, n) (((%s)(x) << (n)) | ((%s)(x) >> (%d - (n))))\n"
          sz t t sz;
        fprintf oc
          "#define ROR%d(x, n) (((%s)(x) >> (n)) | ((%s)(x) << (%d - (n))))\n"
          sz t t sz)
      rot_sizes);
  fprintf oc "\n";

  (* Function signature *)
  let vars = unique_vars (extract_vars sexpr) in
  let return_size = Sexpr.Expr.sizeof sexpr in
  let return_type = size_to_c_type ~signed:desc.output_signed return_size in
  fprintf oc "%s %s(" return_type desc.name;
  (match vars with
  | [] -> fprintf oc "void"
  | _ ->
      List.iteri
        (fun i (vname, size) ->
          if i > 0 then fprintf oc ", ";
          let signed =
            Option.value ~default:false
              (Hashtbl.find_opt desc.input_signs vname)
          in
          fprintf oc "%s %s" (size_to_c_type ~signed size) vname)
        vars);
  fprintf oc ") {\n";

  (* Body *)
  (try
     let c_expr = sexpr_to_c sexpr in
     fprintf oc "    return %s;\n" c_expr
   with Failure msg ->
     fprintf oc "    /* Error: %s */\n" msg;
     fprintf oc "    return 0;\n");
  fprintf oc "}\n\n";

  (* Minimal main *)
  fprintf oc "int main(void) { return 0; }\n";
  close_out oc;
  Format.printf "Written: %s\n" path

(* ===== Main ===== *)

let () =
  let json_file = ref None in
  let outdir = ref None in
  let string_expr = ref None in
  let func_name = ref "target" in
  let signed_vars = ref [] in

  let usage_msg =
    "Usage:\n\
    \  expr_to_c -outdir <dir> [input.json]\n\
    \  expr_to_c -outdir <dir>                       (reads JSON from stdin)\n\
    \  expr_to_c -outdir <dir> -string \"x<64>+y<64>\" -name target_xx\n\n\
     JSON format (object or array of objects):\n\
    \  {\n\
    \    \"name\":         \"target_xx\",\n\
    \    \"expr\":         \"x<64> + y<64>\",\n\
    \    \"input_signs\":  {\"x\": \"unsigned\", \"y\": \"signed\"},\n\
    \    \"output_sign\":  \"unsigned\"\n\
    \  }\n\n\
     Options:"
  in

  let spec =
    [
      ( "-outdir",
        Arg.String (fun s -> outdir := Some s),
        "<dir> Directory to write .c files into (required)" );
      ( "-string",
        Arg.String (fun s -> string_expr := Some s),
        "<expr> Expression string (alternative to JSON input)" );
      ( "-name",
        Arg.String (fun s -> func_name := s),
        "<name> Function name when using -string (default: \"target\")" );
      ( "-signed",
        Arg.String
          (fun s ->
            signed_vars := List.map String.trim (String.split_on_char ',' s)),
        "<vars> Comma-separated signed variables when using -string (e.g. x,y)"
      );
    ]
  in

  Arg.parse spec (fun s -> json_file := Some s) usage_msg;

  let outdir =
    match !outdir with
    | None ->
        Arg.usage spec usage_msg;
        exit 1
    | Some d ->
        if not (Sys.file_exists d) then Unix.mkdir d 0o755;
        d
  in

  let descs =
    match !string_expr with
    | Some expr ->
        let signs = Hashtbl.create 4 in
        List.iter (fun v -> Hashtbl.replace signs v true) !signed_vars;
        [
          {
            name = !func_name;
            expr_str = expr;
            input_signs = signs;
            output_signed = false;
          };
        ]
    | None -> (
        let json =
          match !json_file with
          | None -> Yojson.Basic.from_channel stdin
          | Some path -> (
              try Yojson.Basic.from_file path
              with Sys_error msg ->
                Format.eprintf "Error reading '%s': %s\n" path msg;
                exit 1)
        in
        try parse_json json
        with Failure msg ->
          Format.eprintf "Error parsing JSON: %s\n" msg;
          exit 1)
  in

  List.iter
    (fun desc ->
      match String_to_sexpr.parse desc.expr_str with
      | exception Failure msg ->
          Format.eprintf "Error parsing expr for '%s': %s\n" desc.name msg
      | sexpr -> emit_c_file outdir sexpr desc)
    descs
