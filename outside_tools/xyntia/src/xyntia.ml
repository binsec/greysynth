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

open Xyntia_utils

type synthesis_args = {
    heuristic : Heuristic.kind ;
    time      : int    ;
    dist : Distance.kind ;
    maxdepth : int     ;
    repeat: int     ;
    formula : string option;
    smt : string option;
    smt_to : int option;
    cegis : bool;
    infrules : Distance.infrule list;
    noguidance : bool;
    rules_combinator : string;
    check : bool;
}

type cegis_info = {
    nb_equiv_checks : int;
    time_equiv_check : float;
}

type target_info = {
    orig_size : int;
}

type synthesis_result = {
    synthesis: Heuristic.summary;
    equiv: Checker.equiv_answer;
    cegis : cegis_info option;
    target : target_info;
}

let default_args : synthesis_args = {
    heuristic = ILS;
    time = 60;
    dist = LOGARITH;
    maxdepth = max_int ;
    repeat = 0;
    formula = None;
    smt = None;
    smt_to = None ;
    cegis = false;
    infrules = [];
    noguidance = false;
    rules_combinator = "product";
    check = false;
}

let all_inference_rules : Distance.infrule list = [ MinOffset; MaskOTF; MinShiftLeft; MinUShiftRight; MinMul; MinXor; Rotate; Poly2; Affine; UDivX86; UModX86 ]
let mba_inference_rules : Distance.infrule list = [ MinOffset; MaskOTF; MinShiftLeft; MinUShiftRight; MinMul; MinXor ]
let n_cegis_loop = ref 1

let dump_synthesis args (synth_res : synthesis_result) (module O : Oracle.ORACLE) =
  match Sys.getenv_opt "XYNTIA_DUMP" with
  | None -> ()
  | Some dump_dir ->
      (* Create dump directory if it doesn't exist *)
      (try Unix.mkdir dump_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

      (* Generate unique filename based on timestamp and random number *)
      let timestamp = Unix.time () |> int_of_float in
      let random_id = Random.int 1000000 in
      let dump_filename = Printf.sprintf "%s/xyntia_dump_%d_%d.txt" dump_dir timestamp random_id in

      let oc = open_out dump_filename in
      let fprintf = Printf.fprintf in

      (* Dump synthesis arguments *)
      fprintf oc "=== Synthesis Arguments ===\n";
      fprintf oc "heuristic: %s\n" Heuristic.(string_of_kind args.heuristic);
      fprintf oc "time: %d\n" args.time;
      fprintf oc "dist: %s\n" (Distance.string_of_kind args.dist);
      fprintf oc "maxdepth: %d\n" args.maxdepth;
      fprintf oc "repeat: %d\n" args.repeat;
      fprintf oc "formula: %s\n" (match args.formula with Some f -> f | None -> "None");
      fprintf oc "smt: %s\n" (match args.smt with Some s -> s | None -> "None");
      fprintf oc "smt_to: %s\n" (match args.smt_to with Some t -> string_of_int t | None -> "None");
      fprintf oc "cegis: %b\n" args.cegis;
      fprintf oc "infrules: %s\n" (String.concat ", " (List.map Distance.string_of_infrule args.infrules));
      fprintf oc "noguidance: %b\n" args.noguidance;
      fprintf oc "rules_combinator: %s\n" args.rules_combinator;
      fprintf oc "check: %b\n" args.check;
      fprintf oc "\n";

      (* Dump oracle content: variables, their values, and output *)
      fprintf oc "=== Oracle Content ===\n";
      fprintf oc "Number of variables: %d\n" (O.nvars ());
      fprintf oc "Number of constants: %d\n" (O.nconsts ());
      fprintf oc "Number of samples: %d\n" (O.nsamples ());
      fprintf oc "\n";

      let vars = O.vars () in
      let consts = O.consts () in

      fprintf oc "Variables: ";
      Array.iter (fun v ->
        let var = (v : Tree.variable) in
        fprintf oc "%s " var.name
      ) vars;
      fprintf oc "\n";

      fprintf oc "Constants: ";
      Array.iter (fun (c : Tree.constant) -> fprintf oc "%s=%s " c.name (Bitvector.to_string c.value)) consts;
      fprintf oc "\n\n";

      (* Dump all samples with their values *)
      fprintf oc "Samples:\n";
      for i = 0 to O.nsamples () - 1 do
        let sample = O.get_sample i in
        let inputs = O.sample_inputs sample in
        let (out_var, out_val) = O.sample_output sample in
        fprintf oc "  Sample %d:\n" i;
        fprintf oc "    Inputs: ";
        Array.iter (fun ((v : Tree.variable), value) ->
          fprintf oc "%s=%s " v.name (Bitvector.to_string value)
        ) inputs;
        fprintf oc "\n";
        fprintf oc "    Output: %s=%s\n" out_var.name (Bitvector.to_string out_val)
      done;
      fprintf oc "\n";

      (* Dump synthesis results *)
      fprintf oc "=== Synthesis Results ===\n";
      fprintf oc "Success: %b\n" synth_res.synthesis.success;
      fprintf oc "Expression: %s\n" (Tree.string_of synth_res.synthesis.expression);
      fprintf oc "Simplified: %s\n" (Tree.string_of synth_res.synthesis.simplified);
      fprintf oc "SMTLib: %s\n" synth_res.synthesis.smtlib;
      fprintf oc "Size: %d\n" synth_res.synthesis.size;
      fprintf oc "Time Synthesis: %f\n" synth_res.synthesis.time_synthesis;
      fprintf oc "Time Simplify: %f\n" synth_res.synthesis.time_simplify;
      fprintf oc "Nb Mutations: %d\n" synth_res.synthesis.nb_mutations;
      fprintf oc "\n";

      (* Dump equivalence checking info *)
      fprintf oc "=== Equivalence Check ===\n";
      let equiv_str = match synth_res.equiv with
      | Checker.YES -> "YES"
      | Checker.NO -> "NO"
      | Checker.UKN -> "UNKNOWN"
      in
      fprintf oc "Equivalence: %s\n" equiv_str;
      fprintf oc "\n";

      (* Dump CEGIS info if available *)
      (match synth_res.cegis with
      | Some cegis_info ->
          fprintf oc "=== CEGIS Info ===\n";
          fprintf oc "Number of Equivalence Checks: %d\n" cegis_info.nb_equiv_checks;
          fprintf oc "Time Equivalence Check: %f\n" cegis_info.time_equiv_check;
          fprintf oc "\n"
      | None -> ());

      (* Dump target info *)
      fprintf oc "=== Target Info ===\n";
      fprintf oc "Original Size: %d\n" synth_res.target.orig_size;

      close_out oc;
      Format.eprintf "Dump written to: %s\n" dump_filename

let _remove_timeout () = 
  let open Sys in
  set_signal sigalrm (Signal_handle (fun _ -> ()));
  set_signal sigabrt (Signal_handle (fun _ -> ()))

let _set_timeout time_lim =
  if time_lim <= 0 then invalid_arg "Time limit must be positive";
  let open Sys in
  set_signal sigalrm (Signal_handle (fun _ -> Format.eprintf "halting!\n"; Format.pp_print_flush Format.err_formatter (); raise Exceptions.Halt));
  ignore (Unix.alarm time_lim)

let set_cegis_timeout time_lim =
  (* Only set the handler to catch abort signals 
      The signal must be send from the terminal, for example with
      timeout -s ABRT <timeout> <cmd>
  *)
  if time_lim <= 0 then invalid_arg "Time limit must be positive";
  let open Sys in
  set_signal sigabrt (Signal_handle (fun _ -> raise Exceptions.CEGISHalt))

let get_infrules_combinator args =
    let min_combinator x y = 
        let abs_x = abs_float x in
        let abs_y = abs_float y in
        if abs_x < abs_y then abs_x else abs_y
    in
    match args.rules_combinator with
    | "product" -> ( *. )
    | "min" -> min_combinator
    | _ -> failwith "-infrules-combinator is only available for \"product\" and \"min\""
              

let get_final_distances (type a) (args : synthesis_args) (module D : Distance.DIST) (module O : Oracle.ORACLE) (module T : Tree.TreeConstructor with type data = a) =
  let module G = Distance.GuideModCst (D) (O) (T) in 
  let dists : ((module Distance.VECDIST with type t = a)) list = Distance.create_infrules args.infrules (module D) (module O) (module T) in
  let dists = (module G.NoGuide : Distance.VECDIST with type t = a) :: dists in
  let module FD = Distance.CombineVecDist (struct
      type t = T.data
      let combinator = get_infrules_combinator args                    
      let dists = dists 
  end)
  in (module FD : Distance.VECDIST with type t = T.data)
  
let get_guiding_distance (type a) noguidance (module D : Distance.DIST) (module FD : Distance.VECDIST with type t = a) (module T : Tree.TreeConstructor with type data = a) : (module Distance.VECDIST with type t = a) =
  if noguidance then Distance.vectorizeDist Distance.dist_vec (module D) (module T)
  else (module FD)

let check_equiv (module EChecker : Checker.CheckerType) formula smtlib = 
    EChecker.open_session ();
    let r = EChecker.check formula smtlib in
    EChecker.close_session ();
    r


let run (module O : Oracle.ORACLE) (module MU : Mutations.MUTATIONS) args =
  let distance : (module Distance.DIST) = 
      match args.dist with
      | ARITH -> (module Distance.Arith)
      | HAMMING -> (module Distance.Hamming)
      | XOR -> (module Distance.Xor)
      | LOGARITH -> (module Distance.Logarith)
      | SYNTIA -> (module Distance.Syntia)
  in
  
  let module M = Mutations.Mk_Mutator (O) (MU) in

  let (module FD : Distance.VECDIST with type t = M.data) = 
      get_final_distances args distance (module O) (module M)
  in
  let (module GD : Distance.VECDIST with type t = M.data) = 
      get_guiding_distance args.noguidance distance (module FD) (module M)
  in

  let (module S : Heuristic.S) = Heuristic.of_kind 
      (module M) (module GD) (module FD) (module O) args.heuristic
  in

  let (module EChecker : Checker.CheckerType) =
      Checker.make_checker (module O) args.smt args.smt_to in


  let run_loop () =
      (* set_timeout args.time; *)
      let res = S.search args.maxdepth (float_of_int args.time)
      in res
  in

  let cumul_synth_time = ref 0.0 in
  let cumul_simpl_time = ref 0.0 in
  let cumul_echeck_time = ref 0.0 in
  let cumul_mutations = ref 0 in

  let rec main_loop n =
      let smry = run_loop () in
      cumul_synth_time := !cumul_synth_time +. smry.time_synthesis;
      cumul_simpl_time := !cumul_simpl_time +. smry.time_simplify;
      cumul_mutations := !cumul_mutations + smry.nb_mutations;
      if n <= 1 || smry.success then
          smry
      else
          main_loop (n-1)
  in

  let cegis_loop filename = 
      let rec aux prev : Heuristic.summary option * Checker.equiv_answer = 
          try 
              let smry = main_loop args.repeat in (* Stops (with success=no) either if the synthesis timeout or the CEGIStimeout is reached *)
              try
                  if not smry.success then Some smry, NO
                  else (
                      EChecker.open_session ();
                      let start = Sys.time () in
                      let equiv = EChecker.check filename smry.smtlib in
                      let checktime = (Sys.time ()) -. start in
                      cumul_echeck_time := !cumul_echeck_time +. checktime;
                      match equiv with 
                      | NO -> 
                          let ce = EChecker.get_counter_example () in
                          O.add_sample ce.inputs ce.output;
                          EChecker.close_session ();
                          n_cegis_loop := !n_cegis_loop+1; 
                          aux (Some smry)
                      | e -> EChecker.close_session (); Some smry, e
                  );
              with Exceptions.CEGISHalt -> 
                  EChecker.close_session ();
                  Some smry, (if smry.success then UKN else NO)
          with Exceptions.SynthesisNotStarted -> prev, NO
      in
      aux None
  in

  let smry, isequiv = 
    let smry_opt, isequiv = match args.cegis, args.formula with
    | true, Some filename -> (set_cegis_timeout args.time; cegis_loop filename)
    | true, None -> failwith "-cegis requires -formula" 
    | false, _ -> 
        try 
            let r = main_loop args.repeat in
            Some r, (if r.success then UKN else NO)
        with Exceptions.SynthesisNotStarted -> None, NO
    in
    (* remove_timeout (); *)
    ignore (if Option.is_none smry_opt then (print_endline "Not enough time to start synthesis"; exit 0));
    Option.get smry_opt, isequiv
  in
  (* remove_timeout (); *)
  
  (*ignore (if Option.is_none smry_opt then (Format.eprintf "Not enough time to start synthesis"; raise Exceptions.Halt));
  let smry = Option.get smry_opt in
  *)
  
  let isequiv = 
    (if args.check && Option.is_none args.formula then failwith "-check requires -formula");
    if isequiv = UKN && args.check && Option.is_some args.formula then (
        check_equiv (module EChecker) (Option.get args.formula) smry.smtlib
    ) else isequiv 
  in

  let cegis_smry = if args.cegis then 
    Some {
      nb_equiv_checks = !n_cegis_loop;
      time_equiv_check = !cumul_echeck_time;
    } 
    else None in

  let origsize = match O.get_expr_size () with 
  | Some s -> s
  | None -> -1
  in

  let t_info : target_info = {
    orig_size = origsize; 
  } 
  in

  let full_smry : Heuristic.summary = {
      success = smry.success; 
      expression = smry.expression;
      simplified = smry.simplified;
      smtlib = smry.smtlib;
      size = smry.size;
      time_synthesis = !cumul_synth_time;
      time_simplify = !cumul_simpl_time;
      nb_mutations = !cumul_mutations;
  } in
  

  let synth_res : synthesis_result = {
    synthesis = full_smry;
    equiv = isequiv;
    cegis = cegis_smry;
    target = t_info;
  }
  in

  dump_synthesis args synth_res (module O);

  synth_res

let oracle_from_file min max samplefile = 
    let csts : int array = Array.init 
        (max - min) 
        (fun v -> v + min)  
    in
    Oracle.of_json ~filename:samplefile csts

let get_sygus (module O : Oracle.ORACLE) (module MU : Mutations.MUTATIONS) = 
    let module SyGus = Sygus.SyGusPrinter (O) (MU) in
    SyGus.print ()

let auto_sample (vars : Tree.variable array) = 
    let gen_inputs i = match i with
    | 0 ->  (Array.map (fun (v : Tree.variable) -> (v, Bitvector.zeros v.sz)) vars)
    | 1 ->  (Array.map (fun (v : Tree.variable) -> (v, Bitvector.ones v.sz)) vars)
    | 2 ->  (Array.map (fun (v : Tree.variable) -> (v, Bitvector.max_ubv v.sz)) vars)
    | 3 ->  (Array.map (fun (v : Tree.variable) -> (v, Bitvector.min_sbv v.sz)) vars)
    | 4 ->  (Array.map (fun (v : Tree.variable) -> (v, Bitvector.max_sbv v.sz)) vars)
    | _ -> 
        if i < 90 then 
            Array.map (fun (v : Tree.variable) -> 
                let value = ((Random.int 100) - 50) |> Bitvector.of_int ~size:v.sz in
                (v, value)
            ) vars
        else 
            Array.map (fun (v : Tree.variable) -> 
                let value = Bitvector.rand v.sz in
                (v, value)
            ) vars
    in
    Array.init 100 gen_inputs