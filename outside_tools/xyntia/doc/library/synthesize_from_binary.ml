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

open Xyntia
open Xyntia_utils

let () = 
    (* Initialize the seed (required as Xyntia performs stochastic search) *)
    Random.self_init ();

    let binary = "binary" in
    let config = "config.ini" in
    
    (* Use Binsec to sample and extract the formula of the binary *)
    Sampler.sample binary config None None;
    let samples = Sampler.get_samples () in
    let formula = Sampler.get_formula () in
    
    (* Define the arguments for Xyntia *)
    let synth_args : synthesis_args = { default_args with
        formula = Some formula; (* Set to the path of the formula extracted from Binsec *)
        check = true; (* Set to true to check the equivalence with the groundtruth *)
    } in

    (* Run synthesis for each output detected by Binsec *)
    List.iter (fun sample -> 
        (* Create the oracle with the provided input-output examples *)
        let module O = (val (Oracle.of_json ~filename:sample [||])) in
         (* Create the set of operators that will be used during synthesis *)
        let (module MU : Mutations.MUTATIONS) = Mutations.mutations_of_str (module O: Oracle.ORACLE) "mba" in
        (* Run Xyntia *)
        let smry = run (module O) (module MU) synth_args in
        Printf.printf "%s\n" (Tree.string_of smry.synthesis.simplified);
        Printf.printf "equiv: %s\n" (Checker.answer_to_string smry.equiv);
    ) samples;
