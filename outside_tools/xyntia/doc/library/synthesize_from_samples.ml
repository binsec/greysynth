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

    (* Create input and output variables *)
    let x = Tree.var_of "x" 32 in
    let y = Tree.var_of "y" 32 in
    let res = Tree.var_of "res" 32 in

    (* Create the input-output examples approximating the target expression behaviors *)
    let samples : Oracle.sample array = [|
        (* Inputs                                                       Output *)
        { vars=[| (x, Bitvector.zeros 32); (y, Bitvector.zeros 32) |]; res=(res, Bitvector.zeros 32)};
        { vars=[| (x, Bitvector.ones 32); (y, Bitvector.zeros 32) |]; res=(res, Bitvector.ones 32)};
        { vars=[| (x, Bitvector.zeros 32); (y, Bitvector.ones 32) |]; res=(res, Bitvector.ones 32)};
        { vars=[| (x, Bitvector.ones 32); (y, Bitvector.ones 32) |]; res=(res, Bitvector.of_int ~size:32 2)};
    |] in
    
    (* Create the oracle with the provided input-output examples *)
    let module O = (val (Oracle.of_samples samples [||])) in

    (* Create the set of operators that will be used during synthesis *)
    let (module MU : Mutations.MUTATIONS) = Mutations.mutations_of_str (module O: Oracle.ORACLE) "mba" in

    (* Define the arguments for Xyntia *)
    let synth_args : synthesis_args = default_args in

    (* Run Xyntia *)
    let smry = run (module O) (module MU) synth_args in
    Printf.printf "%s\n" (Tree.string_of smry.synthesis.simplified);
