(*
 * This module defines interface to Advanced Rewriting Library.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 2021 LdBeth
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: LdBeth
 * ldbeth@sdf.org
 *
 *)

module Exp : sig
   type exp = VAR of int
         | MARKED_VAR of int
         | QUANT of (int * (int * Rtype.etype) list * exp * exp)
         | APPL of (int * exp list)
         | LET of (exp * Rtype.etype * exp *exp)
         | CASE of exp * Rtype.etype * ((exp * exp) list)
         | INDEX of (exp * int * int)
         | HIGHLIGHT of exp
         | NORMAL of exp
         | NUM of int
         | RATIONAL of int * int
         | STRING of string
         | CHAR of char
         | REF of int
         | NOEXP

   val prExp: exp -> string
end

module Inner : sig
   val rewrite_in_context: (Exp.exp * Exp.exp * Exp.exp * Renv.env) ->
    Exp.exp
   val rewrite: (Exp.exp * Renv.env) -> (Exp.exp * Renv.env)
   val rewrite_nokb: (Exp.exp * Renv.env) -> (Exp.exp * Renv.env)
   val rewrite2: Renv.env -> Exp.exp -> Exp.exp list
end

module Rtype : sig
   type etype
   type tyvar
type typeDef

(*
 * Parsing/unparsing
 *)
val unparse: etype -> string
val parse: string -> etype
val parseDef: string -> typeDef
val parseWholeDef: string -> (etype * typeDef)
val unparseDef: typeDef -> string
val emptyDef : typeDef
(*
 * etype constructor and destructor functions
 *)
exception TypeError of (etype * etype * (int list))
val newVar : unit -> tyvar
val mkVar: tyvar -> etype
val untypeVar: tyvar -> etype -> tyvar
val mkSlot : tyvar -> etype
val untypeSlot : etype -> tyvar
val mkProduct: int -> etype list -> etype
val untypeProduct: int -> int -> etype -> etype list
val nameProduct: etype -> int
val paramProduct: etype -> (etype list)
val mkTfun: etype -> etype -> etype
val untypeTfun: etype -> etype * etype
val notype: etype
val notetype: etype -> bool
val allNames: etype -> int list
val allDefinitionNames: typeDef -> int list
(*
 * Routines to assist type inferencing in expressions
 *)
exception UndefinedConstructor
val instantiateetype: int -> etype -> (int * etype);;
val applyFunctionetype: etype -> etype -> etype
val getConstructoretype: (etype * typeDef) -> int -> etype
val getReturnetype: etype -> etype
val getArgumentetype: etype -> int -> etype
val getArgumentCount: etype -> int
val getetypeName: etype -> int
val getConstructorList: (etype * typeDef) -> (int list)
val isFiniteetype : (etype * typeDef) -> bool;;
(*
 * etype variable substitution
 *)
type subst
exception NoMapping

val id: subst
val addSubst: subst -> tyvar -> etype -> subst
val get: subst -> tyvar -> etype
val printSubst: subst -> unit

val on: etype -> subst -> etype
val rec_on: etype -> subst -> etype

val tmatch: etype -> etype -> subst -> subst
val unify: subst -> etype -> etype -> subst

val pretype: etype -> string

end

module Renv : sig
   type env
   type parm = S of int
          | E of Exp.exp
          | IL of (int list)
          | SL of (int list)
          | I of int
          | WILD

   val addFunction: env -> (Exp.exp * Rtype.etype * Exp.exp * Exp.exp list) -> (Exp.exp list) -> env
   val addProperty: env -> Exp.exp -> env
   val addViolationRule: env -> Exp.exp -> env
   val newDefinition: env -> (int * Exp.exp list) -> env
   val addTypeDefinition: env -> (Rtype.etype * Rtype.typeDef) -> env
   val addAttrib: env -> int -> (parm list) -> env
   val addImported: env -> string -> env
   val addVarType: env -> (int * Rtype.etype) -> env
   (*val stripVarTypes: env -> (int list) -> env*)
   val setNextUsableVar: env -> int -> env
   exception CircularPrecedence
   val addPrecedence: env -> (int * int) -> env
   val addEqualPrecedence: env -> (int * int) -> env
   val addNameAways: env -> (int list) -> env
   val addSingularRule: env -> Exp.exp -> env
   val addGroup: env -> int -> (int list) -> env
   val addFilter: env -> int -> (int list) -> env
   val addExpanders: env -> int -> (Exp.exp list list) -> env
   val addMinorPrecedence: env -> int -> int -> env
   val addFailedList: env -> Exp.exp -> env
   val addContextRules: env -> (Exp.exp list) -> env
   val clearContextRules: env -> env
   val addOverload: env -> int -> (int list) -> env
   val addConversion: env -> int -> env
end

module Intern : sig
val intern: string -> int
val decode: int -> string
val intern_oriented_rule: int   (* 1 *)
val intern_unoriented_rule: int (* 2 *)
val intern_bool: int            (* 3 *)
val intern_true: int            (* 4 *)
val intern_false: int           (* 5 *)
val intern_unit: int            (* 6 *)
val intern_identity: int        (* 7 *)
val intern_trivial: int         (* 8 *)
val intern_and: int             (* 9 *)
val intern_or: int              (* 10 *)
val intern_equal: int           (* 11 *)
val intern_preceq: int          (* 12 *)
val intern_defined: int         (* 13 *)
val intern_all: int             (* 14 *)
val intern_exists: int          (* 15 *)
val intern_star : int           (* 16 *)
val intern_not : int            (* 17 *)
val intern_if : int             (* 18 *)
val intern_undef : int          (* 19 *)
val intern_set : int            (* 20 *)
val intern_cons : int           (* 21 *)
val intern_nil : int            (* 22 *)
val intern_attr : int           (* 23 *)
val intern_ac : int             (* 24 *)
val intern_a : int              (* 25 *)
val intern_c : int              (* 26 *)
val intern_epo : int            (* 27 *)
val intern_eq : int             (* 28 *)
val intern_derive : int         (* 29 *)
val intern_to : int             (* 30 *)
val intern_eto : int            (* 31 *)
val intern_po : int             (* 32 *)
val intern_smi : int            (* 33 *)
val intern_csmi : int           (* 34 *)
val intern_omi : int            (* 35 *)
val intern_comi : int           (* 36 *)
val intern_smd : int            (* 37 *)
val intern_csmd : int           (* 38 *)
val intern_omd : int            (* 39 *)
val intern_comd : int           (* 40 *)
val intern_mi : int             (* 41 *)
val intern_cmi : int            (* 42 *)
val intern_md : int             (* 43 *)
val intern_cmd : int            (* 44 *)
val intern_solved : int         (* 45 *)
val intern_t : int              (* 46 *)
val intern_goal : int           (* 47 *)
val intern_node : int           (* 48 *)
val intern_branch : int         (* 49 *)
val intern_subterm : int        (* 50 *)
val intern_pattern : int        (* 51 *)
val intern_down : int           (* 52 *)
val intern_top_goal : int       (* 53 *)
val intern_top_node : int       (* 54 *)
val intern_top_branch : int     (* 55 *)
val intern_top_subterm : int    (* 56 *)
val intern_top_pattern : int    (* 57 *)
val intern_top_down : int       (* 58 *)
val intern_subgoals : int       (* 59 *)
val intern_decomp : int         (* 60 *)
val intern_less : int           (* 61 *)
val intern_plus : int           (* 62 *)
val intern_slash : int          (* 63 *)
val intern_percent : int        (* 64 *)
val intern_minus : int          (* 65 *)
val intern_size : int           (* 66 *)
val intern_char : int           (* 67 *)
val intern_concat : int         (* 68 *)
val intern_chr : int            (* 69 *)
val intern_asc : int            (* 70 *)
val intern_def : int            (* 71 *)
val intern_default : int        (* 72 *)
val intern_lambda : int         (* 73 *)
val intern_fn : int             (* 74 *)
val intern_apply : int          (* 75 *)
val intern_nat_plus : int       (* 76 *)
val intern_nat_minus : int      (* 77 *)
val intern_nat_times : int      (* 78 *)
val intern_nat_divide : int     (* 79 *)
val intern_nat_less : int       (* 80 *)
val intern_nat_mod : int        (* 81 *)
val intern_rat_plus : int       (* 82 *)
val intern_rat_minus : int      (* 83 *)
val intern_rat_times : int      (* 84 *)
val intern_rat_divide : int     (* 85 *)
val intern_rat_less : int       (* 86 *)
val intern_rat_mod : int        (* 87 *)
val intern_rat_to_nat : int     (* 88 *)
val intern_nat_to_rat : int     (* 89 *)
val intern_implies : int        (* 90 *)
val intern_fix : int            (* 91 *)

val count : unit -> int
end

module Rtrace : sig
   val indent: unit -> unit
val undent: unit -> unit
val setBlocks: (string list) -> unit
val trace: string -> (unit -> string) -> unit
val trace_list: string -> (unit -> string list) -> unit
val toggle_trace: unit -> unit
val is_trace_on: unit -> bool
end
(* *)