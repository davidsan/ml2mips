(************************************************************************
 *                                                                      *
 *                       MASTER STL M1 anne'e 2005/06                   *
 *                                                                      *
 *                     Cours Compilation Avanceels                      *
 *                                                                      *
 *                       Compilation -> Langage intermediaire           *
 *                                                                      *
 *                         partie de ml2java                            *
 *                                                                      *
 ************************************************************************
 *                                                                      *
 *   env_trans : environnement de traduction                            *
 *                                                                      *
 *   version : $Version$     $Date: 2006/05/01 15:35:26 $                                     *
 *                                                                      *
 *   auteur : Emmanuel Chailloux                                        *
 *                                                                      *
 ************************************************************************)

open Types;;

let temp_symbol = "T";;
let expr_symbol = "E";;
let fun_symbol  = "F";;
let anon_symbol = "A";;

let ml_prefix   = "ML";;
let decl_symbol = "value";;

let gensym_var = 
  let c = ref 0 in 
  function s -> 
    c:=!c+1;
    s^"___"^(string_of_int !c)
;;

(* Registres $t *)
let mips_reg_t = ref (-1)
;;
let mips_gensym_reg_t () = 
	if (!mips_reg_t = 9) then
		mips_reg_t := -1;

   	mips_reg_t:=!mips_reg_t+1;
   	"$t"^(string_of_int !mips_reg_t)
and mips_resetsym_reg_t () =
	mips_reg_t:=-1;
	""
;;

(* Registres $a *)
let mips_reg_a = ref (-1)
;;
let mips_gensym_reg_a () = 
   	if (!mips_reg_a = 3) then
   		mips_reg_a := 0;

   	mips_reg_a:=!mips_reg_a+1;

   	"$a"^(string_of_int !mips_reg_a)
and mips_resetsym_reg_a () =
	mips_reg_a:=-1;
	""
;;

(* Registres $s *)
let mips_reg_s = ref (-1)
;;
let mips_gensym_reg_s () = 
   	mips_reg_s:=!mips_reg_s+1;
   	"$s"^(string_of_int !mips_reg_s)
and mips_resetsym_reg_s () =
	mips_reg_s:=-1;
	""
;;

(* Registres $v *)
let mips_reg_v = ref (-1)
;;
let mips_gensym_reg_v () = 
   	mips_reg_v:=!mips_reg_v+1;
   	"$v"^(string_of_int !mips_reg_v)
and mips_resetsym_reg_v () =
	mips_reg_v:=-1;
	""
;;

let mips_resetsym_tsv () =
	mips_resetsym_reg_t();
	mips_resetsym_reg_s();
	mips_resetsym_reg_v()
;;

let mips_resetsym_all () =
	mips_resetsym_tsv();
	mips_resetsym_reg_a()
;;


let module_name = ref "noname";;
let new_temp ()     = gensym_var temp_symbol;;
let new_name s      = gensym_var s;;
let new_clos_name s = gensym_var s;;
let clos_name name = (!module_name)^"."^(gensym_var name);;
let anon_name () = gensym_var anon_symbol;;


let pair_symbol = ref ",";;
let cons_symbol = ref "::";;
let ref_symbol =  ref "ref";;

let initial_trans_env = 
ref( [] : (string * (string * ml_type)) list )
;;

let initial_special_env = ref (!initial_trans_env);;

let add_trans_env (a,b) = initial_trans_env:=(a,b):: !initial_trans_env;;

let type_unit = Const_type Unit_type;;



