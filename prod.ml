open Types;;
open Typeur;;
open Env_typeur;;
open Env_trans;;
open Langinter;;
let compiler_name = ref "ml2java";;
let object_suffix = ref ".sh";;

(* des valeurs pour certains symboles de env_trans *)
pair_symbol:=",";;
cons_symbol:="::";;
ref_symbol:="ref";;

(* l'environnement initial du traducteur en liaison avec la Runtime *)
let build (s, equiv) =
  let t =
    try List.assoc s !initial_typing_env
    with Not_found ->
      failwith ("building initial_trans_env : for symbol : "^s)
  in (s, (equiv, type_instance t));;

initial_special_env :=
  List.map build [
    "hd","MLruntime.MLhd";
    "tl","MLruntime.MLtl";
    "fst","MLruntime.MLfst";
    "snd","MLruntime.MLsnd"
  ];;


initial_trans_env:=
  let alpha = max_unknown () in
    [",", ("MLruntime.MLpair", Fun_type (Pair_type (alpha, alpha),
					 Pair_type (alpha, alpha)))]@
      ["::", ("MLruntime.MLlist", Fun_type (Pair_type (alpha, alpha),
					    List_type (alpha)))]@

      (
	List.map build
	  [
	    "true" ,"MLruntime.MLtrue";
	    "false","MLruntime.MLfalse";
	    "+","add";
	    "-","sub";
	    "*","mult";
	    "/","div";
	    "=","MLruntime.MLequal";
	    "<","MLruntime.MLltint";
	    "<=","MLruntime.MLleint";
	    ">","MLruntime.MLgtint";
	    ">=","MLruntime.MLgeint";
	    "^", "MLruntime.MLconcat"
	      
	  ]
      )
;;
(* des fonctions d'I/O *)
let output_channel = ref stdout;;
let change_output_channel oc = output_channel := oc;;
let shift_string = String.make 256 ' ';;
let out s = output_string !output_channel s;;
let out_start s nb = out ("\n"^(String.sub shift_string 0 (2 * nb))^s);;
let out_end s nb = out ("\n"^(String.sub shift_string 0 nb)^"}\n");;
let out_line s = out (s^"\n");;

(* à changer *)
let out_before (fr, sd, nb) =
  if sd <>"" then out_start (sd^"   =   ") nb
  else if fr then out_start ("") nb;;

let out_after (fr, sd, nb) =
  if sd <>"" then
    begin
      out ";";
      if fr then out (("return "^sd^";"))
    end
  else if fr then out "";;

(* des fonctions utilitaires pour commenter un peu la production *)

(* ON GARDE *)
let header_main s =
  List.iter out
    [
      "# "^ s ^ ".java" ^ " engendre par ml2java \n"	
    ]
;;


(* ON GARDE *)
let footer_main s =
  List.iter out
    [
      "\n";
     "# Fin du fichier " ^ s ^ ".java\n"
    ]
;;

(* PAS UTILE *)
let header_one s =
  List.iter out
    [ ]
;;


(* pas utile*)
let footer_one s = ();;


(* pas utile *)
let header_two s =
  List.iter out
    [ ]
;;

(* pas utile *)
let footer_two s = ();;

(* utile *)
let header_three s =
  List.iter out
    [ 
      "\n\n";
      "main:\n"
    ]
;;

let fun_entry_point s alloc = 
  List.iter out 
    [ 
      "\n";
      "  addiu $sp, $sp, "^string_of_int(-(alloc))^"\n";
      "  sw    $fp, "^string_of_int(alloc-4)^"($sp)\n";
      "  move  $fp, $sp\n";
    ]
;;


let fun_exit_point s alloc = 
  List.iter out
    [ 
      "\n";
      "  move  $sp, $fp\n";
      "  lw    $fp, "^string_of_int(alloc-4)^"($sp)\n";
      "  addiu $sp, $sp, "^string_of_int((alloc))^"\n";
      "  j     $31\n";
      "  nop\n"
    ] 
;;


(* pas utile *)
let footer_three s =
  List.iter out
    [ ]
;;



(* on recupere le type pour une declaration precise *)
let string_of_const_type ct = match ct with
    INTTYPE -> "MLint "
  | FLOATTYPE -> "MLdouble "
  | STRINGTYPE -> "MLstring "
  | BOOLTYPE -> "MLbool "
  | UNITTYPE -> "MLunit "
;;


let rec string_of_type typ = match typ with
    CONSTTYPE t -> string_of_const_type t
  | ALPHA -> "MLvalue "
  | PAIRTYPE -> "MLpair "
  | LISTTYPE -> "MLlist "
  | FUNTYPE -> "MLfun "
  | REFTYPE -> "MLref "
;;

(* variable global *)
let prod_global_var instr = match instr with
    VAR (v, t) -> 
      out_start ("statica "^"MLvalue "^(*(string_of_type t)*)v^";") 1
  | FUNCTION (ns, t1, ar, (p, t2), instr) ->
      out_start ("statica MLvalue "(*"fun_"^ns^" "*)^ns^"= new MLfun_"^ns^"("^(string_of_int ar)^");") 1
  | _ -> 
      ()
;;

(* partie 2 *)
let prod_two ast_li =
  List.iter prod_global_var ast_li
;;

let get_param_type lv =
  List.map (function (VAR(name, typ)) -> typ
	      | _ -> failwith "get_param_type" ) lv
;;

(* constante *)
let prod_const c = match c with
    INT i -> out ("li $v0 "^(string_of_int i)^"")
  | FLOAT f -> out ("new MLdouble("^(string_of_float f)^")")
  | BOOL b -> out ("new MLbool("^(if b then "true" else "false")^")")
  | STRING s -> out ("new MLstring("^"\""^s^"\""^")")
  | EMPTYLIST -> out ("MLruntime.MLnil")
  | UNIT -> out ("MLruntime.MLlrp")
;;

(* variable locale *)
let rec prod_local_var (fr, sd, nb) (v, t) =
  out_start ("MLvalue "(*(string_of_type t)*)^v^";") nb
;;


(* instructions *)
(* true, "", 2 *)
let rec prod_instr (fr, sd, nb, regv, regt) instr = match instr with
    CONST c -> 
      out ("");
      out_start "# debut CONST " nb;
      out ("\n");
      out_before (fr, sd, nb);
      prod_const c;
      out_after (fr, sd, nb);
      out_start "# fin CONST " nb;
  | VAR (v, t) -> 
      out ("");
      out_start "# debut VAR " nb;
      out ("\n");
      if (nb = 0) && ( sd = "") then ()
      else
	begin
     out_before (fr, sd, nb);
     out v;(* 
	  out_after (fr, sd, nb) *)
	end;
  out_start "# fin VAR \n" nb
  | IF(i1, i2, i3) ->

      out ("");
      out_start "# debut if " nb;
      out_start "if (" nb;
      out ("((MLbool)");
      prod_instr (false,"", nb, regv, regt) i1 ;
      out ")";
      out".MLaccess()";
      out ")";
      prod_instr (fr, sd, nb +1, regv, regt) i2 ;
      out_start "else" (nb);
      prod_instr (fr, sd, nb +1, regv, regt) i3;

      out_start "# fin if " nb;
  | RETURN i -> 
      out ("  "); 
      out_start "# debut return " nb;
      prod_instr (true,"", nb, regv, regt) i;
      out_start "# fin return \n" nb;
  | AFFECT (v, i) ->
      out (""); 
      out_start "# debut affect " nb;
      prod_instr (false, v, nb, regv, regt) i;
      out_start "# fin affect \n" nb;
  | BLOCK(l, i) ->
      out (""); 
      out_start "# debut block " nb; 
     (*  List.iter (fun (v, t, i) -> prod_local_var (false,"", nb +1)
		   (v, t)) l;
 *)
      out_start "# PROD _ INSTRUCTIONS " nb;

      List.iter (fun (v, t, i) -> prod_instr (false, v, nb +1, regv, regt) i) l;
      prod_instr (fr, sd, nb +1, regv, regt) i;
       out_start "# fin block \n" nb 
	
  | APPLY(i1, i2) ->
      out (""); 
      out_start "# debut apply " nb;
      out_before(fr, sd, nb);
      out ("((MLfun)");
      prod_instr (false,"", nb, regv, regt) i1;
      out ")";
      out ".invoke(";
      prod_instr (false,"", nb, regv, regt) i2;
      out")";
      out_after(fr, sd, nb);
      out_start "# fin apply " nb;
  | PRIM ((name, typ), instrl) ->

      out (""); 
      out_start "# debut prim " nb;
      let ltp = get_param_type instrl in
      	out_before (fr, sd, nb);
      	out (name^"( ("^(string_of_type (List.hd ltp))^")");

      	prod_instr (false,"", nb +1, regv, regt) (List.hd instrl);
      	List.iter2 (fun x y -> out (",("^(string_of_type y)^")");
      		      prod_instr (false,"", nb +1, regv, regt) x)
      	  (List.tl instrl) (List.tl ltp);
      	out ")" ;
      	out_after(fr, sd, nb);
        out_start "# fin prim " nb;
	  
  | FUNCTION _ -> 
      out (""); 
      out_start "# debut function " nb;
      ();
      out_start "# fin function " nb;
;;

(* UTILE : to clean *)
let fun_header fn cn =
  List.iter out
    ["\n\n";
     (*		"/**\n"; *)
     "# Declaration de la fonction "^fn^"\n";
     (*		" *    vue comme la classe : "^cn^"\n"; *)
     (*		" */ \n" *)
    ]
;;

(* UTILE invocation *)
let prod_invoke_fun cn ar t lp instr =
  (* out_start "MLvalue invoke_real(" 1;*)
  (* traitement des args *)
  out ("MLvalsue "^(List.hd lp)); (* PREMIER ARG *)
  List.iter (fun x -> out (", MLvalues "^x)) (List.tl lp); (* LE RESTE DES ARGS : POUR LA VIRGULE*)
  (*	out_line ") {"; *)
  prod_instr (true,"",2,0,0) instr;
  (*	 
	 out_start "}" 1;
	 out_line ""
  *)
;;

(* function *)
let prod_fun instr = match instr with
    FUNCTION (ns, t1, ar, (lp, t2), instr) ->
      let class_name = ns 
      and alloc = 8 in
	fun_header ns class_name ;
	out_line (class_name^":");

	fun_entry_point "" alloc;
	out_line "";
	prod_invoke_fun class_name ar t1 lp instr;
	out_line "";
	fun_exit_point "" alloc;

	out_line ("# Fin de la fonction "^class_name)
	  
  | _ -> ()
;;

(* partie un *)
let prod_one ast_li =
  List.iter prod_fun ast_li
;;

(* partie trois *)
let prod_three ast_li =
  List.iter (prod_instr (false,"",0,0,0) ) ast_li
;;

(* point d'entrée *)
let prod_file filename ast_li =
  let obj_name = filename ^ !object_suffix in
  let oc = open_out obj_name in
    change_output_channel oc;
    module_name:= filename;
    try
      let alloc = 8 in
	header_main filename;


(* génération des fonctions *)
	prod_one ast_li;
	footer_one filename;
	
  (* génération des déclaration des gvars *)
  out (" \n#### DEBUT PARTIE 2\n");
  out (" \n#### DEBUT PARTIE 2\n");
  out (" \n#### DEBUT PARTIE 2\n");
  header_two filename;
  prod_two ast_li;
	footer_two filename;

  (* génération du main et des init des gvars *)
  out ( "\n#### DEBUT PARTIE 3\n");
  out ( "\n#### DEBUT PARTIE 3\n");
  out ( "\n#### DEBUT PARTIE 3\n");

	header_three filename;
	fun_entry_point filename alloc;
	prod_three ast_li;
	fun_exit_point filename alloc;

  (* fin *)
	footer_three filename;
	footer_main filename;
	close_out oc
    with x -> close_out oc; raise x;;

