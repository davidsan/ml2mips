open Types;;
open Typeur;;
open Env_typeur;;
open Env_trans;;
open Langinter;;
let compiler_name = ref "ml2mips";;
let asm_suffix = ref ".s";;
let verbose_mode = ref false;;

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
		"-","sub  ";
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
(* let out_start s nb = out ("\n"^(String.sub shift_string 0               *)
(* (nb/(nb+1)+2))^s);;                                                     *)
let out_start s nb = out ("\n"^(String.sub shift_string 0 2)^s);;
let out_end s nb = out ("\n"^(String.sub shift_string 0 nb)^"}\n");;
let out_line s = out (s^"\n");;

(* à changer *)
let out_before (fr, sd, nb) =
	if sd <>"" then out_start ("move  "^sd^", ") nb
	else if fr then out_start ("") nb;;

(* à changer *)
let out_before_constant (fr, sd, nb) =
  if sd <>"" then out_start ("li    "^sd^", ") nb
  else if fr then out_start ("") nb;;

let out_after (fr, sd, nb) =
	if sd <>"" then
		begin
			out "";
			if fr then out (("return "^sd^";"))
		end
	else if fr then out "";;

(* des fonctions utilitaires pour commenter un peu la production *)

(* ON GARDE *)
let header_main s =
	if !verbose_mode then
		List.iter out
			[
			"# "^ s ^ (!asm_suffix) ^ " engendré par "^(!compiler_name)^"\n";
			]
;;

(* ON GARDE *)
let footer_main s =
	if !verbose_mode then
		List.iter out
			[
			"\n";
			"# Fin du fichier " ^ s ^ (!asm_suffix) ^"\n"
			]
;;

(* PAS UTILE *)
let header_one s =
	List.iter out
		[ ]
;;

(* pas utile *)
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
		"  addiu $sp, $sp, "^string_of_int(- (alloc))^"\n";
		"  sw    $fp, "^string_of_int(alloc -4)^"($sp)\n";
		"  move  $fp, $sp\n";
		]
;;

let fun_exit_point s alloc =
	List.iter out
		[
		"\n";
		"  move  $sp, $fp\n";
		"  lw    $fp, "^string_of_int(alloc -4)^"($sp)\n";
		"  addiu $sp, $sp, "^string_of_int((alloc))^"\n";
		"  jr    $ra\n";
		"  nop\n"
		]
;;

let main_entry_point s alloc =
  List.iter out
    [
    "\n";
    "  addiu $sp, $sp, "^string_of_int(- (alloc))^"\n";
    "  sw    $fp, "^string_of_int(alloc -4)^"($sp)\n";
    "  move  $fp, $sp\n";
    ]
;;


let main_print_point s alloc =
  List.iter out
    [
    "\n";
    "  # Affichage de la valeur du programme ($v0)\n";
    "  move  $a0, $v0\n";
    "  li    $v0, 1\n";
    "  syscall\n";
    ]
;;

let main_exit_point s alloc =
  List.iter out
    [
    "\n";
    "  move  $sp, $fp\n";
    "  lw    $fp, "^string_of_int(alloc -4)^"($sp)\n";
    "  addiu $sp, $sp, "^string_of_int((alloc))^"\n";
    "\n";
    "  # Termine l'exécution (exit)\n";
    "  li    $v0, 10\n";
    "  syscall\n";
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

(* variable globale *)
let prod_global_var instr = match instr with
		VAR (v, t) ->
			out_start ("statica "^"MLvalue "^(*(string_of_type t)*)v^";") 1
	| FUNCTION (ns, t1, ar, (p, t2), instr) ->
	(* out_start ("statica MLvalue "(*"fun_"^ns^" "*)^ns^"= new MLfun_"^ns^"("^(string_of_int ar)^");") 1
	*)
			()
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
		INT i -> out (string_of_int i)
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
let rec prod_instr (fr, sd, nb, regv, regt) instr = match instr with
		CONST c ->
      if !verbose_mode then
        begin
    			out ("");
    			out_start "# <const>" nb;
          out ("\n");
        end;
			out_before (fr, sd, nb);
			prod_const c;
			out_after (fr, sd, nb);
      if !verbose_mode then
			  out_start "# </const>" nb;
	| VAR (v, t) ->
			if (nb = 0) && ( sd = "") then
				begin
					if !verbose_mode then
						begin
							out ("");
							out_start "# <var>" nb;
							out_start "# </var>\n" nb;
						end;
					()
				end
			else
				begin
					out_before (fr, sd, nb);
					out v;
					(* out_after (fr, sd, nb) *)
				end;
	| IF(i1, i2, i3) ->
			if !verbose_mode then
				begin
					out ("");
					out_start "# <if>" nb;
				end;
			out_start "if (" nb;
			out ("((MLbool)");
			prod_instr (false,"", nb, regv, regt) i1 ;
			out ")";
			out".MLaccess()";
			out ")";
			prod_instr (fr, sd, nb +1, regv, regt) i2 ;
			out_start "else" (nb);
			prod_instr (fr, sd, nb +1, regv, regt) i3;
			if !verbose_mode then
				begin
					out_start "# </if>" nb;
				end;
	| RETURN i ->
			if !verbose_mode then
				begin
					out ("  ");
					out_start "# <return>" nb;
					()
				end;
			prod_instr (true,"", nb, regv, regt) i;
			
			(* out_start "# copie du dernier registre connu de la dernière       *)
			(* instruction" nb; out_start "# dans le registre $v0 \n" nb;        *)
			if !verbose_mode then
				out_start "# </return>\n" nb;
	| AFFECT (v, i) ->
			if !verbose_mode then
				begin
					out ("");
					out_start "# <affect>" nb;
				end;
			prod_instr (false, v, nb, regv, regt) i;
			if !verbose_mode then
				begin
					out_start "# </affect>\n" nb;
				end;
	| BLOCK(l, i) ->
			if !verbose_mode then
				begin
					out ("");
					out_start "# <block>" nb;
				end;
			(* List.iter (fun (v, t, i) -> prod_local_var (false,"", nb +1) (v,  *)
			(* t)) l;                                                            *)

      if !verbose_mode then			
			List.iter (fun (v, t, i) -> prod_instr (false, v, nb +1, regv, regt) i) l;
			prod_instr (fr, sd, nb +1, regv, regt) i;
			if !verbose_mode then
				begin
					out_start "# </block>\n" nb
				end;
	| APPLY(i1, i2) ->
			if !verbose_mode then
				begin
					out ("");
					out_start "# <apply>" nb;
				end;
			out_before(fr, sd, nb);
			out ("((MLfun)");
			prod_instr (false,"", nb, regv, regt) i1;
			out ")";
			out ".invoke(";
			prod_instr (false,"", nb, regv, regt) i2;
			out")";
			out_after(fr, sd, nb);
			if !verbose_mode then
				begin
					out_start "# </apply>" nb;
				end;
	| PRIM ((name, typ), instrl) ->
			if !verbose_mode then
				begin
					out ("");
					out_start "# <prim>" nb;
				end;
			let ltp = get_param_type instrl in
			out_before (fr, sd, nb);
			out (name^" $v0, ");(* "( ("^(string_of_type (List.hd ltp))^")"); *)
			(* premier arg *)
			prod_instr (false,"", nb +1, regv, regt) (List.hd instrl);
			
			(* second arg *)
			List.iter2 (fun x y -> out (", ");
							prod_instr (false,"", nb +1, regv, regt) x)
				(List.tl instrl) (List.tl ltp);
			out "" ;
			
			out_after(fr, sd, nb);
			if !verbose_mode then
				begin
					out_start "# </prim>" nb;
				end;
	
	| FUNCTION _ ->
			if !verbose_mode then
				begin
					out ("");
					out_start "# <function>" nb;
					out_start "# </function>" nb;
				end;
			();
;;

(* UTILE : to clean *)
let fun_header fn cn =

  if !verbose_mode then
  	List.iter out
  		["\n\n";
  		(* "/**\n"; *)
  		"# Declaration de la fonction "^fn^"\n";
  		(* " * vue comme la classe : "^cn^"\n"; " */ \n" *)
  		]
;;

(* UTILE invocation *)
let prod_invoke_fun cn ar t lp instr =
	(* out_start "MLvalue invoke_real(" 1; traitement des args *)

  if !verbose_mode then
    begin
	    out ("# Argument(s) pour la fonction "^cn^" : ");
	    out (""^(List.hd lp)); (* PREMIER ARG *)
    	List.iter (fun x -> out (", "^x)) (List.tl lp); (* LE RESTE DES ARGS : POUR LA VIRGULE*)
    	(* out_line ") {"; *)    
    end;
  prod_instr (true,"",2,0,0) instr;
(* out_start "}" 1; out_line "" *)
;;

(* function *)
let prod_fun instr = match instr with
		FUNCTION (ns, t1, ar, (lp, t2), instr) ->
			let class_name = ns
			and alloc = 8 in
			fun_header ns class_name ;
			out_line (class_name^":");
			
			fun_entry_point "" alloc;
			prod_invoke_fun class_name ar t1 lp instr;
			out_line "";
			fun_exit_point "" alloc;
			
      if !verbose_mode then
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
	let obj_name = filename ^ !asm_suffix in
	let oc = open_out obj_name in
	change_output_channel oc;
	module_name:= filename;
	try
		let alloc = 8 in
		header_main filename;
    out ("#  .data:\n");
		out ("#  .text:\n"); 
    out ("  j main\n");
		(* génération des fonctions *)
		prod_one ast_li;
		footer_one filename;
		
		(* génération des déclaration des gvars *)
		if !verbose_mode then
			out ("\n# Partie 2\n");
		
(*  header_two filename;
		prod_two ast_li;
		footer_two filename; *)
		
		(* génération du main et des init des gvars *)
		if !verbose_mode then
			out ("\n# Partie 3\n");
		
		header_three filename;
		main_entry_point filename alloc;
		prod_three ast_li;
    main_print_point filename alloc;
		main_exit_point filename alloc;
		
		(* fin *)
		footer_three filename;
		footer_main filename;
		close_out oc
	with x -> close_out oc; raise x;;

