open Types;;
open Typeur;;
open Env_typeur;;
open Env_trans;;
open Langinter;;

let compiler_name = ref "ml2mips";;
let asm_suffix = ref ".s";;
let verbose_mode = ref true;;

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
		"true","1";
		"false","0";
		"+","add  ";
		"-","sub  ";
		"*","mult ";
		"/","div  ";
		"=","beq  ";
		"<","slt  ";
		"<=","sle  ";
		">","sgt  ";
		">=","sge  ";
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

let out_start s nb = out ("\n"^(String.sub shift_string 0 (2))^s);;

let out_end s nb = out ("\n"^(String.sub shift_string 0 nb)^"}\n");;
let out_line s = out (s^"\n");;

(* a changer *)
let out_before (fr, sd, nb) =
	(* if sd = "MIPS_JAL" then out_start ("jal   ") nb *)
	(* else *)
		if sd <>"" then out_start ("move  "^sd^", ") nb
		else if fr then out_start ("") nb
;;

(* a changer *)
let out_before_constant (fr, sd, nb) =
	(* if sd <> "MIPS_JAL" then *)
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
			"# "^ s ^ (!asm_suffix) ^ " engendre par "^(!compiler_name)^"\n";
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
		"  # Termine l'execution (exit)\n";
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
	| UNIT -> ()
;;

(* variable locale *)
let rec prod_local_var (fr, sd, nb) (v, t) =
	(* out_start ("move "^v^", "^mips_gensym_reg_a()) nb; *)
	()
;;

let contains s1 s2 =
	let re = Str.regexp_string s2
	in
	try ignore (Str.search_forward re s1 0); true
	with Not_found -> false

(* instructions *)
let rec prod_instr (fr, sd, nb, au, mn) instr = match instr with
		CONST c ->
			if !verbose_mode then	out_start "# <const>" nb;
			out_before_constant (fr, sd, nb);
			prod_const c;
			out_after (fr, sd, nb);
			if !verbose_mode then out_start "# </const>" nb;
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
					if sd = "" then
						out (v)
					else if sd = "MIPS_ARGS" then
						if not (contains v "$") then
						out_start ("jal   "^v) nb
		end;
	| IF (i1, i2, i3) ->
			if !verbose_mode then out_start "# <if>" nb;
			(* generer un label pour l'alternant et le conséquent 
				et la fin du consequent*)

			let (elsel, endifl) as if_labels = new_cond();
		in

			(* début de l'alternative *)
			(* la condition a déjà été compilé vers le registre $v0 *)
			out_start "beqz  " nb; (* branch on equal zero *)
			prod_instr (false,"", nb, au, mn) i1 ; (* $v0 *)
			out (", "^elsel^"\n");
			(* consequent *)
			prod_instr (fr, "garbage", nb +1, au, mn) i2 ;
			out_start ("j "^endifl) nb;
			out ("\n"^elsel^":");
			(* alternant *)
			prod_instr (fr, "garbage", nb +1, au, mn) i3;
			(* fin de l'alternative *)
			out ("\n"^endifl^":");
			if !verbose_mode then out_start "# </if>" nb;
	| RETURN i ->
			if !verbose_mode then
				begin
					out_start "# <return>" nb;
					()
				end;
			prod_instr (true, sd, nb, au, mn) i;
			
			(* out_start "# copie du dernier registre connu de la derniere       *)
			(* instruction" nb; out_start "# dans le registre $v0 \n" nb;        *)
			if !verbose_mode then
				out_start "# </return>\n" nb;
	| AFFECT (v, i) ->
			if !verbose_mode then out_start "# <affect>" nb;
			prod_instr (false, v, nb, au, mn) i;
			if !verbose_mode then out_start "# </affect>\n" nb;
	
	| BLOCK(l, i) ->
			if !verbose_mode then out_start "# <block>" nb;
			
			List.iter (fun (v, t, i) -> prod_instr (false, v, nb +1, au, mn) i) l;
			if not fr then
				begin
					List.iter (fun (v, t, i) ->
									if not mn then
										prod_local_var (false,"", nb +1) (v, t)
						) l;
				end;
			prod_instr (fr, sd, nb +1, au, mn) i;
			

			if !verbose_mode then out_start "# </block>\n" nb;
	| APPLY(i1, i2) ->
			if !verbose_mode then
				begin
					out ("");
					out_start "# <apply>" nb;
				end;
			(* prod_instr (false,"", nb, au, mn) i2; *)
			out_before(fr, sd, nb);
			(* out ("((MLfun)"); *)
			prod_instr (false,"", nb, au, mn) i1;
			(* out ")"; *)
			(* out ".invoke("; *)
			(* out")"; *)
			(* out_after(fr, sd, nb); *)
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
			out_after(fr, sd, nb);
			out_start "" nb;
			out (name^" ");(* "( ("^(string_of_type (List.hd ltp))^")"); *)
			if (not(contains name "mult" || contains name "div")) then
				begin
					prod_instr (false,"", nb +1, au, mn) (List.hd instrl);
					List.iter (fun x -> out (", ");
									prod_instr (false,"", nb +1, au, mn) x)
						(List.rev (List.tl instrl));
				end
			else
				begin
					prod_instr (false,"", nb +1, au, mn) (List.hd (List.rev (List.tl (List.tl(instrl)))));
					List.iter (fun x -> out (", ");
									prod_instr (false,"", nb +1, au, mn) x)
						(List.tl (List.rev (List.tl(instrl))));
				end;
			
			if (contains name "mult" || contains name "div") then
				begin
					out_start "mflo  " nb;
					prod_instr (false,"", nb +1, au, mn) (List.hd instrl);
				end;
			
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
			out ("  # Arg(s) pour "^cn^" : ");
			out (""^(List.hd lp)); (* PREMIER ARG *)
			List.iter (fun x -> out (", "^x)) (List.tl lp); (* LE RESTE DES ARGS : POUR LA VIRGULE*)
			(* out_line ") {"; *)
		end;
	prod_instr (true,"",2,0, false) instr;
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
	List.iter (prod_instr (false,"",0,0, true) ) ast_li
;;

(* point d'entree *)
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
		(* generation des fonctions *)
		prod_one ast_li;
		footer_one filename;
		
		(* generation des declaration des gvars if !verbose_mode then out      *)
		(* ("\n# Partie 2\n"); header_two filename; prod_two ast_li;           *)
		(* footer_two filename;                                                *)
		
		(* generation du main et des init des gvars *)
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

