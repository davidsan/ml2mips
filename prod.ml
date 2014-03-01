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


let contains s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false
;;

(* des fonctions d'I/O *)
let output_channel = ref stdout;;
let change_output_channel oc = output_channel := oc;;
let shift_string = String.make 256 ' ';;
let out s = output_string !output_channel s;;
let out_start s nb = out ("\n"^(String.sub shift_string 0 (2))^s);;
let out_end s nb = out ("\n"^(String.sub shift_string 0 nb)^"}\n");;
let out_line s = out (s^"\n");;

let out_before (fr, sd, nb) =
  if sd <>"" then out_start ("move  "^sd^", ") nb
  else if fr then out_start ("") nb
;;

let out_before_constant (fr, sd, nb) =
  if (contains sd "$") then out_start ("li    "^sd^", ") nb
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
      "  move  $v0, $zero\n"; (* Pour compatibilité MARS / SPIM sur empty.s *)
      (* Un programme vide rendra zéro par convention pour représenter unit *)
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
    if(not (contains v "value")) then
    begin
    	out_start (v^":") 0;
    	out_start ("      .word 0") 0;
	end;
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
  | FLOAT f -> out (string_of_int (int_of_float f)) (* no floating point op *)
  | BOOL b -> out (if b then "1" else "0")
  | STRING s -> out ("\""^s^"\"")
  | EMPTYLIST -> () (* unsupported *)
  | UNIT -> ()
;;

(* variable locale *)
let rec prod_local_var (fr, sd, nb) (v, t) =
  (* out_start ("move "^v^", "^mips_gensym_reg_a()) nb; *)
  ()
;;


(* instructions *)
let rec prod_instr (fr, sd, nb) instr = match instr with
    CONST c ->
    if !verbose_mode then	out_start "# <const>" nb;
    if (contains sd "$") then (* $v ou $a *)  
    	begin
    	out_before_constant (fr, sd, nb);
    	prod_const c;
    	out_after (fr, sd, nb);
    	end
    else (* variable globale (exemple : a1) *)
    	begin
    	out_before_constant (fr, "$a0", nb); (* load in tmp register *)
    	prod_const c;
    	out_start ("sw    $a0, "^sd) nb;
    	out_after (fr, sd, nb);
    	end;
    if !verbose_mode then out_start "# </const>" nb;
  | VAR (v, t) ->
    if (nb = 0) && ( sd = "") then
      begin
	if !verbose_mode then out_start "# <var>" nb;
      end
    else
      begin
	if sd = "" then
	  out (v)
	else if sd = "MIPS_ARGS" then
	  if not (contains v "$v" || contains v "$a")  then
	    out_start ("jal   "^v) nb
      end;
  | IF (i1, i2, i3) ->
    if !verbose_mode then out_start "# <if>" nb;

    (* generation de labels                                              *)
    let (elsel, endifl) as if_labels = new_cond();
    in
    (* la condition a déjà été compilé vers le registre $v0              *)
    out_start "beqz  " nb; (* branch on equal zero *)
    prod_instr (false,"", nb) i1 ; (* $v0 *)
    out (", "^elsel^"\n");
    (* consequent *)
    prod_instr (fr, "garbage", nb +1) i2 ;
    out_start ("j "^endifl) nb;
    out ("\n"^elsel^":");
    (* alternant *)
    prod_instr (fr, "garbage", nb +1) i3;
    (* fin de l'alternative *)
    out ("\n"^endifl^":");
    if !verbose_mode then out_start "# </if>" nb;
  | RETURN i ->
    if !verbose_mode then
      begin
	out_start "# <return>" nb;
	()
      end;
    prod_instr (true, sd, nb) i;
    if !verbose_mode then
      out_start "# </return>\n" nb;
  | AFFECT (v, i) ->
    if !verbose_mode then out_start "# <affect>" nb;
    if (contains v "value") then
      prod_instr (false, "$v0", nb) i
    else
      prod_instr (false, v, nb) i;
    if !verbose_mode then out_start "# </affect>\n" nb;

  | BLOCK(l, i) ->
    if !verbose_mode then out_start "# <block>" nb;

    List.iter (fun (v, t, i) -> prod_instr (false, v, nb +1) i) l;
    prod_instr (fr, sd, nb +1) i;

    if !verbose_mode then out_start "# </block>\n" nb;
  | APPLY(i1, i2) ->
    if !verbose_mode then out_start "# <apply>" nb;
    out_before(fr, sd, nb);
    prod_instr (false,"", nb) i1;
    if !verbose_mode then out_start "# </apply>" nb;
  | PRIM ((name, typ), instrl) ->
    if !verbose_mode then
      begin
	out ("");
	out_start "# <prim>" nb;
      end;
    out_after(fr, sd, nb);
    out_start "" nb;
    out (name^" "); (* nom de la primitive (add, sub, mult, etc) *)
    let is_mult_or_div =
      (contains name "mult" || contains name "div") in
    if is_mult_or_div then
      begin
	prod_instr (false,"", nb +1) (List.nth instrl 2); (* operande gauche *)
	out (", ");
	prod_instr (false,"", nb +1) (List.nth instrl 1); (* operande droite *)
	out_start "mflo  " nb; (* deplacement du resultat de $lo vers $v0 *)
	prod_instr (false,"", nb +1) (List.nth instrl 0); (* destination *)
      end

    else
      begin
	prod_instr (false,"", nb +1) (List.nth instrl 0); (* destination *)
	out (", ");
	prod_instr (false,"", nb +1) (List.nth instrl 2); (* operande gauche *)
	out (", ");
	prod_instr (false,"", nb +1) (List.nth instrl 1); (* operande droite *)
      end;

    if !verbose_mode then
      begin
	out_start "# </prim>" nb;
      end;

  | FUNCTION _ ->
    if !verbose_mode then
      begin
	out_start "# <function>" nb;
	out_start "# </function>" nb;
      end;
    ();
;;

let fun_header fn cn =
  if !verbose_mode then
    List.iter out
      ["\n\n";
       "# Declaration de la fonction "^fn^"\n";
      ]
;;

(* invocation *)
let prod_invoke_fun cn ar t lp instr =
  if !verbose_mode then
    begin
      out ("  # Arg(s) pour "^cn^" : ");
      out (""^(List.hd lp));
      List.iter (fun x -> out (", "^x)) (List.tl lp);
    end;
  prod_instr (true,"",2) instr;
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

(* generation des fonctions *)
let prod_one ast_li =
  List.iter prod_fun ast_li
;;

(* generation du main *)
let prod_three ast_li =
  List.iter (prod_instr (false,"",0)) ast_li
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
    out ("  .data\n");

    (* generation des declaration des variable du main            *)
    header_two filename; prod_two ast_li; footer_two filename;
    out ("\n");

    out ("  .text\n");
    out ("  j main\n");
    (* generation des fonctions *)
    if !verbose_mode then out ("\n# generation des fonctions\n");
    prod_one ast_li;
    footer_one filename;

    (* generation du main *)
    if !verbose_mode then out ("\n# generation du main\n");

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

