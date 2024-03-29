(************************************************************************
 *                                                                      *
 *                       MASTER STL M2 anne'e 2005/06                   *
 *                                                                      *
 *                                  Cours TEP                           *
 *                                                                      *
 *                           typeur d'un mini-ml                        *
 *                                                                      *
 *                partie de intertypeur/intereval/ml2java               *
 *                                                                      *
 ************************************************************************
 *                                                                      *
 *   typeur.ml : typeur du mini-ml                                      *
 *                                                                      *
 *   version : $Version$     $Date: 2006/05/01 23:58:34 $               *
 *                                                                      *
 *   auteur : Emmanuel Chailloux                                        *
 *                                                                      *
 ************************************************************************)

open Util;;
open Types;;


let new_unknown,reset_unknowns,max_unknown = 
  let c = ref 1 
  and max = ref 10000 
  in 
   ( (function () -> c:=!c+1; if !c >= !max then failwith "No more types";
                     Var_type( ref(Unknown !c))),
     (function () -> c:=1),
     (function () -> Var_type(ref(Unknown !max))))
;;



type quantified_type = Forall of (int list) * ml_type;;

let rec vars_of_type t =
   let rec vars vl = function
     Const_type _ -> vl
   | Var_type vt ->
       ( match !vt with
           Unknown n -> if List.mem n vl then vl else n::vl
         | Instanciated t -> vars vl t
       )
   | Pair_type (t1,t2) -> vars (vars vl t1) t2
   | List_type t -> vars vl t
   | Fun_type (t1,t2) -> vars (vars vl t1) t2
   | Ref_type t -> vars vl t
   in
     vars [] t ;;



let subtract l1 l2 =
   List.flatten (List.map (function id ->
                             if (List.mem id l2) then [] else [id])
                  l1);;

let free_vars_of_type (bv,t) =
   subtract (vars_of_type t) bv
 and bound_vars_of_type (fv,t) =
   subtract (vars_of_type t) fv ;;

let flat ll = List.fold_right (@) ll [];;

let free_vars_of_type_env l =
     flat ( List.map (function (id,Forall (v,t))
                        -> free_vars_of_type (v,t)) l) ;;



 let type_instance st =
   match st with Forall(gv,t) -> 
     let unknowns = List.map (function n -> n,new_unknown()) gv
     in
       let rec instance = function
         Var_type {contents=(Unknown n)} as t ->
            (try List.assoc n unknowns with Not_found -> t)
       | Var_type {contents=(Instanciated t)} -> instance t
       | Const_type ct as t -> t
       | Pair_type (t1,t2) -> Pair_type (instance t1, instance t2)
       | List_type t -> List_type (instance t)
       | Fun_type (t1,t2) -> Fun_type (instance t1, instance t2)
       | Ref_type t -> Ref_type (instance t)
       in
         instance t ;;

type typing_error =
   Unbound_var of string
 | Clash of ml_type * ml_type ;;

exception Type_error of typing_error;;



let occurs n t = List.mem n (vars_of_type t);;

let rec shorten = function
     Var_type (vt) as tt ->
       (match !vt with
            Unknown _  -> tt
          | Instanciated ((Var_type _) as t) ->
              let t2 = shorten t in
                vt := Instanciated t;
                t2
          | Instanciated t -> t
       )
   | t -> t;;


let rec unify_types (t1,t2) =
    let lt1 = shorten t1 and lt2 = shorten t2
    in
      match (lt1,lt2) with
      | Var_type ( {contents=Unknown n} as occn ),
        Var_type {contents=Unknown m} ->
          if n=m then () else occn:= Instanciated lt2
      | Var_type ({contents=(Unknown n)} as occn), _ -> 
          if occurs n lt2
          then raise (Type_error(Clash(lt1,lt2)))
          else occn:=Instanciated lt2
      | _ , Var_type ({contents=(Unknown n)}) -> unify_types (lt2,lt1)
      | Const_type ct1, Const_type ct2 ->
          if ct1=ct2 then () else raise (Type_error(Clash(lt1,lt2)))
      | Pair_type (t1,t2), Pair_type (t3,t4) ->
          unify_types (t1,t3); unify_types(t2,t4)
      | List_type t1, List_type t2 ->  unify_types (t1,t2)
      | Ref_type t1, Ref_type t2 ->  unify_types (t1,t2)
      | Fun_type (t1,t2), Fun_type (t3,t4) -> 
         unify_types (t1,t3); unify_types(t2,t4)
      | _ ->  raise(Type_error(Clash(lt1,lt2)));;


let type_const = function
   Int _ -> Const_type Int_type
 | Float _ -> Const_type Int_type
 | String _ -> Const_type String_type
 | Bool _ ->  Const_type Bool_type
 | Unit ->  Const_type Unit_type
 | Emptylist -> List_type (new_unknown()) ;;

let generalize_types gamma l =
   let fvg = free_vars_of_type_env gamma
   in
     List.map (function (s,t) ->
                (s, Forall(free_vars_of_type (fvg,t),t))) l
 ;;


let acces_type et = match et with 
  Straint(e,t) -> t
| _ -> failwith "acces_type";;

let acces_expr et = match et with 
  Straint(e,t) -> e
| _ -> failwith "acces_expr";;


let rec is_expansive e = match e with 
  Var _ -> false
| Abs _ -> false
| App _ -> true
| Ref _ -> true
| Unop (n,e) -> is_expansive e
| Binop (n,e1,e2) -> is_expansive e1 || is_expansive e2
| Pair (e1,e2) -> is_expansive e1 || is_expansive e2
| Cons (e1,e2) -> is_expansive e1 || is_expansive e2
| Cond (e1,e2,e3) -> is_expansive e1 || is_expansive e2 || is_expansive e3
| Letin (r, n, e1, e2) -> is_expansive e1 || is_expansive e2
| Straint (e,t) -> is_expansive e
| _ -> false
;;

let rec type_expr gamma =
   let rec type_rec expri = 
     match expri with
         Const c -> Straint (expri, type_const c)
       | Var s -> let t = try List.assoc s gamma
          with Not_found -> raise (Type_error(Unbound_var s))
          in  Straint(expri, type_instance t)
       | Unop (s,e) ->
           let t = try assoc s gamma
                   with Not_found -> raise (Type_error(Unbound_var s))
            in
              let t1= type_instance t
              and et = type_rec e in
                let t2 = acces_type et  
                and u = new_unknown() 
                in
                  unify_types(t1, Fun_type(t2,u)); Straint (Unop(s,et), u)
       | Binop(s,e1,e2) ->
          let t = try assoc s gamma
                  with Not_found -> raise (Type_error(Unbound_var s))
          in
            let t0 = type_instance t
            and et1 = type_rec e1
            and et2 = type_rec e2
            in
              let t1 = acces_type et1 
              and t2 = acces_type et2 
              in
              let u = new_unknown()
              in
                unify_types(t0, Fun_type(Pair_type (t1,t2),u));
                Straint(Binop(s,et1,et2),u)

       | Pair (e1,e2) -> 
        let et1 = type_rec e1
        and et2 = type_rec e2 
        in 
          let t1 = acces_type et1 
          and t2 = acces_type et2 
          in 
            Straint(Pair(et1,et2),Pair_type (t1,t2))

       | Cons (e1,e2) ->
          let et1 = type_rec e1 
          and et2 = type_rec e2
          in 
            let t1 = acces_type et1 
            and t2 = acces_type et2 
            in
            unify_types (List_type t1,t2);
            Straint(Cons(et1,et2),t2)

       | Cond (e1,e2,e3) -> 
         let et1 = type_rec e1
         and et2 = type_rec e2
         and et3 = type_rec e3 
         in 
           let t1 = acces_type et1
           and t2 = acces_type et2
           and t3 = acces_type et3
           in 
             unify_types (Const_type Bool_type, t1);
             unify_types (t2,t3);
             Straint(Cond(et1,et2,et3),t2)

       | App (e1,e2) ->
         let et1 = type_rec e1
         and et2 = type_rec e2
         in 
           let t1 = acces_type et1
           and t2 = acces_type et2 
           in 
           let u = new_unknown() 
           in
             unify_types (t1, Fun_type (t2,u));
             Straint(App(et1,et2),u)

       | Abs(s,e) ->
        let t = new_unknown() 
        in 
          let new_env = (s,Forall ([],t))::gamma
          in 
            let et = type_expr new_env e in
            let t2 = acces_type et in
            Straint(Abs(s,et),Fun_type (t, t2))

       | Letin (false,s,e1,e2) -> 
        let et1 = type_rec e1
        in 
          let t1 = acces_type et1 in
          let new_env = generalize_types gamma [(s,t1)]
          in 
            let et2 = type_expr (new_env@gamma) e2 in
            let t2 = acces_type et2 in
              Straint(Letin(false,s,et1,et2),t2)

       | Letin (true,s,e1,e2) -> 
       let u = new_unknown ()
        in 
          let new_env = (s,Forall([],u))::gamma 
          in 
            let et1 = type_expr (new_env@gamma) e1
            in 
            let t1 = acces_type et1 in
              let final_env = generalize_types gamma [(s,t1)]
              in 
                let et2 = type_expr (final_env@gamma) e2
                in
                  let t2 = acces_type et2 in
                    Straint(Letin(true,s,et1,et2),t2)

       | Ref e -> 
             let et = type_expr gamma e in 
               let t = acces_type et in
                  Straint(Ref et,Ref_type t)
       | Straint(e,t) -> 
            let et = type_expr gamma e in 
              unify_types (t, acces_type et);
              expri

   in
     type_rec;;



let print_consttype = function 
  Int_type -> print_string   "int"
| Float_type -> print_string "float"
| String_type -> print_string "string"
| Bool_type -> print_string "bool"
| Unit_type -> print_string "unit"
;;



let ascii i = let s = String.create 1 in s.[0] <- Char.chr  i;s ;;
let var_name n = 
  let rec name_of n = 
     let q,r = ((n / 26), (n mod 26))
     in 
        if q=0 then ascii (96+r)
        else (name_of q)^(ascii (96+r))
   in "'"^(name_of n)
;;



let print_quantified_type (Forall(gv,t)) = 
  let names = 
    let rec names_of = function
      (n,[]) -> []
    | (n,(v1::lv)) -> (var_name n)::(names_of (n+1,lv))
    in (names_of (1,gv))
  in 
    let var_names = combine (rev gv) names
    in 
      let rec print_rec = function 
         Var_type {contents=(Instanciated t)} -> print_rec t 
      |  Var_type {contents=(Unknown n)} -> 
           let name = (try assoc n var_names 
                       with Not_found -> ("t_"^(string_of_int n)))
                    (*raise (Failure "Non quantified variable in type"))*)
           in print_string name
      | Const_type ct -> print_consttype ct
      | Pair_type(t1,t2) -> print_string "("; print_rec t1;
                            print_string " * "; print_rec t2; print_string ")"
      | List_type t -> print_string "(("; print_rec t; print_string ") list)"
      | Fun_type(t1,t2)  -> print_string "("; print_rec t1;
                            print_string " -> "; print_rec t2; print_string ")"
      | Ref_type t -> print_string "(("; print_rec t; print_string ") ref)"
      in 
        print_rec t
;;



let print_type t = print_quantified_type (Forall(free_vars_of_type ([],t),t));;



let typing_handler typing_fun env expr = 
(*  reset_unknowns();*)
  try typing_fun env expr 
  with 
    Type_error (Clash(lt1,lt2)) -> 
        print_string  "Type clash between ";print_type lt1;
        print_endline " and ";print_type lt2; print_newline();
        failwith "type_check"
  | Type_error (Unbound_var s)  ->
        print_string "Unbound variable ";
        print_endline s;
        failwith "type_check"
;;

(*

let type_check e = 
  let t = typing_handler type_expr initial_typing_env e 
  in 
    let qt = snd(hd(generalize_types initial_typing_env ["it",t]))
    in 
      print_string "it : "; print_quantified_type qt; print_newline()
;;
*)

(*

type_check (Const (Int 3));;
type_check (Const (Float 3.2));;
type_check (Abs ("x", Pair(Var "x", Var "x")));;
type_check (Cond (App (Var "=", Pair(Const(Int 0), Const (Int 0)))
                 , Const(Int 2)
                 , Const(Int 5)))
;;
type_check (Letin (true,"fact",
Abs ("x", Cond (App (Var "=",Pair(Var "x",Const(Int 1))) 
                ,Const(Int 1) 
                ,App (Var "*"
                      ,Pair(Var "x"
                           ,App (Var "fact"
                                 ,App (Var"-"
                                       ,Pair(Var "x"
                                             ,Const(Int 1) ))))) ))
, App (Var "fact", Const (Int 4))));;

*)
