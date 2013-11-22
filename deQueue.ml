(*
 * Delauney Cynthia
 * 3009454
 *
 * deQueue.ml
 * Module d'implementation d'un liste doublement chaînées
 *)

type 'a elem = { mutable prev : 'a _elem ; mutable next : 'a _elem ; info : 'a }
and 'a _elem =
    | FVide
    | Elem of 'a elem ;;

type 'a _file = { mutable premier : 'a _elem ; mutable dernier : 'a _elem  } ;;

(* initialise une DeQueue vide *)
let init () = {premier = FVide ; dernier = FVide} ;;

let get_premier file =
    file.premier ;;

let get_dernier file =
    file.dernier ;;

(* 
 * val insere_file : 'a -> 'a file -> unit = <fun> 
 * insere v dans la file
*)

let insere v f =
    match f.premier with
    | FVide -> let rec elem = Elem {prev = FVide ; next = FVide ; info = v} in
                    f.premier <- elem ; f.dernier <- elem 
    | _     -> let ndernier = Elem {prev = f.dernier ; next = FVide ; info = v} in
               match f.dernier with
               | FVide  -> failwith "problème, file.premier <> Fvide alors que file.dernier = FVide\n"
               | Elem x -> x.next <- ndernier ; f.dernier <- ndernier ;;

(* insere v en tête de file *)
let insere_tete v f =
    let npremier = Elem {prev = FVide ; next = f.premier ; info = v} in
    match f.premier with
    | FVide -> f.premier <- npremier ; f.dernier <- npremier 
    | e     -> f.premier <- npremier ;;

(* 
 * 'a _file -> unit = <fun> 
 * retire de la file l'element en tete de file
 *)

let enleve_tete f =
    match f.premier with
    | FVide -> failwith "La file est vide, rien a faire\n"
    | Elem x -> let npremier = x.next in
                match npremier with
                | FVide -> f.premier <- FVide ; f.dernier <- FVide
                | Elem y -> let _ = y.prev <- FVide in
                            f.premier <- npremier ;; 

(* NE PAS UTILISER
 * Cette fonction donne une courbe en O(n^2) ... Je ne sais pas du tout pourquoi ??? !!!!
 *
 * val retirer_elem_file : 'a -> 'a _file -> unit = <fun> 
 * retire l'element elem de la file
 *)

let rec retirer_elem a file =
    match file.premier with 
    | FVide -> failwith "La file est vide, rien a faire (1)\n"
    | Elem x -> ( let rec aux = function
                        | FVide -> failwith "La file est vide, rien a faire (2)\n"
                        | Elem x -> if x.info = a
                                    then (  match x.prev, x.next with
                                            | FVide, FVide -> file.premier <- FVide ; file.dernier <- FVide
                                            | FVide, (Elem suivant_x) -> file.premier <- x.next ; suivant_x.prev <- FVide
                                            | (Elem precedent_x), FVide -> file.dernier <- x.prev ; precedent_x.next <- FVide
                                            | (Elem precedent_x), (Elem suivant_x) -> let _ = precedent_x.next <- x.next in
                                                                    suivant_x.prev <- x.prev ) 
                                    else (aux x.next) 
                    in 
                        (aux file.premier)) ;;

(*
 * 'a _file -> 'a elem
 * renvoie l'élément en tête de file
 *)

let get_sommet f =
    match f.premier with
    | FVide  -> failwith "La file est vide, rien a faire (3)\n"
    | Elem x -> x ;; 

let get_sommet_info f = 
    match f.premier with
    | FVide  -> failwith "La file est vide, rien a faire (3)\n"
    | Elem x -> x.info ;; 

let get_info elem =
    elem.info ;;

let get_next elem =
    elem.next ;;

(* construit une liste à partir d'une _file *)
let to_liste file =
    match file.premier with
    | FVide  -> []
    | Elem x -> ( let rec aux = function
                      | FVide -> []
                      | Elem y -> 
                        let elem = y.info  in 
                                    elem::(aux y.next)
                      in (aux file.premier) ) ;; 

(* affiche une file *)
let affiche file affiche_elem =
    match file.premier with
    | FVide  -> print_string "File : []\n"
    | Elem x -> (   let _ = print_string "File : [" in
                    let rec aux = function
                      | FVide -> Printf.printf "\b]\n"
                      | Elem y ->
                        let _ = affiche_elem y.info in 
                                    (aux y.next)
                      in (aux file.premier) ) ;; 
