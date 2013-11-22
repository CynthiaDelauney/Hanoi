(*
 * Delauney Cynthia
 * 3009454
 *
 * hanoi.ml
 *
 *)

(* ============================================================================= *)
(*                          Declaration des types                                *)
(* ============================================================================= *)

(*
 * (p1, p2, p3, père, liste des configurations voisines) 
 *)

type configuration = int Stack.t * int Stack.t * int Stack.t * pere * liste 
and pere = Null | Pere of configuration 
and liste = Vide | Liste of (configuration list) ;; 

(* ============================================================================= *)
(*                                 UTILITAIRES                                   *)
(* ============================================================================= *)

exception PasdeVNV ;;

let get_num conf =
    let num = ref 0 in 
    let (p1, p2, p3, pere, lst) = conf in 
    let _ = Stack.iter (fun elem -> num := !num + 
                int_of_float(2. *. (3. ** float_of_int(elem - 1)))) p3 in
    let _ = Stack.iter (fun elem -> num := !num + 
                int_of_float(3. ** float_of_int(elem -1))) p2 in !num ;;

(* est_valide renvoie true si la configuration n'a pas était visité *)
let est_valide tab_NV conf =
    tab_NV.(get_num conf) ;;

let push elem p =
    let np = Stack.copy p in
    Stack.push elem np; np ;;

let pop p = 
    let np = Stack.copy p in
    Stack.pop np; np ;;

let top p =
    try
        Stack.top p
    with
    | Stack.Empty -> 0 ;;

let rec liste_conf_voisines (noeud_init : configuration) nbr_disques : configuration list = 
    let (p1, p2, p3, pere, lst) = noeud_init in
    let liste = ref [] in
    let sp1 = top p1 in
    let sp2 = top p2 in
    let sp3 = top p3 in
    (if sp1 = 1 then
        let _ = liste := (pop p1, push 1 p2, p3, pere, lst)::(pop p1, p2, push 1 p3, pere, lst)::(!liste) in
            (match sp2, sp3 with
            | 0, 0                    -> () (* cas où une les deux piles p2 et p3 sont vide *)
            | 0, sp3                  -> liste := (p1, push sp3 p2, pop p3, pere, lst)::(!liste)
            | sp2, 0                  -> liste := (p1, pop p2, push sp2 p3, pere, lst)::(!liste)
            | sp2, sp3 when sp2 < sp3 -> liste := (p1, pop p2, push sp2 p3, pere, lst)::(!liste)
            | sp2, sp3 when sp2 > sp3 -> liste := (p1, push sp3 p2, pop p3, pere, lst)::(!liste)
            | _ -> () (* pas supposé passer par là *) )
    else 
        if sp2 = 1 then
            let _ = liste := (push 1 p1, pop p2, p3, pere, lst)::(p1, pop p2, push 1 p3, pere, lst)::(!liste) in
                (match sp1, sp3 with
                | 0, 0                    -> () (* cas où une les deux piles p2 et p3 sont vide *)
                | 0, sp3                  -> liste := (push sp3 p1, p2, pop p3, pere, lst)::(!liste)
                | sp1, 0                  -> liste := (pop p1, p2, push sp1 p3, pere, lst)::(!liste)
                | sp1, sp3 when sp1 < sp3 -> liste := (pop p1, p2, push sp1 p3, pere, lst)::(!liste)
                | sp1, sp3 when sp1 > sp3 -> liste := (push sp3 p1, p2, pop p3, pere, lst)::(!liste)
                | _ -> () (* pas supposé passer par là *) )
        else 
            let _ = liste := (push 1 p1, p2, pop p3, pere, lst)::(p1, push 1 p2, pop  p3, pere, lst)::(!liste) in
                (match sp1, sp2 with
                | 0, 0                    -> () (* cas où une les deux piles p2 et p3 sont vide *)
                | 0, sp2                  -> liste := (push sp2 p1, pop p2, p3, pere, lst)::(!liste)
                | sp1, 0                  -> liste := (pop p1, push sp1 p2, p3, pere, lst)::(!liste)
                | sp1, sp2 when sp1 < sp2 -> liste := (pop p1, push sp1 p2, p3, pere, lst)::(!liste)
                | sp1, sp2 when sp1 > sp2 -> liste := (push sp2 p1, pop p2, p3, pere, lst)::(!liste)
                | _ -> () (* pas supposé passer par là *) ) ) ;
    !liste ;; 

(*
 * choisir_voisin_nonvisite renvoie un configuration voisine valide de conf
 *)

let choisir_voisin_nonvisite conf file tab_NV nbr_disques : configuration =
    let (p1, p2, p3, pere, lst) = conf in 
    let liste_voisins = ( match lst with
                          | Vide       -> List.filter (est_valide tab_NV) (liste_conf_voisines conf nbr_disques)
                          | Liste(lst) -> lst ) in
    if liste_voisins = [] then raise PasdeVNV
    else 
        let nconf = (p1, p2, p3, pere, Liste(List.tl liste_voisins)) in
        let _ = DeQueue.enleve_tete file in
        let _ = DeQueue.insere_tete nconf file in
            List.hd liste_voisins ;;

(*
 * get_chemin recupère le chemin de la configuration initiale à la configuration conf
 * à partir de la configuration conf on remonte de père en père
 *)

let get_chemin conf =
    let (p1, p2, p3, pere, _) = conf in 
    if pere = Null then failwith "il n'existe pas de chemin"
    else (
        let rec get_chemin_aux (conf : configuration) aux =
            let (p1, p2, p3, pere, _) = conf in 
                (  match pere with
                   | Null                               -> aux
                   | Pere((np1, np2, np3, _, _) as prec) -> 
                        (get_chemin_aux prec ((np1, np2, np3)::aux)) )
        in get_chemin_aux conf [(p1, p2, p3)] ) ;;

(* ============================================================================= *)
(*                              Utilitaires output                               *)
(* ============================================================================= *)

let output_int = function oc -> function x ->
    output_string oc (string_of_int x) ;;

let output_float oc f = output_string oc (string_of_float f) ;;

let print_pile p =
    Printf.printf "(" ;
    Stack.iter (Printf.printf "%d ") p ;
    Printf.printf ")\n" ;;

let print_piles tab_p =
    print_string "\npile p1 : " ;
    print_pile tab_p.(0) ;
    print_string "pile p2 : " ;
    print_pile tab_p.(1) ;
    print_string "pile p3 : " ;
    print_pile tab_p.(2) ;
    print_string " ============ " ;;

let affiche_conf conf =
    let (p1, p2, p3, _, _) = conf in 
        begin
            print_string "\npile p1 : " ;
            print_pile p1 ;
            print_string "pile p2 : " ;
            print_pile p2 ;
            print_string "pile p3 : " ;
            print_pile p3 ;
            print_string " ============ "
        end ;;

let affiche_triplet triplet =
    let (p1, p2, p3) = triplet in 
        affiche_conf (p1, p2, p3, Null, Vide) ;;

let output_ligne oc i temps_cpu =
    begin 
        output_int oc i ;
        output_string oc " " ;
        output_float oc temps_cpu ;
        output_string oc "\n"
    end ;;

let message () = Printf.printf 
    "Entré incorrecte :\n\
    (1) -courbe <algo> <n> : pour generer une courbe temps cpu en fonction de n, avec comme algorithme : hanoi, hanoi1, hanoi2 ou hanoi3 (50 configurations)\n\
    (2) -chemin <algo> <nbr_disques> : pour generer un chemin, avec comme algorithme : hanoi, hanoi1, hanoi2 ou hanoi3\n" ;;

(* ============================================================================= *)
(*                       Fonction d'initialisations                              *)
(* ============================================================================= *)

let random_piquet tab_disques nbr =
    let pile = Stack.create () in
    for i = (Array.length tab_disques) - 1 downto 0 do 
        if ((tab_disques.(i) = true) && (Random.int 5 <= nbr)) 
        then (Stack.push (i + 1) pile ; tab_disques.(i) <- false) 
    done ; pile ;;

let random_dernier_piquet tab_disques = 
    let pile = Stack.create () in
    for i = (Array.length tab_disques) - 1 downto 0 do 
        if (tab_disques.(i) = true) 
        then (Stack.push (i + 1) pile ; tab_disques.(i) <- false)
    done ; pile ;;

(*
 * initialisation_file initialise une file contenant une seule configuration : la configuration initiale
 * cette configuration est déterminée aléatoirement
 *)

let initialisation_file nbr_disques tab_NV =
    let file_sommets = DeQueue.init () in
    let tab_disques = Array.create nbr_disques true in
    let p1 = random_piquet tab_disques 1 in
    let p2 = random_piquet tab_disques 2 in
    let p3 = random_dernier_piquet tab_disques in
    let conf = (p1, p2, p3, Null, Vide) in 
    let _ = flush stdout in
    let _ = (DeQueue.insere conf file_sommets) in
    let _ = tab_NV.(get_num conf) <- false in file_sommets ;;

(* initialise le piquet 1 *)
let initialiser_p1 nbr_disques =
    let p1 = Stack.create () in 
        for i = nbr_disques downto 1 do
            Stack.push i p1
        done ; p1 ;;
(*
 * initialisation_tab_p initialise le tableau tab_p avec tous les disques sur le piquet 1
 *)

let initialisation_tab_p nbr_disques = 
    let tab_p = Array.create 3 (Stack.create ()) in 
    let p1 = initialiser_p1 nbr_disques in
    let _ = tab_p.(0) <- p1 in
    let _ = tab_p.(1) <- Stack.create () in
    let _ = tab_p.(2) <- Stack.create () in tab_p ;;

(*
 * initialisation_tab_p_random initialise le tableau tab_p de façon aléatoire
 *)

let initialisation_tab_p_random nbr_disques =
    let tab_disques = Array.create nbr_disques true in
    let tab_p = Array.create 3 (Stack.create ()) in 
    let _ = tab_p.(0) <- random_piquet tab_disques 1 in
    let _ = tab_p.(1) <- random_piquet tab_disques 2 in
    let _ = tab_p.(2) <- random_dernier_piquet tab_disques in tab_p ;;

(*
 * initialisation de tap_p avec la configuration donnée dans la question 18, nbr_disques = 64
 *)

let initialisation_tab_p_question18 () =
    let tab_p = Array.create 3 (Stack.create ()) in 
    let p1 = Stack.create () in 
    let _ = for i = 40 downto 19 do 
                Stack.push i p1 
            done in
    let _ = for i = 10 downto 1 do 
                Stack.push i p1 
            done in
    let p3 = Stack.create () in
    let _ = for i = 63 downto 41 do 
                Stack.push i p3 
            done in 
    let _ = Stack.push 11 p3 in 
    let p2 = Stack.create () in 
    let _ = Stack.push 64 p2 in 
    let _ = for i = 18 downto 12 do 
                Stack.push i p2 
            done in
    let _ = tab_p.(0) <- p1 in
    let _ = tab_p.(1) <- p2 in
    let _ = tab_p.(2) <- p3 in tab_p ;;

(* initialise le tableau pi d'aprés tab_p *)
let initialisation_pi tab_p nbr_disques =
    let pi = Array.create nbr_disques 0 in
    for j = 0 to 2 do
        Stack.iter (fun disque -> pi.(disque - 1) <- (j + 1)) tab_p.(j)
    done ; pi ;;

(* initialise le tableau pf d'aprés pi *)
let initialisation_pf pi nbr_disques destination =
    let pf = Array.create nbr_disques 0 in 
    let _ = pf.((Array.length pf) - 1) <- destination in 
    for i = ((Array.length pf) - 2) downto 0 do 
        if pi.(i + 1) = pf.(i + 1) then pf.(i) <- pf.(i + 1) 
        else pf.(i) <- 6 - pi.(i + 1) - pf.(i + 1)
    done ; pf ;;

(* ============================================================================= *)
(*                        Algorithmes principaux                                 *)
(* ============================================================================= *)

(*
 * hanoi1 : 
 * - détermine une séquence minimale de déplacements de disques jusqu’à résolution depuis une configuration quelconque
 * - on génére à la volée les voisins d’une configuration dans le graphe de Hanoi
 *)

let rec hanoi1 file_sommets tab_NV nbr_disques conf_final =
    if (!conf_final <> None) || (DeQueue.get_premier file_sommets) = DeQueue.FVide 
    then ()(* fin *)
    else 
        let premier_sommet_file = (DeQueue.get_sommet_info file_sommets) in
        (try
            let x = (choisir_voisin_nonvisite premier_sommet_file file_sommets tab_NV nbr_disques) in
            let (p1, p2, p3, pere, l) = x in 
            (* on actualise le nouveau pere et on met le bit à 1 *)
            let nx = (p1, p2, p3, Pere(premier_sommet_file), l) in
            let _ = if ((Stack.is_empty p1) && (Stack.is_empty p2)) ||
                       ((Stack.is_empty p1) && (Stack.is_empty p3)) ||
                       ((Stack.is_empty p2) && (Stack.is_empty p3)) 
                    then conf_final := Some nx in
            let _ = tab_NV.(get_num nx) <- false in
                (DeQueue.insere nx file_sommets) 
        with
        | PasdeVNV -> (DeQueue.enleve_tete file_sommets) ) ;
        (hanoi1 file_sommets tab_NV nbr_disques conf_final) ;;

(*
 * bouger origine destination tab : 
 * déplace le disque se trouvant en haut de la pile du piquet origine
 * vers le piquet destination
 *)

let bouger piquet1 piquet2 tab_p affichage =
    let src = tab_p.(piquet1 - 1) in
    let dst = tab_p.(piquet2 - 1) in
    try
        let elem = (Stack.pop src) in
        let _ = Stack.push elem dst in 
            if affichage then print_piles tab_p
    with Stack.Empty -> Printf.printf "deplacement impossible %d -> %d\n" piquet1 piquet2 ;;

(*
 * hanoi n origine destination :
 * déplace n disques du piquet origine au piquet destination en passant 
 * par le piquet 6 − origine − destination
 *)

let rec hanoi (nbr_disques : int) (piquet1 : int) (piquet2 : int) tab_p affichage =
    if  nbr_disques > 0 then 
        begin
            hanoi (nbr_disques - 1) piquet1 (6 - piquet1 - piquet2) tab_p affichage ;
            bouger piquet1 piquet2 tab_p affichage;
            hanoi (nbr_disques - 1) (6 - piquet1 - piquet2) piquet2 tab_p affichage
        end ;;

let rec hanoi_deplacement nbr_disques =
    if nbr_disques > 0 then (Big_int.add_big_int Big_int.unit_big_int (Big_int.mult_int_big_int 2 (hanoi_deplacement (nbr_disques - 1)))) 
    else  Big_int.zero_big_int ;;


(*
 * hanoi3 :
 * - calcule le nombre de déplacements de disques d'une configuration initiale quelconque jusqu'à la résolution
 *)

let hanoi3 tab_p nbr_disques destination  =
    let nbr_deplacements = ref Big_int.zero_big_int in
    let pi = initialisation_pi tab_p nbr_disques in
    let pf = initialisation_pf pi nbr_disques destination in
    let _ = if (pi.(0) <> pf.(0)) then nbr_deplacements := (Big_int.add_big_int !nbr_deplacements Big_int.unit_big_int) else () in
        for i = 0 to nbr_disques - 2 do 
              if pi.(i + 1) <> pf.(i + 1) then 
                ( nbr_deplacements := (Big_int.add_big_int !nbr_deplacements Big_int.unit_big_int) ;
                  nbr_deplacements := (Big_int.add_big_int !nbr_deplacements (hanoi_deplacement (i + 1))) )
        done ;
        !nbr_deplacements ;;

(*
 * hanoi2 n origine destination :
 * - détermine une séquence minimale de déplacements de disques jusqu’à résolution depuis une configuration origine quelconque
 * - on décompose le problème de départ en n sous-problèmes
 *)

let hanoi2 nbr_disques destination affichage =
    let tab_p = initialisation_tab_p_random nbr_disques in
    let pi = initialisation_pi tab_p nbr_disques in
    let pf = initialisation_pf pi nbr_disques destination in
    let _ = if (pi.(0) <> pf.(0)) then (bouger pi.(0) pf.(0) tab_p affichage) else () in
        for i = 0 to nbr_disques - 2 do 
              if pi.(i + 1) <> pf.(i + 1) then 
                ( bouger pi.(i + 1) pf.(i + 1) tab_p  affichage ;
                  hanoi (i + 1) (6 - pi.(i + 1) - pf.(i + 1)) pf.(i + 1) tab_p affichage)
        done  ;;

(* ============================================================================= *)
(*                                   main                                        *)
(* ============================================================================= *)

let () =
    let _ = Random.self_init () in
    if Array.length Sys.argv <= 3 then message ()
    else
    match Sys.argv.(1), Sys.argv.(2) with
    | "-courbe", "hanoi1" -> (* 
                              * Generation d'une courbe temps cpu en fonction de n 
                              *)
                              let oc = open_out "donnees.dat" in
                              let _ = Printf.printf "\nPour chaque valeur de n, on calcule le temps moyen d’exécution pour 50 configurations de départ tirées aléatoirement :\n" in
                                  for j = 1 to (int_of_string(Sys.argv.(3))) do
                                      let _ = Printf.printf "Hanoi1 nbr_disques = %d\n" j in
                                      let moy = ref 0. in
                                      for i = 1 to 50 do
                                          let nbr_disques = j in (* n est le nombre de disques *)
                                          let _ = flush stdout in
                                          let conf_final = ref None in
                                          let tab_NV = Array.create (int_of_float(3. ** float_of_int(nbr_disques))) true in
                                          let file_sommets = initialisation_file nbr_disques tab_NV in
                                          let n1 = Sys.time () in
                                          let _ = hanoi1 file_sommets tab_NV nbr_disques conf_final in
                                          let n2 = Sys.time () in
                                               moy := !moy +. (n2 -. n1)
                                      done ;
                                      let _ = moy := !moy /. 50. in output_ligne oc j !moy
                                 done ;
                                 close_out oc ; print_string "\n" ;
                                 if Sys.command "gnuplot genere_cpu_courbe.gnu" = 0 then ()
                                 else print_string "Erreur lors de la génération de la courbe.\n" ;
    | "-chemin", "hanoi1" -> (*
                              * Generation d'un chemin, avec n passé en parametre 
                              *)
                             let conf_final = ref None in
                             let nbr_disques = (int_of_string Sys.argv.(3)) in
                             let tab_NV = Array.create (int_of_float(3. ** float_of_int(nbr_disques))) true in
                             let file_sommets = initialisation_file nbr_disques tab_NV in
                             let _ = hanoi1 file_sommets tab_NV nbr_disques conf_final in 
                             let conf = match !conf_final with 
                                        | None -> failwith "Pas de chemins... la configuration final est Null" 
                                        | Some c -> c in 
                             let _ = List.iter affiche_triplet (get_chemin conf) in print_string "\n" ;
    | "-courbe", "hanoi"  -> (* 
                              * Generation d'une courbe temps cpu en fonction de n 
                              *)
                             let oc = open_out "donnees.dat" in
                                 for i = 1 to (int_of_string Sys.argv.(3)) do
                                     let _ = Printf.printf "Hanoi nbr_disques = %d\n" i in
                                     let _ = flush stdout in
                                     let nbr_disques = i in (* n est le nombre de disques *) 
                                     let tab_p = initialisation_tab_p nbr_disques in (* tab_p est un tableau contenant les trois piquet *)
                                     let n1 = Sys.time () in
                                     let _ = hanoi nbr_disques 1 3 tab_p false in
                                     let n2 = Sys.time () in
                                     let temps_cpu = (n2 -. n1) in
                                        output_ligne oc i temps_cpu
                                 done ;
                                 close_out oc ; print_string "\n" ;
                                 if Sys.command "gnuplot genere_cpu_courbe.gnu" = 0 then ()
                                 else print_string "Erreur lors de la génération de la courbe.\n" ;
    | "-chemin", "hanoi" -> (* 
                             * Generation d'un chemin, avec n passé en parametre 
                             *)
                             let nbr_disques = (int_of_string Sys.argv.(3)) in
                             let tab_p = initialisation_tab_p nbr_disques in (* tab_p est un tableau contenant les trois piquet *)
                             let _ = hanoi nbr_disques 1 3 tab_p true in print_string "\n" ;
    | "-courbe", "hanoi2"  -> (*
                               * Generation d'une courbe temps cpu en fonction de n 
                               *)
                               let oc = open_out "donnees.dat" in
                               let _ = Printf.printf "\nPour chaque valeur de n, on calcule le temps moyen d’exécution pour 50 configurations de départ tirées aléatoirement :\n" in
                                   for j = 1 to (int_of_string(Sys.argv.(3))) do
                                       let _ = Printf.printf "Hanoi2 nbr_disques = %d\n" j in
                                       let moy = ref 0. in
                                       for i = 1 to 50 do
                                           let nbr_disques = j in (* n est le nombre de disques *)
                                           let _ = flush stdout in
                                           let n1 = Sys.time () in
                                           let _ = hanoi2 nbr_disques 3 false in
                                           let n2 = Sys.time () in
                                                moy := !moy +. (n2 -. n1)
                                       done ;
                                       let _ = moy := !moy /. 50. in output_ligne oc j !moy
                                   done ;
                                   close_out oc ; print_string "\n" ;
                                   if Sys.command "gnuplot genere_cpu_courbe.gnu" = 0 then ()
                                   else print_string "Erreur lors de la génération de la courbe.\n" ;
    | "-chemin", "hanoi2" -> (* 
                              * Generation d'un chemin, avec n passé en parametre 
                              *)
                             let nbr_disques = (int_of_string Sys.argv.(3)) in
                             let _ = hanoi2 nbr_disques 3 true in print_string "\n" ;
    | "-courbe", "hanoi3" -> (*
                              * Generation d'une courbe temps cpu en fonction de n 
                              *)
                             let oc = open_out "donnees.dat" in
                             let _ = Printf.printf "\nPour chaque valeur de n, on calcule le temps moyen d’exécution pour 50 configurations de départ tirées aléatoirement :\n" in
                                 for j = 1 to (int_of_string(Sys.argv.(3))) do
                                     let _ = Printf.printf "Hanoi3 nbr_disques = %d\n" j in
                                     let _ = flush stdout in
                                     let moy = ref 0. in
                                     for i = 1 to 50 do
                                         let nbr_disques = j in (* n est le nombre de disques *)
                                         let n1 = Sys.time () in
                                         let tab_p = initialisation_tab_p_random nbr_disques in
                                         let _ = hanoi3 tab_p nbr_disques 3 in
                                         let n2 = Sys.time () in
                                              moy := !moy +. (n2 -. n1)
                                     done ;
                                     let _ = moy := !moy /. 50. in output_ligne oc j !moy
                                 done ;
                                 close_out oc ; print_string "\n" ;
                                 if Sys.command "gnuplot genere_cpu_courbe.gnu" = 0 then ()
                                 else print_string "Erreur lors de la génération de la courbe.\n" ;
    | "-chemin", "hanoi3" -> (*
                              * Generation d'un chemin, avec n passé en parametre 
                              *)
                             let nbr_disques = (int_of_string Sys.argv.(3)) in
                             let tab_p = initialisation_tab_p_random nbr_disques in (*initialisation_tab_p_question18 () in*)  
                             let _ = print_string "\nConfiguration initiale : " in 
                             let _ = print_piles tab_p in
                             begin
                                  let res = (hanoi3 tab_p nbr_disques 3) in
                                  print_string "\nLe nombre de déplacement pour cette configuration est : " ;
                                  print_string (Big_int.string_of_big_int res) ;
                                  print_newline () ;
                                  print_string "Soit " ;
                                  print_string (Big_int.string_of_big_int (Big_int.div_big_int res (Big_int.big_int_of_int 31536000))) ;
                                  print_string " années.\n" 
                                end ;
    | _   -> message () ;;  

