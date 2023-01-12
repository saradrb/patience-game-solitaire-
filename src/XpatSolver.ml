open XpatLib
open Set

let carte_vide = (-1,Card.Trefle)

type game = Freecell | Seahaven | Midnight | Baker

type card = Card.card

(* une colonne est une liste de cartes *)
type colonne = Card.card List.t
type colonnes = colonne FArray.t

(* un depeau est un tableau d'entier ou chaque case represente une couleur et contient le numero de carte dedans *)
type depot= int PArray.t (* case 0:Trefle ; case 1:Pique ;case 2:Coeur ; case 3:Carreau *)

(* registres est un tableau ou chaque case represente un registre qui contient une carte ou vide*)
type registres = Card.card PArray.t


(*l'etat du jeux contient les depots actuels , les registres actuels et la liste de toute les colonnes*)
type etat = {depots: depot;  registres: registres option  ;colonnes: colonnes}


(*fonction qui crée un etat :*)
let make_etat (depots : depot) (registres : registres option) (colonnes : colonnes)=
  {depots= depots; registres= registres; colonnes= colonnes}

(*fonctions set du type etat :*)
let set_depots (etat : etat) (nv_depots : depot)= 
  {etat with depots=nv_depots}

let set_registres (etat : etat) (nv_registres : registres option)= 
  make_etat etat.depots nv_registres etat.colonnes

let set_colonnes (etat : etat) (nv_colonnes : colonnes)= 
  make_etat etat.depots etat.registres nv_colonnes 

(*-----------------------------*)

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)


type config = { mutable game : game; mutable seed: int; mutable mode: mode }
let config = { game = Freecell; seed = 123; mode = Search "" }

let getgame = function
  | "FreeCell"|"fc" -> Freecell
  | "Seahaven"|"st" -> Seahaven
  | "MidnightOil"|"mo" -> Midnight
  | "BakersDozen"|"bd" -> Baker
  | _ -> raise Not_found
  

let split_on_dot name =
  match String.split_on_char '.' name with
  | [string1;string2] -> (string1,string2)
  | _ -> raise Not_found

let set_game_seed name =
  try
    let (sname,snum) = split_on_dot name in
    config.game <- getgame sname;
    config.seed <- int_of_string snum
  with _ -> failwith ("Error: <game>.<number> expected, with <game> in "^
                      "FreeCell Seahaven MidnightOil BakersDozen")





(* fonction qui remplie une colonne avec n cartes *)
let rec fill_col col cards n=
  if n>0 then 
    match (col,cards) with 
    |([],x::l)-> fill_col [x] l (n-1)
    |(_,x1::l) -> fill_col (x1::col) l (n-1)
    |(_,[])-> col
  else col 

 (* fonction qui remplie n registres par une carte chacun *)
let rec fill_reg n registres cards= 
  if n=0 then registres
  else match cards with 
  |[]-> failwith "plus de cartes"
  |x::l->  match registres with
          |Some(reg) ->fill_reg  (n-1) (Some(PArray.set reg (n-1) x))  l  
          |None -> None
  

(* distribution des cartes selon les regles de SeaHeaven 
* 10 colonnes ou chaque une contient 5 carte et deux cartes dans les registres
*)

let distribution_sv etat permut =
  let {depots;registres;colonnes}= etat in
  let rec fill_all_col n colonnes permut =
    match n with
    |e when e=0 -> colonnes 
    |_ -> fill_all_col (n-1) (FArray.set colonnes (n-1) (fill_col [] permut 5)) (List.filteri (fun i a -> i>4) permut)
  in let fill_colonnes = fill_all_col 10 colonnes permut in
  {depots = depots; registres = fill_reg 2 registres (List.rev permut); colonnes =  fill_colonnes }  


(* distribution des cartes selon les regles de Midnight oil
* *)
let distribution_md etat permut =
  let {depots;registres;colonnes}= etat in
  let rec fill_all_col n colonnes permut =
    match n with 
    | e when e=0 -> colonnes 
    |_ -> fill_all_col (n-1) (FArray.set colonnes (n-1) (fill_col [] permut 3)) (List.filteri (fun i a -> i>2) permut)

  in 
{depots = depots; registres = None; colonnes = fill_all_col 18 colonnes permut}  


(* distribution des cartes selon les regles de Baker dozen*)
let distribution_bk etat permut =
  let {depots;registres;colonnes}= etat in
  let rec fill_all_col n colonnes permut =
    match n with 
    | e when e=0 -> colonnes 
    |_ -> (let rec fill_col col cards n = 
          if n>0 then 
          match (col,cards) with 
          |([],x::l)-> fill_col [x] l (n-1)
          |(_,x1::l) ->( match x1 with 
                        |(13,_)-> fill_col (col @ [x1]) l (n-1)
                        | _ ->fill_col (x1::col) l (n-1) )
          |(_,[])-> col
          else col 
        in
           
      fill_all_col (n-1) (FArray.set colonnes (n-1) (fill_col [] permut 4)) (List.filteri (fun i a -> i>3) permut)
    )
  in 
{depots = depots; registres = None; colonnes = fill_all_col 13 colonnes permut}  


(* distribution des cartes selon les regles de FreeCell *)

let distribution_fc etat permut=
  let {depots;registres;colonnes}= etat in
    let rec fill_all_col num_c colonnes permut =
      match num_c with
      |e when e=0 -> colonnes 
      | _ -> if((num_c mod 2)=0) then 
        fill_all_col (num_c-1) (FArray.set colonnes (num_c-1) (fill_col  [] permut 7))  (List.filteri (fun i a -> i>6) permut)
          else fill_all_col (num_c-1) (FArray.set colonnes (num_c-1) (fill_col  [] permut 6))  (List.filteri (fun i a -> i>5) permut)
    in
    {depots = depots; registres = registres; colonnes = fill_all_col 8 colonnes permut}





 
let init_game name permut = 
  let cards = List.map (fun a -> Card.of_num a) permut in
  match name with 
  |Freecell-> distribution_fc { 
      depots=PArray.make 4 0;
      registres= Some (PArray.make 4 (-1,Card.Trefle));
      colonnes=FArray.make 8 []} cards
  |Seahaven-> distribution_sv { 
      depots=PArray.make 4 0;
      registres= Some (PArray.make 4  (-1,Card.Trefle));
      colonnes=FArray.make 10 []} cards
  
  |Baker-> distribution_bk { 
      depots=PArray.make 4 0;
      registres=None;
      colonnes=FArray.make 13 []} cards
  |Midnight-> distribution_md { 
      depots=PArray.make 4 0;
      registres=None;
      colonnes=FArray.make 18 []} cards

let maj_depot depot n= 
  let num =(PArray.get depot n) + 1 in 
    PArray.set depot n num 






(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)

(* --------------------AFFICHAGE -------------------------------------------------------------------------------------------- *)

(*Affichage des colonnes *)
let print_col list_card =
  List.iter (fun card -> Printf.printf "%s " (Card.to_string card)) list_card

let print_col_nums list_card =
  List.iter (fun card -> Printf.printf "%d " (Card.to_num card)) list_card

let print_colonnes colonnes_tab = 
  FArray.iter (fun col ->Printf.printf "COL : "; List.iter (fun card -> Printf.printf "[%s] " (Card.to_string card)) col;Printf.printf "\n" ) colonnes_tab

(* affichage des registres  *)
let print_reg registres_tab =
  match registres_tab with 
  |None -> Printf.printf "Pas de registres"
  |Some(registres) -> PArray.iter (fun reg-> if (reg=(-1,Card.Trefle)) then Printf.printf "REG: vide\n" 
                        else Printf.printf "REG: %s\n" (Card.to_string reg)) registres

(* affichage des depots  *)
let print_depot depots =
  Printf.printf "Trefle : %d \n" (PArray.get depots 0);
  Printf.printf "Pique : %d \n" (PArray.get depots 1);
  Printf.printf "Coeur : %d \n" (PArray.get depots 2);
  Printf.printf "Carreau : %d \n" (PArray.get depots 3)

(* afficher l'etat actuel  *)
let display_state etat = 
  Printf.printf "\n\n\nETAT\n ";
  Printf.printf "les colonnes: \n";
  print_colonnes etat.colonnes;
  print_newline ();
  Printf.printf "les registres: \n";
  print_reg etat.registres;
  print_newline ();
  Printf.printf "les depots: \n";
  print_depot etat.depots

let print_card card =
  Printf.printf "%d %s \n" (Card.to_num card) (Card.to_string card)








(*----------------------------------EXECUTER UN COUP-------------------------------------------------------------------------*)
(*----------------------------------------------*)
(*le type action définie le second mot d'un coup*)
type action = 
  |Card of int
  |V
  |T 

let remove_from_colonne colonne=
  match colonne with 
  |[] -> failwith "colonne vide"
  |x :: colonne' -> colonne'
  
let remove_from_registre registres num_registre=
  PArray.set registres num_registre (-1,Card.Trefle)

let ajouter_registre_vide (card : Card.card) (registres : registres)= 
    (*retourne la position du registre vide si il existe; None sinon*)
    let rec chercher_registre_vide (card : Card.card) (registres : registres) (num_registre : int) (nbr_registre : int)= 
        if num_registre > nbr_registre 
        then None
        else if (PArray.get registres num_registre) = (-1,Card.Trefle)
            then Some(num_registre)
            else chercher_registre_vide card registres (num_registre + 1) nbr_registre
    in 
    match chercher_registre_vide card registres 0 ((PArray.length registres) - 1) with 
    |None -> failwith " coup non valide" (*pas de registre vide coup invalide*)
    |Some i -> PArray.set registres i card 


let ajouter_colonne_vide (card : Card.card) (colonnes : colonnes)= 
    (*retourne la position de la colonne vide si elle existe; None sinon*)
    let rec chercher_colonne_vide (card : Card.card) (colonnes : colonnes) (num_colonne : int) (nbr_colonne : int)= 
        if num_colonne > nbr_colonne
        then None

        else if (FArray.get colonnes num_colonne) = []
            then Some(num_colonne)
            else chercher_colonne_vide card colonnes (num_colonne + 1) nbr_colonne
    in 
    match chercher_colonne_vide card colonnes 0 ((FArray.length colonnes) - 1) with 
    |None -> failwith " coup non valide"  (*pas de colonne vide coup invalide*)
    |Some i -> FArray.set colonnes i [card] 

(* let coup_V (card : Card.card) (colonnes : colonnes)=
  let empty_col = FArray.find_opt (fun col -> col=[]) colonnes in
  match empty_col with 
  |Some(col)-> card :: col 
  |None -> failwith " coup non valide" *)



let coup_carte_vers_carte c1 c2 (colonnes : colonnes) n_coup= 
    (*retourne le numero de la colonne qui a card à son sommet (en tête), retourne None si elle n'existe pas*)
    let rec chercher_colonne (card : Card.card) (colonnes : colonnes) (num_colonne : int) = 
    if num_colonne >= (FArray.length colonnes)
    then None
    else match (FArray.get colonnes num_colonne) with 
      |[]-> chercher_colonne card colonnes (num_colonne + 1) 
      |x::l -> if x = card then Some(num_colonne)
            else chercher_colonne card colonnes (num_colonne + 1) 
    in 
    match (chercher_colonne c2 colonnes 0 ) with 
    |None -> Printf.printf"ECHEC %d\n" n_coup; exit 1 (*pas de colonne avec comme sommet la carte c2 coup invalide*)
    |Some i -> FArray.set colonnes i (c1 :: (FArray.get colonnes i)) 

(* retourne la poition de la colonne qui a card à son sommet  *)
let search_col (card : Card.card) (colonnes :colonnes) = 
  let rec search_aux (card : Card.card) (colonnes :colonnes)(index:int) =
    if index > (FArray.length colonnes)-1 then -1
    else match  (FArray.get colonnes index) with
    |[]-> search_aux card colonnes (index+1)
    |card'::l -> begin  
                match card' with 
                |e when e= card -> index
                |_ -> search_aux card colonnes (index+1)
                end 
  in search_aux card colonnes 0


let search_reg (card : Card.card) (registres:registres) =
  let rec search_aux card registres i =
    if (i > (PArray.length registres)-1) then -1 
    else 
      match (PArray.get registres i) with 
      |e when e = card -> i
      |_ -> let i= search_aux card registres (i + 1) in i
      in search_aux card registres 0


let coup (c1 : int) (c2 : action) (etat : etat) (n_coup:int)= 
  let remove_card c1 etat= 
    let index= search_col (Card.of_num c1) etat.colonnes in (*Printf.printf "c1 is in col %d" index;*)
    if index = (-1) then 
      (
      match etat.registres with 
      |Some(registres) -> let index= (search_reg (Card.of_num c1 )(registres)) in set_registres etat (Some(remove_from_registre registres index))
      |None -> failwith "coup non valide"
      )
    else  set_colonnes etat  (FArray.set etat.colonnes index (remove_from_colonne (FArray.get etat.colonnes index)))
  in let new_etat = remove_card c1 etat in (*Printf.printf"NEW ETAT\n"; print_colonnes new_etat.colonnes;*)
    match c2 with
    |Card(carte) -> set_colonnes new_etat (coup_carte_vers_carte (Card.of_num c1) (Card.of_num carte) new_etat.colonnes n_coup)
    |V -> set_colonnes new_etat (ajouter_colonne_vide (Card.of_num c1) new_etat.colonnes)
    |T -> match new_etat.registres with 
          |None -> failwith "coup non valide"
          |Some(r) -> set_registres new_etat (Some(ajouter_registre_vide (Card.of_num c1) r))



(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)

(* ---------------------------------------------NORMALISATION-------------------------------------------------------- *)



let rec normalize_col colonnes depot n changement =
  (* Printf.printf"\n\n we are in normalize col %d\n" n;  print_colonnes colonnes; print_depot depot ; *)
  let dep_tr = ((PArray.get depot 0) + 1,Card.Trefle) in 
  let dep_pi = ((PArray.get depot 1) + 1,Card.Pique)in 
  let dep_co = ((PArray.get depot 2) + 1,Card.Coeur) in 
  let dep_ca = ((PArray.get depot 3) + 1,Card.Carreau) in 
    if (n < FArray.length colonnes) then
      match FArray.get colonnes n with 
      |[] -> normalize_col colonnes depot (n+1) changement
      |_ -> begin
            (* Printf.printf" \n the card is"; print_card  (List.hd (FArray.get colonnes n)); *)
            match List.hd (FArray.get colonnes n) with
            |e when e=dep_tr ->  let new_depot= maj_depot depot 0 
              in let col=FArray.set colonnes n (remove_from_colonne (FArray.get colonnes n)) in normalize_col col new_depot (n+1) true
            |e when e=dep_pi ->  let new_depot = maj_depot depot 1 
              in let col=FArray.set colonnes n (remove_from_colonne (FArray.get colonnes n)) in normalize_col col new_depot (n+1) true
            |e when e=dep_co ->  let new_depot = maj_depot depot 2 
              in let col=FArray.set colonnes n (remove_from_colonne (FArray.get colonnes n)) in normalize_col col new_depot (n+1) true
            |e when e=dep_ca ->  let new_depot = maj_depot depot 3 
              in let col=FArray.set colonnes n (remove_from_colonne (FArray.get colonnes n)) in normalize_col col new_depot (n+1) true
            |_->  normalize_col colonnes depot (n+1) changement
            end
    else 
      match changement with 
      | true -> normalize_col colonnes depot 0 false 
      | false -> (depot,colonnes)
    

let rec normalize_reg registres depot n  changement=
let dep_tr = ((PArray.get depot 0) + 1,Card.Trefle) in 
let dep_pi = ((PArray.get depot 1) + 1,Card.Pique)in 
let dep_co = ((PArray.get depot 2) + 1,Card.Coeur) in 
let dep_ca = ((PArray.get depot 3) + 1,Card.Carreau) in 
  if n< PArray.length registres then 
    match PArray.get registres n with 
      |e when e=dep_tr ->  let new_depot= maj_depot depot 0 in let reg= remove_from_registre registres n in normalize_reg reg new_depot (n+1) true
      |e when e=dep_pi ->  let new_depot= maj_depot depot 1 in let reg= remove_from_registre registres n in normalize_reg reg new_depot (n+1) true
      |e when e=dep_co ->  let new_depot= maj_depot depot 2 in let reg= remove_from_registre registres n in normalize_reg reg new_depot (n+1) true
      |e when e=dep_ca ->  let new_depot= maj_depot depot 3 in let reg= remove_from_registre registres n in normalize_reg reg new_depot (n+1) true
      |_ -> normalize_reg registres depot (n+1) changement
  else match changement with 
      | true -> normalize_reg registres depot 0 false  
      | false -> (depot,registres)


let rec normalize etat = 
  (* Printf.printf "Avant normalisation \n"; display_state etat  ;
  Printf.printf "Normalisation \n"; *)
  let (depot1,cols) = normalize_col etat.colonnes etat.depots 0 false in  
  let new_etat= make_etat depot1 etat.registres cols in 
  (* Printf.printf "\ncolonnes normalise\n" ; *)
  (* display_state new_etat; *)
  match etat.registres with 
  |None -> new_etat
  |Some(r) -> (let (depot2,regs)= normalize_reg r new_etat.depots 0  false 
              in  match depot2 with
                  |e when e = new_etat.depots -> new_etat
                  |_ -> normalize(make_etat depot2 (Some(regs)) new_etat.colonnes) )

  

(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)

  




(* ---------------------------------------------------------------------------------------------------------------------------------------------------------- *)
(*les fonctions verif verifie si un coup est valide selon les regles du jeux*)


let verif_FreeCell (card : Card.card) (card2 : action) etat =
  match card2 with 
  |V -> (FArray.exists( (=) []) etat.colonnes) 
  |T -> begin match etat.registres with 
         |None -> false 
         |Some(registres) -> PArray.exists (fun card -> card = carte_vide ) registres
        end 
  
  |Card(card') -> 
          if(fst card = fst (Card.of_num card') - 1) then 
          match snd card with 
          |Trefle |Pique -> not(snd (Card.of_num card') = Trefle || snd (Card.of_num card') = Pique) 
          |Coeur |Carreau -> not(snd (Card.of_num card') = Coeur || snd (Card.of_num card') = Carreau) 
      else false 
      

let verif_Seahaven (card : Card.card) (card2 : action) etat =
  
  match card2 with 
  |T -> begin match etat.registres with 
        |None -> false 
        |Some(registres)-> PArray.exists (fun card -> card = carte_vide ) registres
        end 
  |V -> (FArray.exists(fun col -> col = []) etat.colonnes)  && (fst(card)=13)
  |Card(card') -> if(fst card = fst (Card.of_num card') - 1) then (snd card) = (snd (Card.of_num card'))
                  else false 


let verif_Midnight (card : Card.card) (card2 : action) etat =
  match card2 with 
  |T | V -> false 
  |Card(card') -> if(fst card = fst (Card.of_num card') - 1)then (snd card) = (snd (Card.of_num card'))
                  else false 

let verif_Baker (card : Card.card) (card2 : action) etat=
match card2 with 
|T | V -> false 
|Card(card') -> fst card = fst (Card.of_num card') - 1


(* -----------------------------------------LECTURE DU FICHIER SOLUTION ET VERIFICATION DE LA SOLUTION ------------------------------------------------------ *)

(* extraire de la ligne du fichier solution le coup a jouer et l'executer si il est valide ,puis retourner le nouvel etat apres le coup *)
let string_to_action (c:string) = 
  match c with 
  |"T"-> T
  |"V"-> V
  | _ -> Card(int_of_string c) 

let action_string (c:action) =
  match c with
  |V -> "V"
  |T -> "T"
  |Card(a) -> Card.to_string (Card.of_num a)      

let card_on_top card etat =
  FArray.exists(fun col -> if (col=[]) then false else List.hd col = card) etat.colonnes || 
  ( match etat.registres with 
    |Some(registres) -> PArray.exists(fun reg -> reg = card) registres 
    |None -> false )

let validate_coup game (card: Card.card) (card2:action) (etat:etat) = 
  match game with 
  |Freecell -> (verif_FreeCell card card2 etat) && card_on_top card etat
  |Seahaven -> (verif_Seahaven card card2 etat) && card_on_top card etat
  |Baker -> (verif_Baker card card2 etat) && card_on_top card etat
  |Midnight -> (verif_Midnight card card2 etat) && card_on_top card etat

(* Retourne vrai si les depots sont tous completement remplis *)
let depot_full depots =
  PArray.for_all ((=) 13) depots


let treat_line line etat n_coup game=  
  let list_coups = String.split_on_char ' ' line in 
  (* List.iter ( fun coup ->  Printf.printf " %s " coup ) list_coups; *)
  let c1= int_of_string (List.nth  list_coups 0)  in  
  (* print_card (Card.of_num c1); *)
  let c2 = string_to_action (List.nth list_coups 1) in 
  if ( validate_coup game (Card.of_num c1) c2 etat) then let new_etat= coup c1 c2 etat n_coup  in normalize new_etat
  else (Printf.printf "ECHEC %d\n" (n_coup+1); exit 1)


(************************************************************************************************************
    *************************************************************************************************************************
    ****************************************************************************************************************************
    ***********************************************************************************************************************************
    ************************************************************************************************************************
    **************************************************************************************************************************
    ************************************************************************************************************************)
(*********************** JALON 2 *********************************************(* ****************************************************)
 *)(* type etat = {depots: depot;  registres: registres option  ;colonnes: colonnes} *)


    

(********************************************************************************************************
    ***************************************************************************************************************
    ****************************** STRUCTURES *********************************************************************)

type statut = |UTILE |NON_UTILE |SUCCES |INSOLUBLE |ECHEC
 type coup = card * action 

(*noeud est une structure qui contient un etat, son statut , son score(nombre de carte dans le depot),
  et l'historique des coups qui ont mené vers cet etat  *)
type noeud = { etat:etat ; mutable statut :statut ; score: int ; historique : coup List.t ;profondeur:int }

(*AFFICHAGE*************************************************************************)

let print_list_card l = Printf.printf "CARD LIST : " ;List.iter (fun card -> Printf.printf "%s " (Card.to_string card)) l
let print_list_coup l = List.iter (fun (card,action)-> Printf.printf "%s %s\n" (Card.to_string card) (action_string(action))) l
let print_list_coup_num l = List.iter (fun (card,action)-> Printf.printf "%d %s\n" (Card.to_num card) (action_string(action))) l

let display_noeud node = 
    display_state node.etat ; 
    Printf.printf "\n HISTORIQUE \n"; print_list_coup_num node.historique;
    Printf.printf "\n SCORE %d\n" node.score

let print_list_noeud  list_node =
  List.iter (fun noeud -> display_noeud noeud ) list_node
  
let display_list_noeuds list_node = 
    List.iter (fun node -> display_noeud node) list_node
(**********************************************************************************************************)


(*trier les cartes des registres*)
let sort_reg reg_array =
  match reg_array with 
  |Some (reg) -> Some(PArray.sort (fun c1 c2-> compare (Card.to_num c1) (Card.to_num c2)  ) reg)
  |None -> None

(*comapre entre deux etats*)
(*state -> state -> int*)
let compare_state noeud1 noeud2= 
  let etat1 = noeud1.etat in 
  let etat2 = noeud2.etat in 
    match (compare (sort_reg etat1.registres) (sort_reg etat2.registres) ) with 
    | 0 -> compare etat1.colonnes etat2.colonnes
    | _ -> 1

    
let count_depot etat =
 let rec sum_array depot index =
  match  index with
  | 0-> PArray.get depot 0 
  | _ -> (PArray.get depot index) + (sum_array depot (index-1))

 in sum_array etat.depots 3

(* states est un arbre de noeuds etat *)
module States = Set.Make (struct type t = noeud let compare = compare_state end)

type list_coup =  coup List.t
type  strategie = {  list_coup: list_coup;  statut: statut}

type coup_v = |ROI |RIEN |TOUT
type coup_t = |RIEN |TOUT 
type couleur = |ALTERNE |SAME |TOUT

type regle = {coup_V : coup_v ; coup_T : coup_t; coup_meme_couleur : couleur }


(**************************** GENRER LES COUPS POSSIBLES ET LES ETATS FILS ***********************************************************************************)


(*retourne la liste de toutes les cartes au sommet des colonnes *)
let rec list_sommets_col  etat_src index list= 
  if (index = FArray.length etat_src.colonnes) then (list)
  else 
    match FArray.get etat_src.colonnes index with 
    |[] -> list_sommets_col etat_src (index+1) list
    |x::l -> let sommet = x in list_sommets_col etat_src (index+1) (sommet::list)

(*retourne la liste de toute les cartes au sommet des registres*)
let list_sommets_reg etat_src = 
  match etat_src.registres with 
  |None -> []
  |Some(reg) -> begin let rec sommets_reg reg index list = 
                  if (index = PArray.length reg) then list
                  else(
                    let sommet = PArray.get reg index in 
                    match sommet with 
                    |(-1,Card.Trefle) -> sommets_reg reg (index+1) list
                    | _ -> sommets_reg reg (index+1) (sommet::list)
                  )
                in  sommets_reg reg 0 []
              end 

let rec combinaison_multi_sens carte list_carte (list_coup: coup List.t) = 
  match list_carte with
  |[]-> list_coup
  |x::list_carte' -> combinaison_multi_sens carte list_carte' ((carte,Card(Card.to_num x)) :: (x,Card(Card.to_num carte)) :: list_coup)

let rec combinaison_unisens carte list_carte list_coup = 
  match list_carte with
  |[]-> list_coup
  |x::list_carte' -> combinaison_unisens carte list_carte' ((carte,Card(Card.to_num x)):: list_coup)



(* coups carte vers carte  possibles selon l'etat actuel*)
let coup_card_card sommets_cols contenu_reg  = 
  let rec combinaison_cols list_carte (list_coups : coup List.t) =
    match list_carte with 
    |[] -> list_coups
    |x::l -> combinaison_cols l (combinaison_multi_sens x list_carte list_coups)
  in 

  let rec combinaison_regs contenu_reg list_carte (list_coups:coup List.t) =
    match contenu_reg with 
    |[] -> list_coups
    | x::l ->combinaison_regs l list_carte (combinaison_unisens x list_carte list_coups)

  in (combinaison_cols sommets_cols []) @ (combinaison_regs contenu_reg sommets_cols [])


let coup_card_card_valide etat sommets_cols contenu_reg game =
  let liste_coups = coup_card_card sommets_cols contenu_reg in
  match game with 
  |Freecell ->  List.filter (fun (card1,card2) -> verif_FreeCell card1 card2 etat) liste_coups
  |Seahaven -> List.filter (fun (card1,card2) -> verif_Seahaven card1 card2 etat) liste_coups
  |Baker -> List.filter (fun (card1,card2) -> verif_Baker card1 card2 etat) liste_coups
  |Midnight -> List.filter (fun (card1,card2) -> verif_Midnight card1 card2 etat) liste_coups

(*coups carte vers colonnes vide possibles selon regles du jeux  *)
let coup_v etat_colonnes sommets_cols contenu_reg regle =
  match regle.coup_V with 
  |RIEN -> []
  |TOUT -> let rec generer_coup_v sommets_cols list_coup =
            if( FArray.exists ( (=) [])  etat_colonnes) then 
              match sommets_cols with 
              |[]-> list_coup
              |(-1,Card.Trefle)::rest -> generer_coup_v rest list_coup
              |card::rest ->  generer_coup_v rest ((card,V)::list_coup)
           else []
          in generer_coup_v  sommets_cols [] 
  |ROI -> let rec generer_coup_v_roi sommets_cols list_coup =
            if( FArray.exists ( (=) []) etat_colonnes ) then 
              match sommets_cols with 
              |[]-> list_coup
              |(13,_) ::rest -> generer_coup_v_roi rest ((List.hd sommets_cols,V)::list_coup)
              |_::rest ->  generer_coup_v_roi rest list_coup
            else []
          in generer_coup_v_roi  sommets_cols [] 

(*coup carte vers registre vides selon regles du jeux*)
(*retourne une liste de coup du type (int,Action) List *)
let coup_t  etat_registres sommets_cols contenu_reg regle =
  match regle.coup_T with 
  |RIEN -> []
  |TOUT -> begin match etat_registres with 
          | None -> []
          |Some(registres) -> let rec generer_coup_t sommets_cols list_coup =
            if( PArray.exists ((=) (-1,Card.Trefle) ) registres ) then 
              match sommets_cols with 
              |[]-> list_coup
              |card1::rest  -> generer_coup_t rest ((card1,T)::list_coup) 
            else []
          in generer_coup_t  sommets_cols [] 
          end
      

(* Trouver tout les coups possibles valide à partir d'un etat*)
(*prend le nom du jeux et l'etat source -> genere les regles du jeux et genere les coups possible selon les regles de chaque jeux*)
let generer_coups_possibles game (etat_src : etat)  =
  let sommets_cols = list_sommets_col etat_src 0 [] in 
  (* print_list_card sommets_cols; *)
  let contenu_reg = list_sommets_reg etat_src in 
  (* print_list_card contenu_reg; *)
  let regle = match game with 
    |Freecell -> {coup_V = TOUT ; coup_T = TOUT; coup_meme_couleur = ALTERNE }
    |Seahaven -> {coup_V = ROI ; coup_T = TOUT; coup_meme_couleur = SAME }
    |Baker -> {coup_V = RIEN ; coup_T = RIEN; coup_meme_couleur = TOUT }
    |Midnight -> {coup_V = RIEN ; coup_T = RIEN; coup_meme_couleur = SAME }
  in 
  let list_card_card = coup_card_card_valide etat_src sommets_cols contenu_reg game in 
  let list_card_v = coup_v etat_src.colonnes sommets_cols contenu_reg regle in
  let list_card_t = coup_t etat_src.registres sommets_cols contenu_reg regle in 
  list_card_card @ list_card_v @ list_card_t



(* generer une liste de node fils du noeud_src en applicant sur lui à chaque fois un des coups possibles*)
let rec generer_fils (noeud_src:noeud) (list_coups:coup List.t) = 
  match list_coups with 
  |[]-> []
  |(carte,action):: l -> begin let etat_fils = normalize (coup (Card.to_num carte) action noeud_src.etat 1) in 
                        let noeud_fils = {etat=etat_fils; statut=UTILE; score= count_depot etat_fils; historique = ((carte,action)::noeud_src.historique); profondeur= noeud_src.profondeur+1}
                        in noeud_fils :: generer_fils noeud_src l 
                      end 
                      

(*trouver tout les etats atteignable depuis etat_src avec
      un coup valide et les jouter a etat_restant*)
let trouver_fils_atteignables game noeud_src = 
    let list_coups = generer_coups_possibles game noeud_src.etat  in
    generer_fils noeud_src list_coups (*liste des fils resultant pour chaque coup*)


    
(**************************** MISE A JOUR DE L'ARBRE AJOUT DES ETATS DANS L'ARBRE**********************************************)



(* ajouter un etat dans l'arbre des etats_restants en verifiant si il n'exite pas deja dans l'ensemble à 
   visiter et l'ensembles des noeuds deja visités ,  retourner le nouvel ensemble d'etats restants*) 
let ajouter_noeud  noeud etats_restants etats_visite = 
   if  ((States.mem noeud etats_restants) = false && (States.mem noeud etats_visite)= false) then States.add noeud etats_restants  
   else etats_restants 

   (*ajoute les etats contenue dans la liste list_fils dans l'arbre*)
let rec update_arbre list_fils etats_visites etats_restants=
   match list_fils with 
   | [] ->  etats_restants(* retourne le tuple mis à jour*)
   | fils :: list -> update_arbre list etats_visites (ajouter_noeud fils etats_restants etats_visites)




(******************************************* OPTIMISATION DES CHOIX DE NOEUDS A VISITER  ***********************************************************************)


(* Fonction récursive pour effectuer le parcours en largeur *)
let rec parcours_bfs config (noeud: noeud) ( etats_restants : noeud Queue.t) (etats_visites : States.t) (max_coup: int): strategie =
    match  noeud.score with 
    | 52-> {list_coup = noeud.historique ; statut = SUCCES}     (* Solution trouvée *)
    | _ ->if( (noeud.profondeur = max_coup) &&  Queue.is_empty etats_restants) then {list_coup = [] ; statut = ECHEC} (* Pas de solution trouvée *)
          else 
          ( (* Génération des états fils suivants *)
            let list_fils  = trouver_fils_atteignables config.game noeud in 
            (*si le noeud n'a aucun coup possible et qu'il ne reste aucun noeud à visiter alors la partie est insoluble *)
            match list_fils with 
            | [] -> begin if (Queue.is_empty etats_restants) then {list_coup = noeud.historique ; statut = INSOLUBLE} 
                    else let noeud_suivant =  Queue.pop etats_restants  in parcours_bfs config noeud_suivant etats_restants etats_visites max_coup
                  end 
            | _ -> 
              let list_fils = List.filter (fun fils -> not (States.mem fils etats_visites)) list_fils in
              let list_fils = List.sort (fun n1 n2 -> n1.score - n2.score) list_fils in 
              let etats_visites = List.fold_left (fun set elem -> States.add elem set) etats_visites list_fils in
              List.iter (fun fils ->Queue.push fils etats_restants) list_fils; 
              let noeud_suivant =  Queue.pop etats_restants in 
              parcours_bfs config noeud_suivant etats_restants etats_visites max_coup
          )


        




(******************************************* RECHERCHE SOLUTION ********************************************************************************)

(* Version 1 : cette version choisit à chaque fois le noeud avec le plus grand score à chaque fois sinon elle prend 
   celui avec le noeud le plus petit donc avec le moins de carte dans les colonnes et registres confondus,
   nous avons aussi implementé le parcours BFS*)
(*chercher_solution utiliste le parcours bfs  implementé plus haut, il semble y avoir un beug pour l'instant qui reste a résoudre*)
let print_arbre arbre = 
 let arbre_b = States.to_seq arbre in 
 Seq.iter (fun node -> display_noeud node) arbre_b

let add_two_best_scores list_fils etats_visites etats_restants =
  if list_fils =[] then etats_restants
  else 
    let b1 = List.fold_right ( fun noeud max_n -> if (noeud.score > max_n.score) then noeud else max_n ) list_fils (List.hd list_fils) in 
    let list_fils_r = List.filter ( (<>) b1) list_fils in 
    let etats_restants = ajouter_noeud b1 etats_restants etats_visites in 
    if list_fils_r =[] then etats_restants 
    else 
      let b2 = List.fold_right ( fun noeud max_n ->  if (noeud.score > max_n.score) then noeud else max_n ) list_fils_r (List.hd list_fils_r) in
      ajouter_noeud b2 etats_restants etats_visites

let bfs ( etats_restants :States.t) = 
    let sorted_list = List.sort (fun a b -> compare a.profondeur b.profondeur) (States.elements etats_restants)in
    let sorted_set = States.of_list sorted_list in 
    States.choose sorted_set
    
let max_score_noeud ( etats_restants :States.t) : noeud  =
  let first_element= bfs etats_restants in 
  let max_score_noeud = States.fold (fun noeud max_noeud -> if (( max_noeud.profondeur = noeud.profondeur) || ( max_noeud.score > noeud.score))  then max_noeud  else noeud ) etats_restants first_element 
  in Printf.printf" max score %d " max_score_noeud.score; max_score_noeud


  
(*creer l'arbre de possibilité et trouver une strategie gagnante  *)
let rec parcours config noeud etat_restant etat_visite max_coup  = 
  let etat_restant = (States.remove noeud etat_restant) in 
  let etat_visite = (States.add noeud etat_visite) in  
  match noeud.score with 
  | 52 -> {list_coup = noeud.historique ; statut= SUCCES} (*strategie gagnante*)
  (*si le noeud a atteint profondeur max sans trouver de solution et il n'ya plus de noeud à visiter -> echec*)
  |_ -> match noeud.profondeur with  
        | e when e =max_coup -> if (States.is_empty etat_restant) then {list_coup = noeud.historique ; statut =ECHEC} 
                      else let nouvelle_src = max_score_noeud etat_restant in
                            let nouvelle_src = max_score_noeud etat_restant in
                            Printf.printf "\n\n\n Nouvelle source est \n" ;
                            display_noeud nouvelle_src ;
                            parcours config nouvelle_src etat_restant etat_visite max_coup 

        | _ -> let list_fils = trouver_fils_atteignables config.game noeud in
          (*si le noeud n'a aucun coup possible et qu'il ne reste aucun noeud à visiter alors la partie est insoluble *)
          if ( (list_fils=[]) && (States.is_empty etat_restant)) 
            then {list_coup = noeud.historique ; statut = INSOLUBLE} 
          else  
            let etat_restant = update_arbre list_fils  etat_visite etat_restant  in 
             (*  Printf.printf"\n*********************************Voici l'arbre \n";
              print_arbre etat_restant;    *)
            Printf.printf"\n***************************************************** \n";
            let nouvelle_src = max_score_noeud etat_restant in
            Printf.printf "\n\n\n Nouvelle source est \n" ;
            display_noeud nouvelle_src ;
            parcours config nouvelle_src etat_restant etat_visite max_coup 


(* initialise l'arbre et lance le parcours bfs*)
let chercher_solution_bfs config (etat_src:etat) (max_coup:int) = 
  let noeud_src = {etat = normalize etat_src ; 
  statut = UTILE ; 
  score = count_depot etat_src;
  historique = [];
  profondeur=0;
  }  in display_state noeud_src.etat ;
  let etat_restant = Queue.create () in 
  let etat_visite = States.empty  in 
  let strategie = parcours_bfs config noeud_src etat_restant etat_visite max_coup 
  in strategie 


let chercher_solution config (etat_src:etat) (max_coup:int) = 
    let noeud_src = {etat = normalize etat_src ; 
    statut = UTILE ; 
    score = count_depot etat_src;
    historique = [];
    profondeur=0;
    }  in display_state noeud_src.etat ;
    let etat_restant = States.singleton noeud_src  in 
    let etat_visite = States.empty in 
    let strategie =  parcours config noeud_src etat_restant etat_visite max_coup
    in strategie 

  (* let list_coup = generer_coups_possibles config.game etat_src
  in Printf.printf"\n COOUP POSSIBLES \n"; print_list_coup_num list_coup;  *)
  
let statut_to_string statut =
  match statut with 
  |UTILE -> "UTILE"
  |NON_UTILE -> "NON_UTILE"
  |SUCCES -> "SUCCES"
  |INSOLUBLE ->"INSOLUBLE"
  |ECHEC ->"ECHEC"


let read_solution_file conf etat  = match conf.mode with
|Search(sol)-> let strat = chercher_solution conf etat 5 in Printf.printf "%s" (statut_to_string strat.statut)
|Check(sol) -> 
  (*ovrir le fichier solution*)
  let myfile= open_in sol in
  (* lire les lignes du fichier en incrementant a chaque fois le nombre de coups *)
  let rec readline n_coup etat conf=
    (* afficher l'etat actuel du jeux apres chaque coup  *)
    (* let curent_state = display_state etat in  *)
    let etat_normalise = normalize etat in
    (* recuperer la ligne pour executer les coups jusqu'à atteindre la fin du fichier tester si la partie est gagnée ou pas*)
    try let line = input_line myfile in 
    let new_etat = treat_line line etat_normalise  n_coup conf.game in readline (n_coup+1) new_etat conf 
    with End_of_file -> if (depot_full etat.depots) then (Printf.printf "SUCCES\n"; exit 0 )
                             else (Printf.printf "ECHEC %d\n" (n_coup+1); exit 1)
    in readline 0 etat conf
             
  


let treat_game conf =

  let permut = XpatRandom.shuffle conf.seed in
  (* Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed; *)
  (* List.iter (fun n -> Printf.printf "%s "( Card.to_string (Card.of_num n)))permut;
  print_newline (); *)
  let etat = init_game conf.game permut in 
  read_solution_file conf etat; 
  exit 0


 

let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)

    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config
 



 let _ = if not !Sys.interactive then main () else () 
 