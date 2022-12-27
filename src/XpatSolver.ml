open XpatLib


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
  make_etat nv_depots etat.registres etat.colonnes

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
  FArray.iter (fun col ->Printf.printf "COL : "; List.iter (fun card -> Printf.printf "%s " (Card.to_string card)) col;Printf.printf "\n" ) colonnes_tab

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
  |V -> (FArray.exists(fun col -> col = []) etat.colonnes) 
  |T -> begin match etat.registres with 
         |None -> false 
         |Some(registres) -> PArray.exists (fun card -> card = carte_vide ) registres
        end 
  
  |Card(card') -> print_card card ;print_card (Card.of_num card');
          if(fst card = fst (Card.of_num card') - 1) then 
          match snd card with 
          |Trefle |Pique -> if(snd (Card.of_num card') = Trefle || snd (Card.of_num card') = Pique) then false else true
          |Coeur |Carreau -> if(snd (Card.of_num card') = Coeur || snd (Card.of_num card') = Carreau) then false else true
      else false 
      

let verif_Seahaven (card : Card.card) (card2 : action) etat =
  
  match card2 with 
  |T -> begin match etat.registres with 
        |None -> false 
        |Some(registres)-> PArray.exists (fun card -> card = carte_vide ) registres
        end 
  |V -> (FArray.exists(fun col -> col = []) etat.colonnes)  && (fst(card)=13)
  |Card(card') -> if(fst card = fst (Card.of_num card') - 1) then 
        match snd card with 
        |Trefle -> if(snd (Card.of_num card') = Trefle) then true else false
        |Pique -> if(snd (Card.of_num card') = Pique) then true else false
        |Coeur -> if(snd (Card.of_num card') = Coeur) then true else false
        |Carreau -> if(snd (Card.of_num card') = Carreau) then true else false
    else false 


let verif_Midnight (card : Card.card) (card2 : action) etat =
  match card2 with 
  |T | V -> false 
  |Card(card') -> print_card (Card.of_num card'); if(fst card = fst (Card.of_num card') - 1)then 
        match snd card with 
        |Trefle -> if(snd (Card.of_num card') = Trefle) then true else false
        |Pique -> if(snd (Card.of_num card') = Pique) then true else false
        |Coeur -> if(snd (Card.of_num card') = Coeur) then true else false
        |Carreau -> if(snd (Card.of_num card') = Carreau) then true else false
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
  PArray.for_all (fun n_card -> n_card=13) depots


let treat_line line etat n_coup game=  
  let list_coups = String.split_on_char ' ' line in 
  (* List.iter ( fun coup ->  Printf.printf " %s " coup ) list_coups; *)
  let c1= int_of_string (List.nth  list_coups 0)  in  
  (* print_card (Card.of_num c1); *)
  let c2 = string_to_action (List.nth list_coups 1) in 
  if ( validate_coup game (Card.of_num c1) c2 etat) then let new_etat= coup c1 c2 etat n_coup  in normalize new_etat
  else (Printf.printf "ECHEC %d\n" (n_coup+1); exit 1)



(* lire un fichier solution ligne par ligne *)
let read_solution_file conf etat  = match conf.mode with
|Search(sol)-> Printf.printf "Jalon 2" ;exit 0
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
  Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> Printf.printf "%s "( Card.to_string (Card.of_num n)))permut;
  print_newline ();
  let etat = init_game conf.game permut in 
  read_solution_file conf etat

 

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
 