open XpatLib


type game = Freecell | Seahaven | Midnight | Baker

type init_rule = {nb_col:int; nb_depots:int; nb_reg:int}

type card = Card.card

(* une colonne est une liste de cartes *)
type colonne = Card.card List.t

(* un depeau est un tableau d'entier ou chaque case represente une couleur et contient le numero de carte dedans *)
type depot= int PArray.t (* case 0:Trefle ; case 1:Pique ;case 2:Coeur ; case 3:Carreau *)

(* registres est un tableau ou chaque case represente un registre qui contient une carte ou vide*)
type registres = Card.card PArray.t

type colonnes = colonne FArray.t


(*l'etat du jeux contient les depots actuels , les registres actuels et la liste de toute les colonnes*)
type etat = {depots: depot; registres: registres option ; colonnes: colonnes}

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
let config = { game = Freecell; seed = 1; mode = Search "" }

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


let get_rule name = function 
  |Freecell-> {nb_col=8;nb_depots=4;nb_reg=4}
  |Seahaven->{nb_col=10;nb_depots=4;nb_reg=4}
  |Midnight->{nb_col=18;nb_depots=4;nb_reg=0}
  |Baker ->{nb_col=13;nb_depots=4;nb_reg=0}
(* let get_rule name = function 
|Freecell-> {nb_col=8;nb_depots=4;nb_reg=4}
|Seahaven->{nb_col=10;nb_depots=4;nb_reg=4}
|Midnight->{nb_col=18;nb_depots=4;nb_reg=0}
|Baker ->{nb_col=13;nb_depots=4;nb_reg=0} *)


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
  in 
  {depots = depots; registres = fill_reg 2 registres permut; colonnes = fill_all_col 10 colonnes (List.filteri (fun i a -> i>1) permut)}  


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
    |_ -> fill_all_col (n-1) (FArray.set colonnes (n-1) (fill_col [] permut 5)) (List.filteri (fun i a -> i>2) permut)

  in 
{depots = depots; registres = None; colonnes = fill_all_col 18 colonnes permut}  


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


(*Affichage des colonnes *)
let print_col list_card =
  List.iter (fun card -> Printf.printf "%s " (Card.to_string card)) list_card

let print_colonnes colonnes_tab = 
  FArray.iter (fun col ->Printf.printf "COL: "; List.iter (fun card -> Printf.printf "%s " (Card.to_string card)) col;Printf.printf "\n" ) colonnes_tab

(*initialistation du jeux et distribution des cartes*)
(* let init_distribution etat name permut =  
  let cards = List.map (fun a -> Card.of_num a) permut in 
  match name with 
  |Freecell-> distribution_fc etat cards
  |Seahaven-> distribution_sv etat cards 
  |Midnight-> distribution_md etat cards 
  |Baker-> distribution_bk etat cards   *)


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
      colonnes=FArray.make 8 []} cards
  
  |Baker-> distribution_bk { 
      depots=PArray.make 4 0;
      registres=None;
      colonnes=FArray.make 13 []} cards
  |Midnight-> distribution_md { 
      depots=PArray.make 4 0;
      registres=None;
      colonnes=FArray.make 18 []} cards
    
(* TODO : La fonction suivante est à adapter et continuer *)

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut;
  print_newline ();
  (* print_string "C'est tout pour l'instant. TODO: continuer...\n";  *)
  let etat=init_game conf.game permut in
  print_colonnes etat.colonnes; 
  exit 0



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



(*les fonctions verif verifie si une carte peut être ajouté dans une colonne :*)
let verif_FreeCell (card : Card.card) (colonne : colonne)=
    if(fst card = fst (List.hd colonne) + 1)
    then 
        match snd card with 
        |Trefle |Pique -> if(snd (List.hd colonne) = Trefle || snd (List.hd colonne) = Pique) then false else true
        |Coeur |Carreau -> if(snd (List.hd colonne) = Coeur || snd (List.hd colonne) = Carreau) then false else true
    else false 
;;

let verif_Seahaven (card : Card.card) (colonne : colonne)=
    if(fst card = fst (List.hd colonne) + 1)
    then 
        match snd card with 
        |Trefle -> if(snd (List.hd colonne) = Trefle) then true else false
        |Pique -> if(snd (List.hd colonne) = Pique) then true else false
        |Coeur -> if(snd (List.hd colonne) = Coeur) then true else false
        |Carreau -> if(snd (List.hd colonne) = Carreau) then true else false
    else false 
;;

let verif_Midnight (card : Card.card) (colonne : colonne)=
    verif_Seahaven card colonne
;;

let verif_Baker (card : Card.card) (colonne : colonne)=
    fst card = fst (List.hd colonne) + 1
;;

(*ajouter/supprimer une carte dans une colonnes :*)
let ajouter_carte_colonne (card : Card.card) (colonne : colonne) (mode : game)=
    match mode with 
    |Freecell-> if verif_FreeCell card colonne then card :: colonne else failwith "ajout carte dans la colonne impossible"
    |Seahaven-> if verif_Seahaven card colonne then card :: colonne else failwith "ajout carte dans la colonne impossible"
    |Midnight-> if verif_Midnight card colonne then card :: colonne else failwith "ajout carte dans la colonne impossible"
    |Baker -> if verif_Baker card colonne then card :: colonne else failwith "ajout carte dans la colonne impossible"
;;

let supprimer_carte_colonne (colonne : colonne)=
    match colonne with 
    |[] -> failwith "colonne vide"
    |x :: colonne' -> colonne'
;;

(*pour les registres temporaires :*)
let ajouter_carte_registre (card : Card.card) (registres : registres) (num_registre : int)=
    match (PArray.get registres num_registre) with 
    |(-1,Card.Trefle) -> PArray.set registres num_registre card
    |_ -> failwith "coup impossible"
;;

let supprimer_carte_registre (registres : registres) (num_registre : int)=
    PArray.set registres num_registre (-1,Card.Trefle)
;;


(*-------------------------------------------------*)
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
    |None -> registres (*pas de registre vide*)
    |Some i -> PArray.set registres i card 
;;

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
    |None -> colonnes (*pas de colonne vide*)
    |Some i -> FArray.set colonnes i [card] 
;;


let coup_carte_vers_carte c1 c2 (colonnes : colonnes)= 
    (*retourne le numero de la colonne qui a card a son sommet (en tête), retourne None si elle n'existe pas*)
    let rec chercher_colonne (card : Card.card) (colonnes : colonnes) (num_colonne : int) (nbr_colonne : int)= 
    if num_colonne > nbr_colonne
    then None
    else if List.hd (FArray.get colonnes num_colonne) = card
        then Some(num_colonne)
        else chercher_colonne card colonnes (num_colonne + 1) nbr_colonne
    in 
    match (chercher_colonne c2 colonnes 0 ((FArray.length colonnes) - 1)) with 
    |None -> colonnes (*pas de colonne avec comme sommet la carte c2 *)
    |Some i -> FArray.set colonnes i (c1 :: (FArray.get colonnes i)) 
;;


(*----------------------------------------------*)
(*le type action définie le second mot d'un coup*)
type action = 
  |Card of int
  |V
  |T 

let coup (c1 : int) (c2 : action) (etat : etat)= 
    match c2 with
    |Card(carte) -> set_colonnes etat (coup_carte_vers_carte (Card.of_num c1) (Card.of_num carte) etat.colonnes)
    |V -> set_colonnes etat (ajouter_colonne_vide (Card.of_num c1) etat.colonnes)
    |T -> match etat.registres with 
          |None -> etat
          |Some(r) -> set_registres etat (Some(ajouter_registre_vide (Card.of_num c1) r))
;;


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
  
