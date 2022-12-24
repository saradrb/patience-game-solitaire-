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