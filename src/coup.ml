open XpatSolver

(*les fonctions verif verifie si une carte peut être ajouté dans une colonne :*)
let verif_FreeCell card colonne=
    if(fst card = List.hd colonne + 1)
    then 
        match snd card with 
        |Trefle | Pique -> if(List.hd colonne = Trefle || List.hd colonne = Pique) then false else true
        |Coeur |Carreau -> if(List.hd colonne = Coeur || List.hd colonne = Carreau) then false else true
    else false 
;;

let verif_Seahaven card colonne=
    if(fst card = List.hd colonne + 1)
    then 
        match snd card with 
        |Trefle -> if(List.hd colonne = Trefle) then true else false
        |Pique -> if(List.hd colonne = Pique) then true else false
        |Coeur -> if(List.hd colonne = Coeur) then true else false
        |Carreau -> if(List.hd colonne = Carreau) then true else false
    else false 
;;

let verif_Midnight card colonne=
    verif_Seahaven card colonne
;;

let verif_Baker card colonne=
    fst card = List.hd colonne + 1
;;

(*ajouter/supprimer une carte dans une colonnes :*)
let ajouter_carte_colonne card colonne mode=
    match mode with 
    |Freecell-> if verif_FreeCell card colonne then card :: colonne else "ajout carte dans la colonne impossible"
    |Seahaven-> if verif_Seahaven card colonne then card :: colonne else "ajout carte dans la colonne impossible"
    |Midnight-> if verif_Midnight card colonne then card :: colonne else "ajout carte dans la colonne impossible"
    |Baker -> if verif_Baker card colonne then card :: colonne else failwith "ajout carte dans la colonne impossible"
;;

let supprimer_carte_colonne card colonne=
    match colonne with 
    |[] -> failwith "colonne vide"
    |x :: colonne' -> colonne'
;;

(*pour les registres temporaires :*)
let ajouter_carte_registre card registres num_registre=
    match registres.(num_registre) with 
    |(0,0) -> PArray.set registres num_registre card
    |_ -> failwith "coup impossible"
;;

let supprimer_carte_registre registres num_registre=
    PArray.set registres num_registre (-1,Card.Trefle)
;;


(*-------------------------------------------------*)
let ajouter_registre_vide card registres= 
    (*retourne la position du registre vide si il existe; None sinon*)
    let rec chercher_registre_vide card registres num_registre nbr_registre= 
        if num_registre > nbr_registre 
        then None
        else if registres.(i) = (-1,Card.Trefle)
            then Some(i)
            else chercher_registre_vide card registres (num_registre + 1) nbr_registre
    in 
    match chercher_registre_vide card registres 0 ((PArray.length registres) - 1) with 
    |None -> registres (*pas de registre vide*)
    |Some i -> PArray.set registres i card 
;;

let ajouter_colonne_vide card colonnes= 
    (*retourne la position de la colonne vide si elle existe; None sinon*)
    let rec chercher_colonne_vide card colonnes num_colonne nbr_colonne= 
        if num_colonne > nbr_colonne
        then None
        else if (get colonnes i) = []
            then Some(i)
            else chercher_colonne_vide card colonnes (num_colonne + 1) nbr_colonne
    in 
    match chercher_colonne_vide card colonnes 0 ((FArray.length registres) - 1) with 
    |None -> colonnes (*pas de colonne vide*)
    |Some i -> PArrat.set colonnes i [card] 
;;


let coup_carte_vers_carte c1 c2 colonnes= 
    (*retourne le numero de la colonne qui a card a sa tête, retourne None si elle n'existe pas*)
    let rec chercher_colonne card colonnes num_colonne nbr_colonne= 
    if num_colonne > nbr_colonne
    then None
    else if (get colonnes i).hd = card
        then Some(i)
        else chercher_colonne_vide card colonnes (num_colonne + 1) nbr_colonne
    in 
    match (chercher_colonne card colonnes 0 ((FArray.length registres) - 1)) with 
    |None -> colonnes (*pas de colonne avec card en tête*)
    |Some i -> PArrat.set colonnes i (card :: colonnes.(i)) 
;;


(*----------------------------------------------*)

let coup c1 c2 etat= 
    match c2 with
    |Card(x) -> etat.colonnes <- (coup_carte_vers_carte c1 c2 etat.colonnes); etat
    |V -> etat.colonnes <- (ajouter_registre_vide c1 etat.colonnes); etat
    |T -> etat.registres <- (ajouter_registre_vide c1 etat.registres); etat
;;
