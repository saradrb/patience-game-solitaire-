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
    PArray.set registres num_registre (0,0)
;;

let coup carte_depart= 

;;
