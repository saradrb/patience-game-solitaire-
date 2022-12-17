(** In Xpat2, the index of the game is a seed used to shuffle
    pseudo-randomly the cards.
    The shuffle function emulates this permutation generator.
    The input number is the seed (in 1..999_999_999).
    The output list is of size 52, and contains all numbers in 0..51
    (hence without duplicates).
*)

(*a*) 
(*initaliser les paires*)
(**)
let init n

(*b*)
(*trie une liste de tuple par ordre croissant selon leurs premières composantes*)
(**)
let rec quicksort_tuple l 
(*retourne une liste qui contient les premier composant des elements qui sont situé dans ou entre debut et fin de la liste en paramètre*)
(**)
let get_sublist_of_firstcomponent l debut fin
(*retourne une Fifo qui contient les secondes composante des elements entre debut et fin d'une liste après son tri*)
(**)
let f_init l debut fin

(*c*)
(*tirage : renvois un triplé : (Fifo 1, Fifo2, resultat du tirage)*)
(**)
let tirage f1 f2

(*d*) 
(*faire k tirages*)
(**)
let tirage_k k f1 f2

(*e*)
(*applique sur chaque element d'une liste : reduce (limit - position de l'element) *)
(**)
let reduce_list l limit
(*donne une permutation d'une liste*)
(**)
let permutation l

(*------------*)
(**)
val shuffle : int -> int list
