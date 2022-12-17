open Fifo
open Printf

let randmax = 1_000_000_000

let reduce n limit =
  Int.(of_float (to_float n /. to_float randmax *. to_float limit))

let init n =
  (*initalier les premiers composants des paires*)
  let init_first_composant list =
    let rec  init_first_composant_aux list pre index=
      match list with 
      |[] -> [] 
      |(x,y) :: list' -> 
          let x' = 
            if index = 0 then 0
            else (pre + 21) mod 55
          in (x',y) :: init_first_composant_aux list' x' (index + 1)
    in init_first_composant_aux list 0 0
  in 
  (*a initalier les seconds composants des paires*)
  let init_second_composant list graine= 
    let rec  init_second_composant_aux list graine a b index=
      match list with 
      |[] -> [] 
      |(x,y) :: list' -> 
          let y' = 
            if index = 0 then graine 
            else if index = 1 then 1
            else if b <= a then a - b
            else a - b + randmax
          in (x,y') :: init_second_composant_aux list' graine b y' (index + 1)
               
    in init_second_composant_aux list graine 0 0 0 
  in
  init_second_composant (init_first_composant (List.init 55 (fun _ -> (0,0)))) n

let rec quicksort_tuple l =
  (*renvoie une liste qui supprime les element plus grand strictement que k*)
  let rec delete_higher l k = 
    match l with 
    |[] ->[]
    |(x,y) :: l' -> if x <= k then (x,y) :: delete_higher l' k else delete_higher l' k
  in
  (*renvoie une liste qui supprime les element plus pletit que k*)
  let rec delete_smaller l k = 
    match l with 
    |[] ->[]
    |(x,y) :: l' -> if x > k then (x,y) :: delete_smaller l' k else delete_smaller l' k
  in
  match l with 
  |[] -> []
  |(x,y) :: l' -> (quicksort_tuple (delete_higher l' x)) @ [(x,y)] @ (quicksort_tuple (delete_smaller l' x)) 

let get_sublist_of_firstcomponent l debut fin= 
  let rec aux l debut fin index=
    match l with 
    |[] -> []
    |(x,y) :: l' -> 
        if debut > index
        then aux l' debut fin (index + 1)
        else if fin >= index
        then y :: (aux l' debut fin (index + 1))
        else []
  in aux l debut fin 1

let f_init l debut fin=  
  Fifo.of_list (get_sublist_of_firstcomponent (quicksort_tuple l) debut fin)

let tirage f1 f2= 
  let (n1, newf1) = Fifo.pop f1 in
  let (n2, newf2) = Fifo.pop f2 in
  let d = if n1 > n2 then n1 - n2 else n1 - n2 + randmax in
  (Fifo.push n2 newf1, Fifo.push d newf2, d)

let tirage_k k f1 f2= 
  let rec tirage_k_aux k f1 f2 res=
    if k = 0
      then 
        (f1,f2,res) 
      else
        let (newf1, newf2, d) = tirage f1 f2 in 
        tirage_k_aux (k-1) newf1 newf2 (Fifo.push d res)
  in
  tirage_k_aux k f1 f2 Fifo.empty

let reduce_list l limit=
  List.mapi (fun index e -> reduce e (limit - index)) l

let permutation l= 
    let rec permutation_aux l l_ensemble= 
    match l with 
    |[] -> [] 
    |e :: l' -> 
      let nbr = (List.nth l_ensemble e) in 
      List.append (permutation_aux l' (List.filter (fun x -> x <> nbr) l_ensemble)) [nbr]
  in
  permutation_aux l (List.init 52 (fun index -> index))

(*---------------------------------------------------*)

let shuffle n =
  (*a : creation d'une liste des paires*)
  let paires = init n in
  (*b : creation des FIFO initiales*)
  let f1_init = f_init paires 1 24 
  and f2_init = f_init paires 25 55 in
  (*c,d : 165 tirages d'initialisation*)
  let (f1_165, f2_165, ignored_165) = tirage_k 165 f2_init f1_init in
  (*e*)
  (*52 tirages suivants*)
  let (_ , _, tirages_52) = tirage_k 52 f1_165 f2_165 in
  (*reduire la liste des 52 tirages*)
  let reduced_52 = reduce_list (Fifo.to_list tirages_52) 52 in
  (*permutation de la liste des 52 tirages reduite*)
  permutation reduced_52
;; 


(* 
let shuffle_test = function
  | 1 ->
     [13;32;33;35;30;46;7;29;9;48;38;36;51;41;26;20;23;43;27;
      42;4;21;37;39;2;15;34;28;25;17;16;18;31;3;0;10;50;49;
      14;6;24;1;22;5;40;44;11;8;45;19;12;47]
  | 12 ->
     [44;9;28;35;8;5;3;4;11;25;43;2;27;1;24;40;17;41;47;18;
      10;34;39;7;36;29;15;19;30;37;48;45;0;21;12;46;22;13;16;
      33;31;38;23;6;14;49;26;50;20;32;42;51]
  | 123 ->
     [16;51;44;27;11;37;33;50;48;13;17;38;7;28;39;15;4;5;3;6;
      42;25;19;34;20;49;23;0;8;26;30;29;47;36;9;24;40;45;14;
      22;32;10;1;18;12;31;35;2;21;43;46;41]
  | 1234 ->
     [36;37;44;26;9;10;23;30;29;18;4;35;15;50;33;43;28;2;45;
      6;3;31;27;20;7;51;39;5;14;8;38;17;49;0;40;42;13;19;34;
      1;46;22;25;24;12;48;16;21;32;11;41;47]
  | 12345 ->
     [10;12;6;23;50;29;28;24;7;37;49;32;38;30;31;18;13;2;15;4;
      5;47;16;1;0;35;43;40;42;44;46;39;48;20;36;34;8;14;33;11;
      25;45;41;19;3;17;21;51;26;22;27;9]
  | 123456 ->
     [1;7;39;47;5;15;50;49;37;44;29;10;4;23;17;20;0;11;24;14;
      28;35;3;48;8;41;19;46;13;12;36;34;27;9;33;22;43;32;25;30;
      38;6;31;16;51;21;26;18;45;40;42;2]
  | 1234567 ->
     [19;17;31;6;4;14;9;36;35;30;39;40;50;48;42;37;12;3;25;1;
      43;27;5;20;10;51;11;44;46;38;16;22;26;23;21;28;15;7;47;
      13;18;29;32;0;49;34;8;45;24;33;2;41]
  | 22222 ->
     [43;17;21;40;42;47;0;35;23;18;11;29;41;10;45;7;15;25;13;
      51;6;12;33;24;8;34;50;2;30;28;37;3;4;39;49;31;32;14;44;
      22;46;48;9;1;36;5;27;26;38;20;16;19]
  | 222222 ->
     [42;48;16;9;22;21;45;12;40;44;29;31;24;27;33;38;14;15;49;
      37;0;26;10;1;47;4;50;34;23;8;3;2;19;32;13;43;51;6;39;35;
      18;30;11;7;46;17;20;5;41;36;25;28]
  | 2222222 ->
     [17;45;5;4;33;23;10;42;39;3;24;46;6;29;44;27;0;43;2;7;20;
      14;34;8;11;18;15;28;25;49;40;47;48;21;41;9;31;30;36;12;
      51;1;35;26;50;38;32;19;13;37;22;16]
  | 999_999_999 ->
     [22;1;0;21;20;44;23;43;38;11;4;2;19;27;36;9;49;7;18;14;
      46;10;25;35;39;48;51;40;33;13;42;16;32;50;24;47;26;6;34;
      45;5;3;41;15;12;31;17;28;8;29;30;37]
  | _ -> failwith "shuffle : unsupported number (TODO)"
;; 
*)