open XpatLib
open Card

type game = Freecell | Seahaven | Midnight | Baker

type init_rule = {nb_col:int; nb_depots:int; nb_reg:int}
type card = Card.card

(* une colonne est une liste de cartes *)
type colonne = Card.card List.t

(* les 4 depots est un tableau d'entier ou chaque case represente une couleur et contient le numero de carte dedans *)
type depot= int PArray.t (* case 0:Trefle ; case 1:Pique ;case 2:Coeur ; case 3:Carreau *)

(* Les registres sont representé par un tableau ou chaque case represente un registre qui contient une carte ou vide*)
type registres = Card.card option PArray.t

(*l'etat du jeux contient les depots actuels , les registres actuels et la liste de toute les colonnes*)
type etat = {depots: depot; registres: registres option  ;colonnes: colonne FArray.t}

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
  |Seahaven-> {nb_col=10;nb_depots=4;nb_reg=4}
  |Midnight-> {nb_col=18;nb_depots=4;nb_reg=0}
  |Baker -> {nb_col=13;nb_depots=4;nb_reg=0}

let init_game name = match name with 
  |Freecell-> { 
      depots=PArray.make 4 0;
      registres= Some (PArray.make 4 None);
      colonnes=FArray.make 8 []} 
  |Seahaven->{ 
      depots=PArray.make 4 0;
      registres= Some (PArray.make 4 None);
      colonnes=FArray.make 10 []} 
  |Baker->{ 
      depots=PArray.make 4 0;
      registres=None;
      colonnes=FArray.make 13 []} 
  |Midnight->{ 
      depots=PArray.make 4 0;
      registres=None;
      colonnes=FArray.make 18 []} 
  

(*fonction qui remplie une colonne avec n cartes*)
let rec fill_col col cards n=
  if n>0 then 
    match (col,cards) with 
    |([],x::l)-> fill_col [x] l (n-1)
    |(_,x1::l) -> fill_col (x1::col) l (n-1)
    |(_,[])-> col
  else col 

(* fonction qui remplie n registres par une carte chacun *)
let rec fill_reg n registres cards= 
  if n=0 then Some(registres)
  else match cards with 
  |[]-> failwith "plus de cartes"
  |x::l-> match registres with
          |Some reg ->fill_reg  (n-1) (PArray.set reg (n-1) Some(x)) l 
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
    |_ -> fill_all_col (n-1) (FArray.set colonnes (n-1) (fil_col [] permut 3)) (List.filteri (fun i a -> i>2) permut)
  in 
{depots = depots; registres = None; colonnes = fill_all_col 18 colonnes permut}  

(* distribution des cartes selon les regles de Baker dozen*)
let distribution_bk etat permut =
  let {depots;registres;colonnes}= etat in
  let rec fill_all_col n colonnes permut =
    match n with 
    | e when e=0 -> colonnes 
    |_ -> fill_all_col (n-1) (FArray.set colonnes (n-1) (fil_col [] permut 5)) (List.filteri (fun i a -> i>2) permut)
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
let init_distribution etat name permut =  
  match name with 
  |Freecell-> distribution_fc etat permut
  |Seahaven-> distribution_sv etat permut 
  |Midnight-> distribution_Md etat permut 
  |Baker-> distribution_bk etat permut  







(* TODO : La fonction suivante est à adapter et continuer *)

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut;

  print_newline ();
  let new_etat = init_distribution init_game conf.game permut;

  (*print_string "C'est tout pour l'instant. TODO: continuer...\n";
  let cards=List.map (fun a->Card.of_num a) permut in 
  print_colonnes (distribution_fc (init_game config.game ) cards);
   *)
  exit 0;



let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),"<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config
;;

let _ = if not !Sys.interactive then main () else ();;



