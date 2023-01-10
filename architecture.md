LES STRUCTURES CREE : 
---------------------

type game = Freecell | Seahaven | Midnight | Baker
- le type game : représente les différents mode de jeu


type colonne = Card.card List.t
- le type colonne : représente une liste de carte.


type colonnes = colonne FArray.t
- le type colonnes : représent un FArray de colonne, ce type nous permet de stocker les colonnes. 


type depot= int PArray.t
- le type depot : représent un PArray de int, ce type nous permet de stocker les differents depôt, 
    le premier élément du tableau représente le depôt des cartes de type Trefle
    le deuxième élément du tableau représente le depôt des cartes de type Pique
    le deuxième élément du tableau représente le depôt des cartes de type Coeur
    le deuxième élément du tableau représente le depôt des cartes de type Carreau
la valeur de chaque élément représente le nombre de carte dans le depôt.


type registres = Card.card PArray.t
- le type registre : représent un PArray de carte, ce type représente les registre temporaire,
chaque élément du PArray contient une carte ou est vide 
une carte vide est représenté par la carte : (-1,Card.Trefle) (ce choix au lieu de representer le tableau avec un option Card est du a des raisons technique utilitaire)


type etat = {depots: depot;  registres: registres option  ;colonnes: colonnes}
- le type etat : représente une structure de 3 élément, ce type permet de représenter l'etat actuel du jeu et de ces différentes structures.
Cette structure contient : 
    - Les depots : représenté par le type depot. 
    - Les registres temporaires : représenté par le type register option, si le jeu ne contient pas de registre temporarire, cette élément sera égale a None.
    - Les colonnes du jeux : représenté par le type colonnes.


type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)
- Le type mode : est une enumeration qui contient 
    -Check qui contient un string qui represente le nom d'un fichier a verifier
    -Search qui contient un string qui represente le nom d'un fichier ou on écrira la solution


type config = {mutable game : game; mutable seed: int; mutable mode: mode }
- le type config : représente une structure qui contient trois éléments :
    -game : représente le mode de jeu.
    -seed : représente le 
    -mode : représenté le 
les différents champs de cette structures sont mis on mutable afin de pouvoir les modifier au cours du programme


LES FONCTION CREE : 
---------------------

-FONCTION DE CREATION ET MODIFICATION D'UN ETAT : 

make_etat (depots : depot) (registres : registres option) (colonnes : colonnes) : depot -> registres option -> colonnes -> etat
- cette fonction permet de créer un état contenant les trois élément mis en paramètre


let set_depots (etat : etat) (nv_depots : depot)= depot -> etat
- cette fonction permet de modifier le depot d'un etat, elle renvoie ainsi le nouvelle etat avec le nouveau depot mis en paramètre.


let set_registres (etat : etat) (nv_registres : registres option)= registres option -> etat
- cette fonction permet de modifier les registres d'un etat, elle renvoie ainsi le nouvelle etat avec le nouveau tableau de registres mis en paramètre.


let set_colonnes (etat : etat) (nv_colonnes : colonnes)= colonnes -> etat
- cette fonction permet de modifier les colonnes d'un etat, elle renvoie ainsi le nouvelle etat avec le nouveau tableau de colonnes mis en paramètre.


-------------------
let getgame = string -> game
- cette fonction renvoie le mode de jeu 


let rec fill_col col cards n :  int -> 'a PArray.t option -> 'a list -> 'a PArray.t option 
- on utilise cette fonction pour remplie une colonne avec un certain nombre de cartes a partir d'une liste de carte donnée.
Cette fonction est récursive et prend en entrée 3 paramètres:
    -col : La colonne à remplir
    -cards : La liste de cartes à utiliser pour remplir la colonne.
    -n : Le nombre de cartes à ajouter à la colonne.
La fonction vérifie d'abord si le nombre de cartes à ajouter à la colonne est supérieur à 0, si c'est le cas, elle utilise un match pour vérifier la combinaison des entrées. Si la colonne est vide et il y a encore des cartes dans la liste, elle appelle la récursion avec la colonne remplie avec la première carte de la liste, la liste de cartes sans la première carte et n diminué de 1. Si la colonne n'est pas vide et il y a encore des cartes dans la liste, elle appelle la récursion avec la colonne remplie avec la première carte de la liste ajoutée à la colonne et la liste de cartes sans la première carte et n diminué de 1. Si la liste est vide ou si n=0 elle retourne la colonne.
Ainsi cette fonction permet de remplir une colonne avec les n premières cartes de la liste, en utilisant une récursion.


let rec fill_reg n registres cards : int -> 'a PArray.t option -> 'a list -> 'a PArray.t option 
- cette fonction est utilisée pour remplir un certain nombre de registres avec des cartes d'une liste donnée. Cette fonction est récursive et prend en entrée 3 paramètres:
    - n : Le nombre de registres à remplir
    - registres : les registres, un tableau de carte (ou bien None)
    - cards : La liste de cartes à utiliser pour remplir les registres.
La fonction vérifie d'abord si le nombre de registres à remplir est égal à 0, si c'est le cas, elle renvoie les registres sans modification.
Sinon, elle utilise un match pour vérifier si il y a encore des cartes dans la liste cards, s'il n'y en a plus elle lève une exception avec failwith, sinon, elle utilise un autre match pour vérifier si les registres sont définit (si registres egale a None). Si les registres sont définis, elle utilise la fonction PArray.set pour remplacer la dernière carte de l'array de registre à l'indice n-1 avec la première carte de la liste cards et passe à l'appel récursif en diminuant n de 1 et en passant la nouvelle option d'array de registre et la liste de cartes sans la première carte. Si les registres ne sont pas définis, elle retourne None.
Ainsi cette fonction permet de remplir les n premiers registres de l'array avec les n premières cartes de la liste, en utilisant une récursion.


FONCTION DE DISTRIBUTION DE CARTE 
---------------------------------

let distribution_sv etat permut =
- cette fonction permet d'effectuer la distribution des cartes selon les regles de SeaHeaven  

let distribution_md etat permut =
- cette fonction permet d'effectuer la distribution des cartes selon les regles de Midnight oil   

let distribution_bk etat permut =
- cette fonction permet d'effectuer la distribution des cartes selon les regles de Baker dozen  

let distribution_fc etat permut=
- cette fonction permet d'effectuer la distribution des cartes selon les regles de FreeCell  


------------------------------

let init_game name permut : game -> int list -> etat
- cette fonction permet d'initaliser une partie selon son mode de jeu
On utilise un match pour vérifier le mode de jeu spécifié. Selon ce dernier, cette fonction appelle la fonction de distribution assosié a lui avec les paramètres requis.

let maj_depot depot n : maj_depot : depot -> int -> depot
- la fonction maj_depot est utilisée pour mettre à jour un dépôt spécifique en augmentant sa valeur de 1. Elle prend en entrée 2 paramètres:
    -depot : contient le depot à mettre a jour.
    -n : L'indice de la case à mettre à jour, qui represente le depôt (0 pour le depôt Trefle, 1 pour le depôt Pique, case 2 pour le depôt Coeur, et 3 pour le depôt Carreau)
La fonction utilise d'abord la fonction PArray.get pour récupérer le depôt a mettre à jour. Ensuite, elle utilise la fonction PArray.set pour le mettre à jour en l'augmentant de 1. Enfin, elle retourne la structure depot avec le depôt mis a jour.


FONCTION D'AFFICHAGE 
--------------------

let print_col list_card : 'a list -> unit
- cette fonction a pour but d'afficher les cartes dans les colonnes.
elle prend en argument une liste de cartes et utilise la fonction List.iter pour la parcourir. Pour chaque carte, elle utilise la fonction Card.to_string pour obtenir une représentation sous forme de chaîne de caractères de cette carte, et utilise Printf.printf pour l'afficher, suivi d'un espace. Ainsi, toutes les cartes dans la liste sont affichées, séparées par des espaces.


let print_col_nums list_card : 'a list -> unit
- cette fonction a pour but d'afficher les numeros des cartes dans les colonnes.
elle fonctionne de manière similaire à print_col, mais au lieu d'utiliser Card.to_string pour obtenir une représentation sous forme de chaîne de caractères de la carte, elle utilise Card.to_num pour obtenir un nombre représentant cette carte.


let print_colonnes colonnes_tab : 'a FArray.t -> unit  
- cette fonction a pour but d'afficher tout les colonnes ainsi que leurs carte.
cette dernière prend en argument un tableau de colonnes. ensuite elle utilise FArray.iter pour parcourir chaque colonne dans le tableau, et pour chaque colonne, elle utilise Printf.printf pour afficher "COL : ", suivi d'une liste des cartes dans cette colonne en utilisant la fonction List.iter pour parcourir cette liste de cartes, utilise Card.to_string pour obtenir une représentation sous forme de chaîne de caractères de chaque carte, et utilise Printf.printf pour afficher cette représentation, suivie d'un espace. Ainsi, toutes les cartes dans la colonne sont affichées, séparées par des espaces, et une nouvelle ligne est imprimée après cela pour séparer les colonnes.


let print_reg registres_tab : registres option -> unit
 - cette fonction a pour but d'afficher le contenu registres, elle prend un seul argument de type registres option, et elle ne retourne rien.
elle affiche la représentation sous forme de chaîne de chaque carte stockée dans le tableau de registres ou affiche "Pas de registres" s'il n'y a pas de registres temporaires.


let print_depot depots : depot -> unit
 - cette fonction a pour but d'afficher le nombre de carte dans chaque depot.
elle prend un seul argument, depots de type depot, et elle ne retourne rien non plus, elle imprime simplement l'entier stocké à chaque index du tableau depots.


let display_state etat : etat -> unit
- cette fonction a pour but d'afficher l'etat actuel du jeu.
elle prend un seul argument, etat de type etat, et elle ne retourne rien non plus, elle imprime des informations sur l'état du jeu. Elle appelle la fonction print_colonnes sur l'attribut colonnes de etat, la fonction print_reg sur l'attribut registres de etat, et la fonction print_depot sur l'attribut depots de etat.


let print_card card : Card.card -> unit
- cette fonction prend un seul argument, card de type card, et elle ne retourne rien non plus. Elle imprime simplement le numéro qui représente la carte et sa représentation sous forme de chaîne.


EXECUTER UN COUP
----------------
















 