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


type action = |Card of int |V |T 
- le type action : représente le second mot d'une ligne d'un fichier solution
    -Card of int : indique le numéro d'une carte en sommet de colonne ou s'effectura le mouvement
    -V : indique un mouvement vers la première colonne vide disponible.
    -T : indique un mouvement vers un registre temporaire inoccupé.
  

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

let remove_from_colonne colonne : colonne -> colonne
- cette fonction prend un paramètre colonne, qui est de type colonne, et qui retourne une colonne. Son objectif est de retirer la carte du dessus d'une colonne (si elle existe). Si la colonne est vide, il génère une exception failwith "colonne vide", sinon, il retourne le reste de la liste après avoir enlevé le premier élément.
  

let remove_from_registre registres num_registre : registres -> int -> registres
- cette fonction prend deux paramètres : registres qui est de type registres et num_registre qui est de type int et qui retourne registres. Son objectif est de retirer une carte d'un registre spécifique. Cela se fait en définissant le registre à l'index donné num_registre à la carte vide (-1,Card.Trefle) en utilisant PArray.set


let ajouter_registre_vide (card : Card.card) (registres : registres) : Card.card -> registres -> registres
- cette fonction prend deux paramètres : card qui est de type Card.card et registres qui est de type registres et qui retourne registres. Son objectif est d'ajouter une carte au premier registre vide (si il y en a un) en trouvant d'abord la position du premier registre vide en appelant la fonction interne chercher_registre_vide qui parcours de manière récursive le tableau de registres, s'il en trouve un vide, il retourne sa position et ajoute la carte donnée dessus sinon il génère une erreur "coup non valide" s'il n'y a pas de registres vides.


let ajouter_colonne_vide (card : Card.card) (colonnes : colonnes) : Card.card -> colonnes -> colonnes
- cette fonction porcède de manière similaire a ajouter_registre_vide mais avec les colonne à la place des registres, elle prend deux paramètres : card qui est de type Card.card, et colonnes qui est de type colonnes et qui retourne colonnes. Son objectif est d'ajouter une carte à la première colonne vide (si elle existe) en trouvant d'abord la position de la première colonne vide en appelant la fonction interne chercher_colonne_vide qui parcours de manière récursive le tableau de colonnes, s'il en trouve une vide, il retourne sa position et ajoute la carte donnée dessus sinon il génère une erreur "coup non valide" s'il n'y a pas de colonnes vides.


let coup_carte_vers_carte c1 c2 (colonnes : colonnes) n_coup : Card.card -> Card.card -> colonnes -> int -> colonnes
- cette fonction prend trois paramètres : c1 et c2 qui sont de type card et colonnes qui est un tableau de colonnes et n_coup qui est un entier. Elle retourne un tableau de colonne avec c1 en tête de la colonne qui contenait c2 en tête. Elle utilise la fonction interne chercher_colonne qui parcours le tableau de colonnes de manière récursive pour trouver la colonne qui contient la carte c2 en tête, si elle existe, elle ajoute c1 en tête de cette colonne en utilisant FArray.set et retourne le tableau modifié. Si la colonne n'existe pas alors elle génère une erreur "ECHEC" suivi par la valeur de n_coup et quitte le programme.


let search_col (card : Card.card) (colonnes :colonnes) : Card.card -> colonnes -> int
- cette fonction prend deux paramètres : card de type Card.card et colonnes de type colonnes, elle retourne un entier. Elle utilise une fonction interne search_aux pour parcourir de manière récursive le tableau de colonnes, si elle trouve la colonne qui a card en tête elle retourne son index, sinon elle retourne -1.


let search_reg (card : Card.card) (registres:registres) : Card.card -> registres -> int
- search_reg est une fonction qui prend en paramètre une carte card et un tableau de registres registres, elle retourne un entier.
elle utilise une fonction interne search_aux qui est une fonction récursive qui parcourt le tableau de registres en utilisant l'index i comme compteur. Elle compare chaque élément du tableau de registres à la carte card passée en paramètre. Si l'élément correspond à la carte, elle retourne l'index de cet élément. Sinon, elle incrémente l'index et appelle search_aux à nouveau avec les nouveaux paramètres, jusqu'à ce qu'elle ait parcouru tout le tableau ou qu'elle ait trouvé la carte. Si elle a parcouru tout le tableau sans trouver la carte elle retourne -1.
elle déclenche la fonction interne search_aux avec les paramètres card, registres et 0 qui sont respectivement la carte à chercher, le tableau de registres et le point de départ dans le tableau. La fonction retourne l'index de la carte dans le tableau de registres s'il existe, sinon -1.


let coup (c1 : int) (c2 : action) (etat : etat) (n_coup:int) : int -> action -> etat -> int -> etat
- coup est une fonction qui prend en paramètre un entier c1, une action (carte ou T ou V), un objet etat qui représente l'état actuel du jeu et un entier n_coup qui représente le numéro de coup actuel.
la fonction utilise deux fonctions internes : remove_card et new_etat
la fonction remove_card prend en paramètre un entier c1 et un objet etat, elle recherche la colonne contenant la carte c1 en utilisant la fonction search_col et retire cette carte de cette colonne. Si elle ne trouve pas la carte dans les colonnes elle recherche la carte dans les registres en utilisant la fonction search_reg et retire cette carte du registre.
new_etat est défini comme l'état résultant de l'exécution de remove_card c1 etat.
après avoir enlevé la carte c1, la fonction coup va utiliser l'action c2 pour déterminer la prochaine action à effectuer. Si c2 est une carte (nombre qui représente une carte), elle utilise la fonction coup_carte_vers_carte pour placer la carte c1 au sommet de la colonne contenant la carte carte. Si c2 est égal à 'V', elle utilise la fonction ajouter_colonne_vide pour placer la carte c1 au sommet d'une colonne vide. Enfin, si c2 est égal à 'T', elle utilise la fonction ajouter_registre_vide pour placer la carte c1 dans un registre vide. Le résultat final est un nouvel objet etat qui reflète les changements effectués par la fonction coup.


NORMALISATION :
---------------
pour la normalisation, on a deux fonctions normalize_col et normalize_reg qui ont pour but de 'normaliser' les colonnes et les registres d'un état donné, c'est à dire de vérifier si des cartes peuvent être déplacées dans les dépôts selon les règles du jeu. Ces fonctions parcourent toutes les colonnes/registres et pour chaque carte, vérifie si elle est égale à la carte de rang immédiatement supérieur dans le dépôt correspondant (Pique, Coeur, Carreau ou Trèfle). Si c'est le cas, la carte est déplacée dans le dépôt et un 'changement' est enregistré. A la fin du parcours, si un changement a été détecté, on recommence l'opération, sinon on retourne les colonnes/registres normalisées ainsi que le dépôt.

let rec normalize_col colonnes depot n changement : colonnes -> depot -> int -> bool -> depot * colonnes
- cette fonction prend comme argument les colonnes de l'état, les dépôts, un indice de colonne courant (n) et un booléen qui indique s'il y a eu un changement. Elle retourne le dépôt final et les colonnes normalisées.

let rec normalize_reg registres depot n  changement : registres -> depot -> int -> bool -> depot * registres
- cette fonction normalize_reg prend comme argument les registres de l'état, les dépôts, un indice de registre courant (n) et un booléen qui indique s'il y a eu un changement. Elle retourne le dépôt final et les registres normalisés.


FONCTION DE VERIFICATION :
--------------------------
les fonctions de verification verifient si un coup est valide selon les regles du jeux
chacune de ces fonctions prend en entrée une carte, une action, et un état de jeu. Elles renvoient true si l'action est valide et false sinon.

let verif_FreeCell (card : Card.card) (card2 : action) etat : Card.card -> action -> etat -> bool
- verifie pour les régle de jeu FreeCell

let verif_Seahaven (card : Card.card) (card2 : action) etat : Card.card -> action -> etat -> bool
- verifie pour les régle de jeu Seahaven

let verif_Midnight (card : Card.card) (card2 : action) etat : Card.card -> action -> etat -> bool
- verifie pour les régle de jeu Midnight

let verif_Baker (card : Card.card) (card2 : action) etat : Card.card -> action -> etat -> bool
- verifie pour les régle de jeu Baker


FONCTION DE LECTURE DU FICHIER SOLUTION ET VERIFICATION DE LA SOLUTION : 
------------------------------------------------------------------------
Ces fonctions ont pour but d'extraire de la ligne du fichier solution le coup a jouer et l'executer si il est valide ,puis retourner le nouvel etat apres le coup.

let string_to_action (c:string) : string -> action
- cette fonction prend une chaîne de caractères en entrée et la convertit en une action qui peut être soit V, T, ou un numéro de carte (Card(numéro de la carte)). Elle utilise un match sur le paramètre en entré, si ce dernier est égal à "T" ou "V" elle retourne la valeur correspondante. Sinon, elle utilise la fonction int_of_string pour convertir la chaîne en un entier qui est utilisé pour créer une action de type Carte.


let action_string (c:action) : action -> string   
- la fonction action_string fait l'opération inverse de la fonction string_to_action, elle prend une action en entrée et la convertit en chaîne de caractères. Pour cela elle utilise un match pour vérifier le type d'action et retourne le string correspondant. Si l'action est de type carte, il utilise la fonction Card.to_string pour convertir la carte en chaine  


let card_on_top card etat : Card.card -> etat -> bool
- cette fonction prend une carte et un état en entrée et retourne vrai si cette carte est en haut d'une colonne ou d'un registre de l'état donné. Il utilise les fonctions FArray.exists et PArray.exists pour vérifier si la carte se trouve en haut d'une colonne ou d'un registre respectivement.


let validate_coup game (card: Card.card) (card2:action) (etat:etat) : game -> Card.card -> action -> etat -> bool
- la fonction validate_coup prend en entrée le nom du jeu (Freecell, Seahaven, Baker, ou Midnight), une carte, une action, et un état. Il utilise la fonction verif_FreeCell, verif_Seahaven, verif_Baker, verif_Midnight en fonction du jeu pour vérifier si le coup est valide pour ce jeu en particulier. Il utilise également la fonction card_on_top pour vérifier si la carte donnée se trouve en haut d'une colonne ou d'un registre. Il retourne vrai si toutes les conditions sont remplies, sinon il retourne faux.


let depot_full depots : depot -> bool
- La fonction depot_full prend un dépôt en entrée, une représentation de l'état de chaque dépôt. Il utilise la fonction PArray.for_all pour vérifier si tous les éléments dans le dépôt sont égaux à 13, ce qui signifie que tous les dépôts sont remplis.


let treat_line line etat n_coup game : string -> etat -> int -> game -> etat
-  cette fonction prend en entrée une ligne de solution (sous forme d'une chaine de caractère), un état, un numéro de coup et un nom de jeu. Elle utilise la fonction string_to_action pour convertir les chaine de caractères en actions, puis utilise "validate_coup" pour vérifier si l'action est valide. Si elle est valide, la fonction effectue le coup avec la fonction coup, puis normalise l'état avec la fonction "normalize". Si elle n'est pas valide, elle affiche un message d'erreur et termine le programme.



--------------------------------------------------------------------------------
---------------------------------

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
    

type action = |Card of int |V |T 
- le type action : représente le second mot d'une ligne d'un fichier solution
    -Card of int : indique le numéro d'une carte en sommet de colonne ou s'effectura le mouvement
    -V : indique un mouvement vers la première colonne vide disponible.
    -T : indique un mouvement vers un registre temporaire inoccupé.
  

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

let remove_from_colonne colonne : colonne -> colonne
- cette fonction prend un paramètre colonne, qui est de type colonne, et qui retourne une colonne. Son objectif est de retirer la carte du dessus d'une colonne (si elle existe). Si la colonne est vide, il génère une exception failwith "colonne vide", sinon, il retourne le reste de la liste après avoir enlevé le premier élément.
  

let remove_from_registre registres num_registre : registres -> int -> registres
- cette fonction prend deux paramètres : registres qui est de type registres et num_registre qui est de type int et qui retourne registres. Son objectif est de retirer une carte d'un registre spécifique. Cela se fait en définissant le registre à l'index donné num_registre à la carte vide (-1,Card.Trefle) en utilisant PArray.set


let ajouter_registre_vide (card : Card.card) (registres : registres) : Card.card -> registres -> registres
- cette fonction prend deux paramètres : card qui est de type Card.card et registres qui est de type registres et qui retourne registres. Son objectif est d'ajouter une carte au premier registre vide (si il y en a un) en trouvant d'abord la position du premier registre vide en appelant la fonction interne chercher_registre_vide qui parcours de manière récursive le tableau de registres, s'il en trouve un vide, il retourne sa position et ajoute la carte donnée dessus sinon il génère une erreur "coup non valide" s'il n'y a pas de registres vides.


let ajouter_colonne_vide (card : Card.card) (colonnes : colonnes) : Card.card -> colonnes -> colonnes
- cette fonction porcède de manière similaire a ajouter_registre_vide mais avec les colonne à la place des registres, elle prend deux paramètres : card qui est de type Card.card, et colonnes qui est de type colonnes et qui retourne colonnes. Son objectif est d'ajouter une carte à la première colonne vide (si elle existe) en trouvant d'abord la position de la première colonne vide en appelant la fonction interne chercher_colonne_vide qui parcours de manière récursive le tableau de colonnes, s'il en trouve une vide, il retourne sa position et ajoute la carte donnée dessus sinon il génère une erreur "coup non valide" s'il n'y a pas de colonnes vides.


let coup_carte_vers_carte c1 c2 (colonnes : colonnes) n_coup : Card.card -> Card.card -> colonnes -> int -> colonnes
- cette fonction prend trois paramètres : c1 et c2 qui sont de type card et colonnes qui est un tableau de colonnes et n_coup qui est un entier. Elle retourne un tableau de colonne avec c1 en tête de la colonne qui contenait c2 en tête. Elle utilise la fonction interne chercher_colonne qui parcours le tableau de colonnes de manière récursive pour trouver la colonne qui contient la carte c2 en tête, si elle existe, elle ajoute c1 en tête de cette colonne en utilisant FArray.set et retourne le tableau modifié. Si la colonne n'existe pas alors elle génère une erreur "ECHEC" suivi par la valeur de n_coup et quitte le programme.


let search_col (card : Card.card) (colonnes :colonnes) : Card.card -> colonnes -> int
- cette fonction prend deux paramètres : card de type Card.card et colonnes de type colonnes, elle retourne un entier. Elle utilise une fonction interne search_aux pour parcourir de manière récursive le tableau de colonnes, si elle trouve la colonne qui a card en tête elle retourne son index, sinon elle retourne -1.


let search_reg (card : Card.card) (registres:registres) : Card.card -> registres -> int
- search_reg est une fonction qui prend en paramètre une carte card et un tableau de registres registres, elle retourne un entier.
elle utilise une fonction interne search_aux qui est une fonction récursive qui parcourt le tableau de registres en utilisant l'index i comme compteur. Elle compare chaque élément du tableau de registres à la carte card passée en paramètre. Si l'élément correspond à la carte, elle retourne l'index de cet élément. Sinon, elle incrémente l'index et appelle search_aux à nouveau avec les nouveaux paramètres, jusqu'à ce qu'elle ait parcouru tout le tableau ou qu'elle ait trouvé la carte. Si elle a parcouru tout le tableau sans trouver la carte elle retourne -1.
elle déclenche la fonction interne search_aux avec les paramètres card, registres et 0 qui sont respectivement la carte à chercher, le tableau de registres et le point de départ dans le tableau. La fonction retourne l'index de la carte dans le tableau de registres s'il existe, sinon -1.


let coup (c1 : int) (c2 : action) (etat : etat) (n_coup:int) : int -> action -> etat -> int -> etat
- coup est une fonction qui prend en paramètre un entier c1, une action (carte ou T ou V), un objet etat qui représente l'état actuel du jeu et un entier n_coup qui représente le numéro de coup actuel.
la fonction utilise deux fonctions internes : remove_card et new_etat
la fonction remove_card prend en paramètre un entier c1 et un objet etat, elle recherche la colonne contenant la carte c1 en utilisant la fonction search_col et retire cette carte de cette colonne. Si elle ne trouve pas la carte dans les colonnes elle recherche la carte dans les registres en utilisant la fonction search_reg et retire cette carte du registre.
new_etat est défini comme l'état résultant de l'exécution de remove_card c1 etat.
après avoir enlevé la carte c1, la fonction coup va utiliser l'action c2 pour déterminer la prochaine action à effectuer. Si c2 est une carte (nombre qui représente une carte), elle utilise la fonction coup_carte_vers_carte pour placer la carte c1 au sommet de la colonne contenant la carte carte. Si c2 est égal à 'V', elle utilise la fonction ajouter_colonne_vide pour placer la carte c1 au sommet d'une colonne vide. Enfin, si c2 est égal à 'T', elle utilise la fonction ajouter_registre_vide pour placer la carte c1 dans un registre vide. Le résultat final est un nouvel objet etat qui reflète les changements effectués par la fonction coup.


NORMALISATION :
---------------
pour la normalisation, on a deux fonctions normalize_col et normalize_reg qui ont pour but de 'normaliser' les colonnes et les registres d'un état donné, c'est à dire de vérifier si des cartes peuvent être déplacées dans les dépôts selon les règles du jeu. Ces fonctions parcourent toutes les colonnes/registres et pour chaque carte, vérifie si elle est égale à la carte de rang immédiatement supérieur dans le dépôt correspondant (Pique, Coeur, Carreau ou Trèfle). Si c'est le cas, la carte est déplacée dans le dépôt et un 'changement' est enregistré. A la fin du parcours, si un changement a été détecté, on recommence l'opération, sinon on retourne les colonnes/registres normalisées ainsi que le dépôt.

let rec normalize_col colonnes depot n changement : colonnes -> depot -> int -> bool -> depot * colonnes
- cette fonction prend comme argument les colonnes de l'état, les dépôts, un indice de colonne courant (n) et un booléen qui indique s'il y a eu un changement. Elle retourne le dépôt final et les colonnes normalisées.

let rec normalize_reg registres depot n  changement : registres -> depot -> int -> bool -> depot * registres
- cette fonction normalize_reg prend comme argument les registres de l'état, les dépôts, un indice de registre courant (n) et un booléen qui indique s'il y a eu un changement. Elle retourne le dépôt final et les registres normalisés.


FONCTION DE VERIFICATION :
--------------------------
les fonctions de verification verifient si un coup est valide selon les regles du jeux
chacune de ces fonctions prend en entrée une carte, une action, et un état de jeu. Elles renvoient true si l'action est valide et false sinon.

let verif_FreeCell (card : Card.card) (card2 : action) etat : Card.card -> action -> etat -> bool
- verifie pour les régle de jeu FreeCell

let verif_Seahaven (card : Card.card) (card2 : action) etat : Card.card -> action -> etat -> bool
- verifie pour les régle de jeu Seahaven

let verif_Midnight (card : Card.card) (card2 : action) etat : Card.card -> action -> etat -> bool
- verifie pour les régle de jeu Midnight

let verif_Baker (card : Card.card) (card2 : action) etat : Card.card -> action -> etat -> bool
- verifie pour les régle de jeu Baker


FONCTION DE LECTURE DU FICHIER SOLUTION ET VERIFICATION DE LA SOLUTION : 
------------------------------------------------------------------------
Ces fonctions ont pour but d'extraire de la ligne du fichier solution le coup a jouer et l'executer si il est valide ,puis retourner le nouvel etat apres le coup.

let string_to_action (c:string) : string -> action
- cette fonction prend une chaîne de caractères en entrée et la convertit en une action qui peut être soit V, T, ou un numéro de carte (Card(numéro de la carte)). Elle utilise un match sur le paramètre en entré, si ce dernier est égal à "T" ou "V" elle retourne la valeur correspondante. Sinon, elle utilise la fonction int_of_string pour convertir la chaîne en un entier qui est utilisé pour créer une action de type Carte.


let action_string (c:action) : action -> string   
- la fonction action_string fait l'opération inverse de la fonction string_to_action, elle prend une action en entrée et la convertit en chaîne de caractères. Pour cela elle utilise un match pour vérifier le type d'action et retourne le string correspondant. Si l'action est de type carte, il utilise la fonction Card.to_string pour convertir la carte en chaine  


let card_on_top card etat : Card.card -> etat -> bool
- cette fonction prend une carte et un état en entrée et retourne vrai si cette carte est en haut d'une colonne ou d'un registre de l'état donné. Il utilise les fonctions FArray.exists et PArray.exists pour vérifier si la carte se trouve en haut d'une colonne ou d'un registre respectivement.


let validate_coup game (card: Card.card) (card2:action) (etat:etat) : game -> Card.card -> action -> etat -> bool
- la fonction validate_coup prend en entrée le nom du jeu (Freecell, Seahaven, Baker, ou Midnight), une carte, une action, et un état. Il utilise la fonction verif_FreeCell, verif_Seahaven, verif_Baker, verif_Midnight en fonction du jeu pour vérifier si le coup est valide pour ce jeu en particulier. Il utilise également la fonction card_on_top pour vérifier si la carte donnée se trouve en haut d'une colonne ou d'un registre. Il retourne vrai si toutes les conditions sont remplies, sinon il retourne faux.


let depot_full depots : depot -> bool
- La fonction depot_full prend un dépôt en entrée, une représentation de l'état de chaque dépôt. Il utilise la fonction PArray.for_all pour vérifier si tous les éléments dans le dépôt sont égaux à 13, ce qui signifie que tous les dépôts sont remplis.


let treat_line line etat n_coup game : string -> etat -> int -> game -> etat
-  cette fonction prend en entrée une ligne de solution (sous forme d'une chaine de caractère), un état, un numéro de coup et un nom de jeu. Elle utilise la fonction string_to_action pour convertir les chaine de caractères en actions, puis utilise "validate_coup" pour vérifier si l'action est valide. Si elle est valide, la fonction effectue le coup avec la fonction coup, puis normalise l'état avec la fonction "normalize". Si elle n'est pas valide, elle affiche un message d'erreur et termine le programme.



--------------------------------------------------------------------------------
---------------------------------































