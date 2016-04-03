(*******************************************************************)
(*                                                                 *)
(*                                                                 *)
(*                  Robots, projet Programmation avancÃ©e          *)
(*           Institut GalilÃ©e, L3 informatique, S6, 2014-2015     *)
(*                                                                 *)
(*                                                                 *)
(*******************************************************************)

(*  Projet de Damien MEHALA et Damien MEHALA *)

open Graphics

(*******************************************************************)
(*                                                                 *)
(*                     DÃ©clarations de type                        *)
(*                                                                 *)
(*******************************************************************)
type position = { i : int ; j : int } (* sur la grille *)

type objet =
  | Robot of position
  | Debris of position

type niveau =
    { objets : objet list ;
      joueur : position }

type direction =
  | NO | N | NE | E | SE | S | SO | O | Immobile
  | Teleport | Teleport_OK | Attente

type controles = (char * direction) list

(*******************************************************************)
(*                                                                 *)
(*                         ParamÃ¨tres                              *)
(*                                                                 *)
(*******************************************************************)
(* ParamÃ¨tres du jeu *)
let nb_robots = 10           (* par niveau *)
let hauteur = 30 and largeur = 45   (* taille de la grille *)

let valide pos =  (* est-ce qu'une position est bien dans la grille ? *)
  (pos.i>0) && (pos.i<=largeur) && (pos.j>0) && (pos.j<=hauteur)

let (bepo:controles) = ['v',NO ; 'd',N ; 'l',NE ;
                        't',O; 's',Immobile ; 'r',E ;
                        'q',SO ; 'g',S ; 'h',SE ]
let (azerty:controles) = ['a',NO ; 'z',N ; 'e',NE ;
                          'q',O; 's',Immobile ; 'd',E ;
                          'w',SO ; 'x',S ; 'c',SE ]
let (qwerty:controles) = ['q',NO ; 'w',N ; 'e',NE ;
                          'a',O; 's',Immobile ; 'd',E ;
                          'z',SO ; 'x',S ; 'c',SE ]
let (pave_num:controles) = ['7',NO ; '8',N ; '9',NE ;
                            '4',O; '5',Immobile ; '6',E ;
                            '1',SO ; '2',S ; '3',SE ]

(* ParamÃ¨tres du dessin *)
let case = 26     (* taille en pixels d'une case *)
let bord = 4       (* taille autour du dessin *)
let taille_x = (case+1) * largeur (* taille de la zone                *)
let taille_y = (case+1) * hauteur (* d'affichage de la grille         *)

let coul_grille_1 = rgb 117 144 174 and coul_grille_2 = rgb 105 130 157
let choix_coul a = if a mod 2 = 0 then coul_grille_1 else coul_grille_2

let coul_joueur = yellow
let coul_robot = red
let coul_debris = black

let coord_vers_px n = (n-1)*(case+1)
(* Transforme une coordonnÃ©e de la grille en coordonnÃ©e de dessin (pixels) *)

let grille_vers_pixel p =
  assert(valide p);
  (coord_vers_px p.i, coord_vers_px p.j)
(* Transforme une position en couple (x,y) des coordonnÃ©es du coin en
   bas Ã  gauche de la case (pixels) *)
(* LÃ¨ve une exception si la position n'est pas dans la grille *)


(*******************************************************************)
(*                                                                 *)
(*                            Dessin                               *)
(*                       Questions 2 Ã  5                           *)
(*                                                                 *)
(*******************************************************************)
let remplit_case coul i j =
  set_color coul;
  fill_rect (coord_vers_px i) (coord_vers_px j) case case

let dessine_case i j =
  remplit_case (choix_coul (i+j)) i j

let dessine_ligne j =
  let rec aux i =
    if(i>0) then
      begin
	dessine_case i j;
	aux (i-1);
      end
  in aux largeur

let dessine_grille () =
  let rec aux i =
    if(i>0) then
      begin
	dessine_ligne i;
	aux (i-1);
      end
  in aux hauteur

let dessine_joueur pos =
  let (x,y) = grille_vers_pixel pos in
  set_color coul_joueur;
  fill_circle (x+case/2) (y+case/2) (case/2-bord)

let dessine_debris pos =
  let (x,y) = grille_vers_pixel pos in
  set_color coul_debris;
  fill_rect (x+bord) (y+bord) (case-2*bord) (case-2*bord)

let dessine_robot pos =
  let (x,y) = grille_vers_pixel pos in
  set_color coul_robot;
  fill_poly [|x+bord,y+bord ; x+case-bord,y+bord ; x+case/2,y+case-bord |]

let dessine_objet obj = match obj with
  |Robot pos -> dessine_robot pos
  |Debris pos -> dessine_debris pos

let dessine_niveau niv =
  dessine_grille();
  dessine_joueur niv.joueur;
  List.iter dessine_objet niv.objets (* Iterateur map ;) GOTCHA JYM *)

(*******************************************************************)
(*                                                                 *)
(*                     CrÃ©ation d'un niveau                        *)
(*                       Questions 6 Ã  11                          *)
(*                                                                 *)
(*******************************************************************)
let random_pos () = { i=(Random.int largeur)+1; j=(Random.int hauteur)+1 };;

let objet_vers_pos obj = match obj with
  |Robot pos -> pos
  |Debris pos -> pos

let rec conflits pos objL = match objL with
  |[] -> false
  |x::xs -> if(pos = (objet_vers_pos x)) then true
            else conflits pos xs

(* Peut engendrer une recursion infinie si il n'y a plus de *)
(* position repondant au precidat conflits (trop de robots) *)
let rec nouveau objL = let pos = random_pos() in
		       if(conflits pos objL) then nouveau objL
		       else pos

let creer_robots n =
  let rec aux i accL =
    if(i==0) then accL
    else aux (i-1) (Robot (nouveau accL)::accL)
  in
  aux n []

let nouveau_niveau n = 
  let lobjets = creer_robots (n*nb_robots) in
   {objets = lobjets; joueur = nouveau lobjets}

(*******************************************************************)
(*                                                                 *)
(*                          Mouvements                             *)
(*                       Questions 12 Ã  18                         *)
(*                                                                 *)
(*******************************************************************)
(* Robots q 12-17 *)
(* Revoir cette fonction *)
let bouge_robot pos_joueur pos_robot =
  Robot {i = pos_robot.i + (compare pos_joueur.i pos_robot.i);
	 j = pos_robot.j + (compare pos_joueur.j pos_robot.j)}

let bouge_objet pos_joueur obj = match obj with
  |Robot pos_robot -> bouge_robot pos_joueur pos_robot
  |Debris pos_debris -> Debris pos_debris

(* List.map2 demande deux listes de meme longueur... F*CK! *)
(* Creons un iterateur ayant le comportement voulu tail recursive *)
let map2_stat f arg_s l_arg =
  let rec aux l acc = match l with
    |[] -> acc
    |x::xs -> aux xs ((f arg_s x)::acc)
  in aux l_arg []

let bouge_tous_objets lvl =
  { objets = map2_stat bouge_objet lvl.joueur lvl.objets;
    joueur = lvl.joueur }

(* Joueur q 15-18 *)
(* TODO : redefinir immobile par une action ne definissant aucun *)
(* deplacement des robots *)
let char_vers_direction car contrll =
  try List.assoc car contrll with Not_found -> Immobile

exception CONTROL_INCONNUE

let change_pos pos dir =
  match dir with
  |NO -> {i = pos.i-1; j = pos.j+1}
  |N -> {i = pos.i; j = pos.j+1}
  |NE -> {i = pos.i+1; j = pos.j+1}
  |E -> {i = pos.i+1; j = pos.j}
  |SE -> {i = pos.i+1; j = pos.j-1}
  |S -> {i = pos.i; j = pos.j-1}
  |SO -> {i = pos.i-1; j = pos.j-1}
  |O -> {i = pos.i-1; j = pos.j}
  |Immobile -> {i = pos.i; j = pos.j}
  |_ -> raise CONTROL_INCONNUE

let mouvement_possible pos dir = valide (change_pos pos dir)

exception MV_HORS_GRILLE

let mouvement_joueur car lvl contrll =
  let dir = char_vers_direction car contrll in
   if(mouvement_possible lvl.joueur dir == false) then
     raise MV_HORS_GRILLE
   else
     { objets = lvl.objets; joueur = change_pos lvl.joueur dir }

(*******************************************************************)
(*                                                                 *)
(*                     Fin du niveau ?                             *)
(*                    Questions 19 Ã  22                           *)
(*                                                                 *)
(*******************************************************************)
(* revoir la fonction tue *)
let tue pos obj = match obj with (* pos correspond a la pos du joueur *)
  |Debris obj -> obj = pos
  |Robot obj -> obj = pos

(* revoir cette fonction *)
let perdu lvl =
  let rec aux pos lObjets = match lObjets with
    |[] -> false
    |x::xs -> if(tue pos x) then true else aux pos xs
  in aux lvl.joueur lvl.objets

let est_debris obj = match obj with
  |Debris obj-> true
  |_ -> false

let gagne lvl =
  let rec aux lObjets = match lObjets with
    |[] -> true
    |x::xs -> if(est_debris x <> true) then false else aux xs
  in aux lvl.objets

(*******************************************************************)
(*                                                                 *)
(*                         Carambolage                             *)
(*                       Questions 23 Ã  27                        *)
(*                                                                 *)
(*******************************************************************)
(* Extension des methodes List Scanning *)
(* l_exists : (a -> 'a -> bool) -> a list -> a -> b list        *)
(* retourne tous les elements de la liste repondant au predicat *)
(* fournis pour l'argument donné                                *)
(* WARNING : l'argument doit etre du meme type que les elements *)
(* de la liste d'ou en argument une fonction de conversion      *)
(* retourner l'identité si ils sont du meme type                *)
(* C'est définitivement pas l'approche la plus simple mais elle *)
(* est générale                                                 *)
let l_exists p li arg_s f_conv =
  let rec aux l acc = match l with
    |[] -> acc
    |x::xs -> let x_conv = f_conv x in
	       aux xs (if(p arg_s x_conv) then x::acc else acc)
  in aux li []

let objetL_vers_posL objL =
  let rec aux l acc = match l with
    |[] -> acc
    |x::xs -> aux xs (objet_vers_pos(x)::acc)
  in aux objL []

(* predicat p_pos_egale en locale *)
let meme_pos (pos:position) (objetl:objet list) =
  let p_pos_egale pos1 pos2 = 
    if(pos1=pos2) then true else false
  in l_exists p_pos_egale objetl pos objet_vers_pos;;

(* predicat p_pos_diff en locale *)
let differente_pos (pos:position) (objetl:objet list) =
  let p_pos_diff pos1 pos2 =
    if(pos1<>pos2) then true else false
  in l_exists p_pos_diff objetl pos objet_vers_pos;;

(* on recupere les elements ayant une pos differente puis je   *)
(* rajoute un debris a la position indiquée (lel apparement... *)
(* ...la solution est la meme dans le sujet -_-')              *) 
let detruit_pos pos objl =
  List.append (differente_pos pos objl) [Debris pos]

exception DEBUG_DC

let detruit_conflits objL =
  let rec aux l acc = match l with
    |[] -> acc
    |x::xs -> let new_pos = objet_vers_pos x in
	      if(conflits new_pos xs) then
		(*raise DEBUG_DC*)
		aux xs (detruit_pos new_pos acc)
	      else aux xs acc
   in aux objL objL

let carambolage lvl = {objets = detruit_conflits lvl.objets;
		       joueur=lvl.joueur}


(*******************************************************************)
(*                                                                 *)
(*                              Jeu                                *)
(*                       Questions 28 Ã  30                         *)
(*                                                                 *)
(*******************************************************************)
exception Gagne
exception Perdu

let rec vide_touches () = (* vide le buffer de touches *)
  if key_pressed ()
  then (ignore(read_key());vide_touches())

exception TOUCHE_NON_PRESSER

let coup lvl contrll =
  vide_touches();
 (* let capture = wait_next_event([Key_pressed]) in
   if(capture.keypressed) then *)
  let k = read_key() in 
   if(key_pressed() == false) then
     begin
       (* let k = capture.key in *)
       let mv_j = mouvement_joueur k lvl contrll in
       let mv_r = bouge_tous_objets mv_j in
       let collision = carambolage mv_r in collision
     end
   else
     raise TOUCHE_NON_PRESSER

let tour lvl contrll = 
  let apres_coup = coup lvl contrll in
   if(gagne apres_coup) then raise Gagne
   else
     begin
       if(perdu apres_coup) then raise Perdu else apres_coup
     end

let rec jouer contrll lvl =
  dessine_niveau lvl;
  jouer contrll (tour lvl contrll)
  

(*******************************************************************)
(*                                                                 *)
(*                      Fonction principale                        *)
(*                                                                 *)
(*******************************************************************)
let main () =
  Random.self_init (); (* initialise le gÃ©nÃ©rateur alÃ©atoire *)
  close_graph(); (* ferme une Ã©ventuelle fenÃªtre graphique *)
  open_graph ""; (* ouvre une nouvelle fenÃªtre graphique *)
  resize_window taille_x taille_y; (* redimensionne la fenÃªtre *)
  jouer azerty (nouveau_niveau 1)
