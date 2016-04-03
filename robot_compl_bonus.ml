(*******************************************************************)
(*                                                                 *)
(*                                                                 *)
(*                  Robots, projet Programmation avancée           *)
(*           Institut Galilée, L3 informatique, S6, 2014-2015      *)
(*                                                                 *)
(*                                                                 *)
(*******************************************************************)

(*  Projet de Damien MEHALA et Damien MEHALA *)

open Graphics
open Unix

(*******************************************************************)
(*                                                                 *)
(*                     Déclarations de type                        *)
(*                                                                 *)
(*******************************************************************)
type position = { i : int ; j : int } (* sur la grille *)

type objet =
  | Robot of position
  | Debris of position
  | Super_Robot of position

type niveau =
    { objets : objet list ;
      joueur : position ;
      nb_teleportation : int ;
      attente : bool ;
      score : int
    }

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

let (bepo:controles) = ['v',NO ; 'd',N ; 'l',NE ; 'z', Teleport ;
                        't',O; 's',Immobile ; 'r',E ; 'm', Teleport_OK ;
                        'q',SO ; 'g',S ; 'h',SE ; 'f', Attente]
let (azerty:controles) = ['a',NO ; 'z',N ; 'e',NE ; 't', Teleport ;
                          'q',O; 's',Immobile ; 'd',E ; 'g', Teleport_OK ;
                          'w',SO ; 'x',S ; 'c',SE ; 'v', Attente]
let (qwerty:controles) = ['q',NO ; 'w',N ; 'e',NE ; 't', Teleport ;
                          'a',O; 's',Immobile ; 'd',E ; 'g', Teleport_OK ;
                          'z',SO ; 'x',S ; 'c',SE ; 'v', Attente]
let (pave_num:controles) = ['7',NO ; '8',N ; '9',NE ; '-', Teleport ;
                            '4',O; '5',Immobile ; '6',E ; '+', Teleport_OK ;
                            '1',SO ; '2',S ; '3',SE ; '0', Attente ]

(* ParamÃ¨tres du dessin *)
let case = 26     (* taille en pixels d'une case *)
let bord = 4       (* taille autour du dessin *)
let taille_x = (case+1) * largeur          (* taille de la zone                *)
let taille_y = ((case+1) * hauteur) + case (* d'affichage de la grille+bandeau *)

let coul_grille_1 = rgb 117 144 174 and coul_grille_2 = rgb 105 130 157
let choix_coul a = if a mod 2 = 0 then coul_grille_1 else coul_grille_2

let coul_joueur = yellow
let coul_robot = red
let coul_debris = black
let coul_super_robot = green

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

let dessine_robot pos super =
  let (x,y) = grille_vers_pixel pos in
  if(super == true) then set_color coul_super_robot else set_color coul_robot;
  fill_poly [|x+bord,y+bord ; x+case-bord,y+bord ; x+case/2,y+case-bord |]

let dessine_objet obj = match obj with
  |Robot pos -> dessine_robot pos false
  |Super_Robot pos -> dessine_robot pos true
  |Debris pos -> dessine_debris pos

let dessine_niveau niv =
  dessine_grille();
  dessine_joueur niv.joueur;
  List.iter dessine_objet niv.objets (* Iterateur map ;) GOTCHA JYM *)

let wait milli =
  let sec = milli /. 1000. in
  let tm1 = Unix.gettimeofday() in
  while Unix.gettimeofday() -. tm1 < sec do() done

let bg_transition txt =
  wait(650.0);
  set_color black;
  fill_rect 0 0 taille_x taille_y;
  set_color white;
  set_text_size 120;
  moveto (taille_x/2) (taille_y/2);
  draw_string txt

let dessine_bouton_recommencer _color =
  set_color _color;
  fill_rect 557 365 198 43;
  set_color black;
  fill_rect 560 368 192 37;
  set_color white;
  moveto 605 381;
  set_text_size 120; (* ne marche pas *)
  draw_string "Recommencer"

let dessine_bandeau() =
  set_color black;
  fill_rect 0 (taille_y-case) taille_x case

let actualise_teleportation n =
  set_color white;
  moveto 8 (taille_y-20);
  draw_string (String.concat " : " (["Teleportation"; (string_of_int n)]))

let actualise_score n =
  set_color white;
  moveto (taille_x-100) (taille_y-20);
  draw_string (String.concat " : " (["Score"; (string_of_int n)]))

(* Sert a savoir si le curseur survole un bouton carré           *)
(* sera utile plus tard, lors de la création du systeme de score *)
(* int * int -> int * int -> int -> int                          *)
let is_collision_box x_box y_box x y =
  x >= (fst x_box) && x <= (snd x_box) && y >= (fst y_box) && y <=
    (snd y_box)

(******************************************************************)
(*                                                                *)
(*                     Affichage Meilleurs Scores                 *)
(*                                                                *)
(******************************************************************)
let col1 = rgb 25 25 25 and col2 = rgb 53 53 53

let draw_box_score () =
  set_color col1;
  fill_rect 369 345 351 166;
  set_color col2;
  fill_rect 387 357 313 29;

  set_color white;
  moveto 508 365;
  draw_string "Valider"

let draw_best_scores arg_scores =
  set_color col1;
  fill_rect 369 62 479 688;

  (* TOP TITRE *)
  set_color (rgb 129 185 0);
  fill_rect 369 (taille_y-136) 479 76;
  set_color white;
  moveto 387 (taille_y-109);
  set_text_size 48;
  draw_string "Meilleurs scores";

  let rec aux_bandeau i variant scoresL =
    let next_cpl = try List.hd scoresL with
      |Failure txt -> ("TEST", "SCORE_TEST")
    in
    if(i>0) then
      begin
	if((i mod 2) == 0) then set_color col1 else set_color col2;
	fill_rect 369 (taille_y-variant) 479 76;

	(* Nom joueur *)
	set_color white;
	moveto 387 (taille_y-variant+26);
	draw_string (fst next_cpl);

	(* Score joueur *)
	moveto 758 (taille_y-variant+26);
	draw_string (snd next_cpl);

	aux_bandeau (i-1) (variant+68) (List.tl arg_scores)
      end
  in aux_bandeau 2 204

(*******************************************************************)
(*                                                                 *)
(*                     CrÃ©ation d'un niveau                        *)
(*                       Questions 6 Ã  11                          *)
(*                                                                 *)
(*******************************************************************)
let random_pos () = { i=(Random.int largeur)+1; j=(Random.int hauteur)+1 };;

let objet_vers_pos obj = match obj with
  |Robot pos -> pos
  |Super_Robot pos -> pos
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

let creer_robots n super =
  let rec aux i accL =
    if(i==0) then accL
    else
      begin
	match super with
	|false -> aux (i-1) (Robot (nouveau accL)::accL)
	|true  -> aux (i-1) (Super_Robot (nouveau accL)::accL)
      end
  in
  aux n []

(* concat de deux liste de robot & sup robot pas bonne methode *)
(* peut creer des colissions                                   *)
let nouveau_niveau n_lvl n_teleport n_score = 
  let lobjets = List.append (creer_robots (n_lvl*8) false) (creer_robots
  (n_lvl*2) true) in
   { objets = lobjets;
     joueur = nouveau lobjets;
     nb_teleportation = n_teleport;
     attente = false;
     score = n_score
   }

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

let bouge_super_robot pos_joueur pos_robot =
  let first_step = objet_vers_pos(bouge_robot pos_joueur pos_robot) in
  Super_Robot {i = first_step.i + compare pos_joueur.i first_step.i;
	       j = first_step.j + compare pos_joueur.j first_step.j}

let bouge_objet pos_joueur obj = match obj with
  |Robot pos_robot -> bouge_robot pos_joueur pos_robot
  |Super_Robot pos_sr -> bouge_super_robot pos_joueur pos_sr
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
    joueur = lvl.joueur;
    nb_teleportation = lvl.nb_teleportation;
    attente = lvl.attente;
    score = lvl.score;
  }

(* Joueur q 15-18 *)
(* TODO : redefinir immobile par une action ne definissant aucun *)
(* deplacement des robots *)
let char_vers_direction car contrll =
  try List.assoc car contrll with Not_found -> Immobile

(* Evite de renvoyer la meme position *)
let rec teleport pos = 
  let pos_teleport = random_pos() in
  if(pos_teleport = pos) then teleport pos
  else pos_teleport 

let adjacent pos1 pos2 = abs(pos1.i-pos2.i) = 1 || abs(pos1.j-pos2.j) = 1

let case_sure pos lvl =
  let rec aux l = match l with
    |[] -> true
    |x::xs -> let pos_objet = objet_vers_pos x in
	      if(adjacent pos_objet pos && conflits pos_objet lvl.objets)
	       then false
	      else aux xs
  in aux lvl.objets

let rec teleport_sur lvl =
  if(lvl.nb_teleportation <= 0) then teleport lvl.joueur
  else
    let new_pos = teleport lvl.joueur in
     if(case_sure new_pos lvl) then new_pos
     else teleport_sur lvl

exception CONTROL_ATTENTE

let change_pos pos dir lvl =
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
  |Teleport -> teleport pos
  |Teleport_OK -> teleport_sur lvl
  |Attente -> raise CONTROL_ATTENTE

(* Effectivement nous avons besoin de modifier mouvement_possible *)
(* car la teleportation peux pointer sur la position d'un ennemi  *)
(* A DEBUGGER *)
let mouvement_possible pos = valide pos

exception MV_HORS_GRILLE
exception TELEPORTATION_REFUSER

let aux_mv_joueur pos lvl =
  if(mouvement_possible pos == false) then raise MV_HORS_GRILLE
  else { objets = lvl.objets;
         joueur = pos;
         nb_teleportation = lvl.nb_teleportation;
         attente = lvl.attente;
         score = lvl.score
       }

(* si l'on procede comme precedemment, il y aura deux appels a la    *)
(* fonction change_pos dans mouvement_joueur et si la direction est  *)
(* une teleportation il retournera deux position au hasard différente*)
(* dont les conditions ne seront pas                                 *)
(* respectée                                                         *)
(* REVOIR LE PATTERN TELEPORT                                        *)
let mouvement_joueur car lvl contrll =
  let dir = char_vers_direction car contrll in
  let new_pos = change_pos lvl.joueur dir lvl in match dir with
    |Teleport -> if(conflits new_pos lvl.objets)
                  then raise TELEPORTATION_REFUSER
                 else
	          aux_mv_joueur new_pos lvl
    |Teleport_OK -> { objets = lvl.objets;
		      joueur = new_pos;
		      nb_teleportation = (lvl.nb_teleportation-1);
		      attente = lvl.attente;
	              score = lvl.score
		    }
    |_ -> aux_mv_joueur new_pos lvl

(*******************************************************************)
(*                                                                 *)
(*                     Fin du niveau ?                             *)
(*                    Questions 19 Ã  22                           *)
(*                                                                 *)
(*******************************************************************)

let tue pos obj = let pos_obj = objet_vers_pos obj in pos_obj=pos

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
(* a le mérite d'etre générale                                  *)
let l_exists p li arg_s f_conv =
  let rec aux l acc = match l with
    |[] -> acc
    |x::xs -> let x_conv = f_conv x in
	      aux xs (if(p arg_s x_conv) then x::acc else acc)
  in aux li []

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
let detruit_pos pos objL =
  let nw_objL = differente_pos pos objL in
  ( (List.append nw_objL [Debris pos]),
    ((List.length objL)-(List.length nw_objL)) )

(* revoir la fonction, pb de duplication *)
(* FIX: pb dans l'implementation de differente_pos       *) 
(* (Robot {i=1;j=1}) = (Debris {i=1;j=1}) retourne false *)
let detruit_conflits objL =
  let rec aux l acc = match l with
    |[] -> acc
    |x::xs -> let new_pos = objet_vers_pos x in
	      if(conflits new_pos xs) then
		begin
		  let tmp_objL = detruit_pos new_pos (fst acc) in
		  aux xs (fst tmp_objL, (snd tmp_objL + snd acc))
		end
	      else
		aux xs acc
  in aux objL (objL, 0)

let gest_tp nb_telep is_waiting nb_robot = match is_waiting with
  |false -> if(nb_telep < 0) then 0 else nb_telep
  |true -> if(nb_telep < 0) then nb_robot else (nb_telep+nb_robot)

let carambolage lvl = let tmp_objL = detruit_conflits lvl.objets in
		      { objets = fst tmp_objL;
		        joueur = lvl.joueur;
		        nb_teleportation = gest_tp lvl.nb_teleportation
			                           lvl.attente (snd tmp_objL);
		        attente = lvl.attente;
			score = lvl.score + (snd tmp_objL)
		      }


(*******************************************************************)
(*                                                                 *)
(*                              Jeu                                *)
(*                       Questions 28 Ã  30                         *)
(*                                                                 *)
(*******************************************************************)
exception Gagne of (int * int)
exception Perdu

let rec vide_touches () = (* vide le buffer de touches *)
  if key_pressed ()
  then (ignore(read_key());vide_touches())

exception TOUCHE_NON_PRESSER

let set_att_lvl lvl = { objets = lvl.objets;
			joueur = lvl.joueur;
			nb_teleportation = lvl.nb_teleportation;
			attente = true;
			score = lvl.score }

let coup lvl contrll =
  if(lvl.attente == true) then
    bouge_tous_objets(carambolage lvl)
  else
    begin
      vide_touches();
      (*let capture = wait_next_event([Key_pressed]) in
      if(capture.keypressed) then*)
      let k = read_key() in
       if(key_pressed() == false) then
	begin
	  (*let k = capture.key in*)
          let mv_j = try mouvement_joueur k lvl contrll with
	    |CONTROL_ATTENTE -> set_att_lvl lvl
	    |MV_HORS_GRILLE -> lvl (* revoir ici *)
	  in
	  if(mv_j = lvl && char_vers_direction k contrll <> Immobile) then lvl
	  else
	    begin
	      let mv_r = bouge_tous_objets mv_j in
	      let collision = carambolage mv_r in collision
	    end
        end
      else
	raise TOUCHE_NON_PRESSER
   end

let tour lvl contrll =
  let apres_coup = coup lvl contrll in
   dessine_niveau apres_coup;
   if(gagne apres_coup) then
     raise (Gagne (apres_coup.nb_teleportation, apres_coup.score))
   else
     begin
       if(perdu apres_coup) then raise Perdu
       else apres_coup
     end

let rec jouer contrll lvl =
  dessine_niveau lvl;
  dessine_bandeau();
  actualise_teleportation lvl.nb_teleportation;
  actualise_score lvl.score;
  wait(180.0);
  jouer contrll (tour lvl contrll)
  
let rec jeu n_lvl clavier n_teleport n_score =
  try jouer clavier (nouveau_niveau n_lvl n_teleport n_score) with
  |Gagne (n_tp, n_sc) -> begin
                          bg_transition (String.concat " " (["Niveau";string_of_int (n_lvl+1)]));
                          jeu (n_lvl+1) clavier n_tp n_sc;
                         end
  |Perdu -> begin
             bg_transition "Vous avez perdu !";
             dessine_bouton_recommencer white;
	     
	     (* utiliser a l'avenir loop_at_exit                  *)
	     (* finalement, manque de temps on laisse comme ainsi *)
	     (* le processeur va tourner a 100% :D                *)

	     while 1=1 do
	       let capture_mm = wait_next_event([Mouse_motion]) in
	       if(is_collision_box (557,755) (365,408)
	       capture_mm.mouse_x capture_mm.mouse_y) then
		 begin
		   dessine_bouton_recommencer (rgb 129 185 0); (* HOVER *)
		   let capture_mb = wait_next_event([Mouse_motion;Button_down])
		   in if(capture_mb.button) then (* don't work at all *)
		       bg_transition "Niveau 1";
		       wait(850.0);
		       jeu 1 azerty 0 0;
		 end
	        else
		 dessine_bouton_recommencer white
	     done
	    end

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
  bg_transition "Niveau 1";
  wait(850.0);
  jeu 1 azerty 0 0
