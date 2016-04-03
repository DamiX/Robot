# Robot
Ce devoir a été réalisé dans le cadre du cours de "Principe de Programmation" lors de mon année d'étude en Licence 3 Mention Informatique à l'[Université Paris 13](https://www.univ-paris13.fr/). Il sert à la mise en pratique des connaissances acquises sur la programmation fonctionnel avec OCaml.

TO DO
=====
* Exposer les nouvelles fonctionnalités par rapport au jeu initial.

Présentation
============
Robot est un jeu en tour par tour où le joueur cherche à échapper à des robots tueurs. On peut trouver une version en ligne du jeu : [http://ctho.org/games/robots](http://ctho.org/games/robots).

* **robot.ml :** jeu initial
* **robot_comp.ml :** jeu initial + fonctionnalités complémentaires
* **robot_comp_bonus.ml :** jeu initial + fonctionnalités complémentaires + fonctionnalités bonus

Par défaut le clavier azerty est utilisé. Il est toutefois possible de configurer le choix du clavier dans le code source, dans le main.
Les robots ne sont pas "intelligents", ils vont toujours chercher à se rapprocher du joueur même si ça les amène sur des débris (ou sur un autre robot).

Mode d'emploi
=============
````bash
git clone https://github.com/DamiX/Robot.git
cd Robot
ocamlc graphics.cma robot.ml -o jeu_robot
````

Pour les fonctions complémentaire & bonus :
````bash
git clone https://github.com/DamiX/Robot.git
cd Robot
ocamlc graphics.cma unix.cma robot_comp.ml -o jeu_robot_comp
ocamlc graphics.cma unix.cma robot_comp_bonus.ml -o jeu_robot_comp_bonus
````
Credits
=======
[Damien Mehala](mailto:damien.mehala@me.com)

Licence
=======
This program is free software: you can redistribut it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the Licence, or (at your option) any later version.

You should have received a copy of the GNU General Public License along with this program. If not, see : [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/)
