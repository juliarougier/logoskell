# Haskell project

## Définition du language LOGOSKELL

Le langage LOGOSKELL (version 1.0) sert à exprimer le chemin que va suivre un crayon pour dessiner un graphique. Il comporte 4 instructions,toutes avec paramètres :Forward,Left,RightetRepeat. L’instructionForward xfait avancer le crayon de x points dans la direction courante.L’instructionRight x(respectivementLeft x) fait tourner la direction cou-rante de x degrés à droite (respectivement à gauche), dans le sens des ai-guilles d’une montre (respectivement dans le sens inverse). L’instructionRepeat x [ yyy ]répète x fois la suite d’instructions entre crochets. Lesinstructions sont séparées par des virgules et le programme entier se trouveentre crochets.

Les instructions attendues sont donc
 - Forward x
 - Left x
 - Right x
 - Repeat x 
 - [instruction, otherInstruction, ..]

 où x est un entier et entre crochets [ ] les instruction sont séparées par des virgules

 ## Stockage en mémoire des instructions

 On stockera les coordonnées données par les instructions dans une liste de tuples (x, y) qu'on pourra ensuite facilement convertir en programme SVG

 ## fonctions de base à définir pour chaque instruction + une fonction qui renvoie