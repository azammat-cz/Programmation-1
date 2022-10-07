Les nouvelles fonctions pour l'interpréteur sont dans le fichier `builtins.ml` (je les ai mises en évidence dans une zone du code de la ligne 73 à la ligne 104).

Exemples:
- `(add '3 '4 '5)` renvoie `12`
- `(sub '3)` renvoie `-3` (on peut prendre l'opposé)
- `(sub '3 '5)` renvoie `-2`
- `(mult '2 '3 '4)` renvoie `24`
- `(leq '5 '10)` renvoie `t`
- `(leq '5 '4)` renvoie `nil`
