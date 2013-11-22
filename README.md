## Hanoï

Resolution des tours de Hanoi

### Compilation

```
ocamlopt nums.cmxa deQueue.ml -o hanoi hanoi.ml
```

### Usage

* Pour voir le chemin généré par l'algorithme ```algo``` avec  ```n``` disques, avec algo = ```hanoi```, ```hanoi1```, ```hanoi2``` ou ```hanoi3```

```
./hanoi -chemin <algo> n 
```

* Pour afficher la courbe de temps d'exécution pour tous les chemins jusqu'à ```N``` avec l'algorithme ```algo```, avec algo = ```hanoi```, ```hanoi1```, ```hanoi2``` ou ```hanoi3``


```
./hanoi -courbe <algo> N
```

