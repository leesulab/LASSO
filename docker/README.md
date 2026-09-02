# Docker

Objectif final : diffuser l'application web via un container Docker autoheberge.

Le prototype est prepare pour cela avec deux variables d'environnement :

```text
PROJECT_ROOT=/chemin/vers/le/projet
DATA_PATH=/chemin/monte/vers/les/Parquet
```

Le futur container devra monter les Parquet en lecture seule et ne doit pas les copier dans l'image Docker. Le fichier `renv.lock` permet de restaurer les packages R avant de lancer Shiny.

Un Dockerfile n'est pas encore ajoute car Docker n'est pas installe sur la machine de developpement et le chemin de montage final du serveur doit etre confirme. Il faudra tester le build et l'acces aux fichiers reels avant diffusion.
