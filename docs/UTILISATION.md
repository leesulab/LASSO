# Utilisation de l'application

## Prerequis

- R installe sur la machine ;
- les packages declares dans `renv.lock` ;
- un dossier de donnees contenant des fichiers `.parquet` et, si possible, leurs fichiers `-metadata.json` ;
- une liste d'etalons internes ou de suspects si le screening doit etre utilise.

Les donnees analytiques ne sont pas distribuees avec le code. Elles doivent etre obtenues par le canal autorise par le laboratoire.

## Installation

Depuis la racine du projet :

```bash
Rscript --vanilla -e 'install.packages("renv", repos = "https://cloud.r-project.org")'
Rscript -e 'renv::restore(prompt = FALSE)'
```

Le depot contient le squelette `renv` necessaire a l'activation
(`.Rprofile`, `renv/activate.R` et `renv/settings.json`) ainsi que le fichier
`renv.lock`. Ne pas ajouter `renv/library/` a Git : ce dossier est propre a la
machine et sera reconstruit par la restauration.

## Preparer les metadonnees locales

Lorsque les fichiers JSON et Parquet suivent une structure telle que `2024/pos/...` ou `2024/neg/...`, construire l'index local :

```bash
Rscript scripts/build_metadata_index.R /chemin/vers/observatoire-db data/processed/metadata_index.csv
```

Pour construire la liste d'etalons a partir d'un fichier fourni par l'equipe :

```bash
Rscript scripts/build_compounds_reference.R \
  data/reference/etalons-internes.csv \
  data/processed/compounds_reference.csv
```

Ces fichiers restent locaux et ne doivent pas etre envoyes sur GitHub sans validation explicite.

## Lancer l'application

Avec le dossier de donnees par defaut :

```bash
Rscript -e 'shiny::runApp("app", host = "127.0.0.1", port = 7660, launch.browser = FALSE)'
```

Avec un dossier Parquet externe, par exemple une cle USB montee localement :

```bash
cp .env.example .env
# Editer .env puis renseigner DATA_PATH. Le fichier .env n'est jamais clone ni
# versionne : cette etape est obligatoire sur chaque nouvelle machine.
bash scripts/run_local.sh
```

Ouvrir `http://127.0.0.1:7660`. La cle ou le disque contenant les donnees doit rester branche pendant l'utilisation.

### Configurer le dossier de donnees (`DATA_PATH`)

Le repertoire du depot `LASSO` contient le code de l'application. Il est distinct
du repertoire des donnees, qui contient les dossiers annuels, les fichiers JSON
et les fichiers Parquet. Il ne faut ni copier les donnees dans le depot Git, ni
deplacer le depot pour changer de source de donnees.

Si le lancement affiche par exemple :

```text
DATA_PATH does not exist: /media/data/observatoire
```

cela signifie seulement que le chemin indique dans `.env` n'existe pas sur la
machine en cours d'utilisation. Deux solutions sont possibles :

1. Monter ou brancher le disque de donnees a l'emplacement deja configure,
   par exemple `/media/data/observatoire`. Aucun changement dans `.env` n'est
   alors necessaire.
2. Modifier `DATA_PATH` dans `.env` pour qu'il corresponde a l'emplacement
   reel du disque ou du dossier de donnees. C'est la solution recommandee pour
   une cle USB, car son nom ou son point de montage peut changer d'une machine
   a l'autre.

Pour rechercher le chemin reel d'une cle montee sous Linux :

```bash
ls /media/$USER
find /media/$USER -maxdepth 3 -type d \( -iname '*observatoire*' -o -iname '*obsevatoire*' \)
```

Puis editer la configuration locale :

```bash
nano .env
```

Par exemple, si la commande precedente retourne
`/media/boudahmane/NOM_DU_DISQUE/observatoire-db`, utiliser :

```text
DATA_PATH=/media/boudahmane/NOM_DU_DISQUE/observatoire-db
```

Enregistrer avec `Ctrl+O`, `Entree`, puis quitter avec `Ctrl+X`. Verifier avant
de lancer l'application que ce dossier contient bien des Parquet :

```bash
set -a
source .env
set +a
find "$DATA_PATH" -type f -iname '*.parquet' | head
bash scripts/run_local.sh
```

Ne pas creer artificiellement le dossier `/media/data/observatoire` s'il ne
contient pas les donnees : cela ferait disparaitre le message d'erreur sans
donner acces aux fichiers. Le fichier `.env` reste propre a chaque machine et
ne doit pas etre ajoute a Git.

## Parcours recommande

1. Verifier les fichiers disponibles dans `Catalogue`, puis utiliser `Ajouter selection` ou `Ajouter tout`.
   Les fichiers accessibles sont alors ajoutes automatiquement a `Plan screening` et le premier est prepare dans `Parquet`.
2. Verifier ou importer les molecules dans `Etalons internes` et `Suspects`.
3. Utiliser `Parquet` pour inspecter le fichier prepare. Selectionner un etalon
   ou un suspect dans `Etalon ou suspect (facultatif)` remplit son `m/z` et sa RT ; calculer ensuite
   TIC/BPI, l'EIC, ou le spectre MS2 brut dans la meme fenetre RT.
   Un spectre MS2 de reference peut etre importe et compare aux fragments
   observes depuis ce meme onglet. La comparaison est exploratoire et ne
   remplace pas la validation analytique ; son format CSV et son interpretation
   sont decrits dans [Comparaison MS2](MS2_REFERENCE.md).
   Pour un apercu immediat, saisir un `m/z recherche rapide` puis cliquer
   `Rechercher m/z et afficher EIC (MS1)`. L'application recherche cette masse
   sur tout le chromatogramme MS1 avec la tolerance EIC, sans condition de RT,
   puis affiche la chromatographie extraite et la presence ou l'absence d'un
   signal. Le choix facultatif d'une molecule sert uniquement a pre-remplir la
   masse.
   Les courbes sont interactives : survoler un point pour lire ses coordonnees,
   utiliser la molette ou encadrer une zone pour zoomer, puis utiliser le bouton
   de reinitialisation dans la barre d'outils du graphe.
4. Dans `Plan screening`, verifier la selection de fichiers et lancer le screening du lot.
5. Exporter le resultat CSV si le lot doit etre reutilise sans relire les Parquet.
6. Dans `Suivi molecules`, selectionner un blanc de reference, choisir la correction et la normalisation, puis comparer les injections et duplicats.
7. Consulter `Controle` avant une analyse etendue pour detecter une metadonnee manquante ou un schema incompatible.

## Acces Nextcloud

L'onglet `Nextcloud` accepte un lien de partage ou un compte Nextcloud avec mot de passe d'application. Les identifiants saisis restent dans la session Shiny et ne doivent pas etre places dans un fichier Git.

Pour une utilisation sur une machine de production, privilegier un dossier de donnees monte localement et accessible en lecture seule.

## Limites connues

- `Detected` requiert actuellement une coherence `m/z + RT` et une intensite minimale.
- La mesure `rt_area_sum` est une somme discrete des intensites EIC par scan dans la fenetre RT, pas une integration chromatographique continue.
- Le spectre MS2 de l'onglet `Parquet` peut etre compare a une reference CSV,
  mais il agrege les signaux MS2 de la fenetre RT. Cette compatibilite de
  fragments reste exploratoire et ne constitue pas a elle seule une
  identification ni un niveau de confiance publie.
- Cocher `Calculer l'intensite totale MS` dans `Plan screening` uniquement si la normalisation par intensite totale doit etre utilisee ensuite ; sinon le lot evite une seconde lecture complete de chaque fichier.
- Le controle DT est exploratoire.
- La future verification de mobilite utilisera la conversion `CCS attendu + m/z + C1/C2 -> DT attendu`, apres validation de la formule et des parametres de calibration.
- Les resultats de screening sont une aide a l'analyse ; ils ne remplacent pas une validation analytique.
