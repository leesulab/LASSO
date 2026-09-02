# Scripts

Ce dossier est prevu pour les scripts reutilisables :

- import de donnees ;
- conversion ;
- verification de fichiers ;
- requetes DuckDB ;
- generation de donnees intermediaires.

## Scripts disponibles

### `build_metadata_index.R`

Construit un index CSV a partir des fichiers JSON de metadonnees :

```bash
Rscript scripts/build_metadata_index.R
```

Entree par defaut :

```text
data/raw/json
```

Sortie par defaut :

```text
data/processed/metadata_index.csv
```

### `build_compounds_reference.R`

Construit une table propre des molecules a rechercher a partir du CSV des etalons internes :

```bash
Rscript scripts/build_compounds_reference.R
```

Entree par defaut :

```text
data/reference/etalons-internes.csv
```

Sortie par defaut :

```text
data/processed/compounds_reference.csv
```

### `parquet_chromatograms.R`

Contient les fonctions de calcul de base sur les fichiers Parquet :

```text
compute_tic()
compute_bpi()
compute_tic_bpi()
compute_eic()
summarise_eic_detection()
screen_compound_in_file()
screen_compounds_in_file()
```

`compute_tic_bpi()` calcule les deux chromatogrammes pendant la meme lecture du fichier. `screen_compounds_in_file()` collecte les fenetres `m/z` des etalons en une seule requete Arrow, puis evalue chaque etalon en memoire.

Le screening retourne a la fois une decision finale (`Detected` / `Not Detected`) et un niveau de preuve :

```text
0 = aucun signal retenu
1 = m/z + intensite
2 = m/z + RT + intensite
3 = m/z + RT + preuve de mobilite directe
```

La comparaison DT directe est optionnelle (`use_dt`) et exploratoire : le DT varie entre les fichiers. Le niveau 3 actuel reste compatible avec un Parquet exceptionnellement enrichi d'une colonne `ccs`.

Le flux cible pour les Parquet bruts est prepare dans `ccs_drift_time.R` : une fonction externe devra convertir `ccs`, `mz` et les coefficients de calibration `C1/C2` en DT attendu, en ms. Le moteur compare alors ce DT attendu au DT observe seulement pour une molecule deja compatible en `m/z` et `RT`. Tant que la formule et la tolerance scientifique ne sont pas validees, cette comparaison est exportee comme information `exploratoire` et ne modifie pas le niveau de confiance.

Pour un chemin local, les calculs utilisent Arrow. Pour une URL `http://` ou `https://`, le script bascule vers DuckDB et l'extension `httpfs` afin de requeter directement le Parquet distant.

### `nextcloud_public_webdav.R`

Contient les fonctions de navigation Nextcloud via WebDAV, pour un partage public ou un compte :

```text
nextcloud_public_config()
nextcloud_account_config()
nextcloud_connection_config()
list_nextcloud_contents()
nextcloud_file_url()
```

Le jeton de partage ou l'en-tete d'authentification est fourni par la session Shiny. Le mot de passe d'application n'est pas conserve dans les tables, les exports ou le depot.

Test rapide :

```bash
Rscript scripts/parquet_chromatograms.R data/raw/parquet/test/pharma_PT6_replicate_1.parquet
```
