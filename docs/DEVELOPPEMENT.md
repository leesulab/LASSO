# Developpement et contribution

## Objectif

Ce depot contient le prototype R/Shiny de l'Observatoire HRMS. Il doit rester reproductible sans contenir de donnees analytiques, de documents internes ou de secrets.

Pour reprendre le projet apres une interruption, commencer par
[Maintenance et reprise du projet](MAINTENANCE.md). Pour toute evolution du
niveau de confiance 3, suivre exclusivement la procedure de
[Niveau 3 de mobilite](NIVEAU_3_MOBILITE.md) avant de modifier le moteur de
screening.

## Structure utile

```text
app/app.R                       interface et logique Shiny
scripts/parquet_chromatograms.R calculs TIC, BPI, EIC et screening
scripts/nextcloud_public_webdav.R acces Nextcloud/WebDAV
scripts/build_metadata_index.R  index JSON vers Parquet
scripts/build_compounds_reference.R preparation des etalons
scripts/ccs_drift_time.R        adaptateur experimental CCS vers DT
tests/                          tests automatises
docs/                           documentation versionnee
data/                           donnees locales ignorees par Git
Doc/                            notes personnelles ignorees par Git
```

## Installation de developpement

```r
install.packages("renv")
renv::restore()
```

Lancer les tests depuis la racine :

```bash
Rscript tests/test_chromatograms.R
Rscript tests/test_app_server.R
Rscript tests/test_nextcloud_webdav.R
```

Les tests construisent leurs propres petits CSV et Parquet temporaires. Aucun fichier analytique ni liste fournie par le laboratoire n'est necessaire pour les executer.

Puis configurer les chemins locaux et demarrer l'application :

```bash
cp .env.example .env
# Editer .env : DATA_PATH doit pointer vers le dossier des JSON et Parquet.
bash scripts/run_local.sh
```

Le fichier `.env` est ignore par Git. Il peut contenir `DATA_PATH`,
`PROJECT_ROOT` et `PORT`, mais jamais un mot de passe ou un jeton Nextcloud.

## Contrat de donnees

Un Parquet exploitable contient au minimum les colonnes :

```text
rt, scanid, mslevel, mz, intensity
```

Les colonnes `bin` et `dt` sont attendues pour les fichiers LC-IMS-HRMS actuels. `ccs` peut etre absente des donnees brutes.

Les JSON associes sont indexes par leur chemin relatif. Un JSON situe dans `2024/pos/nom-metadata.json` est associe a `2024/pos/nom.parquet`. Un prefixe de stockage Nextcloud tel que `observatoire-db/` est accepte, mais aucun rapprochement par seul nom de fichier n'est autorise : les blancs peuvent partager le meme nom entre annees ou modes. Le chemin `pos` ou `neg` reste la source de mode prioritaire.

## Principes a respecter

- ne jamais ajouter de Parquet, JSON, resultats CSV, jeton ou mot de passe au depot ;
- executer les trois tests avant de proposer une modification ;
- conserver le traitement par fichier et les filtres Arrow/DuckDB afin de ne pas charger les Parquet entiers en memoire ;
- garder les valeurs brutes, le signal corrige du blanc et les valeurs normalisees dans les exports ;
- ne pas promouvoir une verification de mobilite au niveau de confiance 3 sans validation analytique ;
- documenter toute nouvelle colonne, tolerance ou regle de decision dans ce dossier `docs/`.

## Ajouter une fonctionnalite

1. Identifier la couche concernee : calcul dans `scripts/`, interaction dans `app/app.R`, test dans `tests/`.
2. Ajouter ou mettre a jour un test couvrant le comportement attendu et un cas d'erreur.
3. Verifier le comportement sur un petit Parquet local avant un lot distant.
4. Mettre a jour la documentation utilisateur si le parcours de l'interface change.
5. Executer tous les tests, puis relire `git status` avant le commit.

## CCS vers DT

Les fichiers ne doivent pas recevoir une colonne CCS calculee pour chaque signal brut. La voie prevue est :

```text
CCS attendu + m/z + coefficients C1/C2 -> DT attendu
```

La comparaison avec le DT du pic ne doit avoir lieu qu'apres les filtres `m/z` et RT. L'adaptateur `scripts/ccs_drift_time.R` attend une fonction de forme :

```r
convert(ccs, mz, calibration_parameters) -> dt
```

avec `ccs` en A2, `mz` sans unite et `dt` en ms. La formule exacte et la provenance des coefficients C1/C2 doivent etre confirmes avant activation comme critere de confiance.

Les emplacements de code, le contrat exact, les metadonnees de calibration et
les tests a modifier sont decrits dans [Niveau 3 de mobilite](NIVEAU_3_MOBILITE.md).

## Publication GitHub

Utiliser un depot **prive** tant que la liste des molecules, les parametres analytiques ou la documentation ne sont pas explicitement diffusable.

Avant chaque premier envoi, ajouter les fichiers de maniere explicite :

```bash
git add README.md .gitignore .env.example renv.lock
git add app scripts tests docs docker data/README.md data/processed/README.md data/reference/README.md
git status --short
```

Ne lancer `git add .` qu'apres avoir controle attentivement la sortie de `git status --short`. Un fichier deja suivi par Git reste suivi meme s'il est ajoute ensuite dans `.gitignore` ; il faut alors le retirer de l'index avec `git rm --cached chemin/du/fichier` sans supprimer sa copie locale.

## Deploiement

GitHub versionne le code. Le deploiement final est distinct : une image Docker doit installer les packages, executer Shiny derriere un reverse proxy HTTPS, monter les donnees en lecture seule via `DATA_PATH` et fournir les secrets uniquement dans l'environnement du serveur.
