# Maintenance et reprise du projet

Ce document sert de point d'entree a une personne qui reprend l'application
apres plusieurs mois ou annees.

## Demarrage local

1. Installer les dependances verrouillees par `renv`.
2. Copier `.env.example` vers `.env` et definir `DATA_PATH`.
3. Generer localement `data/processed/metadata_index.csv` depuis les JSON et
   `data/processed/compounds_reference.csv` depuis la liste fournie.
4. Demarrer avec `bash scripts/run_local.sh`.

`.env`, les JSON, les Parquet, les CSV de resultats et les listes analytiques
restent locaux et ne doivent jamais etre ajoutes au depot sans accord explicite.

## Architecture

```text
DATA_PATH + JSON locaux
        |
build_metadata_index.R -> data/processed/metadata_index.csv
liste etalons/suspects
        |
build_compounds_reference.R -> data/processed/compounds_reference.csv
        |
app/app.R -> scripts de calcul -> resultats Shiny et exports CSV
```

Les responsabilites principales sont les suivantes :

| Emplacement | Responsabilite |
| --- | --- |
| `app/app.R` | Interface Shiny, etat de session, selection des fichiers, affichage et exports. |
| `scripts/parquet_chromatograms.R` | Requetes Arrow/DuckDB, TIC, BPI, EIC, screening et niveaux de confiance. |
| `scripts/ms2_reference_spectra.R` | Import CSV et comparaison exploratoire des fragments MS2. |
| `scripts/nextcloud_public_webdav.R` | Navigation et lecture distante Nextcloud/WebDAV. |
| `scripts/build_metadata_index.R` | Transformation JSON vers index local des fichiers. |
| `scripts/build_compounds_reference.R` | Normalisation de la liste d'etalons ou de suspects. |
| `scripts/ccs_drift_time.R` | Contrat et adaptation future de la conversion CCS vers DT. |
| `tests/` | Jeux synthetiques et non sensibles pour verifier le comportement. |

Les fichiers locaux sont lus par Arrow. Les URL HTTP Nextcloud sont interrogees
avec DuckDB afin de ne pas telecharger un Parquet entier avant filtrage.

## Heritage LASSO

La branche `observatoire-hrms-prototype` part de `LASSO/main`. Les fichiers
historiques `batch_screening_functions.R` et `nextcloud_suspect_screening.r`
restent a la racine du depot comme reference de l'application LASSO initiale.
La nouvelle application ne les source pas : son point d'entree est
`app/app.R`. Ne pas supprimer ni modifier les scripts historiques sans une
decision explicite de l'equipe.

## Modifier une fonctionnalite

1. Identifier la couche : calcul dans `scripts/`, interface dans `app/app.R`,
   donnees locales dans les scripts de preparation.
2. Ajouter ou adapter un test dans `tests/` avant de modifier le comportement
   scientifique ou une regle de detection.
3. Mettre a jour `docs/UTILISATION.md` si le parcours utilisateur change.
4. Verifier sur un petit Parquet autorise avant un lot Nextcloud ou une annee
   complete.
5. Executer les tests et relire `git status` avant le commit.

## Tests obligatoires

```bash
Rscript tests/test_chromatograms.R
Rscript tests/test_ms2_reference_spectra.R
Rscript tests/test_app_server.R
Rscript tests/test_nextcloud_webdav.R
```

Pour un controle supplementaire sur un disque autorise monte localement :

```bash
DATA_PATH=/chemin/vers/observatoire Rscript tests/test_local_dataset_integration.R
```

Ce dernier test ne doit jamais etre rendu obligatoire dans l'integration continue,
car il depend de donnees analytiques absentes du depot.

## Regles de donnees et de performance

- Les colonnes minimales d'un Parquet sont `rt`, `scanid`, `mslevel`, `mz` et
  `intensity`. La colonne `dt` est necessaire pour la future verification de
  mobilite.
- Ne jamais charger un Parquet complet en memoire pour une analyse de lot. Les
  filtres m/z et niveau MS doivent rester executes par Arrow ou DuckDB.
- Ne jamais faire correspondre un JSON a un Parquet sur le seul nom de fichier :
  les blancs pos et neg peuvent avoir le meme nom. Le chemin relatif est la cle.
- Les resultats exportes doivent conserver les valeurs brutes, corrigees du
  blanc et normalisees, ainsi que les parametres de calcul.

## Preuves de screening

Le moteur distingue les preuves internes suivantes :

```text
Preuve 0 : aucun signal retenu
Preuve 1 : m/z et intensite compatibles
Preuve 2 : m/z, RT et intensite compatibles
Preuve 3 : preuve 2 et preuve de mobilite validee
```

La mobilite via CCS vers DT est actuellement exportee comme resultat
exploratoire. Ne pas la promouvoir a la preuve 3 sans validation analytique. La
procedure complete est dans [Niveau 3 de mobilite](NIVEAU_3_MOBILITE.md).

Ces preuves ne sont pas des niveaux d'identification publies. La comparaison
MS2 dispose de son propre module et de ses limites dans
[Comparaison MS2](MS2_REFERENCE.md).

## Deploiement

GitHub versionne le code ; il ne transporte pas les donnees. En production,
l'application devra etre executee dans un conteneur Docker, derriere HTTPS,
avec le dossier de donnees monte en lecture seule via `DATA_PATH`. Les secrets
Nextcloud doivent etre fournis par l'environnement du serveur et jamais par
une image Docker ou un commit Git.
