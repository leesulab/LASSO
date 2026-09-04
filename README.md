# Observatoire HRMS

Prototype R/Shiny pour visualiser et analyser des donnees de spectrometrie de masse haute resolution issues de l'observation long terme des eaux usees.

L'application lit des fichiers Parquet locaux ou distants, associe leurs metadonnees JSON, calcule TIC/BPI/EIC, realise un screening d'etalons internes ou de suspects, puis permet le suivi temporel des signaux.

## Demarrage rapide

Cloner le depot prive, se placer sur la branche de l'application, puis installer
les dependances R verrouillees :

```bash
git clone git@github.com:leesulab/LASSO.git
cd LASSO
git switch observatoire-hrms-prototype
Rscript -e 'install.packages("renv"); renv::restore()'
```

Les fichiers analytiques ne font pas partie du depot. Avant le premier lancement,
obtenir les donnees par le canal autorise puis preparer localement
`data/processed/metadata_index.csv` et `data/processed/compounds_reference.csv` :

```bash
cp .env.example .env
# Editer .env : renseigner seulement DATA_PATH avec le dossier des JSON et Parquet.
Rscript scripts/build_metadata_index.R /chemin/vers/observatoire data/processed/metadata_index.csv
Rscript scripts/build_compounds_reference.R /chemin/vers/etalons-internes.csv data/processed/compounds_reference.csv
bash scripts/run_local.sh
```

Ouvrir ensuite `http://127.0.0.1:7660`.

`DATA_PATH` peut pointer vers un disque monte localement, par exemple
`/media/data/observatoire`, ou vers tout dossier contenant l'arborescence des
JSON et Parquet. Les fichiers Parquet et les donnees de reference ne sont pas
inclus dans le depot. En cas d'erreur `DATA_PATH does not exist`, consulter la
section [Configurer le dossier de donnees](docs/UTILISATION.md#configurer-le-dossier-de-donnees-data_path) : le depot de code et le dossier de donnees sont distincts.

## Utiliser l'application

1. Ouvrir `Catalogue` pour filtrer les fichiers par annee, mode et type.
2. Ajouter les fichiers retenus, puis les inspecter dans `Parquet` pour afficher
   TIC, BPI, EIC ou un spectre MS2 exploratoire.
3. Importer ou choisir les etalons et suspects, puis lancer un screening courant
   ou de lot dans `Plan screening`.
4. Consulter `Suivi molecules` pour corriger le blanc, normaliser et comparer
   les injections ou duplicats.

Le parcours complet, les limitations scientifiques et l'acces Nextcloud sont
documentes dans [la documentation d'utilisation](docs/UTILISATION.md).

## Documentation

- [Utilisation de l'application](docs/UTILISATION.md)
- [Developpement et contribution](docs/DEVELOPPEMENT.md)
- [Maintenance et reprise du projet](docs/MAINTENANCE.md)
- [Activation future du niveau 3 de mobilite](docs/NIVEAU_3_MOBILITE.md)
- [Details fonctionnels du prototype](app/README.md)

## Contenu versionne

Le depot GitHub doit contenir uniquement le code, les tests, la configuration non secrete et la documentation publique :

```text
app/          application Shiny
scripts/      traitement et acces aux donnees
tests/        tests automatises
docs/         documentation d'utilisation et de developpement
docker/       futur deploiement Docker
renv.lock     versions des packages R
.env.example  exemple de configuration sans secret
scripts/run_local.sh lancement local a partir de .env
```

`Doc/` contient les notes personnelles, comptes rendus et documents de travail du stage. Il reste local et est ignore par Git, comme les fichiers Parquet, JSON, CSV de donnees, exports, identifiants Nextcloud et clones externes.

GitHub sert a versionner et partager le code. Le deploiement final de l'application se fera ensuite dans un conteneur Docker sur une machine hebergee par le laboratoire.
