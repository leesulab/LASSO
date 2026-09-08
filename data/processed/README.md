# Donnees traitees

Emplacement pour les fichiers generes apres nettoyage, filtrage, aggregation ou preparation pour l'application.

Fichiers generes actuellement :

```text
metadata_index.csv
compounds_reference.csv
ms2_reference_spectra.csv (facultatif)
```

`metadata_index.csv` est genere a partir de la racine contenant les JSON et les
Parquet :

```bash
Rscript scripts/build_metadata_index.R /chemin/vers/observatoire-db data/processed/metadata_index.csv
```

Il contient une ligne par Parquet. La colonne `json_available` indique si les
metadonnees JSON ont pu etre associees ; les fichiers sans JSON restent
exploitables avec les informations deduites du chemin et du nom.

`compounds_reference.csv` est genere par :

```bash
Rscript scripts/build_compounds_reference.R
```

`ms2_reference_spectra.csv` est un fichier local facultatif de spectres MS2 de
reference. Il est charge au demarrage et son schema est decrit dans
[`docs/MS2_REFERENCE.md`](../../docs/MS2_REFERENCE.md). Il ne doit pas etre
versionne sans accord explicite sur la diffusion de la bibliotheque utilisee.
