# Donnees traitees

Emplacement pour les fichiers generes apres nettoyage, filtrage, aggregation ou preparation pour l'application.

Fichiers generes actuellement :

```text
metadata_index.csv
compounds_reference.csv
ms2_reference_spectra.csv (facultatif)
```

`metadata_index.csv` est genere par :

```bash
Rscript scripts/build_metadata_index.R
```

`compounds_reference.csv` est genere par :

```bash
Rscript scripts/build_compounds_reference.R
```

`ms2_reference_spectra.csv` est un fichier local facultatif de spectres MS2 de
reference. Il est charge au demarrage et son schema est decrit dans
[`docs/MS2_REFERENCE.md`](../../docs/MS2_REFERENCE.md). Il ne doit pas etre
versionne sans accord explicite sur la diffusion de la bibliotheque utilisee.
