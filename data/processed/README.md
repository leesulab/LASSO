# Donnees traitees

Emplacement pour les fichiers generes apres nettoyage, filtrage, aggregation ou preparation pour l'application.

Fichiers generes actuellement :

```text
metadata_index.csv
compounds_reference.csv
```

`metadata_index.csv` est genere par :

```bash
Rscript scripts/build_metadata_index.R
```

`compounds_reference.csv` est genere par :

```bash
Rscript scripts/build_compounds_reference.R
```
