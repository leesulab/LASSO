# Donnees de reference

Ce dossier sert a ranger les petits fichiers fournis par l'encadrant qui decrivent ce qu'il faut rechercher dans les donnees.

Exemples :

- liste des etalons internes ;
- liste de molecules suspectees ;
- tables de parametres de recherche : `m/z`, `RT`, `DT`, `CCS`, SMILES, etc.

Le fichier CSV des etalons internes peut etre place ici, par exemple :

```text
data/reference/etalons_internes.csv
```

Ces fichiers ne sont pas des fichiers Parquet de mesure. Ils servent plutot d'entree metier pour interroger les fichiers Parquet.

Par defaut, les fichiers de ce dossier ne sont pas versionnes afin d'eviter de publier des donnees fournies par l'encadrant.

