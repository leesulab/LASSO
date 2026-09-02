# Niveau 3 de mobilite : CCS attendu vers DT observe

## Statut actuel

Le niveau 3 signifie : `m/z + RT + preuve de mobilite`. Les Parquet bruts ne
doivent pas recevoir une colonne CCS calculee pour chaque signal : ce calcul
serait couteux et inclurait du bruit de fond et des fragments. La voie retenue
est donc :

```text
CCS attendu de la molecule + m/z attendu + C1/C2 du fichier -> DT attendu
DT attendu compare au DT observe du pic deja valide en m/z et RT
```

Le code calcule deja cette comparaison comme information exploratoire. Elle est
exportee dans `expected_dt_from_ccs`, `ccs_to_dt_match` et
`ccs_to_dt_status`, mais ne modifie pas encore le niveau de confiance. Cette
separation est volontaire tant que la formule et la tolerance n'ont pas ete
validees par l'equipe analytique.

## Donnees necessaires

| Donnee | Emplacement | Unite / regle |
| --- | --- | --- |
| CCS attendu | colonne `ccs` de `compounds_reference.csv` | A2, valeur positive pour chaque molecule concernee. |
| m/z attendu | colonne `mz` de `compounds_reference.csv` | sans unite, valeur positive. |
| DT observe | colonne `dt` du Parquet | ms. |
| C1, C2 de calibration | `metadata_index.csv` | un jeu de coefficients par fichier ou sequence, sans unite a confirmer avec la formule. |

`scripts/build_metadata_index.R` cree deja les colonnes
`has_ccs_calibration`, `ccs_calibration_c1` et `ccs_calibration_c2`, mais les
deux coefficients sont actuellement a `NA`. Lorsque l'equipe fournira un JSON
contenant C1 et C2, modifier ce script a la construction de la ligne d'index,
pres des champs `has_ccs_calibration`, afin d'extraire les vraies cles JSON.
Ne pas deviner le chemin JSON : utiliser un exemple representatif fourni par
l'instrument ou arcMS.

## Fonction a fournir dans arcMS

L'application cherche une fonction exportee par le package R `arcMS` nommee
`convert_ccs_to_drifttime`. Sa signature est strictement :

```r
convert_ccs_to_drifttime <- function(ccs, mz, calibration_parameters) {
  # Appeler ici la formule arcMS validee, sans reconstruire une formule locale.
  # Retourner un nombre en ms, ou list(dt = <nombre en ms>).
}
```

Contrat :

```text
ccs                    scalaire, en A2
mz                     scalaire, sans unite
calibration_parameters liste nommee list(C1 = ..., C2 = ...)
retour                 DT attendu positif, en ms, ou list(dt = ...)
```

La fonction ne doit pas appliquer de tolerance et ne doit pas lire un Parquet.
Elle effectue uniquement la conversion scientifique. `C1` et `C2` peuvent etre
lus sans tenir compte de la casse par l'adaptateur, mais le package doit les
nommer ainsi dans sa sortie ou sa documentation.

Si arcMS conserve un autre nom public, ne dupliquer ni la formule ni le moteur
de screening. Adapter uniquement `arcms_ccs_to_drifttime_converter()` dans
`app/app.R` pour retourner une fonction respectant ce contrat.

## Chemin d'execution deja en place

1. `app/app.R` lit C1/C2 de l'index via `ccs_calibration_for_file()`.
2. `app/app.R` obtient le convertisseur arcMS avec
   `arcms_ccs_to_drifttime_converter()`.
3. Le fichier et chaque molecule sont passes a
   `screen_compounds_in_file()` dans `scripts/parquet_chromatograms.R`.
4. `screen_compound_from_eic()` appelle
   `resolve_expected_drifttime_from_ccs()` seulement apres validation du m/z et
   de la RT.
5. `scripts/ccs_drift_time.R` valide C1/C2, appelle le convertisseur et
   retourne le DT attendu.
6. Le moteur compare DT attendu et DT observe avec `dt_tolerance_pct`.

Cette organisation est importante : la conversion est lancee seulement pour un
pic deja plausible, donc elle ne ralentit pas une analyse de lot avec des
millions de signaux.

## Activation scientifique du niveau 3

Ne faire cette etape qu'apres validation de la formule arcMS, des coefficients
JSON, de l'unite du DT et de la tolerance de mobilite par l'encadrant.

Dans `screen_compound_from_eic()` de
`scripts/parquet_chromatograms.R`, la decision actuelle est volontairement :

```r
} else if (isTRUE(ccs_match)) {
  3L
}
```

Pour rendre la conversion CCS vers DT eligible au niveau 3 tout en conservant
la compatibilite avec un Parquet qui possederait deja une colonne `ccs`, la
condition a valider est :

```r
} else if (isTRUE(ccs_match) || isTRUE(ccs_to_dt_match)) {
  3L
}
```

Il faut ensuite remplacer dans `tests/test_chromatograms.R` l'attente du cas
synthetique calibre : son `confidence_level` doit passer de `2L` a `3L`. Ajouter
aussi des tests pour C1/C2 absents, DT absent, DT hors tolerance et une
conversion qui leve une erreur. Le test ne doit utiliser aucune formule
analytique inventee : il doit injecter un convertisseur synthetique simple.

## Validation avant mise en production

1. Fournir un exemple de JSON avec C1/C2 et un petit Parquet correspondant.
2. Verifier que `metadata_index.csv` contient C1/C2 et que `Controle` les
   affiche comme disponibles.
3. Verifier les colonnes exportees `expected_dt_from_ccs`,
   `ccs_to_dt_error_pct` et `ccs_to_dt_status` sur des etalons connus.
4. Comparer les decisions avec la procedure analytique de reference.
5. Faire valider la tolerance DT et le comportement en cas de calibration
   absente, puis seulement activer le niveau 3.

Tant que cette validation n'est pas terminee, `ccs_to_dt_match` doit rester une
information exploratoire et le niveau 3 ne doit pas etre utilise pour conclure
a une identification.
