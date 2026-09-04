# Comparaison MS2 avec un spectre de reference

## Objectif

L'onglet `Parquet` peut comparer un spectre MS2 observe avec un spectre de
reference fourni par l'utilisateur. Cette fonction sert a examiner les
fragments compatibles avec une molecule choisie.

Le resultat est une **preuve technique exploratoire**. Il ne confirme pas a lui
seul l'identite d'une molecule et ne modifie ni le statut `Detected`, ni les
preuves de screening `m/z + RT + mobilite`.

## Ce que calcule l'application

Pour chaque fragment attendu du spectre de reference, l'application cherche au
plus un fragment observe dans la tolerance `m/z` choisie. Elle affiche :

- le nombre de fragments de reference et le nombre de fragments concordants ;
- la couverture simple et la couverture ponderee par l'intensite relative de la
  reference ;
- un score cosinus calcule sur les fragments de reference ;
- la fraction du signal MS2 observe expliquee par les fragments concordants ;
- le detail des fragments observes, absents et de leur erreur de masse.

Sur le graphe MS2, un cercle vert entoure un fragment de reference concordant.
Une croix rouge au niveau zero indique un fragment attendu mais non observe.

Le score cosinus est restreint aux fragments de reference : ce n'est pas une
recherche complete dans une bibliotheque spectrale et il ne penalise pas a lui
seul tous les signaux supplementaires du spectre observe.

## Format du CSV de reference

Dans l'onglet `Parquet`, telecharger `Modele CSV MS2`, le remplir, puis utiliser
`Importer spectres MS2 CSV`. Les separateurs `;` et `,` sont acceptes. Avec un
separateur `;`, les decimales avec virgule sont acceptees.

Colonnes obligatoires :

| Colonne | Description |
| --- | --- |
| `compound_name` | Nom de la molecule. |
| `fragment_mz` | Masse `m/z` d'un fragment de reference. |
| `relative_intensity` | Intensite relative positive du fragment. |

Colonnes recommandees :

| Colonne | Description |
| --- | --- |
| `reference_id` | Identifiant commun a toutes les lignes d'un meme spectre. |
| `mode` | `pos` ou `neg`. |
| `precursor_mz` | Masse du precurseur/adduit associe au spectre. |
| `collision_energy` | Energie de collision ou methode d'acquisition. |
| `source` | Nom de la bibliotheque ou de la mesure de reference. |
| `library_accession` | Identifiant stable dans la bibliotheque source. |

Exemple :

```csv
reference_id;compound_name;mode;precursor_mz;collision_energy;fragment_mz;relative_intensity;source;library_accession
compound_x_pos_20ev;compound-x;pos;200,1234;20 eV;50,001;100;Bibliotheque validee;REF-001
compound_x_pos_20ev;compound-x;pos;200,1234;20 eV;75,002;48;Bibliotheque validee;REF-001
compound_x_pos_20ev;compound-x;pos;200,1234;20 eV;120,005;21;Bibliotheque validee;REF-001
```

Toutes les lignes portant le meme `reference_id` decrivent un seul spectre. Les
intensites sont renormalisees par l'application entre 0 et 100 pour ce spectre.

Un fichier local facultatif peut aussi etre place dans :

```text
data/processed/ms2_reference_spectra.csv
```

Il est charge au demarrage de l'application, reste ignore par Git et peut etre
remplace dans une session par un import CSV.

## Parcours utilisateur

1. Choisir un Parquet et une molecule dans l'onglet `Parquet`.
2. Verifier son EIC et sa fenetre RT.
3. Cliquer sur `Afficher spectre MS2`.
4. Importer ou selectionner le spectre MS2 de reference correspondant.
5. Regler les seuils exploratoires : tolerance fragments, nombre minimal de
   fragments et score cosinus minimal.
6. Cliquer sur `Comparer au spectre de reference` et examiner le tableau ainsi
   que les marqueurs vert et rouge du graphe.

Les seuils proposes par defaut (`0.01 Da`, `3` fragments, score `0.70`) sont des
valeurs de travail pour explorer l'interface. Ils ne sont pas valides
scientifiquement pour conclure a une identification.

## Pourquoi cela ne suffit pas a identifier une molecule

La comparaison devient difficile a interpreter automatiquement pour plusieurs
raisons :

- les Parquet actuels agregeent les signaux MS2 de toute la fenetre RT ; un pic
  peut donc contenir des fragments de molecules coeluees ;
- le fichier brut ne rattache pas necessairement chaque fragment MS2 a un
  precurseur isole ;
- un spectre de reference doit etre compatible avec le mode, l'adduit, l'energie
  de collision, l'instrument et la methode chromatographique ;
- une bibliotheque publique ou commerciale peut imposer des conditions d'acces,
  des licences ou une API ;
- les fragments, le score et les seuils requis dependent du protocole
  analytique retenu.

La fonction est donc volontairement limitee a une comparaison explicite sur un
fichier. Elle n'est pas executee automatiquement pour chaque molecule de chaque
fichier d'un lot : cela multiplierait les lectures de gros Parquet et
produirait des conclusions non validees si la bibliotheque ou les seuils sont
incomplets.

## Lien avec les niveaux publies

Les colonnes actuelles `confidence_level` et `confidence_label` de l'application
decrivent des preuves de screening internes, pas un niveau d'identification
publie. Elles sont maintenant affichees comme `Preuve 0` a `Preuve 3` pour
eviter la confusion.

Avant de creer une colonne d'identification fondee sur MS2, l'equipe doit
choisir et documenter un cadre de publication, par exemple celui propose par
Schymanski et al. pour communiquer la confiance en HRMS :
[Schymanski et al., 2014](https://pubs.acs.org/doi/10.1021/es5002105).

Il faut ensuite valider avec l'encadrant :

1. la bibliotheque de reference autorisee et sa provenance ;
2. les adducts et energies de collision acceptes ;
3. les fragments diagnostiques et les seuils de similarite ;
4. la regle exacte qui transforme ces elements en niveau publie ;
5. les cas necessitant une verification par etalon authentique.

Apres ces decisions, une extension de `Plan screening` pourra calculer cette
preuve MS2 de facon controlee pour un lot entier et exporter les parametres,
les fragments concordants et la regle appliquee.
