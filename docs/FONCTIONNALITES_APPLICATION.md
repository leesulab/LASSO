# Fonctionnalites de l'application Observatoire HRMS

## Objectif

L'application R/Shiny permet d'explorer et d'analyser les donnees de
spectrometrie de masse haute resolution de l'Observatoire de la Ville. Elle
travaille sur les fichiers bruts au format Parquet et leurs metadonnees JSON.

Elle couvre actuellement quatre usages principaux :

1. reperer, decrire et visualiser les fichiers analytiques ;
2. rechercher des etalons internes ou des molecules suspectes ;
3. comparer les resultats entre injections, duplicats et periodes ;
4. controler la coherence des donnees avant une analyse plus large.

## Sources de donnees

### Donnees locales

- lecture d'un dossier local ou d'une cle USB par la variable `DATA_PATH` ;
- lecture directe des fichiers Parquet, sans copie dans le projet ;
- indexation des fichiers JSON associes pour recuperer annee, mois, mode,
  blanc, duplicat et injection ;
- association Parquet/JSON faite avec le chemin relatif complet
  `annee/mode/fichier`, afin de ne pas confondre deux fichiers de meme nom.

### Donnees Nextcloud

- connexion par lien de partage public ou par compte Nextcloud/WebDAV ;
- navigation dans les dossiers distants ;
- ajout de fichiers distants au catalogue de la session ;
- lecture distante avec DuckDB lorsque le fichier est accessible par HTTP ;
- identifiants et mots de passe conserves uniquement pendant la session
  Shiny, sans export dans les CSV.

## Catalogue

L'onglet `Catalogue` sert a reperer les donnees disponibles avant de lancer
une analyse.

- liste des Parquet locaux et distants indexes ;
- filtres par annee, mode `pos/neg`, mois, duplicat, injection et type de
  fichier ;
- distinction entre echantillons et blancs ;
- selection de fichiers reutilisable dans les autres onglets ;
- affichage de la repartition des fichiers selectionnes ;
- indication de l'association avec les metadonnees JSON.

## Etalons internes

L'onglet `Etalons internes` gere les molecules ajoutees systematiquement aux
echantillons pour suivre la stabilite analytique et servir de reference de
normalisation.

- liste initiale fournie par le laboratoire ;
- filtres par nom et par mode ;
- ajout manuel d'un etalon ;
- import et export CSV ;
- ajout a la selection de screening ;
- suppression des ajouts realises pendant la session.

Les informations utilisables sont le nom, le mode, le `m/z` et, si elles sont
connues, la RT, le DT et le CCS.

## Suspects

L'onglet `Suspects` gere une liste distincte de molecules recherchees en mode
suspect.

- ajout manuel ou import CSV ;
- export de la liste ;
- ajout a la selection de screening ;
- creation de suspects de test a partir des etalons selectionnes.

Un suspect peut etre defini avec son `m/z` seulement. Une RT connue permet
d'obtenir une confiance plus forte lors du screening. La liste officielle de
suspects et les criteres analytiques finaux restent a definir avec l'equipe
analytique.

## Inspection d'un fichier Parquet

L'onglet `Parquet` est destine a l'exploration d'un fichier a la fois.

- choix d'un Parquet local ou Nextcloud ;
- lecture du schema, du nombre de lignes, des niveaux MS et d'un apercu ;
- controle des colonnes requises : `rt`, `scanid`, `mslevel`, `mz`,
  `intensity` ;
- calcul et affichage du TIC ;
- calcul et affichage du BPI ;
- extraction et affichage d'un EIC pour un `m/z` cible ;
- selection possible d'un etalon pour remplir les parametres de l'EIC ;
- mode EIC rapide : recherche libre en MS1 a partir d'un `m/z` et de sa
  tolerance, sans filtre RT ; un etalon ou suspect peut seulement pre-remplir
  la masse, puis l'EIC est affiche avec un statut exploratoire de presence ou
  d'absence de signal ;
- resume numerique de l'EIC dans la fenetre de RT ;
- affichage d'un spectre MS2 brut pour la molecule selectionnee : les signaux
  MS2 dans sa fenetre RT sont regroupes par `m/z`, puis les pics les plus
  intenses sont affiches ;
- import facultatif d'un spectre de reference MS2 et comparaison explicite des
  fragments, avec couverture et score cosinus exploratoires ;
- screening du fichier courant contre la selection de molecules compatible
  avec son mode ;
- export des resultats du screening du fichier.

Les panneaux de resultats peuvent etre agrandis pour inspecter les tableaux et
les graphes sur une page longue.

Les graphes TIC, BPI, EIC et MS2 sont interactifs : le survol affiche les
coordonnees exactes, la molette ou la selection d'une zone permet de zoomer,
et la barre d'outils permet notamment de revenir a l'echelle initiale. Les
principaux maxima locaux sont marques par un triangle orange et les minima
locaux par un triangle bleu pointe vers le bas.

## Screening de lot

L'onglet `Plan screening` permet de traiter plusieurs fichiers et plusieurs
molecules en une operation.

- selection de Parquet locaux et Nextcloud dans une meme liste ;
- affichage du plan `fichier x molecule` avant execution ;
- exclusion automatique des combinaisons incompatibles avec le mode `pos/neg` ;
- parametres de recherche : niveau MS, tolerance `m/z`, tolerance RT,
  intensite minimale et controles de mobilite ;
- option de calcul de l'intensite totale MS pour une normalisation ulterieure ;
- traitement sequentiel des fichiers pour limiter la memoire ;
- poursuite du lot si un fichier echoue, avec une ligne d'erreur explicite ;
- tableau colore des resultats avec filtres, statut et preuve de screening ;
- export CSV du lot ;
- reimport d'un export precedent sans relire les Parquet ;
- envoi d'une ligne de resultat vers `Parquet` pour preparer son EIC ;
- envoi d'une ligne de resultat vers `Suivi molecules`.

### Statut et preuve de screening

Pour chaque molecule, le moteur extrait les points dans la fenetre `m/z`, puis
verifie la RT lorsque celle-ci est renseignee.

| Preuve | Signification |
| --- | --- |
| 0 | aucun signal exploitable |
| 1 | signal coherent en `m/z` uniquement |
| 2 | signal coherent en `m/z + RT` |
| 3 | signal coherent en `m/z + RT + mobilite` |

Par defaut, `Detected` exige la preuve 2 et donc un signal suffisant dans la
fenetre de RT. Le statut n'est pas une identification chimique definitive : il
indique qu'un signal est compatible avec les criteres choisis.

Le controle DT est actuellement exploratoire. La future verification de
mobilite devra convertir un CCS attendu en DT attendu a partir du `m/z` et des
parametres de calibration `C1/C2`, puis comparer ce DT au pic observe.

La comparaison MS2 est realisee dans l'onglet `Parquet` pour une molecule et
un fichier a la fois. Elle ne modifie pas les preuves de screening tant qu'une
regle scientifique validee n'a pas ete definie. Voir
[Comparaison MS2](MS2_REFERENCE.md).

## Suivi des molecules dans le temps

L'onglet `Suivi molecules` reutilise le dernier resultat de screening de lot
pour comparer les mesures entre echantillons, injections et duplicats.

- choix de la molecule et du mode ;
- choix de la mesure : somme des intensites EIC dans la fenetre RT ou intensite
  maximale de cette fenetre ;
- affichage du signal brut ou corrige du blanc ;
- choix manuel d'un blanc de reference du meme mode ;
- normalisation par un etalon interne choisi ;
- normalisation automatique par l'etalon interne du meme mode le plus proche
  en RT ;
- normalisation par l'intensite totale du niveau MS, lorsqu'elle a ete calculee
  pendant le screening ;
- affichage lineaire ou logarithmique ;
- filtrage des statuts affiches ;
- conservation ou masquage visuel des blancs ;
- export de la vue et des valeurs detaillees.

### Blanc et normalisation

La correction du blanc applique :

```text
signal corrige = signal brut - signal du blanc selectionne
```

Les valeurs negatives sont conservees, car elles indiquent que le signal du
blanc est superieur a celui de l'echantillon.

La normalisation peut ensuite appliquer :

```text
signal normalise = signal corrige / signal de reference
```

Le signal de reference peut etre celui d'un etalon interne ou l'intensite totale
MS du meme fichier.

### Injections et duplicats

L'application propose, successivement pour les trois injections puis pour les
duplicats A/B ou C/D :

- affichage separe ;
- moyenne ;
- mediane ;
- exclusion optionnelle de valeurs atypiques par la regle robuste MAD.

Les valeurs individuelles, les valeurs retenues, les exclusions eventuelles et
la variabilite des duplicats sont conserves dans les tableaux et les exports.

## Controle des donnees

L'onglet `Controle` aide a detecter les problemes avant une analyse etendue.

- verification globale de l'index JSON, du catalogue et de la liste de
  molecules ;
- diagnostic par fichier : mode, JSON associe, annee, mois, type, duplicat et
  injection ;
- statut `OK`, `A verifier` ou `A corriger` ;
- analyse a la demande d'un Parquet : schema, colonnes requises, niveaux MS,
  nombre de lignes et colonnes optionnelles `dt` et `ccs`.

Cet onglet ne calcule pas automatiquement TIC, BPI ou screening : il sert a
controler les donnees avec un cout de calcul limite.

## Fichiers d'entree et de sortie

| Element | Role |
| --- | --- |
| `*.parquet` | signaux de spectrometrie de masse bruts |
| `*-metadata.json` | metadonnees d'acquisition et d'echantillon |
| `data/processed/metadata_index.csv` | index local construit a partir des JSON |
| `data/processed/compounds_reference.csv` | liste initiale des etalons internes |
| `screening_lot_*.csv` | resultat de screening exportable et reimportable |

## Limites actuelles

- La somme EIC `rt_area_sum` est une somme discrete d'intensites par scan ; ce
  n'est pas encore une integration chromatographique continue en fonction du
  temps.
- Les resultats `Detected` sont une aide au screening et ne remplacent pas la
  validation analytique d'une molecule.
- Le spectre MS2 affiche est une visualisation brute de tous les signaux MS2 de
  la fenetre RT. Il ne compare pas encore les fragments a un spectre de
  reference et ne modifie donc pas le niveau de confiance du screening.
- La codification scientifique finale des niveaux de fiabilite devra etre
  fixee a partir d'une publication ou d'une recommandation retenue par
  l'equipe, avec la base de spectres de reference, les tolerances sur les
  fragments et une regle de comparaison explicites.
- Le niveau 3 depend d'une verification de mobilite validee ; les Parquet bruts
  actuels ne contiennent pas de colonne CCS exploitable pour tous les signaux.
- La normalisation et l'agregation sont implementables avec plusieurs methodes,
  mais leur choix final doit etre valide par l'equipe analytique.

## Parcours recommande

1. Indexer les JSON et ouvrir `Catalogue` pour verifier les fichiers.
2. Verifier les etalons internes, puis ajouter ou importer des suspects si
   necessaire.
3. Inspecter un Parquet dans l'onglet `Parquet` avec TIC, BPI et un EIC.
4. Selectionner les fichiers et molecules dans `Plan screening`, puis lancer le
   lot.
5. Exporter le lot ou ouvrir `Suivi molecules` pour comparer les injections,
   duplicats et periodes.
6. Consulter `Controle` avant de generaliser l'analyse a davantage de donnees.

Pour l'installation et les commandes de lancement, voir
[`UTILISATION.md`](UTILISATION.md).
