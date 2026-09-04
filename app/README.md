# Application

Ce dossier accueillera le code de l'application web.

La stack retenue pour le projet est R/Shiny, coherent avec les outils `arcMS`, `arcms-dataviz` et `LASSO`.

## Prototype actuel

Fichier principal :

```text
app/app.R
```

Ce prototype lit :

```text
data/processed/metadata_index.csv
data/processed/compounds_reference.csv
data/processed/ms2_reference_spectra.csv (facultatif)
```

Il affiche :

- le catalogue des fichiers indexes ;
- la liste fournie des etalons internes et une liste distincte de suspects, toutes deux completables manuellement ou par import CSV ;
- une selection de travail commune pour choisir les fichiers, les etalons internes et les suspects ;
- un onglet `Nextcloud` pour parcourir un partage public ou un compte WebDAV et ajouter des Parquet distants au catalogue de session ;
- un onglet `Parquet` qui liste les fichiers accessibles via `DATA_PATH` et lit leurs informations avec `arrow` ;
- un plan de screening `fichier x molecule` avec une selection unifiee de Parquet locaux et Nextcloud ;
- un screening de lot reel, avec resultats enrichis des metadonnees du fichier, export CSV et reimport d'un export precedent ;
- un onglet `Suivi molecules` pour comparer les valeurs brutes, corrigees ou normalisees d'un etalon interne ou d'un suspect entre les fichiers d'un lot, puis agreger les injections et duplicats ;
- un onglet `Controle` avec des controles globaux, un diagnostic des metadonnees de chaque Parquet et une analyse a la demande du schema d'un fichier.

Les principaux tableaux et graphiques sont dans des panneaux agrandissables, pour garder un affichage compact puis passer en grand format quand il faut inspecter les donnees.

L'onglet `Parquet` permet d'inspecter et de screener un fichier courant. L'onglet `Plan screening` permet de selectionner plusieurs fichiers locaux et Nextcloud, de verifier les requetes `fichier x molecule`, puis de lancer le screening de tout le lot. Les fichiers sont traites sequentiellement pour limiter la memoire utilisee et pour conserver une erreur eventuelle sur un fichier sans bloquer les suivants. Les resultats sont exportables en CSV et conservent notamment le fichier, son origine, le mode, la date, le duplicat et le numero d'echantillon lorsque ces metadonnees sont disponibles.

Un export `screening_lot_*.csv` peut etre reimporte dans `Plan screening`. Cela restaure les resultats et les vues de suivi sans relire les Parquet. Les chemins et identifiants Nextcloud ne sont pas restaures comme une session de connexion : l'import sert a analyser un lot deja calcule, pas a relancer ce lot.

Dans les resultats du lot, une ligne peut etre selectionnee puis envoyee vers `Parquet` avec `Preparer EIC`. L'application prepare alors le fichier, le mode, le niveau MS, le m/z, la RT et les tolerances utilises pour cette recherche. Le calcul reste volontairement declenche par l'utilisateur avec `Lire les infos`, puis `Calculer EIC` ou `Afficher spectre MS2`. Un spectre MS2 brut peut ensuite etre compare a une reference importee, fragment par fragment. Cette comparaison reste exploratoire, ne modifie pas le resultat de screening et ne constitue pas une identification a elle seule. Pour un export reimporte, le Parquet correspondant doit de nouveau etre disponible dans le catalogue local ou Nextcloud.

La meme ligne peut etre envoyee vers `Suivi molecules` avec `Suivre molecule`. Cette vue filtre directement le lot sur la molecule et le mode concernes, afin de comparer les echantillons, blancs, injections et duplicats. Elle reste utilisable apres reimport d'un export CSV, car elle utilise les resultats deja calcules.

Le moteur applique la meme recherche aux etalons internes et aux suspects. Il ne remplace pas une methode de screening suspect validee scientifiquement : la liste officielle et les criteres analytiques finaux restent a definir avec l'equipe analytique.

## Controle des donnees

L'onglet `Controle` ne relit pas tous les fichiers du catalogue. Il verifie immediatement les informations disponibles dans le catalogue et l'index JSON : mode, association JSON, annee, mois, type, duplicat et injection. Un statut `OK`, `A verifier` ou `A corriger` est attribue a chaque fichier.

Pour verifier le contenu d'un Parquet, choisir un fichier puis utiliser `Analyser le fichier`. L'analyse verifie les colonnes requises (`rt`, `scanid`, `mslevel`, `mz`, `intensity`), la presence de MS1, le nombre de lignes et les colonnes optionnelles `dt` et `ccs`. Elle ne lance ni TIC, ni BPI, ni screening.

## Gestion des etalons internes et des suspects

La liste fournie dans `data/processed/compounds_reference.csv` reste disponible par defaut dans l'onglet `Etalons internes`. L'onglet `Suspects` commence vide : il sert a construire ou importer une liste de molecules a rechercher.

Dans chacun de ces onglets, il est possible d'ajouter une molecule manuellement, d'importer un CSV, de l'ajouter a la selection de screening, d'exporter la liste et de supprimer les ajouts de session du type concerne. L'onglet `Suspects` propose aussi `Creer tests depuis etalons selectionnes` : il copie les etalons internes deja selectionnes en suspects nommes `[test] ...`. Cette commande est prevue uniquement pour tester l'application sans fabriquer a la main une fausse liste de suspects ; les copies restent distinctes des etalons internes.

Le CSV doit contenir au minimum les colonnes suivantes :

```text
name;mode;mz
```

Les colonnes `rt`, `dt` et `ccs` sont optionnelles. Les separateurs `;`, `,` et les decimales avec virgule sont acceptes. Les ajouts manuels et CSV sont conserves pour la session Shiny en cours ; exporter la liste permet de les reutiliser dans une nouvelle session. Un suspect sans RT peut etre recherche par masse, mais il ne pourra pas atteindre un niveau de confiance incluant la coherence RT tant que cette valeur n'est pas renseignee.

Lorsqu'un fichier est ajoute depuis `Catalogue`, il est aussi ajoute a la selection de `Plan screening` et le premier fichier accessible est choisi dans l'onglet `Parquet`. Si un fichier est indexe dans `Catalogue` mais n'apparait pas dans ces onglets, son Parquet n'est pas accessible via `DATA_PATH` ni dans la session Nextcloud ; l'application affiche alors un avertissement explicite.

L'onglet `Parquet` ouvre un fichier accessible depuis la machine qui execute Shiny et affiche :

- le nom du dossier de donnees local ;
- les fichiers `.parquet` detectes, avec le lien disponible vers les metadonnees JSON ;
- le mode `pos/neg` detecte depuis le JSON ou le chemin, avec correction manuelle possible si le fichier n'est pas indexe ;
- le schema ;
- le nombre de lignes ;
- les niveaux MS ;
- un apercu de quelques lignes.
- des calculs TIC/BPI ;
- un calcul EIC autour d'un `m/z` cible ou d'un etalon selectionne.
- une recherche EIC rapide en MS1 a partir d'un `m/z` saisi, sans condition de
  RT ; un etalon ou suspect est facultatif et sert seulement a pre-remplir la
  masse.
- une visualisation MS2 brute dans la fenetre RT de la molecule selectionnee,
  avec regroupement des fragments par `m/z`.
- l'import d'une bibliotheque locale de spectres MS2 et une comparaison
  exploratoire : fragments concordants, couverture et score cosinus.
- des graphes TIC, BPI, EIC et MS2 interactifs, avec survol des coordonnees,
  zoom et reperage des principaux extrema locaux sur les chromatogrammes.
- un screening du fichier courant contre les molecules du meme mode, avec niveaux de preuve `m/z`, `m/z + RT` et une preuve de mobilite quand elle est reellement disponible ;
- un export CSV du resultat, avec les parametres de calcul.

Le screening recherche les points dans la fenetre `m/z`, puis dans la fenetre RT quand l'option de coherence RT est activee. Il conserve les preuves observees, meme lorsqu'elles ne suffisent pas a la decision finale : preuve 1 pour `m/z`, preuve 2 avec RT et preuve 3 avec une preuve de mobilite directe. Ces preuves ne sont pas des niveaux d'identification publies.

Par defaut, `Detected` exige la preuve 2, donc un signal dans la fenetre RT et une intensite suffisante. Le controle DT est disponible uniquement a titre exploratoire et ne constitue pas un critere de decision. Si une colonne `ccs` est presente, elle reste utilisable pour compatibilite. Pour les Parquet bruts ordinaires, la future voie est `CCS attendu + m/z + C1/C2 -> DT attendu`, puis comparaison avec le DT du pic. Le branchement est prepare mais ne sera pas utilise comme preuve de mobilite avant validation de la formule et de la tolerance.

Le format CSV de reference MS2, les scores affiches et les limites de cette comparaison sont documentes dans [`docs/MS2_REFERENCE.md`](../docs/MS2_REFERENCE.md).

## Suivi des molecules

L'onglet `Suivi molecules` reutilise le dernier resultat de `Plan screening` de la session Shiny. Il permet de choisir :

- une molecule, etalon interne ou suspect, et un mode ;
- la somme discrete des intensites EIC dans la fenetre RT (`rt_area_sum`) ou l'intensite maximale dans cette fenetre (`rt_max_intensity`) ;
- le signal brut ou le signal corrige du blanc selectionne ;
- aucune normalisation, une normalisation par un etalon interne choisi, par l'etalon interne du meme mode le plus proche en RT, ou par l'intensite totale du meme niveau MS ;
- pour les injections puis pour les duplicats : les valeurs separees, la moyenne ou la mediane ;
- une exclusion optionnelle des valeurs atypiques avec la regle robuste MAD et son seuil ;
- une echelle lineaire ou logarithmique pour le graphe ;
- les statuts affiches ;
- un blanc de reference parmi les blancs du meme mode.

La correction applique `signal brut - signal du blanc selectionne` et conserve les valeurs negatives. La normalisation utilise ensuite soit le signal d'un etalon de reference `Detected` dans le meme fichier, soit l'etalon interne du meme mode ayant le RT attendu le plus proche de la molecule suivie, soit l'intensite totale du meme niveau MS. Pour le choix automatique, seuls les etalons internes presents dans le lot sont consideres ; le nom retenu, son RT attendu et l'ecart RT sont conserves dans la table et dans les exports. L'intensite totale n'est calculee pendant le screening de lot que lorsque l'option correspondante est cochee, afin d'eviter une seconde lecture complete de chaque Parquet.

`rt_area_sum` est une somme discrete des intensites EIC par scan dans la fenetre RT. Ce n'est pas encore une integration chromatographique continue selon le temps de retention ; sa convention analytique doit etre validee avant une interpretation quantitative.

L'agregation intervient apres ces traitements, d'abord sur les injections d'un meme `sample_group`, puis sur les duplicats d'une meme periode. Les blancs ne sont jamais agreges. Les moyennes et medianes n'utilisent que les signaux `Detected` finis. L'exclusion MAD est visible dans la table des valeurs individuelles et dans l'export brut ; elle ne peut retirer une valeur que si le groupe contient au moins trois signaux exploitables. Avec les deux duplicats A/B ou C/D, elle ne peut donc pas exclure automatiquement l'un des deux points.

La vue finale, la table des valeurs individuelles et les deux exports CSV conservent la methode, le nombre de valeurs utilisees, le nombre de valeurs exclues et la liste des fichiers sources.

Par defaut, `DATA_PATH` vaut :

```text
data/raw/parquet
```

Sur la machine finale, `DATA_PATH` pourra pointer vers le dossier Nextcloud monte sur le serveur.

## Nextcloud distant

L'onglet `Nextcloud` accepte deux modes d'acces :

- `Lien de partage` : un lien public de la forme `https://nextcloud.example.org/s/jeton` et, si besoin, son jeton ;
- `Compte Nextcloud` : l'URL habituelle de Nextcloud, l'identifiant et un mot de passe d'application WebDAV.

Pour le second mode, une URL de l'interface Fichiers contenant `?dir=/observatoire-db` est acceptee : l'application retrouve automatiquement le dossier de depart `observatoire-db`.

Les fichiers ajoutes apparaissent ensuite dans la liste de l'onglet `Parquet`, avec l'origine `Nextcloud`.

Le mode d'ionisation est aussi lu dans le chemin complet du fichier : un Parquet situe dans un dossier `pos` est traite comme positif et un fichier dans `neg` comme negatif. Cette regle est importante pour les blancs, dont le nom peut etre identique dans les deux modes. Si le JSON associe indique un mode different, le chemin est retenu et l'onglet `Controle` signale la divergence.

Les calculs sur un fichier distant utilisent DuckDB et l'extension `httpfs`; les fichiers locaux gardent le backend Arrow. Le jeton ou l'en-tete d'authentification reste dans la session et ne fait pas partie des tableaux ou de l'export CSV.

Variables facultatives pour pre-remplir les champs :

```bash
NEXTCLOUD_PUBLIC_URL=https://nextcloud.example.org/s/partage
NEXTCLOUD_SHARE_TOKEN=jeton-de-partage
NEXTCLOUD_ACCESS_MODE=account
NEXTCLOUD_USERNAME=identifiant-nextcloud
```

Ne pas inscrire ces valeurs dans Git, et ne pas definir le mot de passe d'application dans le depot. Voir `Doc/note_nextcloud_acces_distant_2026-07-28.md` pour les limites et le choix recommande en production.

`PROJECT_ROOT` peut aussi etre defini pour indiquer explicitement la racine du projet, notamment dans un futur container Docker.

## Securite de production

Le lancement ci-dessous est volontairement limite a `127.0.0.1`. Une exposition reseau devra passer par un reverse proxy HTTPS avec authentification utilisateur.

- definir `APP_ENV=production` ;
- definir `NEXTCLOUD_ALLOWED_HOSTS=nas.example.org` pour restreindre les connexions WebDAV a l'hote attendu ;
- utiliser uniquement HTTPS ; `http://` est refuse sauf si `ALLOW_INSECURE_NEXTCLOUD_HTTP=true` est explicitement defini pour le developpement local ;
- un `NEXTCLOUD_SHARE_TOKEN` configure sur le serveur reste cote R et ne pre-remplit plus le navigateur ;
- installer DuckDB `httpfs` pendant la construction de l'image ou le provisionnement du serveur. L'installation dynamique est desactivee, sauf avec `ALLOW_DUCKDB_EXTENSION_INSTALL=true` pour le developpement local ;
- monter les donnees via `DATA_PATH` en lecture seule et ne jamais monter un repertoire hote plus large que necessaire.

Les exports CSV neutralisent les valeurs textuelles pouvant etre interpretees comme des formules par un tableur.

## Lancement

Depuis la racine du projet :

```bash
Rscript -e 'shiny::runApp("app", host = "127.0.0.1", port = 7660, launch.browser = FALSE)'
```

Avec un chemin de donnees explicite :

```bash
DATA_PATH=/chemin/vers/les/donnees Rscript -e 'shiny::runApp("app", host = "127.0.0.1", port = 7660, launch.browser = FALSE)'
```

`DATA_PATH` peut aussi pointer vers un dossier situe sur une cle USB montee localement. Les Parquet sont alors lus directement depuis la cle, sans copie dans le projet ; elle doit rester branchee pendant toute l'utilisation de l'application.

Puis ouvrir :

```text
http://127.0.0.1:7660
```

Depuis R :

```r
shiny::runApp("app", host = "127.0.0.1", port = 7660, launch.browser = FALSE)
```

## Tests

Depuis la racine du projet :

```bash
Rscript tests/test_chromatograms.R
Rscript tests/test_app_server.R
Rscript tests/test_nextcloud_webdav.R
```
