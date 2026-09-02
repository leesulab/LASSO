# Donnees brutes

Emplacement pour les donnees originales : exports instrumentaux, fichiers fournis par l'encadrant, fichiers non modifies.

Attention : ne pas supposer que le nom de fichier seul est unique.

Un meme echantillon peut exister en mode positif et negatif avec le meme nom de base.
Pour eviter les conflits, conserver si possible l'organisation du serveur :

```text
data/raw/json/<annee>/pos/<fichier-metadata.json>
data/raw/json/<annee>/neg/<fichier-metadata.json>
```

ou ajouter explicitement le mode dans le nom local :

```text
<sampleName>-pos-metadata.json
<sampleName>-neg-metadata.json
```
