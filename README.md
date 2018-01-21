## TX
# Dynamic Mobility 
From GPS data, the goal is to determine with high accuracy the transportation modes of a user for each of his trips.

# Organisation du Git

Le repo Git est composé de deux dossiers: data et function.

Le dossier data contient les données utilisées dans le cadre du projet.

Les fonctions qui sont utilisées de manière répétée dans le code sont dans le dossier function.

Dans la racine du repository il y a les quatre fichiers R qui constituent l'essentiel du travail réalisé.

## Changer les données utilisées
Pour remplacer les données par de nouvelles données il faut remplacer le fichier dataset.JSON par le nouveau fichier souhaité dans le même format (format de sortie de l'API de l'appli Mobilité DynAMIque.

## Lancer le code

Télécharger le dossier TX. 

Le code est contenu dans la racine du dossier TX.

Lancer les codes via R studio il faut lancer le premier fichier en premier car c'est à partir de la sortie de ce fichier qu'est faite la classification. 

Si des messages d'erreurs s'affichent il faut telecharger les packages manquants. 

Voici les 4 fichiers R que vous devez lancer.

## Step_1_building_final_dataframe.R
Construction des données depuis les données brutes de l'application.

C'est dans ce fichier que l'on calcul les differentes variables explicatives à savoir la vitesse, l'acceleration, sinuosity et turn angle. 
Ainsi que toutes les variables associées à ces quatres variables comme la moyenne par voyage, l'ecart type,  la médiane et bien d'autre.



