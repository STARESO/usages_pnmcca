---
output:
  html_document: default
  pdf_document: default
---
# PNMCCA - Observatoire des Usages : Modèles de Comptage Terrain

Ce dossier contient les éléments de référence pour les comptages terrain des divers thèmes de l'observatoire des usages.

## Contenu du dossier :

### 1. Le fichier *reference_comptage_terrain_donnees_brutes.xlsx*

Ce fichier contient les informations des métadonnées des jeux de données de comptage bruts (i.e., saisis par les agents de terrain puis intégrés dans les tableurs de référence). Les diverses informations de ces métadonnées sont :

-   **Champ** : le nom du champ. Attention, en cas de tableau à double champ, les deux champs sont séparés dans deux colonnes de description. Cette configuration est nécessaire pour la vérification des données par les scripts R.

-   **Description** : la description de la variable considérée.

-   **Format** : le format de la variable. Les modalités principales sont : entier, texte, qualitative ordinale, qualitative nominale, date, temps. Ces formats sont utilisés par les scripts R de vérification et de compilation des données.

-   **Unité** : l'unité de la variable considérée, si celle-ci existe.

-   **Fourchette de valeur / Modalités** : selon le type de variable, cette colonne contient la fourchette de valeur que peut prendre une variable quantitative ou les modalités d'une variable qualitative nominale ou ordinale.

-   **Commentaires** : divers commentaires à propos du type de variable ou de son acquisition.

### 2. Les fichiers modèles de chaque type de comptage

Les noms de ces fichiers suivent un format standardisé inspiré du modèle des données spatialisées de l'OFB. Ils se présentent sous la forme :\
`us_med_pnmcca_observatoire_comptage_terrain_type_de_comptage_AAAA_MM_JJ.xlsx`, où `AAAA-MM-JJ` représente la date au format adapté pour les analyses de données. À chaque remplissage de données, il est nécessaire de vérifier que la date du nom de fichier a bien été changée.

Ces fichiers comportent des onglets structurés de manière similaire (à l'exception du fichier météo).

-   **Premier onglet** :
    -   *metadata_comptages* : contient les informations générales des comptages (secteur, date, observateurs, commentaires généraux). Il sert de référence pour les autres onglets.
-   **Autres onglets** :
    -   Chaque onglet correspond à un nom de secteur et contient un tableau modèle de chaque type de comptage à remplir avec les données de terrain.

### 3. Le fichier des données météo

Le fichier des données météo diffère légèrement. Il ne contient qu’un seul onglet dans lequel l’ensemble des données météo d’une journée donnée doit être complété. Il contient les informations de tous les secteurs prospectés.Par ailleurs, les cases de données peuvent être complétées assez facilement pour certaines variables aux modalités fixes grâce aux listes déroulantes.

## Fichiers modèles existants :

-   **Activités de loisir** :\
    `us_med_pnmcca_observatoire_comptage_terrain_activites_loisirs_AAAA_MM_JJ.xlsx`

-   **Météo** :\
    `us_med_pnmcca_observatoire_comptage_terrain_meteo_AAAA_MM_JJ.xlsx`

-   **Usagers des plages** :\
    `us_med_pnmcca_observatoire_comptage_terrain_plage_AAAA_MM_JJ.xlsx`

-   **Navires de plaisance (bateaux à voile et à moteur)** :\
    `us_med_pnmcca_observatoire_comptage_terrain_plaisance_AAAA_MM_JJ.xlsx`
