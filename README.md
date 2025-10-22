# Observatoire des Usages : bundle terrain

## Contexte

Ce projet R fait partie de l'écosystème d'analyses des données de l'observatoire des usages du **PNMCCA**, le Parc Naturel Marin du Cap Corse et de l'Agriate. Le rôle de l'**observatoire des usages** consiste à suivre de multiples activités au sein de l'emprise spatiale du Parc en les caractérisants et en faisant l'acquisition de données d'intérêt.

Les grandes catégories d'activités sont :

-   la plaisance en mer (bateaux voiliers et moteurs)

-   les loisirs sur la plage (baignade, bronzage, etc.)

-   la pêche de loisir (du bord, embarquée, sous-marine, etc.)

-   la plongée sous-marine (PMT, Scaphandre, apnée, etc.)

-   les sports motonautiques (ski-nautique, jet-ski, bouées tractées, foil electrique etc.)

-   les sports non motorisés (kayak, paddle, planche à voile, surf, etc.)

-   les transports de passagers et promenades en mer (navettes, excursions, ferries, etc.)

Ce bundle de scripts R permet de traiter les données de terrain collectées par les agents de l'observatoire des usages. Les données collectées sur le terrain correspondent principalement à des comptages de bateaux, de personnes, de véhicules, *etc*. Ces données sont ensuite utilisées pour alimenter les analyses de l'observatoire des usages. Le bundle permet de **vérifier la cohérence** des données collectées sur le terrain, de les **nettoyer**, de les **transformer** et de les **reformater** pour les rendre exploitables.

## Fonctionnement du bundle terrain

Le bundle terrain est composé d'un **script principal** de traitement, diverses **fonctions personnalisées** réalisant chacune des opérations spécifiques du script principal et des **données** **brutes** fournies par l'utilisateur.

Le projet est structuré de la manière suivante :

```         
.
├── data
│   ├── processed
│   └── raw
│       ├── cartographie
│       └── comptages_terrain
│           ├── activites_loisirs
│           ├── debarquements
│           ├── frequentation_terrestre
│           ├── meteo
│           ├── modeles
│           │   ├── README.html
│           │   ├── README.md
│           │   └── reference_comptage_terrain_donnees_brutes.xlsx
│           ├── plage
│           └── plaisance
├── dev
├── logs
├── odu_bundle_terrain.Rproj
├── outputs
│   └── figures
├── R
│   ├── fct_column_format.R
│   ├── fct_compilation_comptage.R
│   ├── fct_conversions.R
│   ├── fct_double_header.R
│   ├── fct_file_checking.R
│   ├── fct_post_compilation.R
│   ├── fct_read_metadata.R
│   ├── fct_sectors.R
│   ├── fct_sheet_header.R
│   └── paths.R
├── README.md
└── main
    ├── odu_compilation_compage_terrain.R
    ├── odu_extraction_bdd_mer.R
    └── odu_plotting_boats_test.R
```

Les fonctions personnalisées sont stockées dans le dossier `R/` à la racine du projet. Le script principal *odu_compilation_comptage_terrain.R* de traitement est stocké dans le dossier `main`/.

Toutes les données sont stockées dans le dossier `data/` à la racine du projet. Les données brutes collectées sur le terrain sont stockées dans le dossier `data/raw/comptages_terrain/`. Les données traitées sont stockées dans le dossier `data/processed/`. Les données cartographiques permettant d'avoir la référence des secteurs du parc sont situés dans `data/raw/cartographie/`.

Afin de pouvoir traiter les données, les nouveaux fichiers excel de comptages doivent être rajoutés dans le dossier `data/raw/comptages_terrain/` spécifiquement dans le sous-dossier correspondant à la catégorie d'activités. Ces fichiers excel doivent suivre le format des modèles fournis dans le dossier `data/raw/comptages_terrain/modeles/`. Plus précisément, chaque fichier doit correspondre à l'ensemble des données collectées à une date pour une catégorie d'activités. Dans le cas des données de comptage (hors météo), le nom d'un onglet correspond au secteur correspondant au comptage.

Lorsque tous les fichiers à traiter ont été placés dans `data/raw/comptages_terrain/` , il suffit d'ouvrir le script *odu_compilation_comptage_terrain.R* et de le compiler, soit en entier, soit en ne sélectionnant que les parties correspondant aux activités ciblées. En cas d'erreurs annoncées dans la console, se référer aux fichiers logs dans le dossier `logs/` correspondant à la date et au temps de compilation et corriger les erreurs identifiées dans les logs directement dans les fichiers excel source (`data/raw/comptages_terrain/)` . Réitérer le processus jusqu'à ce qu'il n'y ait plus d'erreurs.

Les fichiers finaux traités et compilés sont situés dans le dossier `data/processed/` sous format .rds et .csv. Il sont utilisables directement pour des analyses ou pour des représentations graphiques tels que dans le shiny de l'observatoire des usages.

## Autres informations

Auteur : Aubin Woehrel

License : Code non partageable en dehors des contractuels STARESO et PNMCCA
