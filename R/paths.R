#' ---
#' title : "Paths.R"
#' author : Aubin Woehrel
#' date : 2024-09-17
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - CHEMIN DES DOSSIERS ET FICHIERS
#'
#' Description : 
#' Ce script définit les chemins utilisés dans le projet pour les données brutes, 
#' les données traitées et les sorties telles que les fichiers de logs. Il crée 
#' également une liste `paths` qui permet d'accéder à ces chemins par leur nom
#' dans les autres scripts.
#'
#' Les chemins sont organisés par catégories, telles que les données brutes 
#' pour différents types d'observations (par exemple, "plaisance", "meteo", etc.) 
#' et les sorties de données traitées.
#'
#' @section Chemins des données brutes :
#' - `comptage_plaisance` : Chemin vers les données brutes pour le comptage des bateaux de plaisance.
#' - `comptage_activites_loisirs` : Chemin vers les données brutes pour les activités de loisir.
#' - `comptage_meteo` : Chemin vers les données brutes pour les données météorologiques.
#' - `comptage_plage` : Chemin vers les données brutes pour les données de plage.
#' - `comptage_terrestre` : Chemin vers les données brutes pour les observations terrestres.
#' - `comptage_debarquements` : Chemin vers les données brutes pour les débarquements.
#'
#' @section Données de référence :
#' - `comptage_reference` : Chemin vers le jeu de données de référence pour les noms de variables.
#'
#' @section Données traitées :
#' - `processed_plaisance` : Chemin vers les données traitées pour "plaisance".
#' - `processed_plage` : Chemin vers les données traitées pour les plages.
#' - `processed_activites_loisirs` : Chemin vers les données traitées pour "activités loisirs".
#' - `processed_meteo` : Chemin vers les données traitées pour les données météorologiques.
#' - `processed_terrestre` : Chemin vers les données traitées pour les observations terrestres.
#' - `processed_debarquements` : Chemin vers les données traitées pour les débarquements.
#'
#' @section Sorties :
#' - `verification_logs` : Chemin vers les fichiers de logs des processus de vérification.
#'
#' L'objet `paths` est une liste contenant tous les chemins définis, qui peuvent 
#' être accédés par leur nom (par exemple, `paths$comptage_plaisance`).
#'
#' @export
#'
#' @examples
#' # Accéder au chemin des données pour la plaisance
#' paths$comptage_plaisance
#'
#' =============================================================================

# Chemins des données brutes ----

## Comptage terrain ----
comptage_plaisance <- "data/raw/comptages_terrain/plaisance/"
comptage_activites_loisirs <- "data/raw/comptages_terrain/activites_loisirs/"
comptage_meteo <- "data/raw/comptages_terrain/meteo/"
comptage_plage <- "data/raw/comptages_terrain/plage/"
comptage_terrestre <- "data/raw/comptages_terrain/frequentation_terrestre/"
comptage_debarquements <- "data/raw/comptages_terrain/debarquements/"

## Référence noms de variables ----
comptage_reference <- "data/raw/comptages_terrain/modeles/reference_comptage_terrain_donnees_brutes.xlsx"

## Secteurs ----
reference_secteurs <- "data/raw/cartographie/Sec_nav_maj_2023_Corrigée/Sec_nav_maj_2023.csv"


## Survols aériens ----

# Chemins des données traitées ----

## Palettes
palettes <- paste0("data/processed/palettes/")

## Comptage terrain ----
processed_plaisance <- "data/processed/us_med_pnmcca_observatoire_comptage_terrain_plaisance"
processed_plage <- "data/processed/us_med_pnmcca_observatoire_comptage_terrain_plage"
processed_activites_loisirs <- "data/processed/us_med_pnmcca_observatoire_comptage_terrain_activites_loisirs"
processed_meteo <- "data/processed/us_med_pnmcca_observatoire_comptage_terrain_meteo"
processed_terrestre <- "data/processed/us_med_pnmcca_observatoire_comptage_terrain_frequentation_terrestre"
processed_debarquements <- "data/processed/us_med_pnmcca_observatoire_comptage_terrain_debarquements"

# Sorties ----

## Fichiers de logs ----
verification_logs <- "logs/"

# Finaliser l'export des chemins pour le sourcing ----

# Obtenir toutes les variables commençant par "path" et les mettre dans une liste.
# Les chemins peuvent être accédés via paths$le_nom_du_chemin
paths_names <- ls(envir = .GlobalEnv)
paths <- mget(paths_names, envir = .GlobalEnv)
paths <- as.list(paths)

rm(list = paths_names)
rm(list = ls()[!ls() %in% c("paths")])
