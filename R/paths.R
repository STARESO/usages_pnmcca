#' ---
#' title : "Chemins des dossiers et fichiers"
#' author : Aubin Woehrel
#' date : 2024-09-17
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - CHEMINS DES DOSSIERS ET FICHIERS
#'
#' Description : 
#' Ce script définit les chemins utilisés dans le projet pour les données brutes, 
#' les données traitées et les sorties telles que les fichiers logs de 
#' journalisation. Il crée également une liste `paths` qui permet d'accéder à 
#' ces chemins par leur nom dans les autres scripts.
#'
#' Les chemins sont organisés par catégories, telles que les données brutes 
#' pour différents types d'observations (par exemple, "plaisance", "meteo", etc.) 
#' et les sorties de données traitées.
#' 
#' L'objet `paths` est une liste contenant tous les chemins définis, qui peuvent 
#' être accédés par leur nom (par exemple, `paths$comptage_plaisance`).
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

## Survols aériens ----
survols_plaba <- "data/raw/survols/plaba/"
survols_plandeau <- "data/raw/survols/plandeau/"

## Référence noms de variables ----
comptage_reference <- "data/raw/comptages_terrain/modeles/reference_comptage_terrain_donnees_brutes.xlsx"
survols_reference <- "data/raw/survols/modeles/reference_survols_donnees_brutes.xlsx"
agents_reference <- "data/raw/comptages_terrain/modeles/reference_agents_comptage.xlsx"

## Secteurs ----
reference_secteurs <- "data/raw/cartographie/Sec_nav_maj_2023_Corrigée/Sec_nav_maj_2023.csv"


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

## Survols aériens ----
processed_plaba <- "data/processed/us_med_pnmcca_observatoire_survols_plaba"
processed_plandeau <- "data/processed/us_med_pnmcca_observatoire_survols_plandeau"


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
