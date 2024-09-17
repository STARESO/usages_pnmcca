#' Observatoire des Usages: Folders and Files Paths
#'
#' This script defines the paths used in the project for raw data, 
#' processed data, and outputs such as logs. It also creates a 
#' list `paths` that can be used to access these paths by name.
#'
#' The paths are organized by categories such as raw data for different 
#' types of observations (e.g., "plaisance", "meteo", etc.) and processed 
#' data outputs.
#'
#' @section Paths of Raw Data:
#' - `comptage_plaisance`: Path to raw data for "plaisance".
#' - `comptage_activites_loisirs`: Path to raw data for "activites loisirs".
#' - `comptage_meteo`: Path to raw data for meteorological data.
#' - `comptage_plage`: Path to raw data for beach data.
#' - `comptage_terrestre`: Path to raw data for terrestrial observation.
#' - `comptage_debarquements`: Path to raw data for debarquements
#'
#' @section Reference Data:
#' - `comptage_reference`: Path to the reference dataset for variable names.
#'
#' @section Processed Data:
#' - `processed_plaisance`: Path for processed data of "plaisance".
#' - `processed_plage`: Path for processed beach data.
#' - `processed_activites_loisirs`: Path for processed "activites loisirs".
#' - `processed_meteo`: Path for processed meteorological data.
#' - `processed_terrestre`: Path for processed terrestrial observation data.
#' - `processed_debarquements`: Path for processed debarkation data.
#'
#' @section Output:
#' - `verification_logs`: Path for log files of verification processes.
#'
#' The `paths` object is a list containing all the defined paths, 
#' which can be accessed by name (e.g., `paths$comptage_plaisance`).
#'
#' @export
#'
#' @examples
#' # Access the path for plaisance data
#' paths$comptage_plaisance
#'

# Paths of the raw data ----

## Comptage terrain ----
comptage_plaisance <- "data/raw/comptages_terrain/plaisance/"
comptage_activites_loisirs <- "data/raw/comptages_terrain/activites_loisirs/"
comptage_meteo <- "data/raw/comptages_terrain/meteo/"
comptage_plage <- "data/raw/comptages_terrain/plage/"
comptage_terrestre <- "data/raw/comptages_terrain/frequentation_terrestre/"
comptage_debarquements <- "data/raw/comptages_terrain/debarquements/"

# Reference data set for variable names ----
comptage_reference <- "data/raw/comptages_terrain/modeles/reference_comptage_terrain_donnees_brutes.xlsx"

## Survols aériens ----

# Paths of processed data ----

## Palettes
palettes <- paste0("data/processed/palettes/")

## Comptage terrain ----
processed_plaisance <- "data/processed/us_med_pnmcca_observatoire_comptage_terrain_plaisance"
processed_plage <- "data/processed/us_med_pnmcca_observatoire_comptage_terrain_plage"
processed_activites_loisirs <- "data/processed/us_med_pnmcca_observatoire_comptage_terrain_activites_loisirs"
processed_meteo <- "data/processed/us_med_pnmcca_observatoire_comptage_terrain_meteo"
processed_terrestre <- "data/processed/us_med_pnmcca_observatoire_comptage_terrain_frequentation_terrestre"
processed_debarquements <- "data/processed/us_med_pnmcca_observatoire_comptage_terrain_debarquements"

# Outputs ----

## Log files ----
verification_logs <- "logs/"

# Finalize paths output for sourcing ----

# Get all variables that start by "path" and input them in a list. 
# Paths can be accessed through paths$the_name_of_the_path
paths_names <- ls(envir = .GlobalEnv)
paths <- mget(paths_names, envir = .GlobalEnv)
paths <- as.list(paths)

rm(list = paths_names)
rm(list = ls()[!ls() %in% c("paths")])
