#
# Observatoire des Usages : folders and files paths
#


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

# Get all variables that start by "path" and input them in a list. Paths can be accessed through
# paths$the_name_of_the_path
# paths_names <- ls(pattern = "^path", envir = .GlobalEnv)
paths_names <- ls(envir = .GlobalEnv)
paths <- mget(paths_names, envir = .GlobalEnv)
paths <- as.list(paths)

rm(list = paths_names)
rm(list = ls()[!ls() %in% c("paths")])
