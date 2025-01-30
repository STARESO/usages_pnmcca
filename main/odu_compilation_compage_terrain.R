#' ---
#' title : "Compilation terrain"
#' author : Aubin Woehrel
#' date : 2024-09-17
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - COMPILATION DES FICHES DE COMPTAGE TERRAIN
#' 
#' Description : 
#' Ce script gère la compilation et la vérification des données de 
#' comptage terrain de divers types tels que les comptages "plage", "plaisance", 
#' "météo" et "activités loisirs". Les données sont traitées et stockées dans un 
#' format cohérent pour les analyses. Le script utilise une variété de fonctions 
#' personnalisées pour assurer le bon fonctionnement. Ces fonctions sont 
#' localisées dans le dossier "R/" du projet. 
#' Le procédé global de fonctionnement est décrit dans le README du projet, 
#' à lire notamment pour les emplacements de fichiers d'entrée et de sortie.
#' 
#' =============================================================================

# Initialisation ----

## Nettoyage de l'environnement ----
rm(list = ls())

## Importation des bibliothèques ----

# Importation des données
library("openxlsx")
library("readxl")

# Nettoyage des données
library("dplyr")
library("tidyr")
library("stringr")
library("stringi")

# Transformation des données
library("chron")
library("lubridate")

## Importation des références chemins ----
source("R/paths.R")

## Importation des fonctions personnalisées ----
source("R/fct_check_column_content.R")
source("R/fct_column_format.R")
source("R/fct_compilation_comptage.R")
source("R/fct_conversions.R")
source("R/fct_double_header.R")
source("R/fct_file_checking.R")
source("R/fct_mistakes_log.R")
source("R/fct_post_compilation.R")
source("R/fct_read_metadata.R")
source("R/fct_sectors.R")
source("R/fct_sheet_header.R")
source("R/fct_check_observateurs.R")

## Référence des noms de secteurs ----
ref_secteurs <- read.csv(file = paths$reference_secteurs, sep = ";") %>%
  dplyr::mutate(Secteur_simple = stringi::stri_trans_general(Secteur, "Latin-ASCII")) %>%
  dplyr::select(
    "id", 
    "Secteur",
    "Secteur_simple", 
    "Code_sec", 
    "Communes", 
    "Code_INSEE", 
    "Com_Corse"
  )

## Référence des agents de comptage ----
reference_agents <- read.xlsx(paths$agents_reference, sheet = "Agents") %>%
  rename_with(~ stringi::stri_trans_general(., "Latin-ASCII"), everything()) %>%
  rename_with(~ stringr::str_to_lower(.), everything())


# Compilation ----

## Compilation principale ----
compilation_plage <- compilation_comptage("plage")
compilation_plaisance <- compilation_comptage("plaisance")
compilation_meteo <- compilation_comptage("meteo")
compilation_activites <- compilation_comptage("activites_loisirs")
compilation_debarquements <- compilation_comptage("debarquements")

## Post compilation vers format final ----
compilation_plage <- post_compilation(compilation_plage, counting_type = "plage")
compilation_plaisance <- post_compilation(compilation_plaisance, counting_type = "plaisance")
compilation_meteo <- post_compilation(compilation_meteo, counting_type = "meteo")
compilation_activites <- post_compilation(compilation_activites, counting_type = "activites_loisirs")
compilation_debarquements <- post_compilation(compilation_debarquements, counting_type = "debarquements")

# Check général des compilations ----
skimr::skim(compilation_plage)
skimr::skim(compilation_plaisance)
skimr::skim(compilation_meteo)
skimr::skim(compilation_activites)
skimr::skim(compilation_debarquements)


# Sauvegarde des données traitées ----

## Fichiers rds ----
saveRDS(compilation_plaisance, paste0(paths$processed_plaisance, ".rds"))
saveRDS(compilation_plage, paste0(paths$processed_plage, ".rds"))
saveRDS(compilation_meteo, paste0(paths$processed_meteo, ".rds"))
saveRDS(compilation_activites, paste0(paths$processed_activites, ".rds"))
saveRDS(compilation_debarquements, paste0(paths$processed_debarquements, ".rds"))

## Fichiers tsv ----
write.table(compilation_plaisance, paste0(paths$processed_plaisance, ".tsv"), sep = "\t", row.names = FALSE)
write.table(compilation_plage, paste0(paths$processed_plage, ".tsv"), sep = "\t", row.names = FALSE)
write.table(compilation_meteo, paste0(paths$processed_meteo, ".tsv"), sep = "\t", row.names = FALSE)
write.table(compilation_activites, paste0(paths$processed_activites, ".tsv"), sep = "\t", row.names = FALSE)
write.table(compilation_debarquements, paste0(paths$processed_debarquements, ".tsv"), sep = "\t", row.names = FALSE)

## Fichiers csv ----
write.csv2(compilation_plaisance, paste0(paths$processed_plaisance, ".csv"), row.names = FALSE)
write.csv2(compilation_plage, paste0(paths$processed_plage, ".csv"), row.names = FALSE)
write.csv2(compilation_meteo, paste0(paths$processed_meteo, ".csv"), row.names = FALSE)
write.csv2(compilation_activites, paste0(paths$processed_activites, ".csv"), row.names = FALSE)
write.csv2(compilation_debarquements, paste0(paths$processed_debarquements, ".csv"), row.names = FALSE)
