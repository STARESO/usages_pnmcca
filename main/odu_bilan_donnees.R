#' ---
#' title : "Bilan des données"
#' author : Aubin Woehrel
#' date : 2024-09-17
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - BILAN DES DONNEES
#' 
#' Description : 
#' Script permettant de compter le nombre de jeux de données sources, le nombre 
#' de bases de données créées et des stats de nombre de variables et lignes
#' 
#' Note : A implémenter dans le futur directement dans l'appli Osservatoriu
#' 
#' =============================================================================


# Initialisation ----

## Nettoyage de l'environnement ----
rm(list = ls())

## Importation des bibliothèques ----
library("echarts4r")
library("dplyr")
library("tidyr")

## Import des chemins ----
source("R/paths.R")

## Importation des données ----
plaisance <- readRDS(paste0(paths$processed_plaisance, ".rds"))



# Type d'activités
types_comptages <-  c("activites_loisirs", "debarquements", "frequentation_terrestre", 
                      "meteo", "plage", "plaisance")
types_survols <- c("plaba", "plandeau")


# Nombre de fichiers de comptages
nombre_jeux_comptage <- c()
for (type_comptage in types_comptages) {
  file_names <- list.files(paste0("data/raw/comptages_terrain/", type_comptage)) %>%
    .[!grepl("~", .)]  # Suppression des fichiers temporaires
  nombre_jeux_comptage <- c(nombre_jeux_comptage, length(file_names))
}

# Nombre de fichiers de survols
nombre_jeux_survols <- c()
for (type_survol in types_survols) {
  file_names <- list.files(paste0("data/raw/survols/", type_survol)) %>%
    .[!grepl("~", .)]  # Suppression des fichiers temporaires
  nombre_jeux_survols <- c(nombre_jeux_survols, length(file_names))
}

# Jeux de données
bilan_jeux <- tibble(
  type = c(types_comptages, types_survols),
  nombre_de_jeux = c(nombre_jeux_comptage, nombre_jeux_survols)
)


# Extraction du nombre de lignes
# Nombre d'entités 
file_names <- list.files("data/processed") %>%
  .[!grepl("~", .)] %>% # Suppression des fichiers temporaires 
  .[grepl("rds", .)]


# Function to extract type from file names
extract_type <- function(file_name) {
  if (grepl("comptage_terrain", file_name)) {
    sub(".*comptage_terrain_(.*?)\\.rds", "\\1", file_name)
  } else if (grepl("survols", file_name)) {
    sub(".*survols_(.*?)\\.rds", "\\1", file_name)
  } else {
    NA
  }
}

# Calculate rows, campagnes, and campagne_secteurs
file_info <- lapply(file_names, function(file) {
  data <- readRDS(paste0("data/processed/", file))  # Read the RDS file
  list(
    type = extract_type(file),
    rows = nrow(data),
    campagnes = length(unique(data$date)),  # Unique dates
    campagne_secteurs = n_distinct(data %>% select(date, secteur))  # Unique combinations of date and secteur
  )
}) %>% bind_rows()

# Aggregate data by type
total_rows_and_campagnes <- file_info %>%
  group_by(type) %>%
  summarise(
    total_rows = sum(rows, na.rm = TRUE),
    campagnes = sum(campagnes, na.rm = TRUE),
    campagne_secteurs = sum(campagne_secteurs, na.rm = TRUE)
  )


# Update bilan_jeux with the new data
bilan_jeux <- tibble(
  type = c(types_comptages, types_survols),
  nombre_de_jeux = c(nombre_jeux_comptage, nombre_jeux_survols)
) %>%
  left_join(total_rows_and_campagnes, by = "type")

# Paths to metadata files
metadata_paths <- list(
  comptages = "data/raw/comptages_terrain/modeles/reference_comptage_terrain_donnees_brutes.xlsx",
  survols = "data/raw/survols/modeles/reference_survols_donnees_brutes.xlsx"
)

# Function to calculate total variables from metadata
calculate_total_variables <- function(file_path, types) {
  # List all sheets in the metadata file
  sheets <- excel_sheets(file_path)
  
  # Filter sheets corresponding to the specified types
  sheets_to_process <- sheets[sheets %in% types]
  
  total_variables <- c()
  
  for (sheet in sheets_to_process) {
    # Read the sheet and count rows
    data <- read_excel(file_path, sheet = sheet)
    total_variables <- c(total_variables, nrow(data))
  }
  
  # Return total variables for each type
  tibble(
    type = sheets_to_process,
    total_variables = total_variables
  )
}

# Calculate variables for comptages and survols
variables_comptages <- calculate_total_variables(
  file_path = metadata_paths$comptages,
  types = types_comptages
)

variables_survols <- calculate_total_variables(
  file_path = metadata_paths$survols,
  types = types_survols
)

# Combine all variables into a single tibble
total_variables <- bind_rows(variables_comptages, variables_survols)

# Merge with the nombre_jeux tibble
bilan_jeux <- bilan_jeux %>%
  left_join(total_variables, by = "type")

# Export bilan_jeux as an RDS file
readr::write_tsv(bilan_jeux, "data/processed/bilan_jeux.tsv")
saveRDS(bilan_jeux, "data/processed/bilan_jeux.rds")


