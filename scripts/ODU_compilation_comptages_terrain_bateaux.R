#
# --- Observatoire Des Usages - Compilation des fiches de comptage terrain
#

# Initialization ----

## Clean up ----
rm(list = ls())

## Library imports ----

# Data imports
library("openxlsx")
library("readxl")

# Data tidying
library("dplyr")
library("tidyr")
library("stringr")
library("stringi")

# Data transformation
library("chron")
library("lubridate")

## Sourcing Paths 
source("R/paths.R")

## Sourcing custom functions ----
source("R/fct_column_format.R")
source("R/compilation_comptage.R")
source("R/")
source("R/fct_double_header.R")

source("R/fct_file_checking.R")
source("R/fct_sheet_header.R")
source("R/fct_post_compilation.R")
source("R/fct_compilation_comptage.R")
so







### Reference of sector names ----
ref_secteurs <- read.csv(
  paste0(
    "data/raw/cartographie/Sec_nav_maj_2023_Corrigée/Sec _nav_maj_2023.csv"
  ),
  sep = ";"
)

# Create a new sector name with no accents on letters
ref_secteurs <- ref_secteurs %>%
  mutate(Secteur_simple = stringi::stri_trans_general(Secteur, "Latin-ASCII")) %>%
  select("id",
         "Secteur",
         "Secteur_simple",
         "Code_sec",
         "Communes",
         "Code_INSEE",
         "Com_Corse")


### Reference of sector names ----
ref_secteurs <- read.csv(
  paste0(
    "data/raw/cartographie/Sec_nav_maj_2023_Corrigée/Sec _nav_maj_2023.csv"
  ),
  sep = ";"
)

# Create a new sector name with no accents on letters
ref_secteurs <- ref_secteurs %>%
  mutate(Secteur_simple = stringi::stri_trans_general(Secteur, "Latin-ASCII")) %>%
  select("id",
         "Secteur",
         "Secteur_simple",
         "Code_sec",
         "Communes",
         "Code_INSEE",
         "Com_Corse")

## Data imports ----

### Boat counting files import as one common file ----

compilation_plage <- compilation_comptage("plage")
compilation_plaisance <- compilation_comptage("plaisance")
compilation_meteo <- compilation_comptage("meteo")
compilation_activites <- compilation_comptage("activites_loisirs")

compilation_plage <- post_compilation(compilation_plage, counting_type = "plage")
compilation_plaisance <- post_compilation(compilation_plaisance, counting_type = "plaisance")
compilation_meteo <- post_compilation(compilation_meteo, counting_type = "meteo")
compilation_activites <- post_compilation(compilation_activites, counting_type = "activites_loisirs")


# Checking general data stats and characteristics ----
skimr::skim(compilation_plage)
skimr::skim(compilation_plaisance)
skimr::skim(compilation_meteo)
skimr::skim(compilation_activites)


# Save data as rds and csv ----
## rds files ----
saveRDS(compilation_plaisance, paste0(paths$processed_plaisance, ".rds"))
saveRDS(compilation_plage, paste0(paths$processed_plage, ".rds"))
saveRDS(compilation_meteo, paste0(paths$processed_meteo, ".rds"))
saveRDS(compilation_activites, paste0(paths$processed_activites, ".rds"))

## csv files ----
write.csv(compilation_plaisance, paste0(paths$processed_plaisance, ".csv"), row.names = FALSE)
write.csv(compilation_plage, paste0(paths$processed_plage, ".csv"), row.names = FALSE)
write.csv(compilation_meteo, paste0(paths$processed_meteo, ".csv"), row.names = FALSE)
write.csv(compilation_activites, paste0(paths$processed_activites, ".csv"), row.names = FALSE)
