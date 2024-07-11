###                                                                      ###
### --- Observatoire Des Usages - Compilation Fiches terrain Bateaux --- ###
###                                                                      ###

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

## Custom function imports ----
source("R/verification_functions.R")

## Data imports ----

### Reference of sector names ----
ref_secteurs <- read.csv(
  paste0(
    "data/raw/cartographie/Sec_nav_maj_2023_Corrigée/Sec _nav_maj_2023.csv"
  ),
  sep = ";"
)

# Create a new sector name with no accents on letters
ref_secteurs <- ref_secteurs %>%
  mutate(Secteur_simple = stri_trans_general(Secteur, "Latin-ASCII")) %>%
  select("id",
         "Secteur",
         "Secteur_simple",
         "Code_sec",
         "Communes",
         "Code_INSEE",
         "Com_Corse")


### Boat counting files import as one common file ----

bateaux_path <- "data/raw/comptages_terrain/Bateaux/"

# Get all file names of the working directory for boat counting
file_names <- list.files(bateaux_path)

# Remove all temporary files
file_names <- file_names[!grepl("~", file_names)]

# Loop on all the sheets of the first file. All sheet names will go in site column, and
# All the data is concatenated with the same structure as fillMergedCells = TRUE and startRow = 3

wrong_named_dates <- c()
wrong_named_files <- c()

# Initialize the data
comptage_bateaux <- data.frame()

# Looping on all the file names
for (file_name in file_names) {
  # If the file name is not in the good format, add the file name to the wrong_named_files vector
  # File is right if named this way :
  # PNMCCA_usages_plaisance_comptage_terrain_bateaux_" + "YYYY-MM-DD" + ".xlsx"
  
  file_name_coherence <- file_coherence("bateaux", file_name)
  
  if (file_name_coherence) {
    # Extract the date in YYYY-MM-DD format from the file name
    date_comptage <- str_extract_all(file_name, "\\d{4}-\\d{2}-\\d{2}")[[1]]
    
    
    # If the date is not in the file name, add the file name to the wrong_named_dates vector
    if (is.na(date_comptage[1])) {
      wrong_named_dates <- c(wrong_named_dates, file_name)
      warning(paste("The file", file_name, "has no well written date in the name.",
                    "Please rewrite it in a YYYY-MM-DD format."))
      
      # If the date exist, loop over the sheets
    } else {
      file_path <- paste0(bateaux_path, file_name)
      
      
      for (sheet in excel_sheets(file_path)) {
        
        # Read the file for the specific file and sheet
        data_sheet <- read.xlsx(
          xlsxFile = file_path,
          sheet = sheet,
          sep.names = "_",
          fillMergedCells = TRUE,
          startRow = 3
        )
  
        
        if (dim(data_sheet)[2] != 10) {
          warning(
            paste0(
              'Sheet "',
              sheet, '" in file "', file_name,
              '" has not the right number of columns. It has ',
              dim(data_sheet)[2],
              ' columns instead of 10. It was therefore skipped. Please correct.'
            )
          )
          next
          
        } else {
          # Add the sector and date column
          data_sheet <- data_sheet %>%
            mutate(Secteur = rep(sheet, dim(.)[1]),
                   Date = rep(date_comptage, dim(.)[1]))
          
          #  Concatenate the data
          comptage_bateaux <- rbind(comptage_bateaux, data_sheet)
          
        }
        
        
      }
      
    }
    
  } else {
    wrong_named_files <- c(wrong_named_files, file_name)
    warning(paste0('The file "', file_name, '" has no well written name. Please rewrite it in the format :',
                  '"PNMCCA_usages_plaisance_comptage_terrain_bateaux_YYYY-MM-DD.xlsx"'))
  }
  
  
}


comptage_bateaux <- comptage_bateaux %>%
  select("Date", "Secteur", everything()) %>%
  # Remove all the accents in the column names
  rename_with(.fn = function(x) { stri_trans_general(x, "Latin-ASCII") }, .cols = everything()) %>%
  # If column name contains ">" or "<", replace all the _ by nothing
  rename_with(.fn = function(x) { gsub("_", "", x) }, .cols = contains(c(">", "<"))) 


comptage_bateaux <- comptage_bateaux %>%
  mutate(Horaire = times(Horaire),
         Date = as.Date(Date)) 

# Give classes of all columns in dplyr format
column_classes <- lapply(comptage_bateaux, class)

# Output comptage bateaux in processed data as csv and RDS
write.csv(comptage_bateaux, "data/processed/csv/comptages_bateaux.csv")
saveRDS(comptage_bateaux, "data/processed/rds/comptages_bateaux.rds")
