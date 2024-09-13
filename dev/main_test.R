###########################################################################
## --- Observatoire des usages - compilation des fiches de comptage terrain
###########################################################################

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

## Paths & custom verification functions imports ----
source("R/paths.R")
source("R/verification_functions.R")


# Importing boat counting files as one common file ----
compilation_comptage <- function(counting_type) {
  message(paste("Compilation des données de comptage", counting_type))
  
  # Path of data for the given counting type
  path_counting_type <- paths[[paste0("comptage_", counting_type)]]
  
  # Get file names and remove temporary files
  file_names <- list.files(path_counting_type) %>%
    .[!grepl("~", .)]
  
  # Read reference metadata for the counting type
  metadata_reference <- read_metadata(counting_type)
  
  # Initialize error logs
  error_logs <- list(
    wrong_named_files = c(),
    wrong_named_dates = c(),
    wrong_named_sheets = c(),
    wrong_amount_of_sheets_files = c(),
    error_sheet_not_in_meta = c(),
    wrong_column_amount = c(),
    wrong_variable_names = c()
  )
  
  # Initialize output data frame
  comptage_terrain <- data.frame()
  
  # Looping through all the files
  for (file_name in file_names) {
    
    # Check if the file name is coherent
    if (!file_coherence(file_name, counting_type)) {
      error_logs$wrong_named_files <- c(error_logs$wrong_named_files, file_name)
      next
    }
    
    # Extract date from the file name
    date_comptage <- str_extract_all(file_name, "\\d{4}-\\d{2}-\\d{2}")[[1]]
    if (is.na(date_comptage[1])) {
      error_logs$wrong_named_dates <- c(error_logs$wrong_named_dates, file_name)
      next
    }
    
    file_path <- paste0(path_counting_type, file_name)
    list_of_sheets <- excel_sheets(file_path) %>%
      {c(.[grepl("metadata", .)], .[!grepl("metadata", .)])}
    
    # Looping through sheets
    for (sheet in list_of_sheets) {
      if (sheet == "metadata_comptages") {
        # Handle metadata sheet
        metadata_comptage <- read.xlsx(file_path, sheet = sheet) %>%
          mutate(Date = janitor::excel_numeric_to_date(Date))
        
        # Validate number of sheets matches metadata sectors
        if (length(metadata_comptage$Secteur) != length(setdiff(list_of_sheets, "metadata_comptages"))) {
          error_logs$wrong_amount_of_sheets_files <- c(error_logs$wrong_amount_of_sheets_files, file_name)
          break
        }
        
      } else {
        
        if (counting_type != "meteo") {
          # Handle sector sheets
          sector_check <- sector_coherence(sheet)
          if (!sector_check$presence) {
            error_logs$wrong_named_sheets <- c(error_logs$wrong_named_sheets, sheet)
            error_logs$suggested_names_sheets <- c(error_logs$suggested_names_sheets, sector_check$closest_match)
            next
          }
        }
        
        # Handle double header if necessary
        if ("champ2" %in% names(metadata_reference)) {
          data_sheet <- double_header_import(file_path, sheet)
        } else {
          data_sheet <- read.xlsx(file_path, sheet = sheet, sep.names = " ", fillMergedCells = TRUE)
        }
        
        # Validate the column structure
        if (ncol(data_sheet) != ncol(metadata_reference)) {
          error_logs$wrong_column_amount <- c(error_logs$wrong_column_amount, ncol(data_sheet))
          next
        }
        
        # Check for variable name coherence
        variable_name_check <- all(names(data_sheet) == metadata_reference$champ)
        if (!variable_name_check) {
          error_logs$wrong_variable_names <- c(error_logs$wrong_variable_names, names(data_sheet)[!variable_name_check])
          next
        }
        
        # Add sector and date columns
        data_sheet <- data_sheet %>%
          mutate(secteur = sheet, date = date_comptage)
        
        # Append to main data frame
        comptage_terrain <- rbind(comptage_terrain, data_sheet)
      }
    }
  }
  
  # Log any mistakes encountered during the process
  mistakes <- mistakes_log(counting_type, error_logs$wrong_named_files, error_logs$wrong_named_dates, 
                           error_logs$wrong_named_sheets, error_logs$wrong_amount_of_sheets_files, 
                           error_logs$error_sheet_not_in_meta, error_logs$wrong_column_amount, 
                           error_logs$wrong_variable_names)
  
  if (mistakes > 0) {
    message(paste("Il y a", mistakes, "erreurs. Veuillez consulter le fichier log."))
  } else {
    message("Aucune erreur rencontrée lors de la compilation.")
  }
  
  return(comptage_terrain)
}

# Running the compilation for different types
compilation_plage <- compilation_comptage("plage") %>%
  post_compilation("plage")

compilation_plaisance <- compilation_comptage("plaisance") %>%
  post_compilation("plaisance")
