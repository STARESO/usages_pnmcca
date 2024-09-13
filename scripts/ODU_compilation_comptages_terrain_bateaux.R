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

## Paths & custom verification functions imports ----
source("R/paths.R")
source("R/verification_functions.R")

## Data imports ----

### Boat counting files import as one common file ----

compilation_comptage <- function(counting_type) {
  message(paste("Compilation des données de comptage", counting_type))
  
  # Path of data for the given counting type
  path_counting_type <- paths[[paste0("comptage_", counting_type)]]
  
  # Get all file names for the given counting type
  file_names <- list.files(path_counting_type) %>%
    .[!grepl("~", .)]  # Remove temporary files
  
  # Get the reference metadata for the given counting type
  metadata_reference <- read_metadata(counting_type)
  
  # Initialize error logs
  error_logs <- list( 
    wrong_named_files = c(),
    wrong_named_dates = c(),
    wrong_named_sheets = c(),
    wrong_named_sheets_files = c(),
    suggested_names_sheets = c(),
    wrong_amount_of_sheets_files = c(),
    error_sheet_not_in_meta = c(),
    error_sheet_not_in_meta_files = c(),
    error_meta_not_in_sheets = c(),
    error_meta_not_in_sheets_files = c(),
    wrong_column_amount = c(),
    right_column_amount = c(),
    wrong_column_amount_files = c(),
    wrong_column_amount_sheets = c(),
    wrong_variable_names = c(),
    right_variable_names = c(),
    wrong_variable_names_position = c(),
    wrong_variable_names_files = c(),
    wrong_variable_names_sheet = c()
  )
  
  # Initialize output data frame
  comptage_terrain <- data.frame()
  
  # Loop through all files
  for (file_name in file_names) {
    
    # Check file name coherence
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
    
    # Get the list of sheets and place metadata sheets first
    list_of_sheets <- excel_sheets(file_path) %>%
      {c(.[grepl("metadata", .)], .[!grepl("metadata", .)])}
    
    # Loop through sheets
    for (sheet in list_of_sheets) {
      if (sheet == "metadata_comptages") {
        # Handle metadata sheet
        metadata_comptage <- read.xlsx(file_path, sheet = sheet) %>%
          mutate(Date = janitor::excel_numeric_to_date(Date))
        
        # Check number of sheets matches metadata sectors
        if (length(metadata_comptage$Secteur) != length(setdiff(list_of_sheets, "metadata_comptages"))) {
          error_logs$wrong_amount_of_sheets_files <- c(error_logs$wrong_amount_of_sheets_files, file_name)
          break # Move to next file if sheet count is wrong
        } 
        
        # Check if all metadata sectors exist as sheets
        meta_not_in_sheet <- !(metadata_comptage$Secteur %in%
                                 list_of_sheets[!grepl("metadata", list_of_sheets)])
        
        if (sum(meta_not_in_sheet) != 0) {
          error_logs$error_meta_not_in_sheets <- c(error_logs$error_meta_not_in_sheets,
                                                   metadata_comptage$Secteur[meta_not_in_sheet])
          error_logs$error_meta_not_in_sheets_files <- c(error_logs$error_meta_not_in_sheets_files,
                                                         rep(file_name, sum(meta_not_in_sheet)))
          break # Move to next file if metadata mismatch
        }
        
      } else {
        
        # Handle meteo sheets
        if (counting_type == "meteo") {
          if (sheet != "Meteo") {
            error_logs$wrong_named_sheets <- c(error_logs$wrong_named_sheets, sheet)
            error_logs$suggested_names_sheets <- c(error_logs$suggested_names_sheets, "Meteo")
            error_logs$wrong_named_sheets_files <- c(error_logs$wrong_named_sheets_files, file_name)
            next
          }
          
        } else {
          # Check sector names for non-meteo files
          sector_check <- sector_coherence(sheet)
          
          # Log wrong sector name and suggest closest match
          if (!sector_check$presence) {
            error_logs$wrong_named_sheets <- c(error_logs$wrong_named_sheets, sheet)
            error_logs$suggested_names_sheets <- c(error_logs$suggested_names_sheets, sector_check$closest_match)
            error_logs$wrong_named_sheets_files <- c(error_logs$wrong_named_sheets_files, file_name)
            next 
          }
          
          # Check if the sheet exists in metadata_comptage
          if (!sheet %in% metadata_comptage$Secteur) {
            error_logs$error_sheet_not_in_meta <- c(error_logs$error_sheet_not_in_meta, sheet)
            error_logs$error_sheet_not_in_meta_files <- c(error_logs$error_sheet_not_in_meta_files, file_name)
            next
          }
        }
        
        # Read the first two rows of the sheet for header validation
        data_sheet_header <- read.xlsx(
          xlsxFile = file_path,
          sheet = sheet,
          sep.names = " ",
          fillMergedCells = TRUE,
          colNames = FALSE
        ) %>%
          .[1:2, ]
        
        # Verify headers and log errors
        verif_header <- verify_sheet_header(data_sheet_header, metadata_reference, file_name, sheet, error_logs)
        error_logs <- verif_header[[2]]  # Update the error logs
        
        # Skip the sheet if header verification failed
        if (verif_header[[1]]) {
          next
        }
        
        # Import data using double header structure if applicable
        if ("champ2" %in% names(metadata_reference)) {
          data_sheet <- double_header_import(file_path, sheet)
        } else {
          data_sheet <- read.xlsx(file_path, sheet = sheet, sep.names = " ", fillMergedCells = TRUE)
        }
        
        # Clean up column names and format them
        data_sheet <- data_sheet %>%
          rename_with(~ str_replace_all(., " ", "_"), everything()) %>%
          rename_with(~ stringr::str_to_lower(.), everything())
        
        # Add sector and date columns
        if (counting_type != "meteo") {
          data_sheet <- data_sheet %>%
            mutate(
              secteur = rep(sheet, nrow(.)),
              date = rep(date_comptage, nrow(.))
            )
        }
        
        # Concatenate the data
        comptage_terrain <- rbind(comptage_terrain, data_sheet)
        
      } # End of non-metadata sheets
    }  # End of sheets loop
    
  }  # End of files loop
  
  # Log any mistakes encountered
  mistakes <- mistakes_log(counting_type, error_logs)
  
  error_logs <<- error_logs
  
  if (mistakes != 0) {
    message(
      paste0(
        "Il y a ",
        mistakes,
        " erreurs recontrées lors de la compilation.\n",
        "Veuillez consulter le fichier log pour plus d'informations."
      )
    )
  } else {
    message("Aucune erreur rencontrée lors de la compilation.")
  }
  
  message(paste("Données de comptage", counting_type, "compilées."))
  
  return(comptage_terrain)
}



compilation_plage <- compilation_comptage("plage")
compilation_plaisance <- compilation_comptage("plaisance")
compilation_meteo <- compilation_comptage("meteo")
compilation_activites <- compilation_comptage("activites_loisirs")

compilation_plage <- post_compilation(compilation_plage, counting_type = "plage")
compilation_plaisance <- post_compilation(compilation_plaisance, counting_type = "plaisance")
compilation_meteo <- post_compilation(compilation_meteo, counting_type = "meteo")
compilation_activites <- post_compilation(compilation_activites, counting_type = "activites_loisirs")

skimr::skim(compilation_plage)
skimr::skim(compilation_plaisance)

# Save data as rds and csv ----

saveRDS(compilation_plaisance,
        paste0(paths$processed_plaisance, ".rds"))
saveRDS(compilation_plage, paste0(paths$processed_plage, ".rds"))

write.csv(compilation_plaisance,
          paste0(paths$processed_plaisance, ".csv"),
          row.names = FALSE)
write.csv(compilation_plage,
          paste0(paths$processed_plage, ".csv"),
          row.names = FALSE)
