#
# Observatoire Des Usages - Useful verification functions
#

# Initialization ----

## Library imports ----

library(dplyr)
library(stringdist)

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
  mutate(Secteur_simple = stringi::stri_trans_general(Secteur, "Latin-ASCII")) %>%
  select("id",
         "Secteur",
         "Secteur_simple",
         "Code_sec",
         "Communes",
         "Code_INSEE",
         "Com_Corse")


# Functions ----

## Field counting type file coherence check ----

#' Checks the coherence of the name of a field counting type file
#'
#' @param counting_type Field counting type
#' @param file_name Name of the file to check
#'
#' @return Logical value indicating the coherence state of the file name
#' @export
#'
#' @examples
#'
file_coherence <- function(file_name, counting_type) {
  file_name_type <- paste0(
    "us_med_pnmcca_observatoire_comptage_terrain_",
    counting_type,
    "_\\d{4}-\\d{2}-\\d{2}\\.xlsx"
  )
  
  file_name_coherence <- grepl(file_name_type, file_name)
  
  
  return(file_name_coherence)
  
}

## File date coherence check ----

file_date_coherence <- function(file_name,
                                counting_type,
                                date_format = "YYYY_MM_DD",
                                sheet = NULL) {
  # Extracting date from file name
  if (date_format == "YYYY_MM_DD") {
    date_comptage <- str_extract_all(file_name, "\\d{4}-\\d{2}-\\d{2}")[[1]]
  } else if (date_format == "DD_MM_YYYY") {
    date_comptage <- str_extract_all(file_name, "\\d{4}_\\d{2}_\\d{2}")[[1]]
  } else {
    stop("Date format not recognized")
  }
  
  # If the test is just to check the coherence of the date in the file name
  if (is.null(sheet)) {
    if (is.na(date_comptage[1])) {
      return(coherent = FALSE, wrong_date = date_comptage[1])
    } else {
      return(coherent = TRUE, wrong_date = NULL)
    }
    
    # If the test is to check the coherence between the file name and the sheet name
  } else {
    file_path = paste0(
      "data/raw/comptages_terrain/",
      str_to_title(counting_type),
      "/",
      file_name,
      ".xlsx"
    )
    
    
    sheet_date <- read.xlsx(xlsxFile = file_path, sheet = sheet)
    
    which(sheet_date[, 1] == "Métadonnées")
    which(sheet_date[, 1] == "Comptage")
    # select all the rows after métadonnées in column 1 and before Comptage in column 1
    
  }
  
  
}


## Sector name coherence check ----

#' Checks a coherence of a sector name
#'
#' @param sector_name Name of the sector to check
#'
#' @return List containing :
#' - presence of the sector name in the reference list as a Boolean
#' - the closest match if the name is not present
#' @export
#'
#' @examples
sector_coherence <- function(sector_name) {
  sector_presence <- sector_name %in% ref_secteurs$Secteur_simple
  
  if (!sector_presence) {
    closest_index <- amatch(
      sector_name,
      ref_secteurs$Secteur_simple,
      maxDist = 0.5,
      method = "jw"
    )
    
    if (str_to_lower(sector_name) == "petit lotu") {
      closest_match <- "Mezzanu"
    }
    
    else if (!is.na(closest_index)) {
      closest_match <- ref_secteurs$Secteur_simple[closest_index]
    }
  } else {
    closest_match <- NULL
  }
  
  return(list(presence = sector_presence, closest_match = closest_match))
  
}


## Sector name list ----

#' Returns reference of sector names
#'
#' @return Dataframe containing the reference of sector names
#' @export
#'
#' @examples
sector_names <- function() {
  sectors <- ref_secteurs %>%
    select(id,
           Nom_secteur_simple = Secteur_simple,
           Nom_secteur_exact = Secteur)
  
  return(sectors)
  
}


## Writing all mistakes of compilation in a log file ----
mistakes_log <- function(counting_type, error_logs) {
  counting_type <- paste0("comptage_terrain_", counting_type)
  
  log_file <- paste0(
    paths$verification_logs,
    "compilation_",
    counting_type,
    "_",
    format(Sys.time(), "%Y-%m-%d_%Hh%M"),
    ".log"
  )
  file.create(log_file)
  
  write(
    paste(
      " --- Vérification des données de",
      str_replace_all(counting_type, "_", " "),
      "--- \n"
    ),
    log_file,
    append = TRUE
  )
  
  write(
    paste(
      "En cas de présence d'erreurs, veuillez les corriger.",
      "Attention : les données érronées ne sont pas enregistrées dans le jeu",
      "de données compilé.\nElles ne le seront qu'après correction des erreurs indiquées."
    ),
    log_file,
    append = TRUE
  )
  
  number_of_errors <- sum(
    sapply(error_logs[c(
      "wrong_named_files", 
      "wrong_named_dates", 
      "wrong_named_sheets",
      "wrong_amount_of_sheets_files", 
      "error_sheet_not_in_meta",
      "error_meta_not_in_sheets", 
      "wrong_column_amount_files",
      "wrong_variable_names_files"
    )], length)
  )
  
  write(paste0("Nombre total d'erreurs : ", number_of_errors, "\n\n"),
        log_file,
        append = TRUE)
  
  
  # Error feedback for wrong named files
  write("--- Fichiers mal nommés :\n", log_file, append = TRUE)
  
  if (length(error_logs$wrong_named_files) > 0) {
    write(
      paste0(
        "Les fichiers de ",
        str_replace_all(counting_type, "_", " "),
        " doivent être au format ",
        "PNMCCA_usages_plaisance_",
        counting_type,
        "_YYYY-MM-DD.xlsx \n"
      ),
      log_file,
      append = TRUE
    )
    
    for (wrong_file in error_logs$wrong_named_files) {
      write(paste0(" > ", wrong_file, "\n"), log_file, append = TRUE)
    }
    
    write("\n", log_file, append = TRUE)
    
  } else {
    write("Tous les fichiers sont correctement nommés.\n\n",
          log_file,
          append = TRUE)
  }
  
  
  # Error feedback for wrong amount of sheets compared to metadata
  write("--- Nombre d'onglets (secteurs) incohérents :\n",
        log_file,
        append = TRUE)
  
  if (length(error_logs$wrong_amount_of_sheets_files) > 0) {
    write(
      paste0("Les fichiers suivants ont un nombre d'onglets incohérent : \n"),
      log_file,
      append = TRUE
    )
    
    for (wrong_file in error_logs$wrong_amount_of_sheets_files) {
      write(paste0(" > ", wrong_file, "\n"), log_file, append = TRUE)
    }
    
    write(
      paste0(
        "Veuillez faire correspondre le nombre d'onglets avec les métadonnées ",
        "contenues dans l'onglet metadata_comptages.\n\n"
      ),
      log_file,
      append = TRUE
    )
    
  } else {
    write("Le nombre d'onglets est cohérent avec les métadonnées.\n\n",
          log_file,
          append = TRUE)
  }
  
  
  # Error feedback for non-existing sectors in reference database
  write("--- Onglets (secteurs) mal nommés : \n", log_file, append = TRUE)
  
  if (length(error_logs$wrong_named_sheets) > 0) {
    write(
      paste0(
        "Certains noms d'onglets des données sources ne correspondent pas aux noms de secteurs acceptés.\n",
        "Veuillez les renommer avec un nom de secteur valide (c.f. référence des secteurs).\n",
        "Les noms invalides sont affichés pour chaque fichier sous la forme : \n",
        "nom d'onglet refusé --> suggestion la plus proche\n"
      ),
      log_file,
      append = TRUE
    )
    
    for (concerned_file in unique(error_logs$wrong_named_sheets_files)) {
      write(paste0(" > Pour le fichier ", concerned_file, ":\n"),
            log_file,
            append = TRUE)
      
      concerned_sheets <- error_logs$wrong_named_sheets[error_logs$wrong_named_sheets_files == concerned_file]
      concerned_suggestions <- error_logs$suggested_names_sheets[error_logs$wrong_named_sheets_files == concerned_file]
      
      for (i in seq_along(concerned_sheets)) {
        write(
          paste0(
            "\t", concerned_sheets[i],
            " --> ", concerned_suggestions[i], "\n"
          ),
          log_file,
          append = TRUE
        )
      }
    }
    
    write("", log_file, append = TRUE)
    
  } else {
    write("Tous les secteurs sont correctement nommés.\n\n",
          log_file,
          append = TRUE)
  }
  
  
  
  
  
  # Error feedback for inconsistencies between metadata and sheets
  write("--- Incohérence entre les noms des secteurs et les métadonnées : \n",
        log_file,
        append = TRUE)
  
  if (length(error_logs$error_sheet_not_in_meta) > 0 | length(error_logs$error_meta_not_in_sheets) > 0) {
    
    if (length(error_logs$error_sheet_not_in_meta) > 0) {
      write(
        paste0(
          "Les fichiers suivants contiennent des noms d'onglets incohérents: \n"
        ),
        log_file,
        append = TRUE
      )
      
      for (concerned_file in unique(error_logs$error_sheet_not_in_meta_files)) {
        write(paste0(" > Pour le fichier ", concerned_file, ":\n"),
              log_file,
              append = TRUE)
        
        concerned_sheets <- error_logs$error_sheet_not_in_meta[error_logs$error_sheet_not_in_meta_files == concerned_file]
        for (i in seq_along(concerned_sheets)) {
          write(paste0("\t", concerned_sheets[i], "\n"), log_file, append = TRUE)
        }
      }
    }
    
    if (length(error_logs$error_meta_not_in_sheets) > 0) {
      write(
        paste0(
          "Pour les fichiers suivants, certains secteurs sont présents dans l'onglet ",
          "metadata_comptages, mais l'onglet correspondant au secteur n'existe pas : \n"
        ),
        log_file,
        append = TRUE
      )
      
      for (concerned_file in unique(error_logs$error_meta_not_in_sheets_files)) {
        write(paste0(" > Pour le fichier ", concerned_file, ":\n"),
              log_file,
              append = TRUE)
        
        concerned_sheets <- error_logs$error_meta_not_in_sheets[error_logs$error_meta_not_in_sheets_files == concerned_file]
        for (i in seq_along(concerned_sheets)) {
          write(paste0("\t", concerned_sheets[i], "\n"), log_file, append = TRUE)
        }
      }
    }
    
    write("", log_file, append = TRUE)
    
  } else {
    write(
      "Pas d'incohérence entre les noms des secteurs et les métadonnées.\n\n",
      log_file,
      append = TRUE
    )
  }
  
  # Error feedback for wrong number of columns
  write("--- Nombre de variables incorrect : \n", log_file, append = TRUE)
  
  if (length(error_logs$wrong_column_amount) > 0) {
    write(
      paste0(
        "Certains fichiers contiennent un nombre de variables incorrect.\n",
        "Veuillez vérifier que le nombre de colonnes correspond bien à la référence.\n"
      ),
      log_file,
      append = TRUE
    )
    
    for (concerned_file in unique(error_logs$wrong_column_amount_files)) {
      write(paste0(" > Pour le fichier ", concerned_file, ":\n"),
            log_file,
            append = TRUE)
      
      for (concerned_sheet in unique(error_logs$wrong_column_amount_sheets[error_logs$wrong_column_amount_files == concerned_file])) {
        write(paste0("\t Pour l'onglet ", concerned_sheet, ":\n"),
              log_file,
              append = TRUE)
        
        write(
          paste0("\t Nombre de colonnes attendu : ",
                 error_logs$right_column_amount[error_logs$wrong_column_amount_files == concerned_file &
                                                  error_logs$wrong_column_amount_sheets == concerned_sheet], "\n"),
          log_file,
          append = TRUE
        )
        
        write(
          paste0("\t Nombre de colonnes observé : ",
                 error_logs$wrong_column_amount[error_logs$wrong_column_amount_files == concerned_file &
                                                  error_logs$wrong_column_amount_sheets == concerned_sheet], "\n"),
          log_file,
          append = TRUE
        )
      }
    }
    
    write("", log_file, append = TRUE)
    
  } else {
    write("Le nombre de variables est correct.\n\n", log_file, append = TRUE)
  }
  
  # Error feedback for wrong variable names
  write("--- Noms de variables incorrects : \n", log_file, append = TRUE)
  
  if (length(error_logs$wrong_variable_names) > 0) {
    write(
      paste0(
        "Certains noms de variables ne correspondent pas aux noms de variables acceptés.\n",
        "Veuillez les renommer avec un nom de variable valide (c.f. référence des métadonnées).\n",
        "L'erreur pourrait aussi provenir de la mauvaise position des colonnes, censées être à la ligne 1.\n"
      ),
      log_file,
      append = TRUE
    )
    
    for (concerned_file in unique(error_logs$wrong_variable_names_files)) {
      write(paste(" > Pour le fichier ", concerned_file, ":\n"),
            log_file,
            append = TRUE)
      
      for (concerned_sheet in unique(error_logs$wrong_variable_names_sheets[error_logs$wrong_variable_names_files == concerned_file])) {
        write(paste("\t Pour l'onglet ", concerned_sheet, ":\n"),
              log_file,
              append = TRUE)
        
        concerned_variables <- error_logs$wrong_variable_names[error_logs$wrong_variable_names_files == concerned_file &
                                                                 error_logs$wrong_variable_names_sheets == concerned_sheet]
        concerned_positions <- error_logs$wrong_variable_names_position[error_logs$wrong_variable_names_files == concerned_file &
                                                                          error_logs$wrong_variable_names_sheets == concerned_sheet]
        concerned_corrections <- error_logs$right_variable_names[error_logs$wrong_variable_names_files == concerned_file &
                                                                   error_logs$wrong_variable_names_sheets == concerned_sheet]
        
        for (i in seq_along(concerned_variables)) {
          write(
            paste(
              "\t Nom attendu à la colonne",
              str_to_upper(letters[concerned_positions[i]]),
              ":",
              concerned_corrections[i], "\n"
            ),
            log_file,
            append = TRUE
          )
          
          write(
            paste(
              "\t Nom observé à la colonne",
              str_to_upper(letters[concerned_positions[i]]),
              ":",
              concerned_variables[i], "\n"
            ),
            log_file,
            append = TRUE
          )
        }
      }
    }
    
    write("", log_file, append = TRUE)
    
  } else {
    write("Tous les noms de variables sont corrects.\n\n", log_file, append = TRUE)
  }
  
  write(
    paste(
      " --- Fin de la vérification des données de",
      str_replace_all(counting_type, "_", " "),
      "---"
    ),
    log_file,
    append = TRUE
  )
  
  if (number_of_errors != 0) {
    message(paste(
      "Fichier log pour le",
      counting_type,
      "enregistré au chemin :",
      log_file
    ))
  }
  
  return(number_of_errors)
}




#' Converts double header to one clean header
#'
#' @param dataset dataset with double header
#'
#' @return new header
#' @export
#'
#' @examples
double_header_fusion <- function(dataset) {
  if (nrow(dataset) < 2)
    stop("Dataset must have at least two rows for header fusion.")
  
  header_selection <- dataset[1:2, ]
  new_header <- c()
  
  # for all cells, apply str_trans_general to remove accents and replace spaces by nothing
  header_selection <- header_selection %>%
    mutate(across(
      everything(),
      ~ stringi::stri_trans_general(., "Latin-ASCII")
    )) %>%    # Remove accents
    mutate(across(everything(), ~ ifelse(
      grepl("L", .) &
        (grepl("<", .) |
           grepl(">", .)),
      # If 'L' or '<' is present
      gsub("m| ", "", .),
      # Remove 'm' and spaces
      gsub(" ", "_", tolower(.))
    )))                     # Else, replace spaces with underscores and lowercase
  
  # Step 2: Create the new header by combining rows
  new_header <- purrr::map2_chr(header_selection[2, ], header_selection[1, ], function(h2, h1) {
    if (h2 != h1) {
      return(paste(h2, h1, sep = "__"))  # Combine if they differ
    } else {
      return(h1)  # Keep the same if they are identical
    }
  })
  
  new_header <- unname(new_header)
  
  return(new_header)
  
}

#' Converts xlsx data with double header as one clean header dataset using concatenation
#'
#' @param file_path path of the xlsx file
#' @param sheet_name name of the sheet to import
#'
#' @return dataset with clean header
#' @export
#'
#' @examples
double_header_import <- function(file_path, sheet_name) {
  # Import of whole dataset
  data_double <- openxlsx::read.xlsx(
    xlsxFile = file_path,
    sheet = sheet_name,
    sep.names = " ",
    colNames = FALSE,
    fillMergedCells = TRUE
  )
  
  # Double header combined as 1 header by concatenation
  names_data_double <- double_header_fusion(data_double)
  
  # Import of dataset with new header
  data_double <-  openxlsx::read.xlsx(
    xlsxFile = file_path,
    sheet = sheet_name,
    sep.names = " ",
    colNames = FALSE,
    fillMergedCells = TRUE,
    skipEmptyCols = FALSE,
    startRow = 3,
    cols = 1:length(names_data_double)
  )
  
  # Check if the number of columns read matches the header length
  if (ncol(data_double) < length(names_data_double)) {
    # Add empty columns if necessary to match the number of headers
    data_double <- cbind(data_double, matrix(
      NA,
      nrow = nrow(data_double),
      ncol = length(names_data_double) - ncol(data_double)
    ))
  }
  
  # Renaming columns with new header
  colnames(data_double) <- names_data_double
  
  return(data_double)
  
}


## Post compilation processing
post_compilation <- function(compilation_data, counting_type) {
  
  if (counting_type == "plaisance") {
    
    compilation_data <- compilation_data %>%
      # select cols containing __
      tidyr::pivot_longer(cols = contains("__"),
                          names_to = "category",
                          values_to = "nombre") %>%
      # separate category into two columns
      tidyr::separate_wider_delim(col = category,
                                  delim = "__",
                                  names = c("statut", "taille"))
    
  } else if (counting_type == "activites_loisirs") {
    
    compilation_data <- compilation_data %>%
      # select cols containing __
      tidyr::pivot_longer(cols = contains("__"),
                          names_to = "category",
                          values_to = "nombre") %>%
      # separate category into two columns
      tidyr::separate_wider_delim(col = category,
                                  delim = "__",
                                  names = c("type", "usage"))
    
  } else if (counting_type == "plage") {
    compilation_data <- compilation_data %>%
      rename(personnes_plage = personnes_sur_la_plage, 
             personnes_eau = `personnes_dans_l'eau`)
    
  }
  
  compilation_data <- compilation_data %>%
    select("date", "secteur", everything()) %>%
    rename_with(.fn = \(x) stringr::str_to_lower(x)) %>%
    # Remove all the accents in the column names
    rename_with(
      .fn = function(x) {
        stringi::stri_trans_general(x, "Latin-ASCII")
      },
      .cols = everything()
    ) %>%
    mutate(horaire = times(horaire), date = as.Date(date)) %>%
    mutate(annee = year(date),
           mois = month(date, label = TRUE, abbr = FALSE)) %>%
    select(date, annee, mois, secteur, everything())
  
  return(compilation_data)
}



read_metadata <- function(counting_type) {
  # Read metadata and clean it
  metadata <- read.xlsx(
    xlsxFile = paths$comptage_reference,
    sheet = counting_type,
    sep.names = "_"
  ) %>%
    rename_with(~ str_to_lower(.), everything()) %>%
    rename_with(~ stri_trans_general(., "latin-ascii"), everything()) %>%
    rename(modalites = `fourchette_de_valeur_/_modalites`)
  
  if ("champ_2" %in% names(metadata)) {
    metadata <- metadata %>%
      rename(champ1 = champ_1, champ2 = champ_2)
  }
  
  return(metadata)
}


# Verifying sheet header number of columns and names
# Verifying sheet header: number of columns and variable names
verify_sheet_header <- function(data_sheet, metadata_reference, file_name, sheet, error_logs) {
  # Flag to track whether any errors occurred in the current sheet
  error_flag <- FALSE
  
  # Check if it's a double header case (presence of champ2 in metadata)
  if ("champ2" %in% names(metadata_reference)) {
    # Verify the number of columns
    if (ncol(data_sheet) != nrow(metadata_reference)) {
      # Log error if the number of columns does not match
      error_logs$wrong_column_amount <- c(error_logs$wrong_column_amount, ncol(data_sheet))
      error_logs$right_column_amount <- c(error_logs$right_column_amount, nrow(metadata_reference))
      error_logs$wrong_column_amount_files <- c(error_logs$wrong_column_amount_files, file_name)
      error_logs$wrong_column_amount_sheets <- c(error_logs$wrong_column_amount_sheets, sheet)
      error_flag <- TRUE
      return(list(error_flag, error_logs))  # Exit early if the number of columns is incorrect
    }
    
    # Check first row of the double header
    coherent_variable_names1 <- data_sheet[1, ] == metadata_reference$champ1
    # Check second row of the double header
    coherent_variable_names2 <- data_sheet[2, ] == metadata_reference$champ2
    
    # Log errors for first row mismatches
    if (sum(!coherent_variable_names1) != 0) {
      error_logs$wrong_variable_names <- c(error_logs$wrong_variable_names, data_sheet[1, ][!coherent_variable_names1])
      error_logs$right_variable_names <- c(error_logs$right_variable_names, metadata_reference$champ1[!coherent_variable_names1])
      error_logs$wrong_variable_names_position <- c(error_logs$wrong_variable_names_position, which(!coherent_variable_names1))
      error_logs$wrong_variable_names_files <- c(error_logs$wrong_variable_names_files, rep(file_name, sum(!coherent_variable_names1)))
      error_logs$wrong_variable_names_sheet <- c(error_logs$wrong_variable_names_sheet, rep(sheet, sum(!coherent_variable_names1)))
      error_flag <- TRUE
    }
    
    # Log errors for second row mismatches
    if (sum(!coherent_variable_names2) != 0) {
      error_logs$wrong_variable_names <- c(error_logs$wrong_variable_names, data_sheet[2, ][!coherent_variable_names2])
      error_logs$right_variable_names <- c(error_logs$right_variable_names, metadata_reference$champ2[!coherent_variable_names2])
      error_logs$wrong_variable_names_position <- c(error_logs$wrong_variable_names_position, which(!coherent_variable_names2))
      error_logs$wrong_variable_names_files <- c(error_logs$wrong_variable_names_files, rep(file_name, sum(!coherent_variable_names2)))
      error_logs$wrong_variable_names_sheet <- c(error_logs$wrong_variable_names_sheet, rep(sheet, sum(!coherent_variable_names2)))
      error_flag <- TRUE
    }
    
  } else {
    # Single header case: Check number of columns
    if (ncol(data_sheet) != nrow(metadata_reference)) {
      # Log error if column numbers don't match
      error_logs$wrong_column_amount <- c(error_logs$wrong_column_amount, ncol(data_sheet))
      error_logs$right_column_amount <- c(error_logs$right_column_amount, nrow(metadata_reference))
      error_logs$wrong_column_amount_files <- c(error_logs$wrong_column_amount_files, file_name)
      error_logs$wrong_column_amount_sheets <- c(error_logs$wrong_column_amount_sheets, sheet)
      error_flag <- TRUE
      return(list(error_flag, error_logs))  # Exit early if the column count is wrong
    }
    
    # Check the variable names in the single header
    coherent_variable_names <- data_sheet[1, ] == metadata_reference$champ
    if (sum(!coherent_variable_names) != 0) {
      error_logs$wrong_variable_names <- c(error_logs$wrong_variable_names, names(data_sheet)[!coherent_variable_names])
      error_logs$right_variable_names <- c(error_logs$right_variable_names, metadata_reference$champ[!coherent_variable_names])
      error_logs$wrong_variable_names_position <- c(error_logs$wrong_variable_names_position, which(!coherent_variable_names))
      error_logs$wrong_variable_names_files <- c(error_logs$wrong_variable_names_files, rep(file_name, sum(!coherent_variable_names)))
      error_logs$wrong_variable_names_sheet <- c(error_logs$wrong_variable_names_sheet, rep(sheet, sum(!coherent_variable_names)))
      error_flag <- TRUE
    }
  }
  
  # Return the error flag and updated error logs
  return(list(error_flag, error_logs))
}

