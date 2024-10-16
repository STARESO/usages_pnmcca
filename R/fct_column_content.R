#' ---
#' title : "Vérification du contenu des colonnes"
#' author : Aubin Woehrel
#' date : 2024-1O-15
#' version : 1.0
#' ---
#'
#' =============================================================================
#'
#' OBSERVATOIRE DES USAGES - VERIFICATION DU CONTENU DES COLONNES
#'
#' Description :
#' Permet de vérifier le contenu des colonnes d'un jeu de données de comptage
#' de l'observatoire. Plus spécifiquement, cette fonction permet de vérifier si
#' toutes les modalités sont bien inclues dans les métadonnées de référence dans
#' le cas d'une variable qualitative ou bien de vérifier si les valeurs sont
#' bien dans la fourchette de valeur définie dans les métadonnées de référence
#' pour le cas des variables quantitatives, qu'elles soient continues ou
#' entières.
#'
#' =============================================================================


check_column_content <- function(data_sheet,
                                 metadata_reference,
                                 file_name,
                                 sheet_name,
                                 error_logs) {
  
  # Modification des métadonnées de référence, mais seulement pour les colonnes champ
  metadata_reference <- metadata_reference %>%
    mutate(across(
      contains("champ"),
      ~ stringi::stri_trans_general(., "Latin-ASCII")
    )) %>%    # Supprimer les accents
    mutate(across(contains("champ"), ~ ifelse(
      grepl("L", .) & (grepl("<", .) | grepl(">", .)),
      gsub("m| ", "", .),
      # Supprimer les 'm' et les espaces
      gsub(" ", "_", tolower(.))
    )))
  
  
  # Itérations sur les colonnes de la feuille de données
  for (column_to_check in names(data_sheet)) {
    
    # Type de variable de la colonne
    column_type <- metadata_reference %>%
      filter(champ == column_to_check) %>%
      pull(format)
    
    # Récupération des modalités de la variable
    # Dans le cas d'une variable textuelle
    if (column_type == "texte") {
      column_modalites <- metadata_reference %>%
        filter(champ == column_to_check) %>%
        pull(modalites) %>%
        str_split(", ") %>%
        unlist()
      
      column_modalites <- c(column_modalites, NA)
    }
    
    # Si des modalités ou des intervalles numériques existent dans la référence
    if (all(column_modalites != "/", na.rm = TRUE)) {
      
      # Dans le cas d'une variable textuelle
      if (column_type == "texte") {
        
        # Datatable of mistakes
        issues_to_log <- data_sheet %>%
          select(variable = column_to_check) %>%
          mutate(in_reference = variable %in% column_modalites) %>%
          filter(in_reference == FALSE) %>%
          select(-in_reference) %>%
          mutate(suggestion = column_modalites[stringdist::amatch(
            x = variable,
            table = column_modalites,
            maxDist = 0.5,
            method = "jw"
          )])
        
        # Logging errors if they exist
        n_errors <- nrow(issues_to_log)
        if (n_errors > 0) {
          error_logs$wrong_content <- c(error_logs$wrong_content, issues_to_log$variable)
          error_logs$wrong_content_suggestion <- c(error_logs$wrong_content_suggestion, issues_to_log$suggestion)
          error_logs$wrong_content_columns <- c(error_logs$wrong_content_columns, rep(str_to_sentence(column_to_check), n_errors))
          error_logs$wrong_content_sheets <- c(error_logs$wrong_content_sheets, rep(sheet_name, n_errors))
          error_logs$wrong_content_files <- c(error_logs$wrong_content_files, rep(file_name, n_errors))
        }
      }
    }
    
  }
  
  return(error_logs)
}

