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
                                 error_logs, 
                                 is_double_header = FALSE) {
  
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
  
  # Mutation des métadonnées si double en-tête
  if (is_double_header) {
    metadata_reference <- metadata_reference %>%
      mutate(champ = case_when(
        champ1 != champ2 ~ paste0(champ2, "__", champ1),
        TRUE ~ champ1
      ))
  }
  
  # Itérations sur les colonnes de la feuille de données
  for (column_to_check in names(data_sheet)) {
    
    # Type de variable de la colonne
    column_type <- metadata_reference %>%
      filter(champ == column_to_check) %>%
      pull(format)
    
    # Column modalites presence
    column_modalites_presence <- metadata_reference %>%
      filter(champ == column_to_check) %>%
      pull(modalites) %>%
      as.character()
    
    column_shunt <- metadata_reference %>%
      filter(champ == column_to_check) %>%
      select(shunt) %>%
      mutate(shunt = case_when(
        shunt == "Oui" ~ TRUE,
        shunt == "Non" ~ FALSE
      )) %>% 
      pull(shunt)
    
    # Dans le cas d'une colonne de type secteur, la fonction secteur cohérence est itérée sur 
    # toutes les lignes
    if (column_to_check == "secteur") {
      
      issues_to_log <- data_sheet %>%
        select(secteur) %>%
        mutate(
          result = purrr::map(secteur, sector_coherence), 
          in_reference = purrr::map_lgl(result, "presence"), # Extraction du résultat présence/absence
          suggestion = purrr::map_chr(result, "closest_match", .default = NA_character_),
          row_number = row_number()
        ) %>%
        filter(in_reference == FALSE) %>%
        select(-c(in_reference, result)) 
        
      # Logging errors if they exist
      n_errors <- nrow(issues_to_log)
      if (n_errors > 0) {
        error_logs$wrong_content <- c(error_logs$wrong_content, as.character(issues_to_log$secteur))
        error_logs$wrong_content_suggestion <- c(error_logs$wrong_content_suggestion, issues_to_log$suggestion)
        error_logs$wrong_content_line <- c(error_logs$wrong_content_line, issues_to_log$row_number)
        error_logs$wrong_content_columns <- c(error_logs$wrong_content_columns, rep(str_to_sentence(column_to_check), n_errors))
        error_logs$wrong_content_sheets <- c(error_logs$wrong_content_sheets, rep(sheet_name, n_errors))
        error_logs$wrong_content_files <- c(error_logs$wrong_content_files, rep(file_name, n_errors))
      }
      
    } else if (column_to_check == "observateurs") {
      
      liste_agents <- data_sheet %>%
        select(observateurs)
      
      error_logs <- log_wrong_observers(
        liste_agents = liste_agents,
        sheet_name = sheet_name,
        file_name = file_name, 
        counting_type = "meteo",
        error_logs = error_logs)
    }

    # Récupération des modalités de la variable uniquement si 
    # référence existantes ou valides (absence de "/", aucun élément (NA) ou 
    # pattern #shunt pour les descriptions spécifiques qui ne sont pas les modalités elle même)
    if (!((column_modalites_presence == "/") | is.na(column_modalites_presence) | column_shunt)) {
      
      # Dans le cas d'une variable qualitative ou texte
      if (column_type == "texte" | grepl("qualitative", column_type)) {
        
        column_modalites <- metadata_reference %>%
          filter(champ == column_to_check) %>%
          pull(modalites) %>%
          str_split(" ; ") %>%
          unlist()
        
        column_modalites <- c(column_modalites, NA)
      
        # En cas de variable numérique entière ou décimale
      } else if (column_type %in% c("entier", "reel")) {
        
        column_modalites <- metadata_reference %>%
          filter(champ == column_to_check) %>%
          pull(modalites) %>%
          str_replace_all(" ", "") %>%
          str_split("<") %>%
          unlist() %>%
          .[c(1, length(.))] %>%
          as.numeric()
          
      } else {
        column_modalites = "/"
      }
      
      # Si des modalités ou des intervalles numériques existent dans la référence
      if (all(column_modalites != "/", na.rm = TRUE)) {
        
        # Dans le cas d'une variable textuelle
        if (column_type == "texte" | grepl("qualitative", column_type)) {
          
          # Datatable of mistakes
          issues_to_log <- data_sheet %>%
            select(variable = all_of(column_to_check)) %>%
            mutate(in_reference = variable %in% column_modalites,
                   row_number = row_number()) %>%
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
            error_logs$wrong_content <- c(error_logs$wrong_content, as.character(issues_to_log$variable))
            error_logs$wrong_content_suggestion <- c(error_logs$wrong_content_suggestion, issues_to_log$suggestion)
            error_logs$wrong_content_line <- c(error_logs$wrong_content_line, issues_to_log$row_number)
            error_logs$wrong_content_columns <- c(error_logs$wrong_content_columns, rep(str_to_sentence(column_to_check), n_errors))
            error_logs$wrong_content_sheets <- c(error_logs$wrong_content_sheets, rep(sheet_name, n_errors))
            error_logs$wrong_content_files <- c(error_logs$wrong_content_files, rep(file_name, n_errors))
          }
          
        } else if (column_type %in% c("entier", "reel")) {
          
          issues_to_log <- data_sheet %>%
            select(variable = all_of(column_to_check)) %>%
            mutate(in_reference = column_modalites[1] <= variable & variable <= column_modalites[2],
                   row_number = row_number()) %>%
            filter(in_reference == FALSE) %>%
            select(-in_reference) %>%
            mutate(suggestion = paste(column_modalites[1], "<= x <=", column_modalites[2]))
          
          # Logging errors if they exist
          n_errors <- nrow(issues_to_log)
          if (n_errors > 0) {
            error_logs$wrong_content <- c(error_logs$wrong_content, as.character(issues_to_log$variable))
            error_logs$wrong_content_suggestion <- c(error_logs$wrong_content_suggestion, issues_to_log$suggestion)
            error_logs$wrong_content_line <- c(error_logs$wrong_content_line, issues_to_log$row_number)
            error_logs$wrong_content_columns <- c(error_logs$wrong_content_columns, rep(str_to_sentence(column_to_check), n_errors))
            error_logs$wrong_content_sheets <- c(error_logs$wrong_content_sheets, rep(sheet_name, n_errors))
            error_logs$wrong_content_files <- c(error_logs$wrong_content_files, rep(file_name, n_errors))
          }
          
          # logging mistakes 
        }
      }
    }
  }
  
  return(error_logs)
}

