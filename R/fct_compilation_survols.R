#' ---
#' title : "Fonction de compilation des survols"
#' author : Aubin Woehrel
#' date : 2024-09-18
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - FONCTION DE COMPILATION DONNEES SURVOLS
#' 
#' Description :
#' Script contenant la fonction principale de compilation des données des 
#' survols de l'observatoire des usages.
#' 
#' =============================================================================


#' =============================================================================
#' 
#' Fonction principale de compilation pour le traitement et la vérification 
#' des fichiers de survols de l'observatoire des usages.
#'
#' Cette fonction exécute plusieurs étapes de vérification des données, de 
#' référencement des erreurs et de compilation des fichiers de survols annuels
#' dans un seul jeu de données.
#'
#' @param counting_type Type de données de comptage à compiler. 
#' Les valeurs possibles actuelles sont : 
#' \describe{
#'   \item{"plaba"}{Pour les données de survols des plages et de baignade.}
#'   \item{"plandeau"}{Pour les données de survols du plan d'eau.}
#' }
#'
#' @return Un dataframe contenant les données compilées du type de comptage 
#' spécifié. Toutes les erreurs rencontrées pendant le processus sont 
#' enregistrées dans un fichier log à l'aide de la fonction \code{mistakes_log}.
#' 
#' @export
#'
#' @examples
#' compilation_survols("plaba")
#' compilation_comptage("plandeau")

compilation_survols <- function(counting_type) {
  
  message(paste("Compilation des données de survols", counting_type))
  
  # Chemin des données pour le type de comptage spécifié
  path_counting_type <- paths[[paste0("survols_", counting_type)]]
  
  # Récupération de tous les noms de fichiers pour le type de comptage spécifié
  file_names <- list.files(path_counting_type) %>%
    .[!grepl("~", .)]  # Suppression des fichiers temporaires
  
  # Métadonnées de référence pour le type de comptage spécifié
  metadata_reference <- read_metadata(counting_type)
  
  # Initialisation des registres d'erreurs
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
    wrong_variable_names_sheets = c(),
    wrong_column_format = c(),
    expected_column_format = c(),
    wrong_column_format_name = c(),
    wrong_column_format_sheets = c(),
    wrong_column_format_files = c(),
    wrong_content = c(),
    wrong_content_suggestion = c(),
    wrong_content_columns = c(),
    wrong_content_sheets = c(),
    wrong_content_files = c()
  )
  
  # Initialisation data frame final
  compilation_survols <- data.frame()
  
  # Barre de progression
  n_iter <- length(file_names)
  pb <- progress::progress_bar$new(
    format = "(:spin) [:bar] :percent [Temps restant : :eta || Temps écoulé : :elapsedfull]",
    total = n_iter,
    complete = "=",
    incomplete = "-",
    current = ">",
    clear = FALSE,
    width = 100,
    force = TRUE
  )
  
  # Boucle sur tous les fichiers excel
  for (file_name in file_names) {
    
    pb$tick()
    
    # Vérification de la cohérence du nom du fichier
    if (!file_coherence(file_name, counting_type)) {
      error_logs$wrong_named_files <- c(error_logs$wrong_named_files, file_name)
      next
    }
    
    # Extraction date nom fichier
    annee_comptage <- str_extract_all(file_name, "\\d{4}")[[1]]
    
    if (is.na(annee_comptage[1])) {
      error_logs$wrong_named_dates <- c(error_logs$wrong_named_dates, file_name)
      next
    }
    
    file_path <- paste0(path_counting_type, file_name)
    
    # Liste des feuilles-onglets du fichier considéré
    list_of_sheets <- excel_sheets(file_path)
    
    if (length(list_of_sheets) != 1) {
      wrong_amount_of_sheets_files <- c(wrong_amount_of_sheets_files, file_name)
    }
    
    sheet <- list_of_sheets[1]
    
    # Deux premières lignes de la feuille pour valider l'en-tête
    data_sheet_header <- read.xlsx(
      xlsxFile = file_path,
      sheet = sheet,
      sep.names = " ",
      fillMergedCells = TRUE,
      colNames = FALSE
    ) %>%
      .[1,]
    
    # Vérification en-têtes et enregistrement erreurs potentielles
    verif_header <- verify_sheet_header(
      data_sheet_header,
      metadata_reference,
      file_name,
      sheet,
      error_logs)
    
    error_logs <- verif_header[[2]] # MAJ Log erreur en-tête
    
    # Feuille suivante si la vérification de l'en-tête échoue
    if (verif_header[[1]]) {
      next
    }
    
    # Pas de double en tête dans le cas des survols
    double_header <- FALSE
    
    # Import des données
    data_sheet <- read.xlsx(
      file_path, 
      sheet = sheet, 
      sep.names = " ", 
      fillMergedCells = TRUE) %>%
      rename_with(~ stringr::str_to_lower(.), everything())
    
    # Nettoyage noms de colonnes
    data_sheet <- data_sheet %>%
      rename_with(~ stringr::str_replace_all(., " ", "_"), everything()) %>%
      rename_with(~ stringi::stri_trans_general(., "Latin-ASCII"), everything())
    
    # Conversion cellules vides en NA et texte "NA" en valeur NA
    data_sheet <- convert_to_na(data_sheet)
    
    data_sheet <- data_sheet %>%
      mutate(across(everything(), ~ stringi::stri_trans_general(., "Latin-ASCII")))
    
    # Reformatage et check validité formats de variables sur base des métadonnées référence
    format_check <- check_column_format(
      data_sheet,
      metadata_reference,
      file_name,
      sheet,
      error_logs,
      double_header)
    
    # Nouveau format des données + log erreurs
    data_sheet <- format_check$data_sheet
    error_logs <- format_check$error_logs
    
    # Check du contenu des colonnes avec les valeurs des métadonnées de référence
    error_logs <- check_column_content(
      data_sheet =  data_sheet,
      metadata_reference = metadata_reference,
      file_name = file_name,
      sheet_name = sheet,
      error_logs = error_logs,
      is_double_header = double_header
    )
    
    # Concaténation de la feuille au jeu de données final
    compilation_survols <- rbind(compilation_survols, data_sheet)
    
  } # Fin des boucles fichiers

error_logs <<- error_logs

# Enregistrement des erreurs dans un fichier log + nombre d'erreurs total
mistakes <- mistakes_log(counting_type, error_logs)

if (mistakes != 0) {
  message(
    paste0(
      "Il y a ",
      mistakes,
      " erreurs rencontrées lors de la compilation.\n",
      "Veuillez consulter le fichier log pour plus d'informations."
    )
  )
} else {
  message("Aucune erreur rencontrée lors de la compilation.")
}

message(paste("Données de comptage", counting_type, "compilées."))

# Renvoi du jeu de données final compilé
return(compilation_survols)
}
