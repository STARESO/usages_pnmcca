#' ---
#' title : "Fonction de compilation principale"
#' author : Aubin Woehrel
#' date : 2024-09-18
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - FONCTION DE COMPILATION DONNEES COMPTAGE TERRAIN
#' 
#' Description :
#' Script contenant la fonction principale de compilation des données de 
#' comptage terrain de l'observatoire des usages.
#' 
#' =============================================================================


#' =============================================================================
#' 
#' Fonction principale de compilation pour le traitement et la vérification 
#' des fichiers de comptage terrain de l'observatoire des usages.
#'
#' Cette fonction exécute plusieurs étapes de vérification des données, de 
#' référencement des erreurs et de compilation des fichiers de comptage 
#' individuels dans un seul jeu de données.
#'
#' @param counting_type Type de données de comptage à compiler. 
#' Les valeurs possibles actuelles sont : 
#' \describe{
#'   \item{"plage"}{Pour les données de comptage plage.}
#'   \item{"plaisance"}{Pour les données de plaisance.}
#'   \item{"meteo"}{Pour les données météorologiques.}
#'   \item{"activites_loisirs"}{Pour les données d'activités de loisirs.}
#' }
#'
#' @return Un dataframe contenant les données compilées du type de comptage 
#' spécifié. Toutes les erreurs rencontrées pendant le processus sont 
#' enregistrées dans un fichier log à l'aide de la fonction \code{mistakes_log}.
#' 
#' @export
#'
#' @examples
#' compilation_comptage("plage")
#' compilation_comptage("meteo")
#' 

compilation_comptage <- function(counting_type) {
  
  message(paste("Compilation des données de comptage", counting_type))
  
  # Chemin des données pour le type de comptage spécifié
  path_counting_type <- paths[[paste0("comptage_", counting_type)]]
  
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
    wrong_column_format_files = c()
  )
  
  # Initialisation data frame final
  comptage_terrain <- data.frame()
  
  # Barre de progression
  n_iter <- length(file_names)
  pb <- progress::progress_bar$new(
    format = "(:spin) [:bar] :percent [Temps écoulé : :elapsedfull || Temps estimé restant : :eta]",
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
    date_comptage <- str_extract_all(file_name, "\\d{4}-\\d{2}-\\d{2}")[[1]]
    if (is.na(date_comptage[1])) {
      error_logs$wrong_named_dates <- c(error_logs$wrong_named_dates, file_name)
      next
    }
    
    file_path <- paste0(path_counting_type, file_name)
    
    # Liste des feuilles-onglets du fichier considéré
    list_of_sheets <- excel_sheets(file_path) %>%
      {c(.[grepl("metadata", .)], .[!grepl("metadata", .)])}
    
    # Boucle sur les feuilles-onglets du fichier
    for (sheet in list_of_sheets) {
      if (sheet == "metadata_comptages") {
        # Feuille métadonnées des comptages
        metadata_comptage <- read.xlsx(file_path, sheet = sheet) %>%
          mutate(Date = janitor::excel_numeric_to_date(Date))
        
        # Vérification : nombre de feuilles correspond à la liste des secteurs des métadonnées ?
        if (length(metadata_comptage$Secteur) != length(setdiff(list_of_sheets, "metadata_comptages"))) {
          error_logs$wrong_amount_of_sheets_files <- c(error_logs$wrong_amount_of_sheets_files, file_name)
          break # Passe au fichier suivant si le nombre de feuilles est incorrect
        } 
        
        # Vérifie que tous les secteurs de métadonnées existent sous forme de feuilles
        meta_not_in_sheet <- !(metadata_comptage$Secteur %in% 
                                 list_of_sheets[!grepl("metadata", list_of_sheets)])
        
        if (sum(meta_not_in_sheet) != 0) {
          error_logs$error_meta_not_in_sheets <- c(error_logs$error_meta_not_in_sheets,
                                                   metadata_comptage$Secteur[meta_not_in_sheet])
          error_logs$error_meta_not_in_sheets_files <- c(error_logs$error_meta_not_in_sheets_files,
                                                         rep(file_name, sum(meta_not_in_sheet)))
          break # Passe au fichier suivant si les métadonnées ne correspondent pas
        }
        
      } else {
        
        # En cas de comptage terrain conditions météo
        if (counting_type == "meteo") {
          if (sheet != "Meteo") { # Check du nom de la feuille
            error_logs$wrong_named_sheets <- c(error_logs$wrong_named_sheets, sheet)
            error_logs$suggested_names_sheets <- c(error_logs$suggested_names_sheets, "Meteo")
            error_logs$wrong_named_sheets_files <- c(error_logs$wrong_named_sheets_files, file_name)
            next
          }
          
        } else {
          # Vérifie les noms de secteur pour les fichiers non météo
          sector_check <- sector_coherence(sheet)
          
          # Enregistre le mauvais nom de secteur + suggestion de nom correspondant au plus proche
          if (!sector_check$presence) {
            error_logs$wrong_named_sheets <- c(error_logs$wrong_named_sheets, sheet)
            error_logs$suggested_names_sheets <- c(error_logs$suggested_names_sheets, sector_check$closest_match)
            error_logs$wrong_named_sheets_files <- c(error_logs$wrong_named_sheets_files, file_name)
            next 
          }
          
          # Vérifie si la feuille existe dans metadata_comptage
          if (!sheet %in% metadata_comptage$Secteur) {
            error_logs$error_sheet_not_in_meta <- c(error_logs$error_sheet_not_in_meta, sheet)
            error_logs$error_sheet_not_in_meta_files <- c(error_logs$error_sheet_not_in_meta_files, file_name)
            next
          }
        }
        
        # Deux premières lignes de la feuille pour valider l'en-tête
        data_sheet_header <- read.xlsx(
          xlsxFile = file_path,
          sheet = sheet,
          sep.names = " ",
          fillMergedCells = TRUE,
          colNames = FALSE
        ) %>%
          .[1:2, ]
        
        # Vérification en-têtes et enregistrement erreurs potentielles
        verif_header <- verify_sheet_header(data_sheet_header, metadata_reference, file_name, sheet, 
                                            error_logs)
        error_logs <- verif_header[[2]] # MAJ Log erreur en-tête
        
        # Feuille suivante si la vérification de l'en-tête échoue
        if (verif_header[[1]]) {
          next
        }
        
        # Importation données en utilisant la structure d'en-tête double si applicable
        if ("champ2" %in% names(metadata_reference)) { # En tête double
          data_sheet <- double_header_import(file_path, sheet)
        } else { # En tête simple
          data_sheet <- read.xlsx(file_path, sheet = sheet, sep.names = " ", fillMergedCells = TRUE) %>%
            rename_with(~ stringr::str_to_lower(.), everything())
        }
        
        # Nettoyage noms de colonnes
        data_sheet <- data_sheet %>%
          rename_with(~ str_replace_all(., " ", "_"), everything()) 
        
        # Conversion cellules vides en NA et texte "NA" en valeur NA
        data_sheet <- convert_to_na(data_sheet)
        
        # Reformatage et check validité formats de variables sur base des métadonnées référence
        if ("champ2" %in% names(metadata_reference)) {
          format_check <- check_column_format(
            data_sheet, metadata_reference, file_name, sheet, error_logs, is_double_header = TRUE
          )
        } else {
          format_check <- check_column_format(
            data_sheet, metadata_reference, file_name, sheet, error_logs, is_double_header = FALSE
          )
        }
        
        # Nouveau format des données + log erreurs
        data_sheet <- format_check$data_sheet
        error_logs <- format_check$error_logs
        
        
        # Ajout des colonnes date et secteur aux données
        if (counting_type != "meteo") {
          data_sheet <- data_sheet %>%
            mutate(
              secteur = rep(sheet, nrow(.)),
              date = rep(date_comptage, nrow(.))
            )
        }
        
        # Concaténation de la feuille au jeu de données final
        comptage_terrain <- rbind(comptage_terrain, data_sheet)
        
      } # Fin des feuilles autres que metadata_comptages
    }  # Fin boucle feuilles
  }  # Fin boucle fichiers
  
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
  return(comptage_terrain)
}
