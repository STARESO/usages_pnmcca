#' Vérification et conversion des formats de colonnes
#'
#' Cette fonction vérifie et convertit les formats des colonnes d'une feuille de données donnée en fonction 
#' des formats attendus fournis dans une référence de métadonnées. Elle prend en charge différents formats 
#' de données tels que les entiers, les textes, les facteurs ordonnés et nominaux, les dates et les heures 
#' (formats "h" et temps décimal). Les formats invalides ou incorrects sont consignés dans des journaux pour 
#' inspection.
#'
#' @param data_sheet Un tableau de données représentant la feuille de l'Excel à valider et traiter.
#' @param metadata_reference Un tableau contenant les métadonnées avec les colonnes 'champ1' et 'champ2' 
#'        pour les doubles en-têtes, et 'champ' pour les feuilles avec un seul en-tête. Il contient également 
#'        des informations comme 'format'. Les métadonnées spécifient les formats attendus pour les colonnes 
#'        correspondantes de la feuille de données.
#' @param file_name Une chaîne de caractères représentant le nom du fichier traité. Utilisé pour la 
#'        consignation des erreurs.
#' @param sheet_name Une chaîne de caractères représentant le nom de la feuille traitée. Utilisé pour la 
#'        consignation des erreurs.
#' @param error_logs Une liste pour capturer et consigner toutes les erreurs rencontrées lors du traitement 
#'        des formats de colonnes. Cette liste sera mise à jour avec des informations sur les formats erronés 
#'        ou inconnus, ainsi que les noms des colonnes et fichiers/feuilles où des problèmes ont été détectés.
#' @param is_double_header Un booléen indiquant si la feuille de données utilise une structure de double 
#'        en-tête. Si TRUE, la fonction vérifiera à la fois `champ1` et `champ2` dans la référence de 
#'        métadonnées.
#'
#' @return Une liste contenant deux éléments : 
#'         \item{data_sheet}{La feuille de données modifiée avec des colonnes converties aux formats 
#'         attendus.}
#'         \item{error_logs}{Les journaux d'erreurs mis à jour capturant les problèmes de format 
#'         rencontrés.}
#'
#' @examples
#' @export
check_column_format <- function(data_sheet,
                                metadata_reference,
                                file_name,
                                sheet_name,
                                error_logs,
                                is_double_header) {
  
  # Prétraitement des métadonnées pour gérer les accents et les espaces
  metadata_reference <- metadata_reference %>%
    mutate(across(
      everything(),
      ~ stringi::stri_trans_general(., "Latin-ASCII")
    )) %>%    # Supprimer les accents
    mutate(across(everything(), ~ ifelse(
      grepl("L", .) & (grepl("<", .) | grepl(">", .)),
      gsub("m| ", "", .),
      # Supprimer les 'm' et les espaces
      gsub(" ", "_", tolower(.))
    )))
  
  # Parcourir chaque colonne de la feuille de données
  for (col in names(data_sheet)) {
    # Ignorer les colonnes qui ne doivent pas être validées
    if (col %in% c("date", "heure", "secteur")) {
      next
    }
    
    # Décomposer l'en-tête en champ1 et champ2 (même pour les en-têtes non fusionnés)
    header_parts <- split_fused_headers(col)
    
    # Correspondance des métadonnées pour les cas d'en-tête simple et double
    if (is_double_header) {
      metadata_row <- metadata_reference %>%
        filter(champ1 == header_parts$champ1 &
                 champ2 == header_parts$champ2)
    } else {
      metadata_row <- metadata_reference %>%
        filter(champ == header_parts$champ1)
    }
    
    # Si aucun format correspondant trouvé, consigner une erreur
    if (nrow(metadata_row) == 0) {
      error_logs$unknown_format <- c(error_logs$unknown_format, col)
      error_logs$unknown_format_files <- c(error_logs$unknown_format_files, file_name)
      next
    }
    
    # Extraire le format attendu
    expected_format <- metadata_row$format
    current_col <- data_sheet[[col]]
    
    # Conversion pour le format 'entier'
    if (expected_format == "entier") {
      converted_col <- suppressWarnings(as.integer(current_col))
      non_numeric_values <- which(is.na(converted_col) & !is.na(current_col))
      
      if (length(non_numeric_values) > 0) {
        error_logs$wrong_column_format <- c(error_logs$wrong_column_format, class(current_col))
        error_logs$expected_column_format <- c(error_logs$expected_column_format, expected_format)
        error_logs$wrong_column_format_name <- c(error_logs$wrong_column_format_name, col)
        error_logs$wrong_column_format_sheets <- c(error_logs$wrong_column_format_sheets, sheet_name)
        error_logs$wrong_column_format_files <- c(error_logs$wrong_column_format_files, file_name)
        converted_col[non_numeric_values] <- NA  # Remplacer les valeurs invalides
      }
      
      data_sheet[[col]] <- converted_col
      
      # Conversion pour le format 'texte'
    } else if (expected_format == "texte") {
      if (!is.character(current_col)) {
        data_sheet[[col]] <- as.character(current_col)
      }
      
      # Conversion pour le format 'qualitative_ordinale'
    } else if (expected_format == "qualitative_ordinale") {
      if (!is.ordered(current_col)) {
        data_sheet[[col]] <- factor(current_col, ordered = TRUE)
      }
      
      # Conversion pour le format 'qualitative_nominale'
    } else if (expected_format == "qualitative_nominale") {
      if (!is.factor(current_col)) {
        data_sheet[[col]] <- factor(current_col)
      }
      
      # Conversion pour le format 'date'
    } else if (expected_format == "date") {
      converted_col <- suppressWarnings(as.Date(current_col, format = "%Y-%m-%d"))
      non_date_values <- which(is.na(converted_col) & !is.na(current_col))
      
      if (length(non_date_values) > 0) {
        error_logs$wrong_column_format <- c(error_logs$wrong_column_format, class(current_col))
        error_logs$expected_column_format <- c(error_logs$expected_column_format, expected_format)
        error_logs$wrong_column_format_name <- c(error_logs$wrong_column_format_name, col)
        error_logs$wrong_column_format_sheets <- c(error_logs$wrong_column_format_sheets, sheet_name)
        error_logs$wrong_column_format_files <- c(error_logs$wrong_column_format_files, file_name)
        converted_col[non_date_values] <- NA  # Remplacer les dates invalides
      }
      
      data_sheet[[col]] <- converted_col
      
      # Conversion pour le format 'temps'
    } else if (expected_format == "temps") {
      if (all(grepl("h", current_col, ignore.case = TRUE))) {
        # Remplacer "h" par ":00" pour standardiser au format "HH:MM:SS"
        current_col <- gsub("h", ":00:00", current_col, ignore.case = TRUE)
        
        # Utiliser hms::parse_hms pour convertir les heures, gestion des erreurs
        converted_col <- tryCatch(
          hms::parse_hms(current_col),
          error = function(e) NA
        )
      } else {
        # Utiliser la fonction existante pour gérer le format horaire décimal
        converted_col <- try(suppressWarnings(convert_decimal_to_hms(current_col)), silent = TRUE)
      }
      
      non_time_values <- which(is.na(converted_col) & !is.na(current_col))
      
      if (length(non_time_values) > 0) {
        error_logs$wrong_column_format <- c(error_logs$wrong_column_format, col)
        error_logs$wrong_column_format_files <- c(error_logs$wrong_column_format_files, file_name)
        converted_col[non_time_values] <- NA  # Remplacer les heures invalides
      }
      
      data_sheet[[col]] <- converted_col
      
    } else {
      # Si le format n'est pas reconnu, consigner une erreur
      error_logs$unknown_format <- c(error_logs$unknown_format, col)
      error_logs$unknown_format_files <- c(error_logs$unknown_format_files, file_name)
    }
  }
  
  return(list(data_sheet = data_sheet, error_logs = error_logs))
}
