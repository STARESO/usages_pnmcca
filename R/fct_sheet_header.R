#' Vérifie la cohérence de l'en-tête de la feuille, y compris le nombre de colonnes 
#' et les noms des variables
#'
#' Cette fonction vérifie la structure des en-têtes de feuilles de données en 
#' contrôlant le nombre de colonnes et les noms des variables. Elle gère les 
#' cas d'en-têtes simples et doubles et consigne toute divergence entre 
#' l'ensemble de données et les métadonnées de référence.
#'
#' @param data_sheet Un data frame représentant la feuille du jeu de données à vérifier.
#' @param metadata_reference Un data frame contenant les métadonnées de référence 
#' pour les colonnes et les noms des variables.
#' @param file_name Le nom du fichier à partir duquel la feuille est vérifiée.
#' @param sheet Le nom de la feuille à vérifier.
#' @param error_logs Une liste qui enregistre toutes les erreurs rencontrées lors 
#' du processus de vérification.
#'
#' @return Une liste contenant :
#' - `error_flag`: Un booléen indiquant si des erreurs ont été trouvées.
#' - `error_logs`: Les journaux d'erreurs mis à jour, incluant les divergences trouvées.
#' @export
#'
#' @examples
#' # Exemple d'utilisation :
#' verify_sheet_header(data_sheet, metadata_reference, "fichier.xlsx", "Feuille1", error_logs)
verify_sheet_header <- function(data_sheet, 
                                metadata_reference, 
                                file_name, 
                                sheet, 
                                error_logs) {
  # Indicateur pour suivre si des erreurs se sont produites dans la feuille en cours
  error_flag <- FALSE
  
  # Vérifier s'il s'agit d'un cas d'en-tête double (présence de champ2 dans les métadonnées)
  if ("champ2" %in% names(metadata_reference)) {
    # Vérifier le nombre de colonnes
    if (ncol(data_sheet) != nrow(metadata_reference)) {
      # Consigner une erreur si le nombre de colonnes ne correspond pas
      error_logs$wrong_column_amount <- c(error_logs$wrong_column_amount, ncol(data_sheet))
      error_logs$right_column_amount <- c(error_logs$right_column_amount, nrow(metadata_reference))
      error_logs$wrong_column_amount_files <- c(error_logs$wrong_column_amount_files, file_name)
      error_logs$wrong_column_amount_sheets <- c(error_logs$wrong_column_amount_sheets, sheet)
      error_flag <- TRUE
      return(list(error_flag, error_logs))  # Sortir si le nombre de colonnes est incorrect
    }
    
    # Vérifier la première ligne de l'en-tête double
    coherent_variable_names1 <- data_sheet[1, ] == metadata_reference$champ1
    # Vérifier la deuxième ligne de l'en-tête double
    coherent_variable_names2 <- data_sheet[2, ] == metadata_reference$champ2
    
    # Consigner les erreurs pour les divergences dans la première ligne
    if (sum(!coherent_variable_names1) != 0) {
      error_logs$wrong_variable_names <- c(error_logs$wrong_variable_names, data_sheet[1, ][!coherent_variable_names1])
      error_logs$right_variable_names <- c(error_logs$right_variable_names, metadata_reference$champ1[!coherent_variable_names1])
      error_logs$wrong_variable_names_position <- c(error_logs$wrong_variable_names_position, which(!coherent_variable_names1))
      error_logs$wrong_variable_names_files <- c(error_logs$wrong_variable_names_files, rep(file_name, sum(!coherent_variable_names1)))
      error_logs$wrong_variable_names_sheets <- c(error_logs$wrong_variable_names_sheets, rep(sheet, sum(!coherent_variable_names1)))
      error_flag <- TRUE
    }
    
    # Consigner les erreurs pour les divergences dans la deuxième ligne
    if (sum(!coherent_variable_names2) != 0) {
      error_logs$wrong_variable_names <- c(error_logs$wrong_variable_names, data_sheet[2, ][!coherent_variable_names2])
      error_logs$right_variable_names <- c(error_logs$right_variable_names, metadata_reference$champ2[!coherent_variable_names2])
      error_logs$wrong_variable_names_position <- c(error_logs$wrong_variable_names_position, which(!coherent_variable_names2))
      error_logs$wrong_variable_names_files <- c(error_logs$wrong_variable_names_files, rep(file_name, sum(!coherent_variable_names2)))
      error_logs$wrong_variable_names_sheets <- c(error_logs$wrong_variable_names_sheets, rep(sheet, sum(!coherent_variable_names2)))
      error_flag <- TRUE
    }
    
  } else {
    # Cas d'en-tête simple : Vérifier le nombre de colonnes
    if (ncol(data_sheet) != nrow(metadata_reference)) {
      # Consigner une erreur si le nombre de colonnes ne correspond pas
      error_logs$wrong_column_amount <- c(error_logs$wrong_column_amount, ncol(data_sheet))
      error_logs$right_column_amount <- c(error_logs$right_column_amount, nrow(metadata_reference))
      error_logs$wrong_column_amount_files <- c(error_logs$wrong_column_amount_files, file_name)
      error_logs$wrong_column_amount_sheets <- c(error_logs$wrong_column_amount_sheets, sheet)
      error_flag <- TRUE
      return(list(error_flag, error_logs))  # Sortir si le nombre de colonnes est incorrect
    }
    
    # Vérifier les noms de variables dans l'en-tête simple
    coherent_variable_names <- data_sheet[1, ] == metadata_reference$champ
    if (sum(!coherent_variable_names) != 0) {
      error_logs$wrong_variable_names <- c(error_logs$wrong_variable_names, names(data_sheet)[!coherent_variable_names])
      error_logs$right_variable_names <- c(error_logs$right_variable_names, metadata_reference$champ[!coherent_variable_names])
      error_logs$wrong_variable_names_position <- c(error_logs$wrong_variable_names_position, which(!coherent_variable_names))
      error_logs$wrong_variable_names_files <- c(error_logs$wrong_variable_names_files, rep(file_name, sum(!coherent_variable_names)))
      error_logs$wrong_variable_names_sheets <- c(error_logs$wrong_variable_names_sheets, rep(sheet, sum(!coherent_variable_names)))
      error_flag <- TRUE
    }
  }
  
  # Retourner l'indicateur d'erreurs et les journaux d'erreurs mis à jour
  return(list(error_flag, error_logs))
}
