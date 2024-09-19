#' ---
#' title : "Conversions"
#' author : Aubin Woehrel
#' date : 2024-09-17
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - FONCTIONS DE CONVERSION
#' 
#' Description : 
#' Ce script contient des fonctions de conversion utilisées lors du nettoyage
#' des données.
#' 
#' =============================================================================


#' =============================================================================
#' 
#' Conversion des chaînes vides et des textes "NA" en valeur NA
#'
#' @param data_sheet Jeu de données à nettoyer.
#'
#' @return Un data frame nettoyé où les chaînes vides et "NA" ont été remplacées 
#' par des NA.
#'
#' @export
#'
#' @examples
#' cleaned_data <- convert_to_na(my_data_sheet)
#' 
convert_to_na <- function(data_sheet) {
  data_sheet <- data_sheet %>%
    mutate(across(
      everything(),
      ~ na_if(trimws(.), "")  
    )) %>%
    mutate(across(
      everything(),
      ~ na_if(., "NA")    
    ))
  
  return(data_sheet)
}


#' =============================================================================
#' 
#' Conversion heure décimale en format HH:MM:SS
#'
#' Convertit une heure décimale (par ex., 0.5 pour 12h00) au format HH:MM:SS.
#'
#' @param decimal_time Un vecteur numérique avec des heures au format décimal 
#' (par ex., 0.5 = 12h00).
#'
#' @return Un vecteur d'heures au format HH:MM:SS.
#'
#' @export
#'
#' @examples
#' time_formatted <- convert_decimal_to_hms(c(0.5, 0.25, 0.75))
#' 
convert_decimal_to_hms <- function(decimal_time) {
  total_seconds <- as.numeric(decimal_time) * 24 * 60 * 60
  time_formatted <- hms::hms(seconds = total_seconds)
  time_formatted <- hms::round_hms(time_formatted, 60)
  
  return(time_formatted)
}