#' ---
#' title : "Fonctions de vérifications de fichiers"
#' author : Aubin Woehrel
#' date : 2024-09-17
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - FONCTIONS DE VERIFICATION DE FICHIERS
#' 
#' Description : 
#' Script contenant toutes les fonctions permettant de vérifier la cohérence des
#' noms de fichiers et des dates dans les fichiers de comptage terrain.
#' 
#' =============================================================================


#' =============================================================================
#' 
#' Vérifie la cohérence du nom de fichier
#'
#' Cette fonction vérifie si le nom d'un fichier de comptage terrain suit le 
#' format standard attendu.
#'
#' @param file_name Nom du fichier à vérifier.
#' @param counting_type Type de comptage terrain (par ex. "plage", "plaisance").
#'
#' @return Une valeur logique : `TRUE` si le nom du fichier suit le format attendu, `FALSE` sinon.
#' 
#' @export
#'
#' @examples
#' file_coherence("us_med_pnmcca_observatoire_comptage_terrain_plage_2023-08-15.xlsx", "plage")
#' 
file_coherence <- function(file_name, counting_type) {
  
  
  if (counting_type %in% c("plaba", "plandeau")) {
    file_name_type <- paste0(
      "us_med_pnmcca_observatoire_survols_",
      counting_type, 
      "_\\d{4}.xlsx")
    
    
    
  } else if (counting_type %in% 
             c("plage", "plaisance", "meteo", "activites_loisirs", "debarquements")) {
    file_name_type <- paste0(
      "us_med_pnmcca_observatoire_comptage_terrain_",
      counting_type, 
      "_\\d{4}-\\d{2}-\\d{2}\\.xlsx")
  }
  
  file_name_coherence <- grepl(file_name_type, file_name)
  
  return(file_name_coherence)
}


#' =============================================================================
#' 
#' Vérifie la cohérence de la date dans le nom de fichier ou la feuille
#'
#' Cette fonction vérifie si la date dans un nom de fichier suit le format attendu et 
#' compare éventuellement la date dans le nom de fichier à celle d'une feuille spécifique.
#'
#' @param file_name Nom du fichier à vérifier.
#' @param counting_type Type de comptage terrain (par ex. "plage", "plaisance").
#' @param date_format Format de date attendu ("YYYY_MM_DD" ou "DD_MM_YYYY"). 
#' Par défaut "YYYY_MM_DD".
#' @param sheet Optionnel : La feuille avec laquelle vérifier la cohérence de la date. 
#' Si `NULL`, seule la date du nom de fichier est vérifiée.
#'
#' @return Une liste contenant la valeur logique `coherent` indiquant si la date est cohérente, 
#' ainsi que la mauvaise date `wrong_date` si applicable.
#' 
#' @export
#'
#' @examples
#' file_date_coherence("us_med_pnmcca_observatoire_comptage_terrain_plage_2023-08-15.xlsx", "plage")
#' 
file_date_coherence <- function(file_name,
                                counting_type,
                                date_format = "YYYY_MM_DD",
                                sheet = NULL) {
  # Extraction de la date à partir du nom de fichier
  if (date_format == "YYYY_MM_DD") {
    date_comptage <- str_extract_all(file_name, "\\d{4}-\\d{2}-\\d{2}")[[1]]
  } else if (date_format == "DD_MM_YYYY") {
    date_comptage <- str_extract_all(file_name, "\\d{4}_\\d{2}_\\d{2}")[[1]]
  } else {
    stop("Format de date non reconnu")
  }
  
  # Si la vérification concerne uniquement la date dans le nom de fichier
  if (is.null(sheet)) {
    if (is.na(date_comptage[1])) {
      return(list(coherent = FALSE, wrong_date = date_comptage[1]))
    } else {
      return(list(coherent = TRUE, wrong_date = NULL))
    }
    
    # Si la vérification concerne la cohérence entre la date dans le nom de fichier et celle de la feuille
  } else {
    stop("Autres formats non encore pris en charge")
    
    # Exemple pour ajouter une vérification de la cohérence des dates entre les métadonnées et les comptages
    # file_path <- paste0(
    #   "data/raw/comptages_terrain/",
    #   str_to_title(counting_type),
    #   "/",
    #   file_name,
    #   ".xlsx"
    # )
    # sheet_date <- read.xlsx(xlsxFile = file_path, sheet = sheet)
  }
}
