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


check_column_content <- function(
    column_to_check,
    metadata_reference, 
    file_name, 
    sheet_name,
    error_logs) {
  
  
}