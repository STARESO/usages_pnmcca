#' ---
#' title : "Lecture des métadonnées"
#' author : Aubin Woehrel
#' date : 2024-09-17
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - Lecture des métadonnées
#' 
#' Description : 
#' Script contenant toutes les fonctions permettant de lire et nettoyer les
#' métadonnées des différents types de comptages ou survols.  
#' 
#' =============================================================================


#' =============================================================================
#' 
#' Lecture et nettoyage des données pour un type de données spécifique
#'
#' Cette fonction lit les métadonnées du fichier de référence pour le type de
#' comptage donné et applique plusieurs étapes de nettoyage, notamment le
#' renommage des colonnes, la suppression des accents et la gestion des
#' modalités. Elle renomme les colonnes en minuscules, zupprime les accents et
#' renomme les colonnes spécifiques si nécessaire.
#'
#' @param counting_type Type de comptage/survol
#' 
#' @return Un dataframe de métadonnées nettoyé avec les colonnes renommées et
#' prêtes pour une utilisation ultérieure dans le processus de compilation.
#' 
#' @export
#'
#' @examples
#' read_metadata("plaisance")
#' 
read_metadata <- function(counting_type) {
  
  # Choix de la bonne référence selon le type de données
  if (counting_type %in% c("plaba", "plandeau")) {
    path_metadata <- paths$survols_reference
  } else if (counting_type %in% 
             c("plage", "plaisance", "meteo", "activites_loisirs", "debarquements", "telemetre")) {
    path_metadata <- paths$comptage_reference
  } else {
    stop("Le type de comptage n'est pas valide. Veuillez vérifier le nom de comptage")
  }
  
  metadata <- read.xlsx(
    xlsxFile = path_metadata,
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
