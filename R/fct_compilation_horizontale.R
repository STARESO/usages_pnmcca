#' ---
#' title : "Compilation horizontale"
#' author : Aubin Woehrel
#' date : 2025-07-02
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - COMPILATION HORIZONTALE
#' 
#' Description : 
#' Ce script réalise un simili-BD Mer avec 1 ligne = 1 secteur à 1 date et 
#' 1 horaire pour toutes les données de plaisance, météo, plage et activites
#' 
#' =============================================================================

compilation_horizontale <- function(
    data_meteo, 
    data_plaisance, 
    data_activites, 
    data_plage) {
  
  plaisance_horizontal <- data_plaisance %>%
    mutate(desc_col = paste(statut, str_to_lower(type), taille, sep = "_")) %>%
    pivot_wider(., 
                id_cols = c("date", "annee", "mois", "jour", "secteur", "horaire"),
                names_from = desc_col, 
                values_from = nombre) %>%
    mutate(horaire = hms::as_hms(lubridate::round_date(as.POSIXct(horaire), unit = "hour")))
  
  
  plage_horizontal <- data_plage %>%
    mutate(horaire = hms::as_hms(lubridate::round_date(as.POSIXct(horaire), unit = "hour"))) %>%
    select(-c(commentaires, observateurs))
  
  activites_horizontal <- data_activites %>%
    mutate(horaire = hms::as_hms(lubridate::round_date(as.POSIXct(horaire), unit = "hour"))) %>%
    pivot_wider(., 
                id_cols = c("date", "annee", "mois", "jour", "secteur", "horaire"),
                names_from = type, 
                values_from = nombre)
  
  
  pseudo_dbmer <- full_join(data_meteo, plaisance_horizontal, by = join_by(date, annee, mois, jour, secteur, horaire))  
  pseudo_dbmer <- full_join(pseudo_dbmer, activites_horizontal, by = join_by(date, annee, mois, jour, secteur, horaire))
  pseudo_dbmer <- full_join(pseudo_dbmer, plage_horizontal, by = join_by(date, annee, mois, jour, secteur, horaire))  
  
  pseudo_dbmer <- pseudo_dbmer %>%
    arrange(date, secteur, horaire) %>%
    relocate(observateurs, commentaires, commentaires_secteur, .after = last_col()) 
  
  return(pseudo_dbmer)
}

  