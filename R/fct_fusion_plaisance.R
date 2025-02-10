#' ---
#' title : "Fusion Plaisance"
#' author : Aubin Woehrel
#' date : 2025-02-07
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - Fusion des données de plaisance et télémètre
#' 
#' Description : 
#' Ce script contient la fonction nécessaire à la fusion des données de comptage
#' terrain de plaisance avec les données de télémètre afin de comptabiliser 
#' réellement tous les navires présents dans le milieu
#' 
#' =============================================================================



find_closest_time <- function(time, date, secteur, theoretical_df) {
  # Filter theoretical times based on date and secteur
  possible_times <- theoretical_df %>%
    filter(date == !!date, secteur == !!secteur) %>%
    pull(horaire) %>%
    unique(.)
  
  # Handle case where no theoretical times are available
  if (length(possible_times) == 0) {
    return(NA)
  }
  
  # Find and return the closest horaire
  possible_times[which.min(abs(as.numeric(time - possible_times)))]
}


fusion_plaisance <- function(data_plaisance, data_telemetre) {
  # Trouve l'horaire la plus proche de data_plaisance pour les mêmes conditions
  data_telemetre <- data_telemetre %>%
    mutate(
      matched_horaire =
        purrr::pmap_dbl(
          list(horaire, date, secteur),
          ~ find_closest_time(..1, ..2, ..3, data_plaisance)
        )
    )
  
  # Résume les infos télémetre par conditions pour éviter 1 ligne = 1 bateau
  summary_telemetre <- data_telemetre %>%
    group_by(date, secteur, matched_horaire, type, statut, taille) %>%
    summarize(nombre = n(), .groups = "drop") %>%
    filter(!is.na(matched_horaire) & !is.na(statut) & !is.na(taille)) %>%
    mutate(matched_horaire = hms::as_hms(matched_horaire))
  
  # Jointure des données et somme des nombres de bateaux
  result <- data_plaisance %>%
    left_join(
      summary_telemetre,
      by = c("date", "secteur", "horaire" = "matched_horaire", "type", "statut", "taille")
    ) %>%
    mutate(nombre = ifelse(!is.na(nombre.y), nombre.x + nombre.y, nombre.x)) %>%
    select(-nombre.y, -nombre.x) 
  
  # Re-calcul de la colonne total_type_statut avec les nouvelles données de nombre de bateaux
  result <- result %>%
    group_by(date, secteur, horaire, type, statut) %>%
    mutate(total_type_statut = sum(nombre, na.rm = TRUE)) %>%
    ungroup() %>%
    relocate(nombre, .after = taille) 
  
  # Renvoie la nouvelle version de compilation plaisance à jour
  return(result)
}



