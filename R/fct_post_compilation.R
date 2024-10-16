#' ---
#' title : "Post-compilation"
#' author : Aubin Woehrel
#' date : 2024-09-17
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - POST-COMPILATION DES DONNÉES DE COMPTAGE
#' 
#' Description : 
#' Ce script contient la fonction nécessaire au traitement post-compilation des 
#' données de comptage terrain. 
#' 
#' =============================================================================


#' =============================================================================
#' 
#' Traitement post-compilation des données de comptage
#'
#' Cette fonction effectue un traitement supplémentaire sur les données compilées 
#' en fonction du type de comptage. Selon le type, la fonction effectue des 
#' opérations telles que le pivot, le renommage des colonnes et le calcul 
#' des totaux pour des catégories spécifiques.
#'
#' @param compilation_data Data frame contenant les données de comptage compilées.
#' @param counting_type Type de données de comptage à traiter. Peut prendre l'une 
#' des valeurs suivantes : "plaisance", "activites_loisirs", ou "plage".
#'
#' @return Dataframe traité avec des colonnes supplémentaires et un formatage 
#' spécifique au type de comptage.
#' @export
#'
#' @examples
#' # Exemple d'utilisation pour le type de comptage "plaisance" :
#' processed_data <- post_compilation(compilation_data, "plaisance")
#' 
post_compilation <- function(compilation_data, counting_type) {
  
  # Double en-tête comptage plaisance : distinction statut et taille
  if (counting_type == "plaisance") {
    
    compilation_data <- compilation_data %>%
      tidyr::pivot_longer(cols = contains("__"),
                          names_to = "category",
                          values_to = "nombre") %>%
      tidyr::separate_wider_delim(col = category,
                                  delim = "__",
                                  names = c("statut", "taille"))
    
    total_compilation <- compilation_data %>%
      filter(taille == "total") %>%
      select(-taille) %>%
      rename(total_type_statut = nombre)
    
    compilation_data <- compilation_data %>%
      filter(taille != "total") %>%
      inner_join(total_compilation, ., 
                 by = join_by(horaire, type, commentaires, secteur, date, statut)) %>%
      select(everything(), -total_type_statut, total_type_statut)
    
    # Double en-tête comptage activités de loisirs : distinction type et usage 
  } else if (counting_type == "activites_loisirs") {
    
    compilation_data <- compilation_data %>%
      tidyr::pivot_longer(cols = contains("__"),
                          names_to = "category",
                          values_to = "nombre") %>%
      tidyr::separate_wider_delim(col = category,
                                  delim = "__",
                                  names = c("type", "usage"))
    
    # Renommage des colonnes pour le comptage plage
  } else if (counting_type == "plage") {
    compilation_data <- compilation_data %>%
      rename(personnes_plage = personnes_sur_la_plage, 
             personnes_eau = `personnes_dans_l'eau`)
    
  }
  
  # Colonnes temporelles et réorganisation colonnes
  compilation_data <- compilation_data %>%
    mutate(annee = year(date),
           mois = month(date, label = TRUE, abbr = FALSE),
           jour = mday(date)) %>%
    rename_with(
      .fn = function(x) {
        stringi::stri_trans_general(x, "Latin-ASCII")
      },
      .cols = everything()
    ) %>%
    select(date, annee, mois, jour, secteur, everything(), -commentaires, commentaires) %>%
    mutate(date = as.Date(date)) %>%
    arrange(date, secteur, horaire)
  
  return(compilation_data)
}

