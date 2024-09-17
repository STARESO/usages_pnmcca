#' Title
#'
#' @param compilation_data 
#' @param counting_type 
#'
#' @return
#' @export
#'
#' @examples
post_compilation <- function(compilation_data, counting_type) {
  
  if (counting_type == "plaisance") {
    
    compilation_data <- compilation_data %>%
      # select cols containing __
      tidyr::pivot_longer(cols = contains("__"),
                          names_to = "category",
                          values_to = "nombre") %>%
      # separate category into two columns
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
    
  } else if (counting_type == "activites_loisirs") {
    
    compilation_data <- compilation_data %>%
      # select cols containing __
      tidyr::pivot_longer(cols = contains("__"),
                          names_to = "category",
                          values_to = "nombre") %>%
      # separate category into two columns
      tidyr::separate_wider_delim(col = category,
                                  delim = "__",
                                  names = c("type", "usage"))
    
  } else if (counting_type == "plage") {
    compilation_data <- compilation_data %>%
      rename(personnes_plage = personnes_sur_la_plage, 
             personnes_eau = `personnes_dans_l'eau`)
    
  }
  
  compilation_data <- compilation_data %>%
    mutate(annee = year(date),
           mois = month(date, label = TRUE, abbr = FALSE)) %>%
    # Remove all the accents in the column names
    rename_with(
      .fn = function(x) {
        stringi::stri_trans_general(x, "Latin-ASCII")
      },
      .cols = everything()
    ) %>%
    select(date, annee, mois, secteur, everything(), -commentaires, commentaires) %>%
    mutate(date = as.Date(date)) %>%
    arrange(date, secteur, horaire)
  
  return(compilation_data)
}
