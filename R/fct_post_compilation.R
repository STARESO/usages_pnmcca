#' Post-compilation processing of counting data
#'
#' This function performs additional processing on the compiled data based on 
#' the type of counting. Depending on the `counting_type`, the function 
#' performs operations such as pivoting, renaming columns, and calculating 
#' totals for specific categories.
#'
#' @param compilation_data Data frame containing the compiled counting data.
#' @param counting_type Type of counting data to process. Can be one of the 
#' following values: "plaisance", "activites_loisirs", or "plage".
#'
#' @return A processed data frame with additional columns and formatting 
#' according to the counting type.
#' @export
#'
#' @examples
#' # Example usage for plaisance counting type:
#' processed_data <- post_compilation(compilation_data, "plaisance")
post_compilation <- function(compilation_data, counting_type) {
  
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
    
  } else if (counting_type == "activites_loisirs") {
    
    compilation_data <- compilation_data %>%
      tidyr::pivot_longer(cols = contains("__"),
                          names_to = "category",
                          values_to = "nombre") %>%
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
