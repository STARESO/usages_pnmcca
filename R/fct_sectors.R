#' Checks the coherence of a sector name
#'
#' This function checks if a given sector name exists in the reference list 
#' of sector names. If the name is not present, it returns the closest 
#' matching sector name.
#'
#' @param sector_name Name of the sector to check
#'
#' @return A list containing:
#' - `presence`: A Boolean indicating if the sector name is in the reference list
#' - `closest_match`: The closest matching sector name if the name is not present
#' @export
#'
#' @examples
#' sector_coherence("Petit Lotu")
sector_coherence <- function(sector_name) {
  sector_presence <- sector_name %in% ref_secteurs$Secteur_simple
  
  if (!sector_presence) {
    closest_index <- amatch(
      sector_name,
      ref_secteurs$Secteur_simple,
      maxDist = 0.5,
      method = "jw"
    )
    
    if (str_to_lower(sector_name) == "petit lotu") {
      closest_match <- "Mezzanu"
    } else if (!is.na(closest_index)) {
      closest_match <- ref_secteurs$Secteur_simple[closest_index]
    }
  } else {
    closest_match <- NULL
  }
  
  return(list(presence = sector_presence, closest_match = closest_match))
}

#' Returns a reference list of sector names
#'
#' This function returns a dataframe containing the reference of sector 
#' names, including both simple and exact names.
#'
#' @return A dataframe containing sector IDs, simple names, and exact names.
#' @export
#'
#' @examples
#' sector_names()
sector_names <- function() {
  sectors <- ref_secteurs %>%
    select(id,
           Nom_secteur_simple = Secteur_simple,
           Nom_secteur_exact = Secteur)
  
  return(sectors)
}
