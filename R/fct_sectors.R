#' Checks a coherence of a sector name
#'
#' @param sector_name Name of the sector to check
#'
#' @return List containing :
#' - presence of the sector name in the reference list as a Boolean
#' - the closest match if the name is not present
#' @export
#'
#' @examples
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
    }
    
    else if (!is.na(closest_index)) {
      closest_match <- ref_secteurs$Secteur_simple[closest_index]
    }
  } else {
    closest_match <- NULL
  }
  
  return(list(presence = sector_presence, closest_match = closest_match))
  
}


#' Returns reference of sector names
#'
#' @return Dataframe containing the reference of sector names
#' @export
#'
#' @examples
sector_names <- function() {
  sectors <- ref_secteurs %>%
    select(id,
           Nom_secteur_simple = Secteur_simple,
           Nom_secteur_exact = Secteur)
  
  return(sectors)
  
}