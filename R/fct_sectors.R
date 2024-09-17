#' Vérifie la cohérence d'un nom de secteur
#'
#' Cette fonction vérifie si un nom de secteur donné existe dans la liste de 
#' référence des secteurs. Si le nom n'est pas présent, elle retourne le nom 
#' de secteur correspondant le plus proche.
#'
#' @param sector_name Nom du secteur à vérifier.
#'
#' @return Une liste contenant :
#' - `presence`: Un booléen indiquant si le nom du secteur est présent dans la liste de référence.
#' - `closest_match`: Le nom de secteur correspondant le plus proche si le nom n'est pas présent.
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

#' Retourne une liste de référence des noms de secteurs
#'
#' Cette fonction renvoie un data frame contenant la référence des noms de 
#' secteurs, y compris les noms simples et exacts.
#'
#' @return Un data frame contenant les identifiants des secteurs, les noms simples 
#' et les noms exacts des secteurs.
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
