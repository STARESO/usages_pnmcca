#' ---
#' title : "Vérification des noms d'observateurs"
#' author : Aubin Woehrel
#' date : 2025-01-29
#' version : 1.0
#' ---
#'
#' =============================================================================
#'
#' OBSERVATOIRE DES USAGES - VERIFICATION DES NOMS D'OBSERVATEURS
#'
#' Description :
#' Permet de vérifier que les noms des observateurs sont bien présents dans la 
#' liste de référence. Si ce n'est pas le cas, les erreurs des noms sont loggés
#'
#' =============================================================================


#' =============================================================================
#' 
#' Cohérence du nom d'observateur
#' 
#' Cette fonction vérifie si le nom d'un agent donné fait parti de la liste des 
#' observateurs, afin de vérifier toute coquille ou absence de compteur.
observateur_coherence <- function(agent) {
  
  agent_presence <- agent %in% reference_agents$observateur
  
  if (!agent_presence) {
    closest_index <- stringdist::amatch(
      agent,
      reference_agents$observateur,
      maxDist = 0.5,
      method = "jw"
    )
    
    if (!is.na(closest_index)) {
      closest_match <- reference_agents$observateur[closest_index]
    }
    
  } else {
    closest_match <- agent
  }
  
  return(list(presence = agent_presence, closest_match = closest_match))
}

#' =============================================================================
#' 
#' Cohérence d'une liste de noms d'observateurs
#' 
#' Cette fonction itère observateur_coherence sur une liste d'agents donnée.
liste_observateur_coherence <- function(liste_agents) {
  
  liste_agents <- as.character(liste_agents)
  
  if (is.na(liste_agents)) {
    return(list(
      agents = NA_character_,
      presence = TRUE,
      closest_match = NA_character_
    ))
  }
  
  # Séparation des éléments de la liste d'agents par le séparateur virgule et trim espace
  liste_agents <- liste_agents %>%
    str_split_1(., ",") %>%
    str_trim(side = "left")
  
  # Itération observateur cohérence
  results <- purrr::map(.x = liste_agents, .f = observateur_coherence)
  transposed <- purrr::transpose(results)
  
  list(
    agents = liste_agents,
    presence = unlist(transposed$presence, use.names = FALSE),
    closest_match = unlist(transposed$closest_match, use.names = FALSE)
  )
}


#' =============================================================================
#' 
#' Log des erreurs
#' 
#' Cette fonction log les erreurs de noms d'agents
log_wrong_observers <- function(liste_agents, sheet_name, file_name, counting_type, error_logs) {
  
  
  
  if (counting_type == "meteo") {
    
    for (i in 1:nrow(liste_agents)) {
      results <- liste_observateur_coherence(liste_agents[i, ])
      
      wrong_indices <- which(!results$presence)
      
      # If there are incorrect observers, log them
      if (length(wrong_indices) > 0) {
        error_logs$wrong_observer <- c(error_logs$wrong_observer, results$agents[wrong_indices])
        error_logs$wrong_observer_suggestion <- c(error_logs$wrong_observer_suggestion, results$closest_match[wrong_indices])
        error_logs$wrong_observer_line <- c(error_logs$wrong_observer_line, rep(i, length(wrong_indices)))
        error_logs$wrong_observer_files <- c(error_logs$wrong_observer_files, rep(file_name, length(wrong_indices)))
      }
    }
    
  } else {
    
    # Apply liste_observateur_coherence to the list of observers
    results <- liste_observateur_coherence(liste_agents)
    
    # Find indices of incorrect observers (presence == FALSE)
    wrong_indices <- which(!results$presence)
    
    # If there are incorrect observers, log them
    if (length(wrong_indices) > 0) {
      error_logs$wrong_observer <- c(error_logs$wrong_observer, results$agents[wrong_indices])
      error_logs$wrong_observer_suggestion <- c(error_logs$wrong_observer_suggestion, results$closest_match[wrong_indices])
      error_logs$wrong_observer_sector <- c(error_logs$wrong_observer_sector, rep(sheet_name, length(wrong_indices)))
      error_logs$wrong_observer_files <- c(error_logs$wrong_observer_files, rep(file_name, length(wrong_indices)))
    }
  }
  
  return(error_logs)
}
