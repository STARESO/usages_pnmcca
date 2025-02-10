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
                                  names = c("statut", "taille")) %>%
      
      # Transformation notation taille
      mutate(taille = str_replace_all(
        taille,
        c("<L<" = "_", "L<" = "", "L>" = "")
      ))
    
    total_compilation <- compilation_data %>%
      filter(taille == "total") %>%
      select(-taille) %>%
      rename(total_type_statut = nombre)
    
    compilation_data <- compilation_data %>%
      filter(taille != "total") %>%
      inner_join(total_compilation, ., 
                 by = join_by(horaire, type, commentaires, commentaires_secteur,
                              observateurs, secteur, date, statut)) %>%
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
  
  if (counting_type %in% c("plaba", "plandeau")) {
    compilation_data <- compilation_data %>%
      rename(date = date_camp) %>%
      mutate(annee = year(date),
             mois = month(date, label = TRUE, abbr = FALSE),
             jour = mday(date)) %>%
      rename_with(
        .fn = function(x) {
          stringi::stri_trans_general(x, "Latin-ASCII")
        },
        .cols = everything()
      ) 
    
    # Recatégorisation des activites dans les grandes catégories correspondantes
    if (counting_type == "plandeau") {
      compilation_data <- compilation_data %>%
        mutate(categorie_usage = case_when(
          act %in% c(
            "Plaisance a moteur non habitable", 
            "Plaisance a voile habitable", 
            "Plaisance a moteur habitable",
            "Plaisance a voile non habitable"
            ) ~ 
            "plaisance",
          
          act %in% c(
            "Sports motonautiques sur VNM",
            "Bouee tractee"
            ) ~ 
            "sports_motonautiques",
          
          act %in% c(
            "Kayak", 
            "Stand-Up Paddle", 
            "Barque", 
            "Monocoque (optimiste, deriveur)", 
            "Planche a voile",
            "Pedalo",
            "Multicoque (catamaran, trimaran)",
            "Kite surf (ou planche aerotractee)",
            "Bodyboard",
            "Wingfoil",
            "Activites de baignade avec equipement" # Pas sûr de celui-ci
            ) ~ 
            "activites_non_motorisees",
          
          act %in% c("Peche de loisir embarquee sans precision") ~ 
            "peche_de_loisirs",
          
          act %in% c("Velo tout terrain") ~ 
            "activites_des_plages",
          
          act %in% c("Plongee scaphandre") ~ 
            "plongee",
        
          act %in% c("Transport de passagers - bateau-bus ou bacs", 
                     "Transport de passagers - ferry",
                     "Promenade en mer sur bateau moteur") ~ 
            "transport_des_passagers_et_promenades_en_mer",
          
          act %in% c("Objet temoignant d'une activite de loisir non identifiee") ~
            "non_identifie",
          
          TRUE ~ act
        ))
      
    } else if (counting_type == "plaba") {
      compilation_data <- compilation_data %>%
        mutate(categorie_usage = case_when(
          act %in% c(
            "PMT/Randonnee subaquatique", 
            "Baignade", 
            "Bronzage/repos sur la plage"
          ) ~ "activites_des_plages",
          TRUE ~ act
        ))
    }
    
    compilation_data <- compilation_data %>%
      select(
        id_acti, 
        lon_x, 
        lat_y, 
        date, 
        annee, 
        mois, 
        jour, 
        secteur, 
        nom_acti, 
        act, 
        categorie_usage,
        everything()) %>%
      mutate(date = as.Date(date))
    
  } else {
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
      select(date, annee, mois, jour, secteur, everything(), observateurs, -commentaires, 
             commentaires) %>%
      mutate(date = as.Date(date)) %>%
      arrange(date, secteur, horaire)
    
    if (!counting_type %in% c("meteo", "telemetre")) {
      compilation_data <- compilation_data %>%
        relocate(commentaires, commentaires_secteur, .after = last_col())
    }
  }
  
  compilation_data <- compilation_data %>%
    mutate(across(where(is.character) | where(is.factor), ~ stri_trans_general(., "Latin-ASCII"))) %>%
    mutate(across(where(is.character) | where(is.factor), ~ str_replace_all(., "&#10;", " "))) %>%
    mutate(across(where(is.character) | where(is.factor), ~ str_replace_all(., ";", ",")))
    
 
  return(compilation_data)
}

