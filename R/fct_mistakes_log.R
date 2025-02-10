#' ---
#' title : "Journalisation des erreurs de compilation"
#' author : Aubin Woehrel
#' date : 2024-09-17
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - JOURNALISATION DES ERREURS DE COMPILATION
#' 
#' Description : 
#' Ce script contient une fonction permettant de générer une journalisation des
#' erreurs rencontrées lors de la compilation des fichiers de comptage terrain.
#' Le fichier de journalisation est organisé par type d'erreur et des suggestions
#' de correction sont fournies. Il est enregistré sous format .log.
#' 
#' =============================================================================


#' =============================================================================
#' 
#' Journalisation des erreurs de compilation
#'
#' Cette fonction génère un fichier journal détaillant les erreurs rencontrées 
#' lors de la compilation des fichiers de comptage terrain. 
#'
#' @param counting_type Type de comptage terrain (par exemple, "plage", "plaisance").
#' @param error_logs Liste contenant toutes les erreurs rencontrées lors du traitement.
#'
#' @return Nombre total d'erreurs trouvées.
#' @export
#'
#' @examples
#' mistakes_log("plaisance", error_logs)
#' 
mistakes_log <- function(counting_type, error_logs) {
  
  accepted_counting <- c("plage", "plaisance", "meteo", "activites_loisirs", "debarquements")
  # Initialisation ----
  # Type de comptage
  counting_type_simple <- counting_type
  counting_type <- paste0("comptage_terrain_", counting_type)
  
  # Emplacement du nouveau fichier log
  log_file <- paste0(
    paths$verification_logs,
    "compilation_",
    counting_type,
    "_",
    format(Sys.time(), "%Y-%m-%d"),
    ".log"
  )
  
  # Création du fichier log
  file.create(log_file)
  
  # Initialisation du fichier log
  
  write(paste0(
    "------ ", 
    "Vérification des données de ",
    str_replace_all(counting_type, "_", " "),
    " ------\n"
  ), log_file, append = TRUE)
  
  # Nombre total d'erreurs
  number_of_errors <- sum(
    sapply(error_logs[c(
      "wrong_named_files", 
      "wrong_named_dates", 
      "wrong_named_sheets",
      "wrong_amount_of_sheets_files", 
      "error_sheet_not_in_meta",
      "error_meta_not_in_sheets", 
      "wrong_column_amount_files",
      "wrong_variable_names_files",
      "unknown_format",
      "wrong_column_format",
      "wrong_content",
      "wrong_observer"
    )], length)
  )
  
  
  write(paste0("Nombre total d'erreurs trouvées durant la vérification : ", 
               number_of_errors),
        log_file,
        append = TRUE)
  
  if (number_of_errors > 0) {
    write(
      paste0(
        "Les erreurs spécifiques sont indiquées dans la suite de ce document.\n",
        "Attention : les fichiers contenant des erreurs ne sont pas enregistrées dans le jeu de données compilé.\n",
        "La correction des erreurs permettra d'inclure ces données dans le fichier compilé final.\n"
      ),
      log_file,
      append = TRUE
    )
  } else {
    write(paste0("\n"), log_file, append = TRUE)
  }
  
  
  # Fichiers mal nommés ----
  write(paste0(
    "---------------------------------------------------------------- \n",
    "Fichiers mal nommés :\n",
    "---------------------------------------------------------------- \n"
  ), log_file, append = TRUE)
  
  if (length(error_logs$wrong_named_files) > 0) {
    write(
      paste0(
        "Les fichiers de ", counting_type, 
        " doivent être au format PNMCCA_usages_plaisance_", 
        counting_type, "_YYYY-MM-DD.xlsx \n"
      ),
      log_file,
      append = TRUE
    )
    
    for (wrong_file in error_logs$wrong_named_files) {
      write(paste0(" > ", wrong_file, "\n"), log_file, append = TRUE)
    }
    
    write("\n", log_file, append = TRUE)
    
  } else {
    write("Tous les fichiers sont correctement nommés.\n", log_file, append = TRUE)
  }
  

  if (counting_type_simple %in% accepted_counting) {
    
    # Nombre d'onglets incohérents ----
    write(paste0(
      "---------------------------------------------------------------- \n",
      "Nombre d'onglets (secteurs) incohérents :\n",
      "---------------------------------------------------------------- \n"
    ), log_file, append = TRUE)
    
    if (length(error_logs$wrong_amount_of_sheets_files) > 0) {
      write("Les fichiers suivants ont un nombre d'onglets incohérent : \n", 
            log_file, append = TRUE)
      
      for (wrong_file in error_logs$wrong_amount_of_sheets_files) {
        write(paste0(" > ", wrong_file, "\n"), log_file, append = TRUE)
      }
      
      write(
        "Veuillez faire correspondre le nombre d'onglets avec les métadonnées.\n\n", 
        log_file, append = TRUE
      )
      
    } else {
      write("Le nombre d'onglets est cohérent avec les métadonnées.\n", 
            log_file, append = TRUE)
    }
    
    # Onglets mal nommés ----
    write(paste0(
      "---------------------------------------------------------------- \n",
      "Onglets (secteurs) mal nommés : \n",
      "---------------------------------------------------------------- \n"
    ), log_file, append = TRUE)
    
    if (length(error_logs$wrong_named_sheets) > 0) {
      write(
        paste0(
          "Certains noms d'onglets des données sources ne correspondent pas aux ",
          "noms de secteurs acceptés.\n",
          "Veuillez les renommer avec un nom de secteur valide.\n"
        ),
        log_file,
        append = TRUE
      )
      
      for (concerned_file in unique(error_logs$wrong_named_sheets_files)) {
        write(paste0(" > Pour le fichier ", concerned_file, ":\n"), log_file, append = TRUE)
        
        concerned_sheets <- error_logs$wrong_named_sheets[
          error_logs$wrong_named_sheets_files == concerned_file]
        concerned_suggestions <- error_logs$suggested_names_sheets[
          error_logs$wrong_named_sheets_files == concerned_file]
        
        for (i in seq_along(concerned_sheets)) {
          write(
            paste0("\t", concerned_sheets[i], " --> ", concerned_suggestions[i], "\n"),
            log_file,
            append = TRUE
          )
        }
      }
      
      write("", log_file, append = TRUE)
      
    } else {
      write("Tous les secteurs sont correctement nommés.\n", log_file, append = TRUE)
    }
    
    # Incohérences entre les métadonnées et les onglets ----
    write(paste0(
      "---------------------------------------------------------------- \n",
      "Incohérence entre les noms des secteurs et les métadonnées : \n",
      "---------------------------------------------------------------- \n"
    ), log_file, append = TRUE)
    
    if (length(error_logs$error_sheet_not_in_meta) > 0 | 
        length(error_logs$error_meta_not_in_sheets) > 0) {
      
      if (length(error_logs$error_sheet_not_in_meta) > 0) {
        write("Les fichiers suivants contiennent des noms d'onglets incohérents :\n", 
              log_file, append = TRUE)
        
        for (concerned_file in unique(error_logs$error_sheet_not_in_meta_files)) {
          write(paste0(" > Pour le fichier ", concerned_file, ":\n"), log_file, append = TRUE)
          
          concerned_sheets <- error_logs$error_sheet_not_in_meta[
            error_logs$error_sheet_not_in_meta_files == concerned_file]
          
          for (i in seq_along(concerned_sheets)) {
            write(paste0("\t", concerned_sheets[i], "\n"), log_file, append = TRUE)
          }
        }
      }
      
      if (length(error_logs$error_meta_not_in_sheets) > 0) {
        write(
          paste0(
            "Certains secteurs présents dans l'onglet metadata_comptages n'ont ",
            "pas de feuilles correspondantes :\n"
          ),
          log_file,
          append = TRUE
        )
        
        for (concerned_file in unique(error_logs$error_meta_not_in_sheets_files)) {
          write(paste0(" > Pour le fichier ", concerned_file, ":\n"), log_file, append = TRUE)
          
          concerned_sheets <- error_logs$error_meta_not_in_sheets[
            error_logs$error_meta_not_in_sheets_files == concerned_file]
          
          for (i in seq_along(concerned_sheets)) {
            write(paste0("\t", concerned_sheets[i], "\n"), log_file, append = TRUE)
          }
        }
      }
      
      write("", log_file, append = TRUE)
      
    } else {
      write("Pas d'incohérence entre les noms des secteurs et les métadonnées.\n", 
            log_file, append = TRUE)
    }
  }
  
  
  # Nombre de variables incorrect ----
  write(paste0(
    "---------------------------------------------------------------- \n",
    "Nombre de variables incorrect : \n",
    "---------------------------------------------------------------- \n"
  ), log_file, append = TRUE)
  
  if (length(error_logs$wrong_column_amount) > 0) {
    write(
      paste0(
        "Certains fichiers contiennent un nombre de variables incorrect.\n",
        "Veuillez vérifier que le nombre de colonnes correspond à la référence.\n"
      ),
      log_file,
      append = TRUE
    )
    
    for (concerned_file in unique(error_logs$wrong_column_amount_files)) {
      write(paste0(" > Pour le fichier ", concerned_file, ":\n"), log_file, append = TRUE)
      
      for (concerned_sheet in unique(error_logs$wrong_column_amount_sheets[
        error_logs$wrong_column_amount_files == concerned_file])) {
        
        write(paste0("\t Pour l'onglet ", concerned_sheet, ":\n"), log_file, append = TRUE)
        
        write(
          paste0(
            "\t Nombre de colonnes attendu : ",
            error_logs$right_column_amount[
              error_logs$wrong_column_amount_files == concerned_file &
                error_logs$wrong_column_amount_sheets == concerned_sheet], "\n"
          ),
          log_file,
          append = TRUE
        )
        
        write(
          paste0(
            "\t Nombre de colonnes observé : ",
            error_logs$wrong_column_amount[
              error_logs$wrong_column_amount_files == concerned_file &
                error_logs$wrong_column_amount_sheets == concerned_sheet], "\n"
          ),
          log_file,
          append = TRUE
        )
      }
    }
    
    write("", log_file, append = TRUE)
    
  } else {
    write("Le nombre de variables est correct.\n", log_file, append = TRUE)
  }
  
  # Noms de variables incorrects ----
  write(paste0(
    "---------------------------------------------------------------- \n",
    "Noms de variables incorrects : \n",
    "---------------------------------------------------------------- \n"
  ), log_file, append = TRUE)
  
  if (length(error_logs$wrong_variable_names) > 0) {
    write(
      paste0(
        "Certains noms de variables ne correspondent pas aux noms de variables ",
        "acceptés.\nVeuillez les renommer avec un nom de variable valide.\n"
      ),
      log_file,
      append = TRUE
    )
    
    for (concerned_file in unique(error_logs$wrong_variable_names_files)) {
      write(paste0(" > Pour le fichier ", concerned_file, ":\n"), log_file, append = TRUE)
      
      for (concerned_sheet in unique(error_logs$wrong_variable_names_sheets[
        error_logs$wrong_variable_names_files == concerned_file])) {
        
        write(paste0("\t Pour l'onglet ", concerned_sheet, ":\n"), log_file, append = TRUE)
        
        concerned_variables <- error_logs$wrong_variable_names[
          error_logs$wrong_variable_names_files == concerned_file &
            error_logs$wrong_variable_names_sheets == concerned_sheet]
        
        concerned_positions <- error_logs$wrong_variable_names_position[
          error_logs$wrong_variable_names_files == concerned_file &
            error_logs$wrong_variable_names_sheets == concerned_sheet]
        
        concerned_corrections <- error_logs$right_variable_names[
          error_logs$wrong_variable_names_files == concerned_file &
            error_logs$wrong_variable_names_sheets == concerned_sheet]
        
        for (i in seq_along(concerned_variables)) {
          write(
            paste0(
              "\t Nom attendu à la colonne ",
              str_to_upper(letters[concerned_positions[i]]),
              " : ", concerned_corrections[i], "\n"
            ),
            log_file,
            append = TRUE
          )
          
          write(
            paste0(
              "\t Nom observé à la colonne ",
              str_to_upper(letters[concerned_positions[i]]),
              " : ", concerned_variables[i], "\n"
            ),
            log_file,
            append = TRUE
          )
        }
      }
    }
    
    write("", log_file, append = TRUE)
    
  } else {
    write("Tous les noms de variables sont corrects.\n", log_file, append = TRUE)
  }
  
  # Types de variables non reconnus ----
  write(paste0(
    "---------------------------------------------------------------- \n",
    "Noms de types de variables non reconnus : \n",
    "---------------------------------------------------------------- \n"
  ), log_file, append = TRUE)
  
  
  if (length(error_logs$unknown_format) > 0) {
    write(
      paste0(
        "Certains types de variables ne correspondent pas aux types de ",
        "variables acceptés.\nVeuillez vous référer aux métadonnées.\n"
      ),
      log_file,
      append = TRUE
    )
    
    for (concerned_file in unique(error_logs$unknown_format_files)) {
      write(paste0(" > Pour le fichier ", concerned_file, ":\n"), log_file, append = TRUE)
      
      for (concerned_variable in error_logs$unknown_format[
        error_logs$unknown_format_files == concerned_file]) {
        write(paste0("\t", concerned_variable, "\n"), log_file, append = TRUE)
      }
    }
    
    write("", log_file, append = TRUE)
    
  } else {
    write("Tous les types de variables sont reconnus.\n", log_file, append = TRUE)
  }
  
  # Types de variables erronés ----
  write(paste0(
    "---------------------------------------------------------------- \n",
    "Types de variable erronés : \n",
    "---------------------------------------------------------------- \n"
  ), log_file, append = TRUE)
  
  if (length(error_logs$wrong_column_format) > 0) {
    write(
      paste0(
        "Certains types de variables ne correspondent pas aux types de ",
        "variables acceptés.\nVeuillez vérifier les colonnes correspondantes.\n"
      ),
      log_file,
      append = TRUE
    )
    
    for (concerned_file in unique(error_logs$wrong_column_format_files)) {
      write(paste0(" > Pour le fichier ", concerned_file, ":\n"), log_file, append = TRUE)
      
      for (concerned_sheet in unique(error_logs$wrong_column_format_sheets[
        error_logs$wrong_column_format_files == concerned_file])) {
        
        write(paste0("\t Pour l'onglet ", concerned_sheet, ":\n"), log_file, append = TRUE)
        
        concerned_column_name <- error_logs$wrong_column_format_name[
          error_logs$wrong_column_format_files == concerned_file &
            error_logs$wrong_column_format_sheets == concerned_sheet]
        concerned_expected <- error_logs$expected_column_format[
          error_logs$wrong_column_format_files == concerned_file &
            error_logs$wrong_column_format_sheets == concerned_sheet]
        concerned_column_class <- error_logs$wrong_column_format[
          error_logs$wrong_column_format_files == concerned_file &
            error_logs$wrong_column_format_sheets == concerned_sheet]
        
        for (i in seq_along(concerned_column_name)) {
          write(
            paste0(
              "\t \t Pour la colonne : ", concerned_column_name[i], "\n",
              "\t \t \t Type de variable attendu : ", concerned_expected[i], "\n",
              "\t \t \t Type de variable observé : ", concerned_column_class[i], "\n"
            ),
            log_file,
            append = TRUE
          )
        }
        
        write("", log_file, append = TRUE)
      }
    }
    
    write("", log_file, append = TRUE)
    
  } else {
    write("Tous les types de variables sont corrects.\n", log_file, append = TRUE)
  }
  
  
  # Erreur de contenu des colonnes ----
  write(paste0(
    "---------------------------------------------------------------- \n",
    "Contenus de colonnes erronés : \n",
    "---------------------------------------------------------------- \n"
  ), log_file, append = TRUE)
  
  if (length(error_logs$wrong_content) > 0) {
    
    write(
      paste0(
        "Certaines colonnes contiennent des éléments qui ne coincident pas avec les informations ",
        "de la base de données de référence. \nCes élements peuvent être de type texte ou numérique.", 
        " Si vous considérez que les éléments indiqués comme étant erronés sont justes, \nveuillez les ", 
        "ajouter dans la base de données de référence, située au chemin : \n", 
        paths$comptage_reference, 
        "\n\nVeuillez sinon corriger les erreurs suivantes dans les fichiers bruts :\n"
      ),
      log_file,
      append = TRUE
    )
    
    for (concerned_file in unique(error_logs$wrong_content_files)) {
      write(paste0(" > Pour le fichier ", concerned_file, ":\n"), log_file, append = TRUE)
      
      for (concerned_sheet in unique(error_logs$wrong_content_sheets[
        error_logs$wrong_content_files == concerned_file])) {
        
        write(paste0("\t Pour l'onglet ", concerned_sheet, ":\n"), log_file, append = TRUE)
        
        for (concerned_column in unique(error_logs$wrong_content_columns[
          error_logs$wrong_content_files == concerned_file & 
          error_logs$wrong_content_sheets == concerned_sheet])) {
          
          write(paste0("\t \t Pour la colonne ", concerned_column, ":\n"), log_file, append = TRUE)
          
          concerned_positions <-  error_logs$wrong_content_files == concerned_file &
            error_logs$wrong_content_sheets == concerned_sheet &
            error_logs$wrong_content_columns == concerned_column
          
          concerned_content <- error_logs$wrong_content[concerned_positions]
          concerned_suggestions <- error_logs$wrong_content_suggestion[concerned_positions]
          concerned_lines <- error_logs$wrong_content_line[concerned_positions]
          
          for (unique_content in unique(concerned_content)) {
            concerned_sub_positions <- concerned_content == unique_content
            
            if (length(concerned_lines[concerned_sub_positions]) > 1) {
              write(
                paste0(
                  "\t \t Aux lignes ", paste(
                    concerned_lines[concerned_sub_positions], collapse = "-"), " :"
                ),
                log_file,
                append = TRUE
              )
              
            } else {
              write(
                paste0(
                  "\t \t A la ligne ", paste(
                    concerned_lines[concerned_sub_positions], collapse = "-"), " :"
                ),
                log_file,
                append = TRUE
              )
            }
            
            write(
              paste0(
                "\t \t \t Contenu observé : ", unique_content, "\n",
                "\t \t \t Contenu suggéré : ", unique(concerned_suggestions[concerned_sub_positions])
                , "\n"
                
              ),
              log_file,
              append = TRUE
            )
          }
          
        }
      }
    }
    
    write("", log_file, append = TRUE)
    
  } else {
    write("Tous les contenus sont corrects.\n", log_file, append = TRUE)
  }
  
  # Erreur de noms d'observateurs ----
  write(paste0(
    "---------------------------------------------------------------- \n",
    "Noms d'observateurs erronés : \n",
    "---------------------------------------------------------------- \n"
  ), log_file, append = TRUE)
  
  if (length(error_logs$wrong_observer) > 0) {
    
    write(
      paste0(
        "Les noms de certains agents sont erronés ou ne sont pas présents dans la base de données ",
        "de référence. Veuillez soit corriger les erreurs, compléter les noms manquants ",
        "ou rajouter les agents dans la liste de référence, située au chemin : \n",
        paths$agents_reference,
        "\n Les noms erronés et la suggestion la plus proche sont indiqués de la manière :\n",
        "nom erroné --> suggestion la plus proche\n",
        "Si le nom n'est pas présent dans la référence, ne pas prendre la suggestion en compte.\n"
      ),
      log_file,
      append = TRUE
    )
    
    for (concerned_file in unique(error_logs$wrong_observer_files)) {
      write(paste0(" > Pour le fichier ", concerned_file, ":\n"), log_file, append = TRUE)
    
      if (counting_type == "comptage_terrain_meteo") {
        
        for (concerned_line in unique(error_logs$wrong_observer_line[
          error_logs$wrong_observer_files == concerned_file])) {
          
          write(paste0("\t Pour la ligne ", concerned_line, ":\n"), log_file, append = TRUE)
          
          
          concerned_observers <- error_logs$wrong_observer[
            error_logs$wrong_observer_files == concerned_file &
              error_logs$wrong_observer_line == concerned_line 
          ]
          
          concerned_suggestions <- error_logs$wrong_observer_suggestion[
            error_logs$wrong_observer_files == concerned_file &
              error_logs$wrong_observer_line == concerned_line 
          ]
          
          for (i in seq_along(concerned_observers)) {
            if (!is.na(concerned_observers[i])) {
              
              write(
                paste0("\t\t\t", concerned_observers[i], " --> ", concerned_suggestions[i], "\n"),
                log_file,
                append = TRUE
              )
              
            }
          }
          
        }
        
      } else {
        
        for (concerned_sector in unique(error_logs$wrong_observer_sector[
          error_logs$wrong_observer_files == concerned_file])) {
          
          write(paste0("\t Pour le secteur ", concerned_sector, ":\n"), log_file, append = TRUE)
          
          
          concerned_observers <- error_logs$wrong_observer[
            error_logs$wrong_observer_files == concerned_file &
              error_logs$wrong_observer_sector == concerned_sector 
          ]
          
          concerned_suggestions <- error_logs$wrong_observer_suggestion[
            error_logs$wrong_observer_files == concerned_file &
              error_logs$wrong_observer_sector == concerned_sector 
          ]
          
          for (i in seq_along(concerned_observers)) {
            if (!is.na(concerned_observers[i])) {
              
              write(
                paste0("\t\t\t", concerned_observers[i], " --> ", concerned_suggestions[i], "\n"),
                log_file,
                append = TRUE
              )
              
            }
          }
          
        }
        
      }
    }
    
    write("", log_file, append = TRUE)
    
  } else {
    write("Tous les noms d'agents sont corrects.\n", log_file, append = TRUE)
  }
  
  # Fin du journal
  
  
  write(paste0(
    "---------------------------------------------------------------------------- \n",
    "Fin de la vérification des données de ",
    str_replace_all(counting_type, "_", " "),
    "\n",
    "----------------------------------------------------------------------------"
  ), log_file, append = TRUE)
  
  if (number_of_errors != 0) {
    message(paste("Fichier log enregistré au chemin :", log_file))
  }
  
  # Renvoi du nombre d'erreurs
  return(number_of_errors)
}
