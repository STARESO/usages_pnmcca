test <- left_join(
  compilation_plaisance,
  compilation_plaisance_corrected,
  by = c(
    "date",
    "annee",
    "mois",
    "jour",
    "secteur",
    "horaire",
    "type",
    "observateurs",
    "statut",
    "taille"
  )
) %>%
  relocate(nombre.y, .after = nombre.x) %>%
  filter(nombre.x != nombre.y)



compilation_plaisance_horizontal <- compilation_plaisance_corrected %>%
  mutate(desc_col = paste(statut, type, taille, sep = "_")) %>%
  pivot_wider(., 
              id_cols = c("date", "annee", "mois", "jour", "secteur", "horaire", "observateurs"),
              names_from = desc_col, 
              values_from = nombre) %>%
  relocate(annee, mois, jour, observateurs, .after = last_col())
  
