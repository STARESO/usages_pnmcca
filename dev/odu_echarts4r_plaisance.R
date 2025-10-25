#' ---
#' title : "Plaisance echarts4r"
#' author : Aubin Woehrel
#' date : 2024-09-17
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - PLAISANCE ECHARTS4R
#' 
#' Description : 
#' 
#' =============================================================================


# Initialisation ----

## Nettoyage de l'environnement ----
rm(list = ls())

## Importation des bibliothèques ----
library("echarts4r")
library("dplyr")
library("tidyr")

## Import des chemins ----
source("R/paths.R")

## Importation des données ----
plaisance <- readRDS(paste0(paths$processed_plaisance, ".rds"))

# Petites modifs pour compléter mois manquants
months_to_keep <- c("juin", "juillet", "août", "septembre")

plaisance <- plaisance %>%
  complete(annee, mois = months_to_keep, type, statut, taille,
           fill = list(nombre = NA, total_type_statut = NA)) %>%
  mutate(mois = factor(mois, levels = months_to_keep)) %>%
  arrange(annee, mois, secteur, horaire, type, statut, taille)  %>%
  select(date, annee, mois, jour, secteur, horaire, everything())

plaisance_sans_taille <- plaisance %>%
  select(-c(taille, nombre)) %>%
  distinct() 

# Total bateaux timeline an
plaisance_sans_taille %>%
  select(annee, mois, total_type_statut) %>%
  group_by(annee, mois) %>%
  summarise(total = sum(total_type_statut, na.rm = TRUE)) %>%
  group_by(annee) %>%
  e_charts(mois, timeline = TRUE) %>%
  e_bar(total) %>%
  e_tooltip(trigger = "item") 

# Total bateaux timeline mois
plaisance_sans_taille %>%
  select(annee, mois, total_type_statut) %>%
  group_by(annee, mois) %>%
  summarise(total = sum(total_type_statut, na.rm = TRUE)) %>%
  mutate(annee = as.character(annee)) %>%
  group_by(mois) %>%
  e_charts(annee, timeline = TRUE) %>%
  e_bar(total) %>%
  e_tooltip(trigger = "item") 

# Total bateaux timeline mois-secteur
t1 <- plaisance_sans_taille %>%
  select(annee, mois, total_type_statut, secteur) %>%
  group_by(annee, mois, secteur) %>%
  filter(secteur %in% c("Lotu", "Saleccia")) %>%
  summarise(total = sum(total_type_statut, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = secteur, values_from = total) %>%
  mutate(annee = as.character(annee)) %>%
  arrange(mois, annee) %>%
  group_by(annee) %>%
  e_charts(mois, timeline = TRUE) %>%
  e_bar(Lotu) %>%
  e_bar(Saleccia) %>%
  e_tooltip(trigger = "item") %>%
  e_title("Nombre total de bateaux de plaisance", "Lotu & Saleccia")

t2 <- plaisance_sans_taille %>%
  filter(secteur == "Lotu") %>%
  group_by(annee, mois, type) %>%
  summarise(total = sum(total_type_statut, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = type , values_from = total) %>%
  mutate(annee = as.character(annee)) %>%
  arrange(mois, annee) %>%
  group_by(annee) %>%
  e_charts(mois, timeline = TRUE) %>%
  e_bar(Moteur) %>%
  e_bar(Voilier) %>%
  e_mark_point() %>%
  e_tooltip(trigger = "item") %>%
  e_title("Plaisance au Lotu", "Bateaux à Moteurs et Voiliers")

t3 <- plaisance_sans_taille %>%
  filter(secteur == "Saleccia") %>%
  group_by(annee, mois, type) %>%
  summarise(total = sum(total_type_statut, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = type , values_from = total) %>%
  mutate(annee = as.character(annee)) %>%
  arrange(mois, annee) %>%
  group_by(annee) %>%
  e_charts(mois, timeline = TRUE) %>%
  e_bar(Moteur) %>%
  e_bar(Voilier) %>%
  e_mark_point() %>%
  e_tooltip(trigger = "item") %>%
  e_title("Plaisance à Saleccia", "Bateaux à Moteurs et Voiliers")



# Disinction voilier/moteur timeline an
plaisance_sans_taille %>%
  rename(total = total_type_statut) %>%
  group_by(annee) %>%
  tidyr::pivot_wider(names_from = type, values_from = total) %>%
  e_charts(mois, timeline = TRUE) %>%
  e_bar(Voilier) %>%
  e_bar(Moteur)

# Disinction voilier/moteur timeline mois
plaisance_sans_taille %>%
  rename(total = total_type_statut) %>%
  group_by(mois) %>%
  tidyr::pivot_wider(names_from = type, values_from = total) %>%
  mutate(annee = as.character(annee)) %>%
  e_charts(annee, timeline = TRUE) %>%
  e_bar(Voilier) %>%
  e_bar(Moteur)

# Create a numeric month order
t <- plaisance %>%
  group_by(annee, mois, taille) %>%
  summarise(nombre = sum(nombre, na.rm = TRUE), .groups = "drop") %>%
  complete(mois = months_to_keep, annee, taille, fill = list(nombre = 0)) %>% # Fill missing months
  pivot_wider(names_from = taille, values_from = nombre) %>% 
  arrange(annee, mois) %>%
  group_by(annee) 

# Generate the echarts visualization with timeline
t %>%
  e_charts(mois, timeline = TRUE) %>%
  e_bar(`L<8`) %>%
  e_bar(`8<L<18`) %>%
  e_bar(`18<L<24`) %>%
  e_bar(`24<L<45`) %>%
  e_bar(`L>45`)
# Par horaire


  