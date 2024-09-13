#                                                                    
# --- Observatoire Des Usages - Compilation Fiches terrain Bateaux --- 
#                                                                   

# Initialization ----

## Clean up ----
rm(list = ls())

## Library imports ----

# Data tidying
library("dplyr")
library("tidyr")

# Plotting
library("ggplot2")
library("ggpubr")
library("paletteer")

## Paths & custom verification functions imports ----
source("R/paths.R")
source("R/verification_functions.R")



# Read comptages_bateaux
#comptage_bateaux <- readRDS("data/processed/rds/comptages_bateaux.rds")
comptage_plage <- readRDS(paste0(paths$comptage_processed, "comptage_plage_processed.rds"))


# Plot the evolution of the number of boats over time for each sector ----

## Prepare data ----

# Compute the number of anchored boats per Secteur and per day with seperation by Type

total_ancre <- comptage_bateaux %>%
  # Computer the number of anchored boats per Secteur and per day (sum of columns 5 to 9)*
  mutate(Nb_bateaux = rowSums(select(., c(5:9)))) %>%
  group_by(Secteur, Type, Date) %>%
  
  summarise(Nb_bateaux = sum(Nb_bateaux, na.rm = TRUE)) %>%
  ungroup() %>%
  # Extract year from date in column Year
  mutate(Year = as.numeric(format(Date, "%Y")), Month = paste0(format(Date, "%m"))) %>%
  mutate(Year_month = paste0(Year, "-", Month)) %>%
  mutate(Type = factor(Type), Date = as.character(Date)) %>%
  filter(Secteur %in% c("Saleccia", "Lotu", "Petit Lotu", "Fiume Santu"))



ggplot(data = total_ancre, aes(x = Month, y = Nb_bateaux, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Secteur ~ Year) +
  theme_pubr() +
  labs(title = "Evolution du nombre de bateaux par secteur", x = "Date", y = "Nombre de bateaux") +
  # Use colorblind palette
  scale_fill_colorblind() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  # rotate angle of labels of dates to 90°
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = total_ancre, aes(x = Month, y = Nb_bateaux)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Secteur ~ Year) +
  theme_pubr() +
  labs(title = "Evolution du nombre de bateaux ancrés par secteur", x = "Date", y = "Nombre de bateaux") +
  # Use colorblind palette
  scale_fill_colorblind() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  # rotate angle of labels of dates to 90°
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Comptage plage ----

comptage_plage <- comptage_plage %>%
  select(-c(Observations)) %>%
  pivot_longer(cols = Personnes_sur_la_plage:Vaches, names_to = "Type", values_to = "nb")


comptage_plage %>%
  ggplot(., aes(x = Annee, y = nb, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Type ~ ., scales = "free") +
  paletteer::scale_fill_paletteer_d("RColorBrewer::Dark2") +
  theme_pubr() +
  coord_flip()


comptage_plage %>%
  ggplot(., aes(x = Annee, y = nb, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Type ~ Secteur, scales = "free_y") +
  paletteer::scale_fill_paletteer_d("RColorBrewer::Dark2") +
  theme_pubr() +
  # Change orientation of the x-axis labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



comptage_plage %>%
  filter(Type == "Personnes_dans_l'eau",
         Secteur %in% c("Lotu", "Saleccia")) %>%
  mutate(Annee = as.factor(Annee)) %>%
  ggplot(., aes(x = Annee, y = nb, fill = Annee)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Secteur ~ Mois, scales = "free") +
  paletteer::scale_fill_paletteer_d("RColorBrewer::Dark2") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 





