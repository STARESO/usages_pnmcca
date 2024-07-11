###                                                                      ###
### --- Observatoire Des Usages - Compilation Fiches terrain Bateaux --- ###
###                                                                      ###

# Initialization ----

## Clean up ----
rm(list = ls())


# Read comptages_bateaux
comptage_bateaux <- readRDS("data/processed/rds/comptages_bateaux.rds")


# Plot the evolution of the number of boats over time for each sector ----

## Load libraries ----
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(ggthemes)

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
