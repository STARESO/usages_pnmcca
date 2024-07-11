###                                                       ###
### --- Observatoire des Usages - Extractions BDD Mer --- ###
###                                                       ###


# Initialization ----

## Clean up ----
rm(list = ls())

## Working directory ----
setwd("~/Travail/Stareso/Observatoire Des Usages/BDD")

## Library imports ----

# Data imports
library(readxl)

# Data tidying
library(dplyr)
library(tidyr)

# Plotting
library(ggplot2)
library(ggpubr)

## Data import ----

# BDD Mer
BDD_mer <- read_excel(
  "Sources/Activités de loisirs PNMCCA/Activité de plaisance/BD MER/BD_mer_20231026.xlsx",
  sheet = "BD_mer",
)

# Modify the class of heure to date and keep all the rest for re-import
column_classes <- lapply(BDD_mer, class)
column_classes$heure <- "date"
column_classes$date <- "date"
column_classes$date <- "date"
column_classes <- column_classes %>%
  unlist()
# Replace "character" by "text"
column_classes <- column_classes %>%
  gsub("character", "text", .)

BDD_mer <- read_excel(
  "Sources/Activités de loisirs PNMCCA/Activité de plaisance/BD MER/BD_mer_20231026.xlsx",
  sheet = "BD_mer",
  col_types = column_classes
)

# Codes communes
v_commune_2023 <- read.csv(
  "~/Travail/Stareso/Observatoire Des Usages/BDD/Sources/Codes INSEE/v_commune_2023.csv"
) %>%
  filter(DEP == "2B")



# First modifications : Reformating BDD_mer identifying variables and pooling AIS values ----
names(BDD_mer)
unique(BDD_mer$protocole)
unique(BDD_mer$code_prot)

NB_par_methode <- BDD_mer %>%
  group_by(protocole, date) %>%
  summarise(NB = n())

# Format heure as HH:mm
BDD_mer$heure <- format(BDD_mer$heure, "%H:%M")

# Protocole as lower to prevent capital letters mistakes
BDD_mer$protocole <- tolower(BDD_mer$protocole)

# Reformating BDD_mer in longer format

BDD_mer_long <- BDD_mer %>%
  # reorder last columns just after column heure
  select(1:14, 95:100, 15:94) %>%
  mutate(across(21:dim(.)[2], as.numeric)) %>%
  pivot_longer(cols = 21:dim(.)[2],
               names_to = "categorie",
               values_to = "valeur") %>%
  # New column ais with TRUE if ais in categorie
  mutate(ais = grepl("ais", categorie)) %>%
  mutate(type = case_when(
    grepl("M", categorie) ~ "M",
    grepl("V", categorie) ~ "V",
    TRUE ~ categorie
  )) %>%
  # New column with extracted number if a number is present in categorie, also considering .
  # for decimal separator
  mutate(taille = as.numeric(gsub("[^0-9.]", "", categorie))) %>%
  mutate(
    statut = case_when(
      grepl("bouee_T", categorie) ~ NA,
      grepl("bouee", categorie) ~ "bouee",
      grepl("dpl", categorie) ~ "deplacement_lent",
      grepl("sta", categorie) ~ "stationnaire",
      grepl("transit", categorie) ~ "transit",
      grepl("total", categorie) ~ "total",
      grepl("mouil", categorie) ~ "total_mouillage",
      !is.na(taille) ~ "ancre",
      TRUE ~ NA_character_
    )
  )

# Compute sum of values for ais and no ais for ancre status
BDD_mer_long_ancre <- BDD_mer_long %>%
  filter(statut == "ancre") %>%
  # remove the ais_ if present in categorie
  mutate(categorie = gsub("ais_", "", categorie)) %>%
  group_by(secteur, date, heure, protocole, type, taille) %>%
  # Summarise valeur but keep all other categories except "categorie"
  mutate(valeur = sum(valeur, na.rm = TRUE)) %>%
  select(-ais) %>%
  distinct()

# Remodel BDD_mer_long by changing new ancre status with summed ais/no ais values
BDD_mer_long <- BDD_mer_long %>%
  filter(statut != "ancre") %>%
  select(-ais) %>%
  bind_rows(., BDD_mer_long_ancre)

rm(BDD_mer_long_ancre)

# Checking the number of values for each protocole
protocole_par_zone <- BDD_mer_long %>%
  select(protocole, secteur, date, heure) %>%
  distinct() %>%
  group_by(protocole, secteur) %>%
  summarize(nb_comptage = n()) %>%
  # If name is present in protocole, value of protocole name is yes when pivot wider
  pivot_wider(
    names_from = protocole,
    names_expand = TRUE,
    values_from = nb_comptage,
    values_fill = list(nb_comptage = 0)
  )

# Correcting metadata values ----

## Secteur check ----
secteur_check <- BDD_mer_long %>% group_by(id, secteur, code_sec) %>%
  summarise(n = n(), date = list(unique(as.character(date))))

## Commune check ----
commune_check <- BDD_mer_long %>% group_by(code_insee, commune) %>% summarise(n = n())

commune_check <- v_commune_2023 %>%
  rename(code_insee = COM) %>%
  filter(code_insee %in% commune_check$code_insee) %>%
  left_join(commune_check, by = "code_insee")
 


BDD_mer_long %>% group_by(zone_met) %>% summarise(n = n())
BDD_mer_long %>% group_by(meteo) %>% summarise(n = n())
temp_amount <- BDD_mer_long %>% group_by(temp) %>% summarise(n = n())
BDD_mer_long %>% group_by(force_vent) %>% summarise(n = n())
BDD_mer_long %>% group_by(etat_mer) %>% summarise(n = n())
BDD_mer_long %>% group_by(dir_vent) %>% summarise(n = n())


# New dataset for visual census of water surfaces ----
# This dataset extracted by BDD_mer corresponds to the interpretable values of the corresponding
# protocoles :
# - "Comptage visuel simple" (CVS) - Occasional observations without a protocol
# - "Comptage visuel technique" (CVT) - Observations with spatial data (absent in this dataset)
# - "Comptage PNM/CDL/CDC" (PAR) - Visual census with defined protocol


BDD_comptage_eau <- BDD_mer_long %>%
  filter(code_prot %in% c("CVS", "CVT", "PAR")) %>%
  filter(!categorie %in% c("usag_plag", "baigneur", "pmt", "peche_bord", "chasse_sm"))


## Computing new status of movement ----



# Extracting data of airplane counting ----
