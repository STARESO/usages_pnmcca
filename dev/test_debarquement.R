#' ---
#' title : "Test debarquements"
#' author : Aubin Woehrel
#' date : 2024-10-15
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - TEST DEBARQUEMENTS
#' 
#' Description : 
#' 
#' =============================================================================


# Initialisation ----

## Nettoyage de l'environnement ----
rm(list = ls())

## Importation des bibliothèques ----
library("dplyr")
library("tidyr")
library("echarts4r")

## Import des chemins ----
source("R/paths.R")

compilation_debarquements <- readRDS(paste0(paths$processed_debarquements, ".rds"))

names(compilation_debarquements)
unique(compilation_debarquements$horaire)

bilan_societes <- compilation_debarquements %>% 
  group_by(societes) %>%
  summarize(count = n(),
            nombre_arrivees = sum(nombre_arrivees),
            nombre_de_personnes = sum(nombre_de_personnes))

bilan_societes

bilan_societes %>%
  e_charts(societes) %>%
  e_pie(nombre_arrivees) %>%
  e_legend(show = FALSE) %>%
  e_tooltip(trigger = "item", formatter = e_tooltip_pie_formatter("decimal"))
