# Script test header fusion

rm(list = ls())
library("readxl")
library("openxlsx")
library("dplyr")

source("R/verification_functions.R")

file_path <- "data/raw/comptages_terrain/modeles/us_med_pnmcca_observatoire_comptage_terrain_plaisance_YYYY-MM-DD.xlsx"
excel_sheets(file_path)


data_long <- read.xlsx(
  xlsxFile = file_path,
  sheet = "Saleccia (long)",
  sep.names = " ",
  fillMergedCells = TRUE
)


data_wide <- read.xlsx(
  xlsxFile = file_path,
  sheet = "Saleccia (wide)",
  sep.names = " ",
  colNames = FALSE,
  fillMergedCells = TRUE
)


plaisance_wide <- double_header_import(file_path, "Saleccia (wide)")

plaisance_long <- plaisance_wide %>%
  # select cols containing __
  tidyr::pivot_longer(cols = contains("__"),
                      names_to = "category",
                      values_to = "nombre") %>%
  # separate category into two columns
  tidyr::separate_wider_delim(col = category,
                              delim = "__",
                              names = c("statut", "taille"))



file_path2 <- "data/raw/comptages_terrain/activites/us_med_pnmcca_observatoire_comptage_terrain_activites_loisirs_2021-07-28.xlsx"
activite_wide <- double_header_import(file_path2, "Saleccia")


activite_long <- activite_wide %>%
  # select cols containing __
  tidyr::pivot_longer(cols = contains("__"),
                      names_to = "category",
                      values_to = "nombre") %>%
  # separate category into two columns
  tidyr::separate_wider_delim(col = category,
                              delim = "__",
                              names = c("type", "usage"))

