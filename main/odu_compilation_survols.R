#' ---
#' title : "Compilation survols"
#' author : Aubin Woehrel
#' date : 2024-10-25
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - COMPILATION DES DONNEES DE SURVOLS 
#' 
#' Description : 
#' Ce script gère la compilation et la vérification des données de survols de 
#' type "plaba" (plage et baignade) et usages (tout ce qui est sur le plan 
#' d'eau) afin d'obtenir un format cohérent pour les analyses et représentations
#' graphiques. Le script utilise une variété de fonctions personnalisées pour 
#' assurer le bon fonctionnement du traitement. Ces fonctions sont localisées 
#' dans le dossier "R/" du projet. Le procédé global de fonctionnement est 
#' décrit dans le README du projet, à lire notamment pour les emplacements de 
#' fichiers d'entrée et de sortie.
#' 
#' =============================================================================

# Initialisation ----

## Nettoyage de l'environnement ----
rm(list = ls())

## Importation des bibliothèques ----

# Importation des données
library("openxlsx")
library("readxl")

# Nettoyage des données
library("dplyr")
library("tidyr")
library("stringr")
library("stringi")

# Transformation des données
library("chron")
library("lubridate")

## Importation des références chemins ----
source("R/paths.R")

## Importation des fonctions personnalisées ----
source("R/fct_check_column_content.R")
source("R/fct_column_format.R")
source("R/fct_compilation_survols.R")
source("R/fct_conversions.R")
source("R/fct_double_header.R")
source("R/fct_file_checking.R")
source("R/fct_mistakes_log.R")
source("R/fct_post_compilation.R")
source("R/fct_read_metadata.R")
source("R/fct_sectors.R")
source("R/fct_sheet_header.R")

## Référence des noms de secteurs ----
ref_secteurs <- read.csv(file = paths$reference_secteurs, sep = ";") %>%
  dplyr::mutate(Secteur_simple = stringi::stri_trans_general(Secteur, "Latin-ASCII")) %>%
  dplyr::select(
    "id", 
    "Secteur",
    "Secteur_simple", 
    "Code_sec", 
    "Communes", 
    "Code_INSEE", 
    "Com_Corse"
  )

# Compilation ----

## Compilation principale plaba ---- 
compilation_plaba <- compilation_survols("plaba")
compilation_plaba <- post_compilation(compilation_plaba, counting_type = "plaba")

skimr::skim(compilation_plaba)

# Affichage manuel des occurences de chaque colonne pour vérifier si des erreurs
# non détectées par compilation_survols peuvent encore être corrigées.
table(sort(compilation_plaba$date))
table(sort(compilation_plaba$secteur))
table(sort(compilation_plaba$nom_acti))
table(sort(compilation_plaba$d_heur_cam))
table(sort(compilation_plaba$f_heur_cam))
table(sort(compilation_plaba$n_campagne))
table(sort(compilation_plaba$zone))
table(sort(compilation_plaba$meteo))
table(sort(compilation_plaba$dir_vent))
table(sort(compilation_plaba$etat_mer))
table(sort(compilation_plaba$cod_act))
table(sort(compilation_plaba$act))
table(sort(compilation_plaba$method_num))
table(sort(compilation_plaba$supp_num))
table(sort(compilation_plaba$date_num))
table(sort(compilation_plaba$auteur_num))
table(sort(compilation_plaba$org_num))
table(sort(compilation_plaba$ech_num))
table(sort(compilation_plaba$com_num))
table(sort(compilation_plaba$ic))
table(sort(compilation_plaba$commune))
table(sort(compilation_plaba$code_insee))
table(sort(compilation_plaba$code_sec))
table(sort(compilation_plaba$layer))
table(sort(compilation_plaba$path))

comparaison_denominations_plaba <- compilation_plaba %>%
  select(nom_acti, act, categorie_usage) %>%
  group_by(nom_acti, act, categorie_usage) %>%
  summarize(count = n())


## Compilation principale plandeau ----
compilation_plandeau <- compilation_survols("plandeau")
compilation_plandeau <- post_compilation(compilation_plandeau, counting_type = "plandeau")

skimr::skim(compilation_plandeau)

table(sort(compilation_plandeau$date))
table(sort(compilation_plandeau$secteur))
table(sort(compilation_plandeau$nom_acti))
table(sort(compilation_plandeau$d_heur_cam))
table(sort(compilation_plandeau$f_heur_cam))
table(sort(compilation_plandeau$n_campagne))
table(sort(compilation_plandeau$zone))
table(sort(compilation_plandeau$meteo))
table(sort(compilation_plandeau$dir_vent))
table(sort(compilation_plandeau$etat_mer))
table(sort(compilation_plandeau$cod_act))
table(sort(compilation_plandeau$act))
table(sort(compilation_plandeau$taille_nav))
table(sort(compilation_plandeau$etat_nav))
table(sort(compilation_plandeau$method_num))
table(sort(compilation_plandeau$supp_num))
table(sort(compilation_plandeau$date_num))
table(sort(compilation_plandeau$auteur_num))
table(sort(compilation_plandeau$org_num))
table(sort(compilation_plandeau$ech_num))
table(sort(compilation_plandeau$com_num))
table(sort(compilation_plandeau$ic))
table(sort(compilation_plandeau$commune))
table(sort(compilation_plandeau$code_insee))
table(sort(compilation_plandeau$code_sec))
table(sort(compilation_plandeau$layer))
table(sort(compilation_plandeau$path))
table(sort(compilation_plandeau$categorie_usage))

comparaison_denominations_plandeau <- compilation_plandeau %>%
  select(nom_acti, act, categorie_usage) %>%
  group_by(nom_acti, act, categorie_usage) %>%
  summarize(count = n())

# Enregistrement des données traitées ----

## Fichiers rds ----
saveRDS(compilation_plaba, paste0(paths$processed_plaba, ".rds"))
saveRDS(compilation_plandeau, paste0(paths$processed_plandeau, ".rds"))

## Fichiers tsv ----
write.table(compilation_plaba, paste0(paths$processed_plaba, ".tsv"), sep = "\t", row.names = FALSE)
write.table(compilation_plandeau, paste0(paths$processed_plandeau, ".tsv"), sep = "\t", row.names = FALSE)

## Fichiers csv ----
write.csv2(compilation_plaba, paste0(paths$processed_plaba, ".csv"), row.names = FALSE)
write.csv2(compilation_plandeau, paste0(paths$processed_plandeau, ".csv"), row.names = FALSE)

