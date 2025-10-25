#' ---
#' title : "Survols cartographie"
#' author : Aubin Woehrel
#' date : 2024-09-17
#' ---
#'
#' =============================================================================
#'
#' OBSERVATOIRE DES USAGES - SURVOLS CARTOGRAPHIE
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

## Import des chemins ----
source("R/paths.R")

## Import des jeux de données ----
plaba <- readRDS(paste0(paths$processed_plaba, ".rds"))
plandeau <- readRDS(paste0(paths$processed_plandeau, ".rds"))


# Fonctions utilitaires ----
load_bordure_pnm <- function() {

  shp_file <- here::here("data/raw/cartographie/pnm/N_ENP_PNM_S_000.shp")

  borders_pnm <- sf::st_read(shp_file) %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::filter(NOM_SITE == "cap Corse et Agriate")

}

secteur_processing <- function() {

  ref_secteurs <- utils::read.csv(
    here::here("data/raw/cartographie/Sec_nav_maj_2023_Corrigée/sec_nav_maj_2023.csv"),
    sep = ";"
  )

  ref_secteurs <- ref_secteurs %>%
    dplyr::mutate(Secteur_simple = stringi::stri_trans_general(Secteur, "Latin-ASCII")) %>%
    dplyr::select("id", "Secteur", "Secteur_simple", "Code_sec", "Communes", "Code_INSEE", "Com_Corse")

  return(ref_secteurs)
}

load_secteurs <- function() {
  shp_file <- here::here("data/raw/cartographie/Sec_nav_maj_2023_Corrigée/Sec_nav_maj_2023.shp")
  sf_sectors <- sf::st_read(shp_file) %>%
    dplyr::mutate(Secteur = stringi::stri_trans_general(Sec_csv, "Latin-ASCII"))

  # Transform CRS to WGS84 (EPSG:4326)
  sf_sectors <- sf::st_transform(sf_sectors, crs = 4326)
}


#' sector_map UI Function
#'
#' @description Module to show sector map.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Card
#' @importFrom leaflet leafletOutput
mod_sector_map_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shinyMobile::f7Card(
      title = "Secteurs",
      div(
        style = "height: calc(100vh - 280px);",  # Adjust height dynamically
        leaflet::leafletOutput(ns("shapefile_map"), height = "100%")  # Set height to 100% of the parent div
      )
    )
  )
}


#' sector_map Server Functions
#'
#' @noRd
library(sf)
mod_sector_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load the shapefile (assuming it is an sf object)
    shapefile_data <- load_secteurs()  # This should return an sf object
    pnm_border <- load_bordure_pnm()

    # Validate and fix the geometry if necessary
    shapefile_data <- sf::st_make_valid(shapefile_data)

    # Calculate centroids from polygons in shapefile_data
    shapefile_data_centroids <- sf::st_centroid(shapefile_data)

    # Check if centroids have valid coordinates
    if (!is.null(shapefile_data_centroids)) {
      coords <- sf::st_coordinates(shapefile_data_centroids)
    } else {
      stop("Error: Centroid coordinates could not be calculated.")
    }

    # Render the map
    output$shapefile_map <- leaflet::renderLeaflet({
      leaflet::leaflet(data = shapefile_data) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery) %>%
        leaflet::addPolygons(data = pnm_border, color = "lightblue", weight = 2) %>%
        leaflet::addPolygons(
          color = "black",
          fillColor = "darkgray",
          fillOpacity = 0.9,
          weight = 1,
          label = ~Secteur,
          labelOptions = leaflet::labelOptions(
            interactive = TRUE,
            noHide = FALSE,
            direction = 'auto'
          ),
          popup = ~paste(
            "<b>Secteur:</b>", Secteur, "<br>",
            "<b>Code:</b>", Code_sec, "<br>",
            "<b>Commune:</b>", Communes, "<br>",
            "<b>Code INSEE:</b>", Code_INSEE, "<br>",
            "<b>Com Corse:</b>", Com_Corse
          )
        )
    })

    # Observe map zoom and dynamically add labels
    observe({
      zoom_level <- input$shapefile_map_zoom  # Capture current zoom level of the map

      map <- leaflet::leafletProxy(ns("shapefile_map"))

      # Add labels only if zoom level is >= x
      if (!is.null(zoom_level) && zoom_level >= 13 && !is.null(coords)) {
        # Clear any existing labels
        map %>% leaflet::clearGroup("labels") %>%
          leaflet::addLabelOnlyMarkers(
            lng = coords[, 1],  # longitude
            lat = coords[, 2],  # latitude
            label = shapefile_data$Secteur,  # Label by sector name
            labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "auto"),
            group = "labels"
          )
      } else {
        # Clear labels when zoom level is too low or data is invalid
        map %>% leaflet::clearGroup("labels")
      }
    })
  })
}