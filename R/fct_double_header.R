#' ---
#' title : "Fonctions de double en-tête"
#' author : Aubin Woehrel
#' date : 2024-09-17
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' OBSERVATOIRE DES USAGES - FONCTIONS DE DOUBLE EN-TÊTE
#' 
#' Description : 
#' Script contenant toutes les fonctions permettant de gérer le cas de doubles
#' en-têtes dans les jeux de données de base. Exemple : cas des comptages 
#' plaisance où le premier en-tête correspond à la taille du bateau et le 
#' deuxième en-tête au statut du bateau (ancré, en mouvement, etc.). Ces données 
#' sont transformées pour n'obtenir qu'un seul en-tête, standard utilisable 
#' pour les analyses statistiques et représentations visuelles sous R.
#' 
#' =============================================================================


#' =============================================================================
#' 
#' Fusion double en-tête en un seul en-tête
#'
#' Cette fonction combine les deux premières lignes d'un jeu de données avec 
#' un double en-tête en concaténant les deux lignes en un seul en-tête propre. 
#' Elle supprime les accents, remplace les espaces par des _ et gère 
#' les cas où des symboles spéciaux (comme '<', '>', et 'L') sont présents dans
#' le cas des comptages de plaisance.
#'
#' @param dataset Jeu de données à double en-tête
#'
#' @return Vecteur de caractères représentant le nouvel en-tête combiné.
#' @export
#'
#' @examples
#' double_header_fusion(my_dataset)
#' 
double_header_fusion <- function(dataset) {
  
  if (nrow(dataset) < 2)
    stop("Le jeu de données doit contenir au moins deux lignes pour la fusion d'en-tête.")
  
  header_selection <- dataset[1:2, ]
  new_header <- c()
  
  # Transformations : suppression accents, remplacement espaces 
  # par _ et gestion symboles "<", ">", "L"
  header_selection <- header_selection %>%
    mutate(across(
      everything(),
      ~ stringi::stri_trans_general(., "Latin-ASCII")
    )) %>%
    mutate(across(everything(), ~ ifelse(
      grepl("L", .) & (grepl("<", .) | grepl(">", .)),
      gsub("m| ", "", .),
      gsub(" ", "_", tolower(.))
    ))) 
  
  # Concatène parties de l'en-tête si elles sont différentes ; sinon, garde tel quel
  new_header <- purrr::map2_chr(
    header_selection[2, ],
    header_selection[1, ],
    function(h2, h1) {
      if (h2 != h1) {
        return(paste(h2, h1, sep = "__"))
      } else {
        return(h1)
      }
    }
  )
  
  new_header <- unname(new_header)
  return(new_header)
}


#' =============================================================================
#' 
#' Import feuille Excel avec un double en-tête et création d'un en-tête propre.
#'
#' Cette fonction importe une feuille Excel avec un double en-tête, concatène 
#' les deux lignes d'en-tête en un seul en-tête propre, et renvoie le jeu de 
#' données avec ce nouvel en-tête. Elle s'assure que le nombre de colonnes du 
#' jeu de données correspond à celui du nouvel en-tête.
#'
#' @param file_path Chemin vers le fichier Excel (.xlsx).
#' @param sheet_name Nom de la feuille à importer.
#'
#' @return Un jeu de données avec un en-tête unique après la fusion du double en-tête.
#' @export
#'
#' @examples
#' double_header_import("mon_fichier.xlsx", "Feuille1")
#' 
double_header_import <- function(file_path, sheet_name) {
  
  # Import jeu de données complet sans noms de colonnes
  data_double <- openxlsx::read.xlsx(
    xlsxFile = file_path,
    sheet = sheet_name,
    sep.names = " ",
    colNames = FALSE,
    fillMergedCells = TRUE
  )
  
  # Nouvel en-tête fusionné
  names_data_double <- double_header_fusion(data_double)
  
  # Import jeu de données à partir de la troisième ligne
  data_double <- openxlsx::read.xlsx(
    xlsxFile = file_path,
    sheet = sheet_name,
    sep.names = " ",
    colNames = FALSE,
    fillMergedCells = TRUE,
    skipEmptyCols = FALSE,
    startRow = 3,
    cols = 1:length(names_data_double)
  )
  
  # Check correspondance nombre de colonnes avec en-tête
  if (ncol(data_double) < length(names_data_double)) {
    data_double <- cbind(
      data_double, 
      matrix(NA, nrow = nrow(data_double), 
             ncol = length(names_data_double) - ncol(data_double))
    )
  }
  
  # Renomme avec nouvel en-tête
  colnames(data_double) <- names_data_double
  
  return(data_double)
}


#' =============================================================================
#' 
#' Division d'un en-tête fusionné en deux parties.
#'
#' Cette fonction divise un en-tête fusionné (par ex., "ancre__L<8m") en ses 
#' deux composants et renvoie ces parties sous forme de champ1 et champ2. Si 
#' l'en-tête n'est pas fusionné, champ1 et champ2 sont identiques.
#'
#' @param fused_header Chaîne de caractères représentant un en-tête fusionné.
#'
#' @return Une liste avec deux éléments : champ1 (la première partie de l'en-tête) 
#' et champ2 (la deuxième partie de l'en-tête).
#' @export
#'
#' @examples
#' split_fused_headers("ancre__L<8m")
#' 
split_fused_headers <- function(fused_header) {
  
  parts <- strsplit(fused_header, "__")[[1]] # Sépare les parties de l'en-tête
  
  # Si une seule partie, définir champ1 et champ2 comme identiques.
  if (length(parts) == 1) {
    return(list(champ1 = parts[1], champ2 = parts[1]))
  } else {
    return(list(champ1 = parts[2], champ2 = parts[1])) 
  }
}
