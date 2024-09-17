#' Title
#'
#' @param counting_type 
#'
#' @return
#' @export
#'
#' @examples
read_metadata <- function(counting_type) {
  # Read metadata and clean it
  metadata <- read.xlsx(
    xlsxFile = paths$comptage_reference,
    sheet = counting_type,
    sep.names = "_"
  ) %>%
    rename_with(~ str_to_lower(.), everything()) %>%
    rename_with(~ stri_trans_general(., "latin-ascii"), everything()) %>%
    rename(modalites = `fourchette_de_valeur_/_modalites`)
  
  if ("champ_2" %in% names(metadata)) {
    metadata <- metadata %>%
      rename(champ1 = champ_1, champ2 = champ_2)
  }
  
  return(metadata)
}