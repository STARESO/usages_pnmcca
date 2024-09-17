#' Reads and cleans metadata for the specified counting type.
#'
#' This function reads metadata from the reference file for the given 
#' counting type and applies several cleaning steps, including renaming 
#' columns, removing accents, and handling modalities. It renames columns 
#' to lower case, removes accents, and renames specific columns as needed.
#'
#' @param counting_type The type of counting data to read metadata for. 
#' This should correspond to the sheet name in the metadata reference file.
#'
#' @return A cleaned metadata dataframe with the columns renamed and 
#' processed, ready for further use in the compilation process.
#' @export
#'
#' @examples
#' read_metadata("plaisance")
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
