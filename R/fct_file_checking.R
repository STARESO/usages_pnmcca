#' Check the Coherence of a File Name
#'
#' This function checks if the name of a field counting type file follows the expected naming format.
#'
#' @param file_name The name of the file to check.
#' @param counting_type The type of field counting (e.g., "plage", "plaisance").
#'
#' @return A logical value: `TRUE` if the file name follows the expected format, `FALSE` otherwise.
#' 
#' @export
#'
#' @examples
#' file_coherence("us_med_pnmcca_observatoire_comptage_terrain_plage_2023-08-15.xlsx", "plage")
#' 
file_coherence <- function(file_name, counting_type) {
  file_name_type <- paste0(
    "us_med_pnmcca_observatoire_comptage_terrain_",
    counting_type,
    "_\\d{4}-\\d{2}-\\d{2}\\.xlsx"
  )
  
  file_name_coherence <- grepl(file_name_type, file_name)
  
  return(file_name_coherence)
}



#' Check Date Coherence in File Name or Sheet
#'
#' This function checks if the date in a file name follows the expected format and optionally 
#' compares the date in the file name to the date in a sheet.
#'
#' @param file_name The name of the file to check.
#' @param counting_type The type of field counting (e.g., "plage", "plaisance").
#' @param date_format The expected date format ("YYYY_MM_DD" or "DD_MM_YYYY"). Default is "YYYY_MM_DD".
#' @param sheet Optional: The sheet to check date coherence with. If `NULL`, only the file name is checked.
#'
#' @return A list with logical value `coherent` indicating whether the date is coherent, and `wrong_date` if applicable.
#' 
#' @export
#'
#' @examples
#' file_date_coherence("us_med_pnmcca_observatoire_comptage_terrain_plage_2023-08-15.xlsx", "plage")
file_date_coherence <- function(file_name,
                                counting_type,
                                date_format = "YYYY_MM_DD",
                                sheet = NULL) {
  # Extracting date from file name
  if (date_format == "YYYY_MM_DD") {
    date_comptage <- str_extract_all(file_name, "\\d{4}-\\d{2}-\\d{2}")[[1]]
  } else if (date_format == "DD_MM_YYYY") {
    date_comptage <- str_extract_all(file_name, "\\d{4}_\\d{2}_\\d{2}")[[1]]
  } else {
    stop("Date format not recognized")
  }
  
  # If just checking the file name's date coherence
  if (is.null(sheet)) {
    if (is.na(date_comptage[1])) {
      return(list(coherent = FALSE, wrong_date = date_comptage[1]))
    } else {
      return(list(coherent = TRUE, wrong_date = NULL))
    }
    
    # If checking coherence between file name and sheet date
  } else {
    file_path <- paste0(
      "data/raw/comptages_terrain/",
      str_to_title(counting_type),
      "/",
      file_name,
      ".xlsx"
    )
    
    sheet_date <- read.xlsx(xlsxFile = file_path, sheet = sheet)
    
    # Logic to check date between metadata and comptage can be added here
  }
}