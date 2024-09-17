#' Converts double header to one clean header
#'
#' This function combines the first two rows of a dataset with a double 
#' header by concatenating the two rows into a single clean header. 
#' It removes accents, replaces spaces with underscores, and handles cases 
#' where special symbols (like '<', '>', and 'L') are present.
#'
#' @param dataset A dataset with a double header (first two rows).
#'
#' @return A character vector representing the new combined header.
#' @export
#'
#' @examples
#' double_header_fusion(my_dataset)
double_header_fusion <- function(dataset) {
  if (nrow(dataset) < 2)
    stop("Dataset must have at least two rows for header fusion.")
  
  header_selection <- dataset[1:2, ]
  new_header <- c()
  
  # Apply transformations: remove accents, replace spaces with underscores
  # and handle "<", ">", "L"
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
  
  # Combine header parts if different; otherwise, keep as is
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


#' Imports an Excel sheet with a double header and creates a clean header.
#'
#' This function imports an Excel sheet with a double header, concatenates 
#' the two header rows into one clean header, and returns the dataset 
#' with this new header. It ensures that the number of columns in the 
#' dataset matches the new header.
#'
#' @param file_path The path to the Excel (.xlsx) file.
#' @param sheet_name The name of the sheet to import.
#'
#' @return A dataset with a single clean header after merging the double header.
#' @export
#'
#' @examples
#' double_header_import("my_file.xlsx", "Sheet1")
double_header_import <- function(file_path, sheet_name) {
  # Import the whole dataset with no column names
  data_double <- openxlsx::read.xlsx(
    xlsxFile = file_path,
    sheet = sheet_name,
    sep.names = " ",
    colNames = FALSE,
    fillMergedCells = TRUE
  )
  
  # Create the new header by combining the first two rows
  names_data_double <- double_header_fusion(data_double)
  
  # Import the dataset starting from the third row, with the new header
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
  
  # Ensure the number of columns matches the header
  if (ncol(data_double) < length(names_data_double)) {
    data_double <- cbind(
      data_double, 
      matrix(NA, nrow = nrow(data_double), 
             ncol = length(names_data_double) - ncol(data_double))
    )
  }
  
  # Rename columns with the new header
  colnames(data_double) <- names_data_double
  
  return(data_double)
}


#' Splits a fused header into two parts.
#'
#' This function splits a fused header (e.g., "ancre__L<8m") into its 
#' two component parts, returning them as champ1 and champ2. If the header 
#' is not fused, champ1 and champ2 are identical.
#'
#' @param fused_header A character string representing a fused header.
#'
#' @return A list with two elements: champ1 (the first part of the header) 
#' and champ2 (the second part of the header).
#' @export
#'
#' @examples
#' split_fused_headers("ancre__L<8m")
split_fused_headers <- function(fused_header) {
  parts <- strsplit(fused_header, "__")[[1]]
  
  # If there's only one part, set champ1 and champ2 as identical
  if (length(parts) == 1) {
    return(list(champ1 = parts[1], champ2 = parts[1]))
  } else {
    return(list(champ1 = parts[2], champ2 = parts[1])) 
  }
}
