#' Converts double header to one clean header
#'
#' @param dataset dataset with double header
#'
#' @return new header
#' @export
#'
#' @examples
double_header_fusion <- function(dataset) {
  if (nrow(dataset) < 2)
    stop("Dataset must have at least two rows for header fusion.")
  
  header_selection <- dataset[1:2, ]
  new_header <- c()
  
  # for all cells, apply str_trans_general to remove accents and replace spaces by nothing
  header_selection <- header_selection %>%
    mutate(across(
      everything(),
      ~ stringi::stri_trans_general(., "Latin-ASCII")
    )) %>%    # Remove accents
    mutate(across(everything(), ~ ifelse(
      grepl("L", .) &
        (grepl("<", .) |
           grepl(">", .)),
      # If 'L' or '<' is present
      gsub("m| ", "", .),
      # Remove 'm' and spaces
      gsub(" ", "_", tolower(.))
    )))                     # Else, replace spaces with underscores and lowercase
  
  # Step 2: Create the new header by combining rows
  new_header <- purrr::map2_chr(header_selection[2, ], header_selection[1, ], function(h2, h1) {
    if (h2 != h1) {
      return(paste(h2, h1, sep = "__"))  # Combine if they differ
    } else {
      return(h1)  # Keep the same if they are identical
    }
  })
  
  new_header <- unname(new_header)
  
  return(new_header)
  
}



#' Converts xlsx data with double header as one clean header dataset using concatenation
#'
#' @param file_path path of the xlsx file
#' @param sheet_name name of the sheet to import
#'
#' @return dataset with clean header
#' @export
#'
#' @examples
double_header_import <- function(file_path, sheet_name) {
  # Import of whole dataset
  data_double <- openxlsx::read.xlsx(
    xlsxFile = file_path,
    sheet = sheet_name,
    sep.names = " ",
    colNames = FALSE,
    fillMergedCells = TRUE
  )
  
  # Double header combined as 1 header by concatenation
  names_data_double <- double_header_fusion(data_double)
  
  # Import of dataset with new header
  data_double <-  openxlsx::read.xlsx(
    xlsxFile = file_path,
    sheet = sheet_name,
    sep.names = " ",
    colNames = FALSE,
    fillMergedCells = TRUE,
    skipEmptyCols = FALSE,
    startRow = 3,
    cols = 1:length(names_data_double)
  )
  
  # Check if the number of columns read matches the header length
  if (ncol(data_double) < length(names_data_double)) {
    # Add empty columns if necessary to match the number of headers
    data_double <- cbind(data_double, matrix(
      NA,
      nrow = nrow(data_double),
      ncol = length(names_data_double) - ncol(data_double)
    ))
  }
  
  # Renaming columns with new header
  colnames(data_double) <- names_data_double
  
  return(data_double)
  
}


#' Title
#'
#' @param fused_header 
#'
#' @return
#' @export
#'
#' @examples
split_fused_headers <- function(fused_header) {
  parts <- strsplit(fused_header, "__")[[1]]
  
  # If there's only one part, return it as champ1 and NA as champ2
  if (length(parts) == 1) {
    return(list(champ1 = parts[1], champ2 = parts[1]))
  } else {
    return(list(champ1 = parts[2], champ2 = parts[1])) # Return both parts if present
  }
}