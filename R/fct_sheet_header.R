#' Verifies the coherence of the sheet header, including the number of columns 
#' and variable names
#'
#' This function checks the structure of the dataset sheet headers by verifying 
#' the number of columns and the names of variables. It handles both single and 
#' double headers, and logs any discrepancies found between the dataset and the 
#' reference metadata.
#'
#' @param data_sheet A data frame representing the sheet from the dataset to verify.
#' @param metadata_reference A data frame containing the reference metadata for 
#' the columns and variable names.
#' @param file_name The name of the file from which the sheet is being verified.
#' @param sheet The name of the sheet being verified.
#' @param error_logs A list that stores all logged errors encountered during 
#' the verification process.
#'
#' @return A list containing:
#' - `error_flag`: A boolean value indicating whether any errors were found.
#' - `error_logs`: The updated error logs including any discrepancies found.
#' @export
#'
#' @examples
#' # Example usage:
#' verify_sheet_header(data_sheet, metadata_reference, "file.xlsx", "Sheet1", error_logs)
verify_sheet_header <- function(data_sheet, 
                                metadata_reference, 
                                file_name, 
                                sheet, 
                                error_logs) {
  # Flag to track whether any errors occurred in the current sheet
  error_flag <- FALSE
  
  # Check if it's a double header case (presence of champ2 in metadata)
  if ("champ2" %in% names(metadata_reference)) {
    # Verify the number of columns
    if (ncol(data_sheet) != nrow(metadata_reference)) {
      # Log error if the number of columns does not match
      error_logs$wrong_column_amount <- c(error_logs$wrong_column_amount, ncol(data_sheet))
      error_logs$right_column_amount <- c(error_logs$right_column_amount, nrow(metadata_reference))
      error_logs$wrong_column_amount_files <- c(error_logs$wrong_column_amount_files, file_name)
      error_logs$wrong_column_amount_sheets <- c(error_logs$wrong_column_amount_sheets, sheet)
      error_flag <- TRUE
      return(list(error_flag, error_logs))  # Exit early if the number of columns is incorrect
    }
    
    # Check first row of the double header
    coherent_variable_names1 <- data_sheet[1, ] == metadata_reference$champ1
    # Check second row of the double header
    coherent_variable_names2 <- data_sheet[2, ] == metadata_reference$champ2
    
    # Log errors for first row mismatches
    if (sum(!coherent_variable_names1) != 0) {
      error_logs$wrong_variable_names <- c(error_logs$wrong_variable_names, data_sheet[1, ][!coherent_variable_names1])
      error_logs$right_variable_names <- c(error_logs$right_variable_names, metadata_reference$champ1[!coherent_variable_names1])
      error_logs$wrong_variable_names_position <- c(error_logs$wrong_variable_names_position, which(!coherent_variable_names1))
      error_logs$wrong_variable_names_files <- c(error_logs$wrong_variable_names_files, rep(file_name, sum(!coherent_variable_names1)))
      error_logs$wrong_variable_names_sheets <- c(error_logs$wrong_variable_names_sheets, rep(sheet, sum(!coherent_variable_names1)))
      error_flag <- TRUE
    }
    
    # Log errors for second row mismatches
    if (sum(!coherent_variable_names2) != 0) {
      error_logs$wrong_variable_names <- c(error_logs$wrong_variable_names, data_sheet[2, ][!coherent_variable_names2])
      error_logs$right_variable_names <- c(error_logs$right_variable_names, metadata_reference$champ2[!coherent_variable_names2])
      error_logs$wrong_variable_names_position <- c(error_logs$wrong_variable_names_position, which(!coherent_variable_names2))
      error_logs$wrong_variable_names_files <- c(error_logs$wrong_variable_names_files, rep(file_name, sum(!coherent_variable_names2)))
      error_logs$wrong_variable_names_sheets <- c(error_logs$wrong_variable_names_sheets, rep(sheet, sum(!coherent_variable_names2)))
      error_flag <- TRUE
    }
    
  } else {
    # Single header case: Check number of columns
    if (ncol(data_sheet) != nrow(metadata_reference)) {
      # Log error if column numbers don't match
      error_logs$wrong_column_amount <- c(error_logs$wrong_column_amount, ncol(data_sheet))
      error_logs$right_column_amount <- c(error_logs$right_column_amount, nrow(metadata_reference))
      error_logs$wrong_column_amount_files <- c(error_logs$wrong_column_amount_files, file_name)
      error_logs$wrong_column_amount_sheets <- c(error_logs$wrong_column_amount_sheets, sheet)
      error_flag <- TRUE
      return(list(error_flag, error_logs))  # Exit early if the column count is wrong
    }
    
    # Check the variable names in the single header
    coherent_variable_names <- data_sheet[1, ] == metadata_reference$champ
    if (sum(!coherent_variable_names) != 0) {
      error_logs$wrong_variable_names <- c(error_logs$wrong_variable_names, names(data_sheet)[!coherent_variable_names])
      error_logs$right_variable_names <- c(error_logs$right_variable_names, metadata_reference$champ[!coherent_variable_names])
      error_logs$wrong_variable_names_position <- c(error_logs$wrong_variable_names_position, which(!coherent_variable_names))
      error_logs$wrong_variable_names_files <- c(error_logs$wrong_variable_names_files, rep(file_name, sum(!coherent_variable_names)))
      error_logs$wrong_variable_names_sheets <- c(error_logs$wrong_variable_names_sheets, rep(sheet, sum(!coherent_variable_names)))
      error_flag <- TRUE
    }
  }
  
  # Return the error flag and updated error logs
  return(list(error_flag, error_logs))
}
