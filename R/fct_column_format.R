#' Check and Convert Column Formats
#'
#' This function checks and converts the column formats of a given data sheet based on the expected formats 
#' provided in a metadata reference. It supports different data formats such as integers, text, qualitative 
#' ordinal and nominal factors, dates, and times (both "h" and decimal time formats). Invalid or incorrect 
#' formats are logged for further inspection.
#'
#' @param data_sheet A data frame representing the sheet from the Excel file to be validated and processed.
#' @param metadata_reference A data frame containing metadata with columns 'champ1' and 'champ2' for double 
#'        headers and 'champ' for simple header sheets. It also contains info such as 'format'. 
#'        The metadata specifies expected formats for the corresponding columns in the data sheet.
#' @param file_name A string representing the name of the file being processed. Used for error logging.
#' @param sheet_name A string representing the name of the sheet being processed. Used for error logging.
#' @param error_logs A list to capture and log any errors encountered during the processing of column formats.
#'        This list will be updated with information about wrong or unknown formats, along with the column and 
#'        file/sheet names where issues were found.
#' @param is_double_header A boolean value indicating whether the data sheet uses a double-header structure. 
#'        If TRUE, the function will check for both `champ1` and `champ2` in the metadata reference.
#'
#' @return A list containing two elements: 
#'         \item{data_sheet}{The modified data sheet with columns converted to their expected formats.}
#'         \item{error_logs}{The updated error logs capturing any format issues encountered.}
#'
#' @examples
#' @export
check_column_format <- function(data_sheet,
                                metadata_reference,
                                file_name,
                                sheet_name,
                                error_logs,
                                is_double_header) {
  
  # Preprocess metadata to handle accents and spaces
  metadata_reference <- metadata_reference %>%
    mutate(across(
      everything(),
      ~ stringi::stri_trans_general(., "Latin-ASCII")
    )) %>%    # Remove accents
    mutate(across(everything(), ~ ifelse(
      grepl("L", .) & (grepl("<", .) | grepl(">", .)),
      gsub("m| ", "", .),
      # Remove 'm' and spaces
      gsub(" ", "_", tolower(.))
    )))
  
  # Iterate over each column in the data sheet
  for (col in names(data_sheet)) {
    # Skip columns that shouldn't be validated
    if (col %in% c("date", "heure", "secteur")) {
      next
    }
    
    # Deconstruct the header into champ1 and champ2 (even for non-fused headers)
    header_parts <- split_fused_headers(col)
    
    # Match metadata for both single and double header cases
    if (is_double_header) {
      metadata_row <- metadata_reference %>%
        filter(champ1 == header_parts$champ1 &
                 champ2 == header_parts$champ2)
    } else {
      metadata_row <- metadata_reference %>%
        filter(champ == header_parts$champ1)
    }
    
    # If no matching format found, log an error
    if (nrow(metadata_row) == 0) {
      error_logs$unknown_format <- c(error_logs$unknown_format, col)
      error_logs$unknown_format_files <- c(error_logs$unknown_format_files, file_name)
      next
    }
    
    # Extract the expected format
    expected_format <- metadata_row$format
    current_col <- data_sheet[[col]]
    
    # Handle conversion for the 'entier' format
    if (expected_format == "entier") {
      converted_col <- suppressWarnings(as.integer(current_col))
      non_numeric_values <- which(is.na(converted_col) & !is.na(current_col))
      
      if (length(non_numeric_values) > 0) {
        error_logs$wrong_column_format <- c(error_logs$wrong_column_format, class(current_col))
        error_logs$expected_column_format <- c(error_logs$expected_column_format, expected_format)
        error_logs$wrong_column_format_name <- c(error_logs$wrong_column_format_name, col)
        error_logs$wrong_column_format_sheets <- c(error_logs$wrong_column_format_sheets, sheet_name)
        error_logs$wrong_column_format_files <- c(error_logs$wrong_column_format_files, file_name)
        converted_col[non_numeric_values] <- NA  # Replace invalid values
      }
      
      data_sheet[[col]] <- converted_col
      
      # Handle conversion for the 'texte' format
    } else if (expected_format == "texte") {
      if (!is.character(current_col)) {
        data_sheet[[col]] <- as.character(current_col)
      }
      
      # Handle conversion for 'qualitative_ordinale' format
    } else if (expected_format == "qualitative_ordinale") {
      if (!is.ordered(current_col)) {
        data_sheet[[col]] <- factor(current_col, ordered = TRUE)
      }
      
      # Handle conversion for 'qualitative_nominale' format
    } else if (expected_format == "qualitative_nominale") {
      if (!is.factor(current_col)) {
        data_sheet[[col]] <- factor(current_col)
      }
      
      # Handle conversion for 'date' format
    } else if (expected_format == "date") {
      converted_col <- suppressWarnings(as.Date(current_col, format = "%Y-%m-%d"))
      non_date_values <- which(is.na(converted_col) & !is.na(current_col))
      
      if (length(non_date_values) > 0) {
        error_logs$wrong_column_format <- c(error_logs$wrong_column_format, class(current_col))
        error_logs$expected_column_format <- c(error_logs$expected_column_format, expected_format)
        error_logs$wrong_column_format_name <- c(error_logs$wrong_column_format_name, col)
        error_logs$wrong_column_format_sheets <- c(error_logs$wrong_column_format_sheets, sheet_name)
        error_logs$wrong_column_format_files <- c(error_logs$wrong_column_format_files, file_name)
        converted_col[non_date_values] <- NA  # Replace invalid dates
      }
      
      data_sheet[[col]] <- converted_col
      
      # Handle conversion for 'temps' format
    } else if (expected_format == "temps") {
      if (all(grepl("h", current_col, ignore.case = TRUE))) {
        # Replace "h" with ":00" to standardize time to "HH:MM:SS"
        current_col <- gsub("h", ":00:00", current_col, ignore.case = TRUE)
        
        # Use hms::parse_hms to convert the time, catching errors
        converted_col <- tryCatch(
          hms::parse_hms(current_col),
          error = function(e) NA
        )
      } else {
        # Use your earlier function to handle decimal time format
        converted_col <- try(suppressWarnings(convert_decimal_to_hms(current_col)), silent = TRUE)
      }
      
      non_time_values <- which(is.na(converted_col) & !is.na(current_col))
      
      if (length(non_time_values) > 0) {
        error_logs$wrong_column_format <- c(error_logs$wrong_column_format, col)
        error_logs$wrong_column_format_files <- c(error_logs$wrong_column_format_files, file_name)
        converted_col[non_time_values] <- NA  # Replace invalid times
      }
      
      data_sheet[[col]] <- converted_col
      
    } else {
      # If format not recognized, log an error
      error_logs$unknown_format <- c(error_logs$unknown_format, col)
      error_logs$unknown_format_files <- c(error_logs$unknown_format_files, file_name)
    }
  }
  
  return(list(data_sheet = data_sheet, error_logs = error_logs))
}
