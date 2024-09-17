#' Convert Empty Strings and "NA" Text to NA
#'
#' This function replaces empty strings and the text "NA" with actual NA values.
#'
#' @param data_sheet A data frame to clean.
#'
#' @return A cleaned data frame with empty strings and "NA" replaced by NA.
#'
#' @export
#'
#' @examples
#' cleaned_data <- convert_to_na(my_data_sheet)
convert_to_na <- function(data_sheet) {
  data_sheet <- data_sheet %>%
    mutate(across(
      everything(),
      ~ na_if(trimws(.), "")    # Convert empty strings to NA
    )) %>%
    mutate(across(
      everything(),
      ~ na_if(., "NA")          # Convert text "NA" to actual NA
    ))
  
  return(data_sheet)
}


#' Convert Decimal Time to "HH:MM:SS" Format
#'
#' Converts decimal time (e.g., 0.5 for 12:00 PM) to "HH:MM:SS" format.
#'
#' @param decimal_time A numeric vector with decimal times (e.g., 0.5 = 12:00 PM).
#'
#' @return A vector of times in "HH:MM:SS" format.
#'
#' @export
#'
#' @examples
#' time_formatted <- convert_decimal_to_hms(c(0.5, 0.25, 0.75))
convert_decimal_to_hms <- function(decimal_time) {
  total_seconds <- as.numeric(decimal_time) * 24 * 60 * 60
  time_formatted <- hms::hms(seconds = total_seconds)
  time_formatted <- hms::round_hms(time_formatted, 60)
  
  return(time_formatted)
}