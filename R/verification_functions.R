















# Function to convert blanks and text "NA" to actual NA values
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







convert_decimal_to_hms <- function(decimal_time) {
  # Convert decimal day to seconds (1 day = 24 hours * 60 minutes * 60 seconds)
  total_seconds <- as.numeric(decimal_time) * 24 * 60 * 60
  
  # Use hms to create a time object in "HH:MM:SS" format
  time_formatted <- hms::hms(seconds = total_seconds)
  time_formatted <- hms::round_hms(time_formatted, 60)
  
  
  return(time_formatted)
}
