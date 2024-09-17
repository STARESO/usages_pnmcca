###########################################################################
## --- Observatoire des usages - compilation des fiches de comptage terrain
###########################################################################

data_sheet

test <- check_column_format(data_sheet, metadata_reference, file_name, sheet, error_logs)

metadata_reference <- metadata_reference %>%
  mutate(across(
    everything(),
    ~ stringi::stri_trans_general(., "Latin-ASCII")
  )) %>%    # Remove accents
  mutate(across(everything(), ~ ifelse(
    grepl("L", .) &
      (grepl("<", .) |
         grepl(">", .)),
    # If 'L' or '<' is present
    gsub("m| ", "", tolower(.)),
    # Remove 'm' and spaces
    gsub(" ", "_", tolower(.))
  ))) # Replace spaces by underscores and make everything lowercase
  

for (col in names(data_sheet)) {
  
  if (!col %in% c("date", "heure", "secteur")) {
    # Deconstruct the fused header
    header_parts <- split_fused_headers(col)
    print(header_parts$champ1)
    print(header_parts$champ2)
    metadata_row <- metadata_reference %>%
      filter(champ1 == header_parts$champ1 & champ2 == header_parts$champ2)
    print(metadata_row$format)
    print("----------------------------------------------")
  }
  
}



         