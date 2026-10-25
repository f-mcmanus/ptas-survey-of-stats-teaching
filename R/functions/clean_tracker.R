## clean_tracker 

#purpose: for cleaning multiple recruitment_tracker columns at once
#creates (yes/no) responses depending on whether a department was contacted or not

clean_tracker <- function(data, vars) {
  
  for(var in vars) {
    
    data <- data %>%
      mutate(
        "{var}" := case_when(
          is.na( .data[[var]]) ~ "no", # Blank cell = department not contacted
          # .data[[var]] == "NA" ~ "no", # "NA" = don't need to contact
          grepl(";",  .data[[var]]) ~ "yes", # ";" = multiple dates inputted
          .data[[var]] %in% c(45000:45300) ~ "yes", # Excel dates
          .default =  .data[[var]]))
    
    if(var == "second_enquiry_email") {
      data <- data %>%
        mutate("{var}" := case_when(
          .data[[var]] == "COMPLETED SURVEY" ~ "no",
          .data[[var]] == "LINK SENT" ~ "no",
          .data[[var]] == "?" ~ "no",
          .default =  .data[[var]]))}}
  
  return(data)
  
}

# EXAMPLE:
# list_cols <- c("col1", "col2", "col3")
#
# tracker_full <- tracker_full %>%
#   tracker_cleaner(list_cols)
