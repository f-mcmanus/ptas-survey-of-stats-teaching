# q8_binomial_model_functions

# Prepare_for_model -------------------------------------------------------

prepare_for_model <- function(data, col_name) {
  
  #' prepare_for_model
  #'
  #' data: the cleaned survey_data data frame
  #' col_name: a factor with "Yes", "No", and "Don't Know" responses
  #'
  #' returns: data frame with specified column a binomial variable (0/1)
  #' example: prepare_for_model(survey_data, "q8_study_design_1")
  
  data <- data %>%
    rename(is_this_taught = col_name) %>%
    filter(is_this_taught != "Don't Know") %>% 
    mutate(
      is_this_taught = case_when(
        is_this_taught == "Yes" ~ 1, 
        is_this_taught == "No" ~ 0))
  
}


# get_probability_estimates -----------------------------------------------

calculate_probabilities <- function(model_estimates) {
  
  model_estimates <- model_estimates %>% 
    clean_names() %>% 
    select(estimate) %>% 
    rownames_to_column("parameter")
  # mutate(
  #   column_number = as.numeric(column_number)
  # )
  
  intercept <- model_estimates %>% 
    filter(parameter == "Intercept") %>% 
    pull(estimate) 
  
  model_estimates <- model_estimates %>% 
    mutate(probability = case_when(
      parameter == "Intercept" ~ plogis(intercept),
      parameter %in% 
        c("discipline1", "discipline2", "discipline3") ~ 
        plogis(intercept + estimate)
    ),
    discipline = case_when(
      parameter == "Intercept" ~ "Psych",
      parameter == "discipline1" ~ "Bio",
      parameter == "discipline2" ~ "Biomed",
      parameter == "discipline3" ~ "Med", 
    ))# %>% 
  # Remove estimate as it confused joins with other datasets
  # select(-estimate)
}

