### Cleaning recruitment tracker data


# Set-up ------------------------------------------------------------------
library(here)
library(tidyverse)

source(here("R/functions", "clean_tracker.R"))

# Load data ---------------------------------------------------------------
tracker_data <- read_csv(here("data", "tracker_data.csv"))

# Check data --------------------------------------------------------

tracker_data %>%
  pull(completed_survey) %>%
  is.na() %>%
  sum() # 0 - All departments are eligible

# Bio/Biomed have shared teaching, so nrow() > departments contacted
nrow(tracker_data) 

completed_survey_responses <- tracker_data %>% 
  group_by(completed_survey) %>% 
  summarise(num_responses = n()) %>% 
  arrange(desc(num_responses))

completed_survey_responses

# Standardise responses to yes/no -----------------------------------------------

tracker_data <- tracker_data %>% 
  mutate(completed_survey = factor(completed_survey))

tracker_data <- tracker_data %>%
  mutate(completed_survey = case_when(
    completed_survey %in% c("1", "2") ~ "yes",
    completed_survey %in% c("0", "0;_0", "0;_0;_0", 
                            "biological_sciences", "biomedical_sciences", "psychology", "medicine")
    ~ "no",
    # NA indicates a blank cell
    # but "NA" was used to indicate that a department was not eligible
    completed_survey == "NA" ~ "ineligible",
    .default = completed_survey))

# Standardise the remaining columns
column_names <- tracker_data %>%
  select(enquiry_email:enquiry_email_reminder) %>%
  names()

tracker_data <- tracker_data %>%
  clean_tracker(column_names)

# Data structure ----------------------------------------------------------
tracker_data <- tracker_data %>%
  mutate(across(everything(), factor)) %>%
  select(university_id, discipline, completed_survey:enquiry_email_reminder)

glimpse(tracker_data)

# Save data files ----------------------------------------------------------
saveRDS(tracker_data, file= here("data/clean_data", "clean_recruitment_tracker.rds"))
write.csv(tracker_data, file= here("data/clean_data", "clean_recruitment_tracker.csv"), row.names=FALSE)

