### Cleaning Script ###

# Set-up ------------------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(stats)
library(knitr)
library(here)
library(janitor)

# load data ---------------------------------------------------------------

survey_data <- read_csv(here("data", "survey_data.csv"))

# explore data ------------------------------------------------------------

# no. of responses by discipline and total number of participants
responses_by_discipline <- 
  table(survey_data$discipline) %>% 
  addmargins()
responses_by_discipline

# number of universities
number_of_universities <- survey_data %>% 
  select(university_id) %>% 
  unique() %>% 
  nrow()
number_of_universities

# Set factor levels -------------------------------------------------------

survey_data <- survey_data %>% 
  mutate(across(c(
    response_id:university_id, 
    discipline_check:same_teaching,
    study_design:elective_courses,
    credits_sd:which_years,
    prefinal_project:q8_ethics_3),
    ~ factor(.x)))

survey_data <- survey_data %>% 
  mutate(degree_years = factor(degree_years, ordered = TRUE))

survey_data <- survey_data %>% 
  mutate(discipline = factor(
      discipline,
      levels=c(
        "Biological Sciences (e.g., Ecology, Evolution, Plant Sciences, Molecular Biology).",
        "Biomedical Sciences (e.g., Neuroscience, Reproductive Biology, Pharmacology, Infectious Diseases)",
        "Medicine",
        "Psychology"),
      labels = c(
        "Bio",
        "Biomed",
        "Med",
        "Psych")))

survey_data <- survey_data %>% 
  mutate(course_packaging = factor(
    course_packaging,
    levels=c(
      "Instruction in Data Analysis / quantitative Study Design is embedded in teaching unit(s) that encompass additional topics",
      "Both (please provide details below)",
      "Provides one or more teaching units devoted to these topics"),
    labels=c(
      "Integrated", 
      "Both",
      "StandAlone"),
    ordered = TRUE))

survey_data <- survey_data %>%
  mutate(teaching_approach = factor(
    teaching_approach,
    levels= c(
      "Mathematical  (e.g. teaching the mathematics of the Normal Distribution, or proofs of a t-test)",
      "Both, emphasising Mathematical",
      "An equal balance of Mathematical and Functional.",
      "Both, emphasising Functional",
      "Functional (e.g. emphasising how experimental design matches with an appropriate analysis approach, without emphasising the mathematics behind the analysis)"),
    labels = c(
      "allMath",
      "emphMath",
      "equalMathFunc",
      "emphFunc",
      "allFunc"),
    ordered = TRUE
  ))

credit_levels <- c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

survey_data <- survey_data %>% 
  mutate(
    credits_sd = factor(
      credits_sd,
      levels = credit_levels,
      ordered = TRUE),
    credits_da = factor(
      credits_da,
      levels = credit_levels,
      ordered = TRUE))

survey_data <- survey_data %>% 
  mutate(software_format = factor(
    software_format,
    levels = c("Script-based (e.g., R, Python, SAS, SPSS using syntax, other)",
               "Both",
               "Push-button based (e.g., Minitab, JMP, SPSS using the drop-down menus, Excel, Prism, Jamovi, JASP, other)",
               "Neither"),
    labels = c("script", "both", "push-button", "neither"),
    ordered = TRUE
  ))

# save data file -------------------------------------------------------------------------------------------------------

# save survey_data as rds file
saveRDS(survey_data, file= here("data/clean_data", "clean_survey_data.rds"))

# save survey_data as csv file
write.csv(survey_data, file= here("data/clean_data", "clean_survey_data.csv"), row.names=FALSE)

