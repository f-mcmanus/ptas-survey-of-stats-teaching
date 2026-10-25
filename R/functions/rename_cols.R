## rename_cols function
# takes survey columns produced by qualtrics and returns informative ones

rename_cols <- function(data) {

# Rename columns for question 8
  list_names <- data %>%
    names()
  
  list_tidy <- list_names %>%
    gsub("44", "8_study_design", x = .) %>% 
    gsub("45", "8_interpreting_results", x = .) %>%
    gsub("46", "8_data_handling", x = .) %>%
    gsub("47", "8_stats_concepts", x = .) %>%
    gsub("48", "8_stats_tests", x = .) %>%
    gsub("49", "8_QRP", x = .) %>%
    gsub("50", "8_open_research", x = .) %>%
    gsub("51", "8_ethics", x = .)
    
  
  data <- setNames(data, list_tidy)
  
  # Select and rename the other columns
  data <- data %>% select("status",
                          "progress",
                          "response_id",
                          "consent" = "q3",
                          "university" = "q4",
                          "name" = "q5",
                          "email" = "q6",
                          "role" = "q7",
                          "years_teaching" = "q8",
                          "discipline" = "q9",
                          "discipline_open" = "q11",
                          "discipline_check" = "q16",
                          "eco_evo" = "q72",
                          "same_teaching" = "q17",
                          "subdiscipline" = "q19",
                          "subdiscipline_open" ="q20",
                          "study_design" = "q22",
                          "assessment_sd" = "q23",
                          "data_analysis" = "q24",
                          "assessment_da" = "q25",
                          "course_packaging" = "q26",
                          "teaching_approach" = "q27",
                          "elective_courses" = "q28",
                          "q4_anything_else_open" = "q81",
                          "degree_years" = "q29",
                          "degree_years_open" = "q82",
                          "credits_sd" = "q85",
                          "format_sd" = "q89",
                          "credits_da" = "q86",
                          "format_da" = "q88",
                          "which_years" = "q32",
                          "training_sd_open" = "q33",
                          "training_da_open" = "q34",
                          "software_format" = "q35",
                          "dominant_software" = "q36",
                          "prefinal_project" = "q37",
                          "prefinal_project_compulsory" = "q38",
                          "prefinal_project_assessed" = "q39",
                          "final_project" = "q40",
                          "final_project_compulsory" = "q41",
                          "final_project_assessed" = "q42",
                          "q8_study_design_1" : "q8_ethics_3",
                          "q8_other_open" = "q52",
                          "further_discussion_open" = "q53",
                          "another_subdiscipline_open" = "q83",
                          "recommendation_open" ="q54",
                          "challenges_open" = "q56",
                          "approach_open" = "q57",
                          "anything_else_open" = "q58")
  
}

