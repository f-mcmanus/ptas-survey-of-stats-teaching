
# Set_labels --------------------------------------------------------------

set_labels <- function(topic) {
  
  if(topic == "study_design"){
    
    study_design <- c(
      "Observational vs experimental approaches",
      "Matching statistical methods to Study Design<br>and to the scientific question / hypothesis",
      "Select appropriate control groups",
      "Need for replication in a study",
      "How to select study materials that are representative of study’s goals",
      "Identifying or avoiding confounding variables",
      "Pseudo-replication",
      "Power analysis: consequences of low-powered studies",
      "Power analysis: application",
      "Blinding",
      "Use of randomization throughout an experiment",
      "Blocking",
      "Balanced vs. unbalanced designs"
    )
    
    return(study_design)
  }
  
  if(topic == "interpreting_results"){
    
    interpreting_results <- c(
      "Interpreting results in relation to p-values",
      "Interpreting results in relation to effect size with confidence intervals",
      "Basing conclusions on multiple lines of evidence<br>(e.g. effect size vs. p-value; complementary experiments)"
    )
    
    return(interpreting_results)
  }
  
  if(topic == "data_handling"){
    
    data_handling <- c(
      "Data cleaning and preparation",
      "Visualising data"
    )
    
    return(data_handling)
  }
  
  if(topic == "stats_concepts"){
    
    stats_concepts <- c(
      "Exploratory vs. confirmatory studies",
      "Descriptive statistics",
      "Probability",
      "Distributions",
      "Central limit theorem",
      "Estimating with uncertainty<br>(understanding standard error and confidence intervals)",
      "Null hypothesis testing vs. parameter estimation"
    )
    
    return(stats_concepts)
  }
  
  if(topic == "stats_tests"){
    
    stats_tests <- c(
      "t-tests",
      "Frequency tests (e.g. Chi-squared)",
      "Correlation",
      "Simple Linear Regression",
      "ANOVA",
      "Data transformation",
      "Non-parametric statistics",
      "General linear models with multiple independent variables",
      "General linear mixed effects models",
      "Non-linear regression",
      "Generalized linear (mixed) models",
      "Generalised estimating equations",
      "Path analysis",
      "Structural equation modelling (SEM)",
      "Principal component analysis (PCA)",
      "Exploratory factor analysis (EFA)",
      "Confirmatory factor analysis (CFA)",
      "Likelihood",
      "Bayesian Statistics",
      "Computational methods (e.g. bootstrapping, randomisation tests)",
      "Meta-analysis",
      "Machine learning",
      "Deep learning"
    )
    
    return(stats_tests)
  }
  
  if(topic == "QRP"){
    
    QRP <- c(
      "Hypothesising after results known (HARKing)",
      "p-hacking",
      "Problems arising from unreported variables",
      "Problems arising from unreported covariates",
      "Problems arising from unreported analyses",
      "Rounding p-values",
      "Problems arising from excluding data to obtain a more desirable conclusion",
      "Adding data after analysis to obtain a more desirable p-value",
      "Problems arising from switching analysis to obtain a more desirable conclusion",
      "Not reporting problems with the study design, analysis or conclusions",
      "Data fabrication"
    )
    
    return(QRP)
  }
  
  if(topic == "open_research"){
    
    open_research <- c(
      "Study pre-registration",
      "Registered reports",
      "Data sharing",
      "FAIR data availability practices",
      "Open notebooks",
      "Data organisation (e.g. file and folder structure)",
      "Metadata (e.g. a data dictionary)",
      "Appropriate data storage"
    )
    
    return(open_research)
  }
  
  if(topic == "ethics"){
    
    ethics <- c(
      "Informed consent and participant rights",
      "Ethical data storage and management",
      "Ethical principles (e.g. respect, competence, responsibility, and integrity"
    )
    
    return(ethics)
  }
}

# Prepare_for_ggplot ------------------------------------------------------

# prepare_for_ggplot <- function(data, vars, gg_labels) {
#   
#   # Extract the number of columns in the topic
#   number_of_columns <- data %>% 
#     select(vars) %>% 
#     ncol() %>% 
#     as.numeric()
#   
#   # Pivot longer to prepare for ggplot
#   data <- data %>%
#     select(university_id, discipline, vars) %>% 
#     pivot_longer(cols = vars, names_to = "column_name", values_to = "response") %>% 
#     drop_na(response) %>% 
#     mutate(
#       column_name = factor(column_name),
#       question_num = as.numeric(str_split_i(column_name, "_", -1)),
#       question_text = factor(question_num, levels = c(1:number_of_columns), labels = gg_labels)) %>% 
#     filter(response != "Don't Know")
#   
# }


# Prepare effects for ggplot ----------------------------------------------

# add_question_text_column <- function(data, vars, gg_labels) {
#   
#   number_of_columns <- survey_data %>% 
#     select(vars) %>% 
#     ncol %>% 
#     as.numeric()
#   
#   data <- data %>%
#     mutate(question_text = factor(
#       question_num, 
#       levels = c(1:number_of_columns), 
#       labels = gg_labels))
#   
# }
