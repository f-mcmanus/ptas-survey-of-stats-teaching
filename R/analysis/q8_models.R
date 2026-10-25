# q8 models
# Script to run all 70 models in question 8 and store the model objects

# Set-up ------------------------------------------------------------------
library(janitor) # for clean_names()
library(broom.mixed) # for tidy()
library(ggrepel) # for geom_label_repel
library(posterior) # for quantile2()
library(pbapply)
library(here)
library(brms)
library(tidyverse)

set.seed(1234)

# Directory to store models and model lists
folder_name <- "store"

if (!dir.exists(here(folder_name))) {
  
  dir.create(here(folder_name))
  
}

# For running prior checks and the Bernoulli model
source(here("R/functions", "q8_model_functions.R"))

# Load data ---------------------------------------------------------------
survey_data <- read_rds(here("data/clean_data", "clean_survey_data.rds"))

# Define topics and column names -----------------------------------------------------------
list_of_topics <- 
  c("study_design", "interpreting_results", "data_handling", "stats_concepts",
    "stats_tests", "QRP", "open_research", "ethics")

column_names <- survey_data %>% 
  select(all_of(starts_with("q8"))) %>% 
  names()

# Model settings ----------------------------------------------------------
brm_formula_q8 <- brmsformula(is_this_taught ~ discipline,
                              family = bernoulli())

priors_q8 <- c(
  prior(cauchy(0, 10), class = "Intercept"),
  prior(cauchy(0, 2.5), class = "b")
)

# Explore data ------------------------------------------------------------
response_by_discipline_tables <- lapply(column_names, function(is_x_taught) {
  
  survey_data %>% 
    rename(question = is_x_taught) %>% 
    select(discipline, question) %>% 
    group_by(discipline) %>% 
    count(question) %>% 
    pivot_wider(names_from = question, values_from = n)
  
})

names(response_by_discipline_tables) <- column_names

# Prior predictive checks -------------------------------------------------

# Default priors
default_priors_list <- pblapply(list_of_topics, function(topic) {
  
  survey_data <- prepare_for_model(survey_data, str_glue("q8_{topic}_1"))
  default_prior <- get_prior(brm_formula_q8, data = survey_data)
  
})

names(default_priors_list) <- list_of_topics

# Prior contrasts
prior_contrasts_list <- pblapply(list_of_topics, function(topic) {
  
  data <- prepare_for_model(survey_data, str_glue("q8_{topic}_1"))
  
  # Check contrasts for the new priors
  contrasts(survey_data$discipline) = "contr.sum"
  contrasts(survey_data$discipline)
  
})

names(prior_contrasts_list) <- list_of_topics

# Prior predictive check
prior_pc_list <- pblapply(list_of_topics, function(topic) {
  
  data <- prepare_for_model(survey_data, str_glue("q8_{topic}_1"))
  
  prior_predictive_check <- brm(brm_formula_q8,
                                data = data,
                                prior = priors_q8,
                                sample_prior = "only",
                                iter = 24000,
                                control = list(adapt_delta = 0.999, 
                                               stepsize = 0.001,
                                               max_treedepth = 20),
                                file = here(
                                  folder_name,
                                  str_glue("pc_{topic}")))
  
  
})

names(prior_pc_list) <- list_of_topics

# Prior conditional effects
prior_effects_list <- pblapply(list_of_topics, function(topic) {
  
  prior_pc <- prior_pc_list[[topic]]
  
  prior_effects <- conditional_effects(prior_pc, effects = "discipline")
  
})

names(prior_effects_list) <- list_of_topics

# Run models --------------------------------------------------------------

model_list <- pblapply(column_names, function(is_x_taught) {
  
  survey_data <- prepare_for_model(survey_data, is_x_taught)
  
  contrasts(survey_data$discipline) = "contr.sum"
  
  # New code to run some models at higher iterations - see closed issue for more detail
  models_with_iteration_20000 <- c("q8_study_design_11", 
                                     "q8_stats_tests_1")
  
  models_with_iteration_60000 <- c("q8_stats_concepts_2",
                                   "q8_interpreting_results_1")
  
  
  if (is_x_taught %in% models_with_iteration_20000) {
    num_iter = 20000
    
  } else if  (is_x_taught %in% models_with_iteration_60000) {
    num_iter = 60000
    
  } else {
    num_iter = 6000
  }
  
  q8_model <- brm(brm_formula_q8,
                  data = survey_data,
                  prior = priors_q8,
                  iter = num_iter, 
                  control = list(adapt_delta = 0.999, 
                                 stepsize = 0.001, 
                                 max_treedepth = 20),
                  file = here(folder_name, is_x_taught))
  
  q8_model
  
})

names(model_list) <- column_names


# Prior summary -----------------------------------------------------------

prior_summary_list <- pblapply(column_names, function(is_x_taught) {
  
  prior_summary(model_list[[is_x_taught]])
  
})

names(prior_summary_list) <- column_names

# Model summary -----------------------------------------------------------
model_summary_list <- pblapply(column_names, function(is_x_taught) {
  
  summary(model_list[[is_x_taught]])
  
})

names(model_summary_list) <- column_names

# Probability estimates ---------------------------------------------------
probability_estimates_list <- pblapply(column_names, function(is_x_taught) {
  
  draws_df <- as_draws_df(model_list[[is_x_taught]]) %>% 
    reframe(
      # Psychology is the intercept, set by contrasts.sum
      # discipline1-3 refer to Bio, Biomed, and Medicine
      Psych = mean(b_Intercept - (b_discipline1 + b_discipline2 + b_discipline3)),
      Bio = mean(b_Intercept + b_discipline1),
      Biomed = mean(b_Intercept + b_discipline2),
      Med = mean(b_Intercept + b_discipline3)) %>% 
    pivot_longer(
      data = ., 
      cols = everything(), 
      names_to = "discipline", 
      values_to = "logodds") %>% 
    mutate(
      probability = plogis(logodds),
      column_name = is_x_taught,
      question_num = as.numeric(str_split_i(is_x_taught, "_", -1)))
  
})

names(probability_estimates_list) <- column_names


# credible intervals ------------------------------------------------------

credible_intervals_95_list <- pblapply(column_names, function(is_x_taught) {
  
  draws_df <- as_draws_df(model_list[[is_x_taught]]) %>% 
    reframe(
      Psych = quantile2(b_Intercept - (b_discipline1 + b_discipline2 + b_discipline3), probs = c(0.05, 0.95)),
      Bio = quantile2(b_Intercept + b_discipline1, probs = c(0.05, 0.95)),
      Biomed = quantile2(b_Intercept + b_discipline2, probs = c(0.05, 0.95)),
      Med = quantile2(b_Intercept + b_discipline3, probs = c(0.05, 0.95))) %>% 
    rownames_to_column("credible_interval") %>%
    mutate(credible_interval = case_when(
      credible_interval == "1" ~ "lower",
      credible_interval == "2" ~ "upper")) %>%
    pivot_longer(
      data = .,
      cols = c(Psych, Bio, Biomed, Med),
      names_to = "discipline",
      values_to = "CrI") %>%
    pivot_wider(
      data = .,
      names_from = credible_interval,
      values_from = CrI) %>%
    mutate(
      lower = plogis(lower),
      upper = plogis(upper),
      column_name = is_x_taught,
      question_num = as.numeric(str_split_i(is_x_taught, "_", -1)))
  
})

names(credible_intervals_95_list) <- column_names


# Model plot --------------------------------------------------------------
model_plot_list <- pblapply(column_names, function(is_x_taught) {
  
  plot(model_list[[is_x_taught]])
  
})

names(model_plot_list) <- column_names

# Post-predictive check ---------------------------------------------------
pp_check_list <- pblapply(column_names, function(is_x_taught) {
  
  pp_check(model_list[[is_x_taught]], type = "bars", ndraws = 100)
  
})

names(pp_check_list) <- column_names

# Conditional effects -----------------------------------------------------
conditional_effects_list <- pblapply(column_names, function(is_x_taught) {
  
  conditional_effects_df <- conditional_effects(model_list[[is_x_taught]], effects = "discipline")[[1]]
  
  # Prepare for ggplot
  conditional_effects_df <- conditional_effects_df %>%
    janitor::clean_names() %>%
    mutate(
      column_name = is_x_taught,
      question_num = as.numeric(str_split_i(is_x_taught, "_", -1)))
})

names(conditional_effects_list) <- column_names


# Sensitivity analysis ----------------------------------------------------
# Determines the extent to which the prior affects the results
sensitivity_analysis_list <- pblapply(column_names, function(is_x_taught) {
  
  model_fixed <- 
    broom.mixed::tidy(model_list[[is_x_taught]], 
                      effects = "fixed", 
                      conf.level = 0.95, 
                      fix.intercept = FALSE) %>%
    mutate(
      ci_width = abs(conf.low - conf.high)
    )
  
})

names(sensitivity_analysis_list) <- column_names

# Sensitivity plot --------------------------------------------------------
sensitivity_plot_list <- pblapply(column_names, function(is_x_taught) {
  
  sensitivity_plot <- 
    sensitivity_analysis_list[[is_x_taught]] %>%
    mutate(
      theta = c(0, 0, 0, 0),
      sigma_prior = c(10, 2.5, 2.5, 2.5),
      # std.error is the standard deviation
      z = abs((estimate - theta) / std.error),
      s = 1 - (std.error^2 / sigma_prior^2)
    ) %>%
    ggplot(aes(s, z, label = term)) +
    geom_point() +
    geom_label_repel(arrow = arrow()) +
    xlim(0, 1) + ylim(0, 10)
  
})

names(sensitivity_plot_list) <- column_names

# Save lists -------------------------------------------------------------------

saveRDS(default_priors_list, file = here(folder_name, "default_priors_list.rds"))
saveRDS(prior_contrasts_list, file = here(folder_name, "prior_contrasts_list.rds"))
saveRDS(prior_pc_list, file = here(folder_name, "prior_pc_list.rds"))
saveRDS(prior_effects_list, file = here(folder_name, "prior_effects_list.rds"))
saveRDS(prior_summary_list, file = here(folder_name, "prior_summary_list.rds"))
saveRDS(model_summary_list, file = here(folder_name, "model_summary_list.rds"))
saveRDS(probability_estimates_list, file = here(folder_name, "probability_estimates_list.rds"))
saveRDS(credible_intervals_95_list, file = here(folder_name, "credible_intervals_95_list.rds"))
saveRDS(model_plot_list, file = here(folder_name, "model_plot_list.rds"))
saveRDS(pp_check_list, file = here(folder_name, "pp_check_list.rds"))
saveRDS(conditional_effects_list, file = here(folder_name, "conditional_effects_list.rds"))        
saveRDS(sensitivity_analysis_list, file = here(folder_name, "sensitivity_analysis_list.rds"))     
saveRDS(sensitivity_plot_list, file = here(folder_name, "sensitivity_plot_list.rds"))
saveRDS(response_by_discipline_tables, file = here(folder_name, "response_by_discipline_tables.rds"))
