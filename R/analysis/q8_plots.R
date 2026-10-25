# q8 ggplot
# Script to run one bar plot per topic, with CrIs for each question
# Note: this is a roundabout way of generating plots, but it works

# Set-up ------------------------------------------------------------------
library(janitor) # for clean_names()
library(ggtext)
library(pbapply)
library(here)
library(tidyverse)

source(here("R/functions", "q8_plot_functions.R"))

# Load data ---------------------------------------------------------------
survey_data <- read_rds(here("data/clean_data", "clean_survey_data.rds"))

# Load lists -------------------------------------------------------------
# sources models.R which creates all the lists
source(here("R/analysis/q8_models.R"))

# ggplot settings ---------------------------------------------------------

plot_colour <- "skyblue4"

plot_theme <- theme_bw() +
  theme(
    plot.title = element_text(size = 18),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size= 10),
    axis.title.y = element_blank(),
    axis.text.y = element_markdown(size = 12, hjust = 1),
    axis.ticks.y = element_blank(),
    strip.text = element_text(size = 14),
    strip.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none")

# Prepare secondary data sets for ggplot ----------------------------------------

conditional_effects_data <- reduce(conditional_effects_list, full_join)
probabilities_data <- reduce(probability_estimates_list, full_join)
credible_intervals_95_data <- reduce(credible_intervals_95_list, full_join)

# ggplot with credible intervals ------------------------------------------------------------------

list_of_topics <-
  c("study_design", "interpreting_results", "data_handling", "stats_concepts",
    "stats_tests", "QRP", "open_research", "ethics")

ggplot_list <- lapply(list_of_topics, function(topic) {
  
  column_names <- survey_data %>%
    select(all_of(starts_with(str_glue("q8_{topic}")))) %>%
    names()
  
  number_of_columns <- column_names %>% 
    length() %>% 
    as.numeric()
  
  new_labels <- set_labels(topic)
  
  # Add question text and remove Don't Know responses
  survey_data <- survey_data %>%
    select(university_id, discipline, column_names) %>% 
    pivot_longer(cols = column_names, names_to = "column_name", values_to = "response") %>% 
    drop_na(response) %>% 
    mutate(
      column_name = factor(column_name),
      question_num = as.numeric(str_split_i(column_name, "_", -1)),
      question_text = factor(question_num, levels = c(1:number_of_columns), labels = new_labels)) %>% 
    filter(response != "Don't Know")
  
  # Set factor levels for the response column, so that Yes is blue + No is white
  survey_data <- survey_data %>% 
    mutate(response = factor(response, levels = c("No", "Yes")))
  
  credible_intervals_95_data <- credible_intervals_95_data %>% 
    filter(column_name %in% column_names) %>% 
    mutate(question_text = factor(
      question_num, 
      levels = c(1:number_of_columns), 
      labels = new_labels))
  
  probabilities_data <- probabilities_data %>%
    filter(column_name %in% column_names) %>%
    mutate(question_text = factor(
      question_num,
      levels = c(1:number_of_columns),
      labels = new_labels))
  
  # ggplot
  ggplot_with_credible_intervals <- ggplot() +
    geom_bar(
      data = survey_data,
      aes(x = fct_rev(question_text), fill = response),
      position = "fill", 
      color = "black") +
    geom_errorbar(
      data = credible_intervals_95_data,
      aes(x = question_text, ymin = lower, ymax = upper),
      width = .4,
      linewidth = .8) +
    
    geom_point(
      data = probabilities_data,
      aes(x = question_text, y = probability),
      size = 2) +
    coord_flip() +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(
      "%", 
      breaks = c(0, 0.25, 0.5, 0.75, 1), 
      labels = c(0, 25, 50, 75, 100)) +
    facet_wrap(
      ~ factor(
        discipline,
        labels = c("Biological Sciences", "Biomedical Sciences", "Medicine", "Psychology")),
      nrow = 1) +
    scale_fill_manual(values = c("white", plot_colour)) +
    plot_theme
  
  # # Prepping sample size data
  # sample_size_data <- sample_size_data %>% 
  #   filter(column_name %in% column_names) %>%
  #   mutate(question_text = factor(
  #     question_num,
  #     levels = c(1:number_of_columns),
  #     labels = new_labels)) %>% 
  #   filter(question != "Don't Know")
  
  # Add sample size ----
  # Prepare data for ggplot
  # sample_size_data <- sample_size_data %>% 
  #   summarise(
  #     discipline = discipline,
  #     
  #   )
  # 
  sample_size <- survey_data %>%
    group_by(discipline, question_text) %>%
    summarise(total_yes_no = length(response)) %>%
    ungroup()
  
  # Add sample sizes for context
  ggplot_with_sample_size <-
    ggplot_with_credible_intervals +
    geom_text(data = sample_size,
              aes(x = fct_rev(question_text), y = 1.1, label = total_yes_no))
  
  
  ggsave(
    plot = ggplot_with_sample_size, 
    here("figures", str_glue("{topic}_plot.jpg")), 
    device = "jpeg", 
    width = 1280, 
    height = 637, 
    units = "px", 
    dpi = 100)
  
  
  
})

# Set names in list
names(ggplot_list) <- list_of_topics
