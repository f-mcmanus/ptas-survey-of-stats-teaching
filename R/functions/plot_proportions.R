## plot_proportions

#by: Ziska
#purpose: plots proportions of responses for each level across disciplines

plot_proportions <- function(data, var, new_levels = old_levels) {
  
  old_levels <- data %>% 
    pull( {{ var }} ) %>% 
    levels()

  
  #set theme settings (can be overridden with `+ theme()`)
  # plot_theme <- theme(
  #   plot.title = element_text(size = 18),
  #   axis.title.x = element_blank(),
  #   axis.text.x = element_text(size= 12),
  #   axis.title.y = element_text(size = 14),
  #   axis.text.y = element_text(size = 10),
  #   strip.text = element_text(size = 14))
  
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
  
  #breaks
  breaks_x <- c(0, 0.25, 0.5, 0.75, 1)
  breaks_x_perc <- c(0, 25, 50, 75, 100)
  
  
  #create a table of proportions
  props_table <- data %>%
    select(discipline, {{ var }}) %>%
    table() %>%
    prop.table(margin = 1) %>%
    data.frame()
  
  print(props_table)
  
  #plot proportions
  plot <- ggplot() +
    geom_col(
      data = props_table,
      aes(x = factor({{var}}, labels = new_levels), y = Freq), 
      position = "dodge",
      fill = plot_colour,
      color = "black"
    ) +
    facet_wrap( ~ factor(discipline, labels = c("Biological Sciences", "Biomedical Sciences", "Medicine", "Psychology")), nrow = 1) +
    scale_x_discrete(labels = label_wrap(10), drop = FALSE) +
    #scale_y_continuous(limits = c(0, 1)) +
    scale_y_continuous("%", breaks = breaks_x, labels = breaks_x_perc, limits = c(0,1)) +
   # labs(y = "Proportion") +
    theme_bw() +
    plot_theme
  
  plot
}






