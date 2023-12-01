#'
#'
#' EXPLORATORY DATA ANALYSIS FOR ALE CURVES
#' 
#' 


# DEPENDENCIES ------------------------------------------------------------

suppressMessages(library(R.utils))
suppressMessages(library(tidyverse))
suppressMessages(library(patchwork))

cmd <- cmdArgs()

# READING DATA ------------------------------------------------------------

ale_t_x <- readRDS(file = cmd$ale_curves)


# PLOTS -------------------------------------------------------------------

df <- ale_t_x %>% 
  mutate(
    task = paste("Task", task),
    task2 = "1",
    n2 = n/1000,
    ymax = ale +  cmd$ribbon*n2,
    ymin = ale -  cmd$ribbon*n2) %>% 
  bind_rows(
    mutate(ale_t_x,
           task2 = task,
           task = "All tasks")
    )


plot_by_feature <- function(f){
  df %>% 
    filter(feature == f) %>% 
    ggplot(aes(y = ale, x = x)) +
    geom_ribbon(aes(group = task2, ymax = ymax, ymin = ymin), 
                alpha = 0.5
                ) +
    geom_line(aes(group = task2)) +
    scale_y_continuous(limits = c(cmd$ymin, cmd$ymax)) +
    facet_wrap(. ~ task + feature, ncol = 1) +
    labs(y = "",
         x = ""
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 6),
      panel.spacing.x = unit(0, "pt"),
      strip.background = element_rect(fill = "gray90", color = NA),
      plot.margin = margin(0, 0, 0, 0, "cm")
    )
}

p1 <- plot_by_feature("x1")
p2 <- plot_by_feature("x2")
p3 <- plot_by_feature("x3")
p4 <- plot_by_feature("x4")
p5 <- plot_by_feature("x5")

p <- p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 5)



# SAVING PLOTS ------------------------------------------------------------

ggsave(p, file = cmd$out_plot, 
       dpi = "print",
       width = cmd$width,
       height = cmd$height,
       units = "cm")
