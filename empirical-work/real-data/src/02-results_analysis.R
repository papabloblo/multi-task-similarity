#' Analysis of the results of the similarity measure applied
#' to the Parkinson dataset.
#' 


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(patchwork)


# DATA IMPORT -------------------------------------------------------------

similarity <- readRDS("empirical-work/real-data/data/similarity/similarity_task_summary.RDS")
ale <- readRDS("empirical-work/real-data/data/ale_by_task_var.RDS")
models <- readRDS("empirical-work/real-data/data/models.RDS")

similarity_var <- readRDS("empirical-work/real-data/data/similarity/similarity_task_var_summary.RDS")


# SIMILARITY MATRIX PLOT --------------------------------------------------

p <- similarity %>% 
  mutate(
    task1 = as.numeric(task1),
    task2 = as.numeric(task2)
    ) %>% 
  ggplot(
    aes(x = task1, 
        y = task2, 
        fill = similarity
        )
    ) + 
  geom_raster() + 
  theme_void() + 
  theme(
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20, face = "bold"),
    legend.key.size = unit(1.5, "cm"),
    legend.text = element_text(size = 20)
    ) +
  labs(
    fill = "Multi-task\n similarity",
    ) +
  scale_x_continuous(
    breaks = c(1, seq(5, 40, by = 5), 42),
    expand = expansion(0),
    position = "top"
    ) +
  scale_y_reverse(
    breaks = c(1, seq(5, 40, by = 5), 42),
    expand = expansion(0)
    ) + 
  scale_fill_viridis_c(
    option = "A", 
    # colors ordered from lightest to darkest
    direction = -1
    )


ggsave(p, 
       file = "empirical-work/real-data/plots/matrix_similarity.png", 
       dpi = "print",
       width = 25,
       height = 20,
       units = "cm")

#' Patient 15 exhibits high values of multi-task similarity with 
#' the vast majority of the other patients.

similarity %>% 
  filter(task1 %in% c("14", "15")) %>% 
  View()


#' IDEA 1: table with the five lowest values and the five highest 
#' of patient 15 and 14 (for example) with the others
#'  DONE  manually in a latex table
#'  

#' IDEA 2: show the ALE plots for the three most important features
#' for patient 15 with respect to the five most similar patients 
#' and with respect the five least similar patients
#' 


# Most similar using the most important feature


plot_ale_similar <- function(task_orig, 
                             ale, 
                             models,
                             similarity, 
                             n_tasks = 5,
                             n_feat = 3,
                             most = TRUE, 
                             ribbon = 1,
                             limits_y = NULL,
                             n.breaks_x = 4){
  most_imp_var <-
    sort(
      models[[task_orig]]$importance, 
      decreasing = TRUE
    )[1:n_feat]
  
  aux <- similarity[similarity$task1 == task_orig,]
  most_sim_tasks <- aux$task2[order(aux$similarity, decreasing = !most)[1:n_tasks]]
  
  df <- ale %>% 
    filter(
      task %in% c(task_orig, most_sim_tasks),
      feature %in% names(most_imp_var)) %>% 
    group_by(task, feature) %>% 
    mutate(
      highlight = ifelse(task == task_orig, "1", "0"),
      task = factor(task, levels = c(most_sim_tasks, task_orig), ordered = TRUE),
      # task = paste("Task", task),
      n2 = n/max(n),
      ymax = ale +  ribbon*n2,
      ymin = ale -  ribbon*n2,
      
      feature = paste0(feature, " (Importance: ",round(most_imp_var[feature], 2)*100, "%)"),
      feature = factor(feature, 
                       levels = paste0(names(most_imp_var), " (Importance: ",round(most_imp_var, 2)*100, "%)"),
                       ordered = TRUE), 
      ) %>% 
    ungroup()
  
  p <- df %>% 
    filter(task != task_orig) %>% 
    ggplot( aes(y = ale, x = x, group = task, 
               color = highlight, fill = highlight,
               ymax = ymax, ymin = ymin
               )
            ) +
    geom_ribbon(alpha = 0.7) +
    geom_line() +  
    
    geom_ribbon(data = filter(df, task == task_orig),
                alpha = 0.7) +
    geom_line(data = filter(df, task == task_orig)) +  
    labs(y = "",
         x = ""
    ) +
    theme_minimal() +
    scale_y_continuous(limits = limits_y, n.breaks = n.breaks_x) + 
    scale_x_continuous(n.breaks = n.breaks_x) +
    scale_color_manual(values = c("1" = "gray25", "0" = "gray80")) + 
    scale_fill_manual(values = c("1" = "gray25", "0" = "gray80")) + 
    theme(
      strip.text = element_text(size = 6),
      strip.background = element_rect(fill = "gray90", color = NA),
      legend.position = "none"
    ) +
    facet_wrap(. ~ feature, ncol = n_tasks, scales = "free_x")
  
  if (most){
    p + labs(title = paste("Most similar tasks to task", task_orig))
  } else {
    p + labs(title = paste("Least similar tasks to task", task_orig))
  }
}


limits_y <- c(-6, 5)
n.breaks_x <- 4

p1 <- plot_ale_similar("14", 
                       ale, 
                       models,
                       similarity,
                       n_tasks = 5,
                       n_feat = 2,
                       most = TRUE,
                       ribbon = .4,
                       limits_y = limits_y,
                       n.breaks_x = n.breaks_x
) 

p2 <- plot_ale_similar("14", 
                       ale, 
                       models,
                       similarity,
                       n_tasks = 5,
                       n_feat = 2,
                       most = FALSE,
                       ribbon = .4,
                       limits_y = limits_y,
                       n.breaks_x = n.breaks_x
) 
p3 <- plot_ale_similar("15", 
                 ale, 
                 models,
                 similarity,
                 n_tasks = 5,
                 n_feat = 2,
                 most = TRUE,
                 ribbon = .4,
                 limits_y = limits_y,
                 n.breaks_x = n.breaks_x
                 ) 

p4 <- plot_ale_similar("15", 
                 ale, 
                 models,
                 similarity,
                 n_tasks = 5,
                 n_feat = 2,
                 most = FALSE,
                 ribbon = .4,
                 limits_y = limits_y,
                 n.breaks_x = n.breaks_x
                 ) 




p <- p1 + p2 + p3 + p4 

ggsave(
  filename = "empirical-work/real-data/plots/most-least-sim-tasks-14-15.png", 
  plot = p,
  width = 25,
  height = 20,
  dpi = "print",
  units = "cm"
  )
