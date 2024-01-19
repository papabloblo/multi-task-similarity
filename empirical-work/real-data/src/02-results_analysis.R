#' Analysis of the results of the similarity measure applied
#' to the Parkinson dataset.
#' 


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)



# DATA IMPORT -------------------------------------------------------------

similarity <- readRDS("empirical-work/real-data/data/similarity/similarity_task_summary.RDS")


# Similarity matrix plot

p <- similarity %>% 
  mutate(
    task1 = as.numeric(task1),
    task2 = as.numeric(task2)
    ) %>% 
  ggplot(aes(x = task1, y = task2, fill = similarity)) + 
  geom_raster() + 
  theme_void() + 
  theme(axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 20)
        ) +
  labs(
    fill = "Multi-task\n similarity",
    # x = "Patient 1",
    # y = "Patient 2",
    ) +
  scale_x_continuous(breaks = c(1, seq(5, 40, by = 5), 42),
                     expand = expansion(0),
                     position = "top") +
  # scale_y_continuous(breaks = seq(5, 40, by = 5),
  #                    expand = expansion(0)) +
  scale_y_reverse(breaks = c(1, seq(5, 40, by = 5), 42),
                  expand = expansion(0)) + 
  scale_fill_viridis_c(option = "A", 
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
  filter(task1 %in% c("15", "14")) %>% 
  View()


#' IDEA 1: table with the five lowest values and the five highest 
#' of patient 15 and 14 (for example) with the others
#'  DONE  manually in a latex table
#'  

#' IDEA 2: show the ALE plots for the three most important features
#' for patient 15 with respect to the five most similar patients 
#' and with respect the five least similar patients
#' 

ale <- readRDS("empirical-work/real-data/data/ale_by_task_var.RDS")
models <- readRDS("empirical-work/real-data/data/models.RDS")

most_imp_var <- names(sort(models[["15"]]$importance, decreasing = TRUE)[1:3])

sort(models[["15"]]$importance, decreasing = TRUE)[1:3]
sort(models[["14"]]$importance, decreasing = TRUE)[1:3]

ale %>% 
  filter(
    task %in% as.character(c(15, 9, 19, 29, 26, 24)),
    feature %in% most_imp_var) %>% 
  mutate(
    highlight = ifelse(task == "15", "1", "0")
  ) %>% 
  ggplot(aes(x = x, y = ale)) + 
  geom_line(aes(group = task, color = highlight)) + 
  facet_wrap(. ~ feature, ncol = 3, scales = "free_x")




ale %>% 
  filter(
    task %in% as.character(c(15, 39, 18, 22, 36, 28)),
    feature %in% most_imp_var) %>% 
  mutate(
    highlight = ifelse(task == "15", "1", "0")
  ) %>% 
  ggplot(aes(x = x, y = ale)) + 
  geom_line(aes(group = task, color = highlight)) + 
  facet_wrap(. ~ feature, ncol = 3, scales = "free_x")


ale %>% 
  filter(
    task %in% as.character(c(14, 37, 13, 1, 2, 25)),
    feature %in% most_imp_var) %>% 
  mutate(
    highlight = ifelse(task == "14", "1", "0")
  ) %>% 
  ggplot(aes(x = x, y = ale)) + 
  geom_line(aes(group = task, color = task)) + 
  facet_wrap(. ~ feature, ncol = 3, scales = "free_x")

ale %>% 
  filter(
    task %in% as.character(c(14, 29, 15, 35, 31, 24)),
    feature %in% most_imp_var) %>% 
  mutate(
    highlight = ifelse(task == "14", "1", "0")
  ) %>% 
  ggplot(aes(x = x, y = ale)) + 
  geom_line(aes(group = task, color = task)) + 
  facet_wrap(. ~ feature, ncol = 3, scales = "free_x")



# Most similar using the most important feature

plot_ale_similar <- function(task_orig, 
                            ale, 
                            models,
                            similarity, 
                            n_tasks = 5,
                            n_feat = 1,
                            most = TRUE){
  most_imp_var <- names(
    sort(
      models[[task]]$importance, 
      decreasing = TRUE
      )[1:n_feat]
    )
  
  
  aux <- similarity[similarity$task1 == task_orig,]
  most_sim_tasks <- aux$task2[order(aux$similarity, decreasing = !most)[1:n_tasks]]
  
  ale %>% 
    filter(
      task %in% c(task_orig, most_sim_tasks),
      feature %in% most_imp_var
      ) %>% 
    mutate(
      highlight = ifelse(task == task_orig, "1", "0"),
      feature = factor(feature, levels = most_imp_var, ordered = TRUE)
    ) %>% 
    ggplot(aes(x = x, y = ale)) + 
    geom_line(aes(group = task, color = highlight)) + 
    facet_wrap(. ~ feature, ncol = n_tasks, scales = "free_x") +
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


plot_ale_similar("15", 
                 ale, 
                 models,
                 similarity,
                 n_tasks = 3,
                 n_feat = 1,
                 most = TRUE)


plot_ale_similar("14", 
                 ale, 
                 models,
                 similarity,
                 n_tasks = 3,
                 n_feat = 1,
                 most = TRUE)


plot_ale_similar("14", 
                 ale, 
                 models,
                 similarity,
                 n_tasks = 3,
                 n_feat = 1,
                 most = FALSE)


plot_ale_similar("13", 
                 ale, 
                 models,
                 similarity,
                 n_tasks = 10,
                 n_feat = 1,
                 most = TRUE)


plot_ale_similar("13", 
                 ale, 
                 models,
                 similarity,
                 n_tasks = 10,
                 n_feat = 1,
                 most = FALSE)


ale %>% 
  filter(
    task %in% as.character(c(15, 9, 19, 29, 26, 24)),
    feature == "test_time") %>% 
  mutate(
    highlight = ifelse(task == "15", "1", "0")
  ) %>% 
  ggplot(aes(x = x, y = ale)) + 
  geom_line(aes(group = task, color = highlight))


ale %>% 
  filter(
    task %in% as.character(c(14, 37, 13, 1, 2, 25)),
    feature == "test_time") %>% 
  mutate(
    highlight = ifelse(task == "14", "1", "0")
  ) %>% 
  ggplot(aes(x = x, y = ale)) + 
  geom_line(aes(group = task, color = highlight)) + 
  facet_wrap(. ~ feature, ncol = 3, scales = "free_x")

