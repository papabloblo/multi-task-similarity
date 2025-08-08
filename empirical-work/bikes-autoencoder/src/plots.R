library(tidyverse)

ales <- read_csv('empirical-work/bikes-autoencoder/data/ales.csv')

p <- ales %>%
  mutate(
    feature = paste0('Latent feature ', feature+1)
    ) %>% 
  # filter(feature == 1) %>% 
  ggplot(aes(x = ale_x, y = ale_y, group = task)) +
  geom_line(alpha = .1) +
  labs(x = '', y = '') +
  facet_grid(~feature, scales = "free_x") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 6),
    strip.background = element_rect(fill = "gray90", color = NA),
    legend.position = "none"
  )   


ggsave(
  filename = "empirical-work/bikes-autoencoder/plots/bikes-autoencoder-ales.png", 
  plot = p,
  width = 20,
  height = 7,
  dpi = "print",
  units = "cm"
)
