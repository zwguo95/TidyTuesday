library(MetBrewer)
library(cowplot)
library(showtext)
library(tidyverse)

font_add_google("EB Garamond")
showtext_auto()

afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
language_scripts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv')
language_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')

data <- afrisenti %>%
  group_by(language_iso_code) %>%
  summarise(positive_prop = round(sum(label == "positive")/n(), digits = 2)) %>%
  left_join(languages)

img_b <- png::readPNG("cup.png") 
cup <- grid::rasterGrob(img_b, interpolate = T) 

data  %>% 
  mutate(language = fct_reorder(language, -positive_prop)) %>% 
  ggplot(aes(language, positive_prop)) +
  geom_col(width = 0.45, fill ="#e9851d") +
  annotation_custom(cup, ymin = -0.25, ymax = 1.15, xmin = -10.02, xmax = 12) +
  facet_wrap(~ language, nrow = 2, scales = "free_x", strip.position = "bottom") +
  scale_y_continuous(limits = c(0, 1.45), breaks = seq(0, 0.6, by = 0.2),
                     labels = c("0", "20%", "40%", "60%")) +
  theme_minimal(base_family = "EB Garamond") +
  theme(plot.caption = element_text(size = 5, color = "grey50"),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.title = element_text(size = 15, margin = margin(t = 10, b = 5, l = 0, r = 0), hjust = 0.5),
        plot.background = element_rect(fill="#fdf7ee", color="#fdf7ee"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b=0)),
        axis.ticks = element_blank(),
        strip.background  = element_rect(color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(vjust = 1, size = 7),
        strip.background.x = element_rect(fill = NA),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 6, margin = margin(r=0)),
        panel.background = ) +
  labs(x = "", y = "", title = "Positive Sentiments in African Languages", subtitle = "Proportion of positive tweets across 14 different African languages",
       caption = str_wrap("Data collected from the AfriSenti dataset | Visualization by Zhaowen Guo"), width = 300)
