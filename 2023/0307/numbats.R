library(MetBrewer)
library(ozmaps)
library(cowplot)
library(showtext)
library(tidyverse)

font_add_google("Pragati Narrow")
showtext_auto()

numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')


# spatial distribution of numbats
australia <- ozmap_data("states", border = "grey80")
numbats_map <- numbats %>%
  mutate(month_num = lubridate::month(day),
         year_num = lubridate::year(day)) %>%
  filter(year_num >= 2010 & year_num <= 2022) %>%
  select(decimalLatitude, decimalLongitude)

p1 <- ggplot(australia) + 
  geom_sf(fill = "#f5f5f2", color = "grey30") +
  geom_point(data = numbats_map, 
             aes(x=decimalLongitude, y=decimalLatitude),
             color="#574571", size=1.5, alpha=0.7) + 
  labs(x="", y="") +
  theme_minimal(base_family = "Pragati Narrow") + 
  theme(plot.background = element_rect(color = NA,
                                       fill = NA),
        panel.background = element_rect(color = NA,
                                        fill = NA),
        plot.margin = margin(t=40),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank())

# temporal variation of numbats
numbats.2010s <- numbats %>%
  mutate(month_num = lubridate::month(day),
         year_num = lubridate::year(day)) %>%
  filter(year_num >= 2010 & year_num <= 2022) %>%
  group_by(year_num, month_num, month) %>%
  summarise(n = n()) %>%
  arrange(month_num, .by_group = T)
  ungroup()

grid <- data.frame(year_num = rep(2010:2022, each = 12),
                   month_num = rep(1:12, 13),
                   month = rep(month.abb, 13))
numbats.df <- numbats.2010s %>%
  full_join(grid) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  group_by(year_num, month_num) %>%
  arrange(month_num, .by_group = T) %>%
  ungroup()

numbats.df$month <- factor(numbats.df$month, levels = month.abb)

p2 <- ggplot(numbats.df, aes(x = year_num, y = month)) + 
  geom_tile(aes(fill = n), color = "grey40") + 
  scale_y_discrete(limits = rev(month.abb)) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018, 2020, 2022)) +
  scale_fill_gradient(low = "#f5f5f2", high = "#2d223c",
                      name = "Number of Sightings") + 
  labs(x="",y="") +
  theme_minimal(base_family = "Pragati Narrow") + 
  theme(plot.background = element_rect(color = NA, fill = NA),
        panel.background = element_rect(color = NA, fill = NA),
        plot.margin = margin(t=40),
        legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, margin = margin(b=0)),
        panel.grid = element_blank(),
        text = element_text(size = 40))


(p2 | p1) + 
  patchwork::plot_annotation(
    title = "When and where were numbat sightings recorded in Australia?",
    caption = str_wrap("Data source: Atlas of Living Australia | Visualization by Zhaowen Guo")) &
  theme(plot.background = element_rect(fill = "#f5f5f2"),
        panel.background = element_rect(fill = "#f5f5f2"),
        text = element_text(family = "Pragati Narrow"),
        plot.title = element_text(size = 70,
                                  hjust = 0.5,
                                  face = "bold",
                                  margin = margin(t=20)),
        plot.caption = element_text(size = 35,
                                    margin = margin(b=10)))


library(ggview)
ggview(width = 13, height = 10, units = "in")
ggsave("numbat.png", width = 13, height = 10)
