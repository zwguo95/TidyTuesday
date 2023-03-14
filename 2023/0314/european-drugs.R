library(showtext)
library(lubridate)
library(tidyverse)

font_add_google("Roboto")
font_add_google("Oswald", regular.wt = 400)
font_add_google("Lora")
showtext_auto()


# get logo
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

magnifier <- get_png("magnifier.png")

# read drug dataset
drugs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')

drugs <- drugs %>%
  mutate(prep_time = as.numeric(marketing_authorisation_date - date_of_opinion))

# covid drugs
covid <- drugs %>%
  filter(prep_time > 0 & authorisation_status == "authorised") %>%
  filter(year(date_of_opinion) >= 2020) %>%
  mutate(covid = ifelse(grepl("COVID", therapeutic_area) == T, 1, 0)) %>%
  arrange(prep_time)

# covid drug dataframe within maginifier
covid_drugs <- data.frame(prep_time = covid$prep_time[1:8])
covid_drugs$prep_time_recoded <- c(rep(-8, 6), -3, 43)  

ggplot() +
  geom_point(data=covid[1:8,],
             aes(y = 0, x = prep_time), 
             size = 1,
             alpha = 0.5, 
             color =  "#fff179") +
  geom_point(data = covid[9:282,],
             aes(y = 0, x = prep_time),
             color = "grey80",
             size = 1) +
  geom_line(aes(y = 0, x = 0:8),
            color =  "#fff179",
            size = 1) +
  geom_line(aes(y = 0, x = 9:419),
            color =  "grey80",
            size = 1) + 
  geom_curve(data = covid[1:8,],
             aes(x = 0, xend = prep_time, y = 0, yend = 0),
             curvature = -0.5,
             size = 1.5, 
             color = "#fff179",
             alpha = 0.5) + 
  geom_curve(data = covid[9:282,], aes(x = 0, xend = prep_time, y = 0, yend = 0),
             curvature = -0.5,
             size = 1.5,
             color = "grey50",
             alpha = 0.5) + 
  annotation_custom(magnifier, xmin = -50, xmax = 99, ymin= -0.001, ymax = -0.03) +
  geom_curve(data = covid_drugs, aes(x = -13, xend = prep_time_recoded, y = -0.013, yend = -0.013),
             curvature = -0.6, size = 1.5, color =  "#fff179",
             alpha = 0.5) +
  geom_line(aes(y = -0.013, x = -13:43), color = "#fff179") +
  geom_point(aes(x = 43, y = -0.013), size = 1, color = "#fff179") +
  geom_point(aes(x = -2, y = -0.013), size = 1, color = "#fff179") +
  annotate(geom = "text", x = 39, y = -0.0145, color = "#fff179",
          label = "8 days", size = 10) +
  annotate(geom = "text", x = -1, y = -0.0145, color = "#fff179",
           label = "2 days", size = 10) +
  
  annotate(geom = "text", x = 200, y = 0.05, 
           label = "EUROPEAN DRUG DEVELOPMENT", 
           hjust = "center", family = "Oswald", size = 40, color = "white") +
  annotate(geom = "text", x = 200, y = 0.043, 
           label = "Market Entry Timeframe Following Approval During the COVID-19 Pandemic", 
           hjust = "center", family = "Roboto", size = 25, color = "white") +
  
  annotate(geom = "text", x = 92, y = -0.0127,
           label = str_wrap("COVID drugs entered the market within 8 days after approval", width = 25),
           size = 15,
           family = "Lora",
           lineheight = 0.3,
           color ="white") +

  annotate(geom = "text", x = 350, y = -0.0127,
           label = str_wrap("Non-COVID drugs can take up to 419 days to enter the market after approval", width = 25),
           size = 15,
           family = "Lora",
           lineheight = 0.3,
           color ="white") +
  annotate(geom = "curve", x = 395, xend = 419, y = -0.01, yend = -0.001,
           linewidth = 1, 
           curvature = 0.25, arrow = arrow(length = unit(1.25, "mm")), color = "grey80") +
  annotate(geom = "text", x = 350, y = -0.04,
           label = "Data source: European Medicines Agency | Visualization by Zhaowen Guo", 
           size = 12,
           color = "grey80",
           family = "Roboto") +
  labs(y = "", x = "") +
  theme_minimal(base_family = "Lora") +
  theme(plot.background = element_rect(fill = "black", color = "black"),
        panel.grid = element_blank(),
        axis.text = element_blank())

library(ggview)
ggview(width = 14, height = 14/1.618, units = "in")
ggsave("european-drugs.png", width = 14, height = 14/1.618)
