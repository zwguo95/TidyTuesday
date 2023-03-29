library(lubridate)
library(tidyverse)
library(waffle)
library(showtext)
library(cowplot)

font_add_google("Playfair Display")
font_add_google("Handlee")
showtext_auto()

# load time zone transition data
transitions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/transitions.csv')
transitions <- transitions %>%
  mutate(begin = as_datetime(begin),
         begin_year = year(begin),
         end = as_datetime(end),
         end_year = year(end))

# whether has daylight saving for north countries
dls <- transitions %>%
  filter(begin_year == 2021, # for the first two months
         dst == TRUE) %>%
  mutate(begin_month = month(begin),
         end_month = month(end)) %>%
  select(zone, begin_month, end_month) %>%
  mutate(end_month = ifelse(begin_month >= 9, end_month + 12, end_month))

length(unique(dls$zone)) # 210 time zones
table(dls$begin_month)
table(dls$end_month)

time_zones <- rep(NA, 12)
for (i in 1:12) {
  time_zones[i] <- dls %>%
    rowwise() %>%
    filter(begin_month <= i, end_month > i) %>%
    nrow()
}

data <- data.frame(months = 1:12,
                   percentage = as.integer((time_zones/210) * 100),
                   non_percentage = 100 - as.integer((time_zones/210) * 100)) %>%
  pivot_longer(cols = 2:3) %>%
  mutate(color = ifelse(name == "percentage", 1, 0))

months <- as_labeller(c(`1` = "Jan",
                        `2` = "Feb",
                        `3` = "Mar",
                        `4` = "Apr",
                        `5` = "May",
                        `6` = "Jun",
                        `7` = "Jul",
                        `8` = "Aug",
                        `9` = "Sep",
                        `10` = "Oct",
                        `11` = "Nov",
                        `12` = "Dec"))

p <- ggplot(data,
       aes(fill = name,
           values = value))  +
  facet_wrap(vars(months), labeller = months) +
  coord_fixed() +
  geom_waffle(flip = T, 
              nrows = 10,
              height = 0.9, width = 0.9,
              color = "white") + 
  scale_fill_manual(values = c("grey80", "#574571")) +
  labs(title = "TRANSITIONS IN DAYLIGHT SAVING TIME",
       subtitle = "Percentages of time zones that observed Daylight Saving Time (DST) in 2021",
       caption = str_wrap("Data source: IANA tz database | Visualization by Zhaowen Guo", width = 300)) + 
  theme_void(base_family = "Playfair Display") + 
  theme(legend.position = "none") +
  theme(
    plot.background = element_rect(fill = "#FFFFF9", color = NA),
    text = element_text(size = 45, hjust = 0.5, color = "grey40"),
    plot.title = element_text(size = 70, hjust = 0.2, color = "grey10",
                              margin = margin(t=10, b=10)),
    plot.subtitle = element_text(size = 50, hjust = -0.9, color = "grey10",
                                 margin = margin(b = 20)),
    plot.caption = element_text(size = 30, color = "grey40", 
                                margin = margin(t=10, b=10)),
    plot.margin = margin(l=125, r=20)) 

ggdraw(p) + 
  draw_label(label = "88% of time zones \n were observing DST \n in September", y = 0.16, x = 0.16,
             size = 35, color = "grey40", lineheight = 0.3, hjust = 0,
             fontfamily = "Handlee")

library(ggview)
ggview(width = 14, height = 14/1.618, units = "in")
ggsave("daylight.png", width = 14, height = 14/1.618)
