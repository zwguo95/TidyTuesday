library(showtext)
library(ggimage)
library(magick)
library(ggtext)
library(tidyverse)

font_add_google("VT323", "vt")
font_add_google("Roboto","Roboto", regular.wt=400)
showtext_auto()

languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

languages <- languages %>%
  mutate(year_range = case_when(
    appeared >= 1970 & appeared < 1980 ~ 1,
    appeared >= 1980 & appeared < 1990 ~ 2,
    appeared >= 1990 & appeared < 2000 ~ 3,
    appeared >= 2000 & appeared < 2010 ~ 4
  ))

images <- c("icons/java.png",
            "icons/python.png",
            "icons/clojure.png",
            "icons/sql.png")

# basic summary
table(languages$features_has_comments)
table(languages$features_has_line_comments)
unique(languages$line_comment_token)
table(languages$appeared)

# top 4 frequently used tokens 
top <- languages %>%
  group_by(line_comment_token) %>%
  summarise(num_language = n()) %>%
  drop_na(line_comment_token) %>%
  arrange(desc(num_language)) %>%
  top_n(4)

# "Python", "PL/SQL", "Java", "Clojure"

# comment tokens
tokens <- languages %>%
  filter(line_comment_token %in% top$line_comment_token) %>%
  group_by(line_comment_token, year_range) %>%
  summarise(num_language = n()) %>%
  na.omit() %>%
  ungroup() %>%
  mutate(yaxis = rep(c(3,1,4,2), each = 4)) %>%
  arrange(desc(yaxis))

icon_data <- data.frame(ximage = rep(0.3, 4),
                        xlabel = rep(0.5, 4),
                        y = c(4,3,2,1),
                        labels = c("//", "#", ";", "--"),
                        image = images)

ggplot() +
  geom_tile(data = tokens, aes(y = yaxis, x = year_range, fill = num_language), color = "black",
            height = 0.5, width = 0.5) +
  scale_fill_gradient(low = "grey70", high = "grey20") +
  geom_text(data = tokens, aes(y = yaxis, x = year_range, label = num_language), color = "white", size = 15) +
  geom_text(data = icon_data, aes(x = xlabel, y = y, label = labels), vjust = 1, color = "white", size = 25) +
  geom_image(data = icon_data, aes(x = ximage, y = y, image = images), asp=1.5, size=0.05) +
  annotate(geom = "text", label = "1970s", x = 1, y = 0.6, color = "white", size = 15, family = "Roboto") +
  annotate(geom = "text", label = "1980s", x = 2, y = 0.6, color =  "white", size = 15, family = "Roboto") +
  annotate(geom = "text", label = "1990s", x = 3, y = 0.6, color = "white", size = 15, family = "Roboto") +
  annotate(geom = "text", label = "2000s", x = 4, y = 0.6, color = "white", size = 15, family = "Roboto") +
  
  labs(x = "", y = "", title = "Number of Programming Languages that Use Line Comment Tokens", 
       subtitle = "Among the 4,303 programming languages listed in the Programming Language DataBase, the most commonly used line comment tokens in descending order are //, #, ;, and --. These tokens are used in several popular programming languages including Java, Python, Clojure, and SQL.",
       caption = str_wrap("Data source: Programming Language DataBase | Visualization by Zhaowen Guo", width = 300)) +
  theme_void(base_family = "vt") + 
  theme(panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
        legend.position = "none",
        plot.margin = margin(l=15, r=15),
        plot.title = element_text(colour = "#66FF00",
                                  face = "bold",
                                  size = 90,
                                  hjust = 0.5,
                                  margin = margin(t=15)),
        plot.subtitle = element_textbox_simple(
          lineheight = 0.2,
          colour = "#66FF00",
          hjust = 0,
          size = 70,
          margin = margin(t = 10, l=20, r=20, b = 20)
        ),
        plot.caption = element_textbox_simple(
          lineheight = 0.4,
          colour = "#66FF00",
          size = 50,
          margin = margin(t = 10, l=20, b=15)
        ))

library(ggview)
ggview(width = 14, height = 14/1.618, units = "in")
ggsave("programming-languages.png", width = 14, height = 14/1.618)
