# Load library ------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)


# Load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 48)

drwho_episodes <- tuesdata$drwho_episodes
drwho_directors <- tuesdata$drwho_directors
drwho_writers <- tuesdata$drwho_writers


# Load font ---------------------------------------------------------------
font_add_google("Philosopher", "Philosopher")
showtext_auto()

font = "Philosopher"

# Graph -------------------------------------------------------------------

drwho_episodes |> 
  filter(type == "episode") |> 
  ggplot(aes(x = rating, y = uk_viewers)) +
  geom_point(color = "#003b6f", size = 2) +
  scale_y_continuous(limits = c(2,12), breaks = c(4, 8, 12), labels = c("4m", "8m", "12m")) +
  facet_wrap(~ season_number, ncol = 3) +
  labs(title = 'Dr. Who',
       caption = "By @zahiernasrudin. #Tidytuesday",
       subtitle = "Ratings vs. Viewership in the UK: Per Season",
       x = "Rating",
       y = "UK Viewers") +
  theme_light() +
  theme(
    text = element_text(family = font, size = 50, lineheight = 0.3),
    plot.title = element_text(face = "bold", size = 100),
    plot.subtitle = element_text(size = 70, margin = margin(b = 15)),
    plot.caption = element_text(color = "grey", 
                                margin = margin(t = 15),
                                hjust = 0),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left,
    strip.background = element_rect(fill = "#003b6f",linewidth = 0.5),
    strip.text = element_text(face = "bold")
  ) 



ggsave("image.jpeg", dpi = 320, width = 8, height = 8)  
