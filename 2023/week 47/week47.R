# Load library ------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)


# Load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 47)
rladies_chapters <- tuesdata$rladies_chapters


# Load font ---------------------------------------------------------------
font_add_google("Oswald", "Oswald")
showtext_auto()

font = "Oswald"

# Wrangling ---------------------------------------------------------------

## By year
rladies_group <- rladies_chapters |> 
  count(year, location)


# Graph -------------------------------------------------------------------

rladies_group |> 
  ggplot(aes(x = year, y = n, fill = location)) +
  geom_col() +
  geom_text(aes(label = n),
            family = font,
            size = 11,
            vjust = -0.5,
            color = "black",
            fontface = "italic",
            data = filter(rladies_group, year %in% c(2012:2019))) +
  geom_text(aes(label = n),
            family = font,
            size = 11,
            color = "black",
            fontface = "italic",
            position = position_stack(vjust = 0.5),
            data = filter(rladies_group, !year %in% c(2012:2019))) +
  scale_x_continuous(breaks = 2012:2023, labels = 2012:2023) +
  scale_y_continuous(limits = c(0,1000)) +
  scale_fill_manual(values = c("#b298dc", "#ffd6ff"),
                    labels = c("In Person",
                               "Online")) +
  coord_cartesian(expand = F) +
  guides(fill = guide_legend(keyheight = unit(9, units = "mm"))) +
  labs(title = 'R-Ladies Chapter Events',
       caption = "By @zahiernasrudin. #Tidytuesday") +
  theme_minimal() +
  theme(
    text = element_text(family = font, size = 40, lineheight = 0.3),
    plot.title = element_text(face = "bold", size = 80),
    plot.caption = element_text(color = "grey", 
                                margin = margin(t = 15),
                                hjust = 0,
                                size = 25),
    legend.position = c(0.065, 0.85), 
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
  ) 
  

ggsave("image.jpeg", dpi = 320, width = 8, height = 7)  
