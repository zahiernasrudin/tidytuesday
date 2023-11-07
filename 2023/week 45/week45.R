# Load library ------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(waffle)
library(showtext)
library(ggtext)

# Load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 45)
house <- tuesdata$house


# Load font ---------------------------------------------------------------
font_add_google("Roboto Slab", "Roboto Slab")
showtext_auto()

font = "Roboto Slab"

# Wrangling ---------------------------------------------------------------

### Get percentage
total_vote <- house |>
  ## General election 2022
  filter(stage == "GEN",
         special == FALSE,
         year == 2022) |> 
  mutate(party = fct_other(party, keep = c("DEMOCRAT", "REPUBLICAN"), other_level = "OTHERS"),
         party = replace_na(party, "OTHERS")) |> 
  mutate(party = as_factor(party)) |> 
  ## Get winner seat
  slice_max(candidatevotes,by = c("state", "district")) |> 
  reframe(n =  length(candidate), .by = c(year, state, party)) |> 
  mutate(percentage = round(n / sum(n) * 100), .by = c(year, state)) 



# Graph -------------------------------------------------------------------

desired_level_state <- total_vote |> 
  group_by(party) |> 
  arrange(desc(percentage), .by_group = TRUE) |> 
  pull(state) |> 
  unique()


total_vote |> 
  mutate(state = factor(state, levels = desired_level_state)) |> 
  arrange(party) |> 
  ggplot(aes(fill = party, values = percentage)) +
  geom_waffle(color = "gray", n_rows = 5, show.legend = F) +
  facet_wrap(~ state, ncol = 5) +
  scale_fill_manual(values = c("REPUBLICAN" = "#2a9d8f",
                               "DEMOCRAT" = "#e9c46a")) +
  labs(title = "2022 United States House of Representatives elections",
       subtitle = "House seats (%): <span style = 'color:#2a9d8f;'>Republican</span> vs
       <span style = 'color:#e9c46a;'>Democrat</span>",
       caption = "By @zahiernasrudin. #Tidytuesday") +
  theme_void() +
  theme(plot.caption = element_text(family = font, color = "grey20", size = 35,
                                    margin = margin(t = 15)),
        plot.title = element_text(size = 70, family = font, face = "bold",
                                  margin = margin(t = 5), hjust = 0.5),
        plot.subtitle = element_markdown(size = 55, family = font,
                                     margin = margin(b = 20,t = 10),
                                     hjust = 0.5),
        strip.text = element_text(size = 38, family = font),
        panel.spacing.y = unit(1, "lines"),
        panel.spacing.x = unit(2, "lines"),
        panel.background = element_rect(fill = "#F3F3F0", linewidth = 0),
        plot.background = element_rect(fill = "#F3F3F0", linewidth = 0),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) 


dir.create("img")
ggsave("img/image.jpeg",
       width = 10, height = 10)
