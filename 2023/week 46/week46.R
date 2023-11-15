# Load library ------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(viridis)
library(scales)
library(classInt)

# Load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 46)
diwali <- tuesdata$diwali_sales_data


# Load font ---------------------------------------------------------------
font_add_google("Roboto Slab", "Roboto Slab")
showtext_auto()

font = "Roboto Slab"

# Wrangling ---------------------------------------------------------------

## By state & Category
diwali_group <- diwali |> 
  reframe(Amount = sum(Amount, na.rm = T), .by = c(State, Product_Category)) 


# Graph -------------------------------------------------------------------

# bins
brk <- classIntervals(diwali_group$Amount,
                            n = 6,
                            style = "equal")$brks


diwali_group |> 
  ggplot(aes(State, Product_Category, fill = Amount)) + 
  geom_tile(color = "white", linewidth = 0.2) +
  labs(title = "Diwali Sales",
       fill = NULL,
       y = "Category",
       caption = "By @zahiernasrudin. #Tidytuesday") +
  scale_fill_viridis_c(option = "plasma", label = comma, breaks = brk) +
  guides(fill = guide_legend(
    direction = "horizontal",
    keyheight = unit(10, units = "mm"),
    keywidth = unit(12, units = "mm"),
    nrow = 1,
    label.hjust = .5,
    label.position = "bottom"
  )) +
  theme_minimal() +
  theme(plot.title = element_text(size = 60, family = font, face = "bold",
                                  margin = margin(b = 15),  hjust = 0.5),
        plot.caption = element_text(size = 25, family = font, face = "bold",
                                  margin = margin(t = 15),  hjust = 0.5),
        axis.text = element_text(size = 30, family = font),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        axis.title = element_text(size = 30, family = font),
        legend.position = "top",
        legend.text = element_text(size = 25, family = font),
        panel.grid = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))


dir.create("img")
ggsave("img/image.jpeg", dpi = 320, width = 7, height = 7)  
