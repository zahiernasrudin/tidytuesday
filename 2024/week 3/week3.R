# Load library ------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(gt)

# Load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 3)

polling_places <- tuesdata$polling_places


# Load font ---------------------------------------------------------------
font_add_google("Graduate", "Graduate")
showtext_auto()

font = "Graduate"


# Wrangling ---------------------------------------------------------------

### |- Tidy ----
total_polling <- polling_places |> 
  # get year
  mutate(election_year = year(election_date)) |> 
  ## data
  reframe(no_polling = n(), .by = c(state, election_year)) |> 
  ## get only the latest year
  filter(election_year == max(election_year))


# Table -------------------------------------------------------------------

table_plot <- total_polling |> 
  select(-election_year) |> 
  arrange(desc(no_polling)) |> 
  head(10) |> 
  gt() |> 
  # add header
  tab_header(title = md("**Number of Polling Places in the United States**"),
             subtitle = md("*2020: Top 10 Polling Places*")) |> 
  # add source
  tab_source_note("@zahiernasrudin. #Tidytuesday") |> 
  # format number
  fmt_number(columns = no_polling, decimals = 0) |> 
  # change font type
  opt_table_font(
    font = list(
      google_font(name = font)
    )
  ) |> 
  # change column name
  cols_label(
    state = "State",
    no_polling = "Number of polling places",
  ) |> 
  # add row striping
  opt_row_striping() |> 
  ## some other formatting
  tab_options(column_labels.background.color = "#585d63",
    source_notes.font.size = 6,
    table.font.size = 12,
    table.width = px(300),
    heading.title.font.size = 21,
    table.border.top.width = px(3),
    data_row.padding = px(7)
  )

table_plot

gtsave(table_plot,"image.png")  
 