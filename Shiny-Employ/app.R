#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load library ------------------------------------------------------------

library(shiny)
library(tidyverse)
library(plotly)
library(shinycssloaders)

# Load data ---------------------------------------------------------------

df_employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')


# Format data -------------------------------------------------------------

df_employed <- df_employed %>%
    filter(!is.na(industry),
           !industry %in% c("Men", "Women", "White", 
                            "Black or African American", "Asian")) %>%
    mutate(industry = fct_lump(industry, 14, w = employ_n),
           component = case_when(
               race_gender %in% c("Men", "Women") ~ "Gender",
               race_gender == "TOTAL" ~ "Total",
               TRUE ~ "Race"),
           industry = fct_reorder(industry, employ_n, sum))


# Colour Palette ----------------------------------------------------------

col_value <- c("#f94144", "#f3722c", "#f9844a","#f8961e", 
               "#ffc300", "#f9c74f", "#06d6a0","#90be6d","#43aa8b",
               "#4d908e", "#577590", "#006d77","#4a4e69","#277da1", "#22223b")


# Set theme ---------------------------------------------------------------

theme_set(theme_minimal())


# Define UI for application -----------------------------------------------


ui <- fluidPage(
    navbarPage(
        id = "tabset",
        "US Employment",
        
        tabPanel("Employment by year",
            selectizeInput(
            inputId = "year", 
            label = "Select year", 
            choices = unique(df_employed$year), 
            selected = c("2015","2016","2017","2018","2019", "2020"),
            multiple = TRUE
        ),
        
        plotlyOutput("yearplot") %>%
            withSpinner()),
        
        tabPanel("Employment Breakdown",
                 selectizeInput(
                     inputId = "component",
                     label = "Breakdown:",
                     choices = unique(df_employed$component),
                     selected = "Gender",
                     multiple = FALSE
                 ),
                 
                 plotlyOutput("breakdownplot") %>%
                     withSpinner()
                 
                 
                 )
    )
)



# Define server logic -----------------------------------------------------

server <- function(input, output) {
    
    output$yearplot <- renderPlotly({
        a <- df_employed %>%
            filter(component == "Total") %>%
            group_by(year, industry) %>%
            mutate(year = factor(year)) %>%
            summarise(employ_n = sum(employ_n)) %>%
            filter(year %in% c(input$year)) %>%
            ggplot(aes(year, employ_n, fill = industry)) +
            geom_col() +
            scale_y_continuous(labels = scales::comma) +
            scale_fill_manual(values = col_value) +
            labs(y = "No. employed in industry",
                 x = "Year") 
        
        ggplotly(a) %>%
            layout(title="Employment by year",
                   font=list(family = "sans serif",
                             size = 15),
                   legend = list(x = 100, y = 0.5))
        
    })
    
    
    
    output$breakdownplot <- renderPlotly({
        b <- df_employed %>%
            group_by(year,industry, race_gender, component) %>%
            summarise(employ_n = sum(employ_n)) %>%
            filter(component == input$component) %>%
            ggplot(aes(x = year, y = employ_n, colour = race_gender)) +
            geom_line() +
            facet_wrap(~industry, scales = "free_y") +
            scale_y_continuous(labels = scales::comma) +
            labs(colour = "",
                 y = "No. employed in industry",
                 x = "Year")
        
        ggplotly(b, height = 800, width = 1200) %>%
            layout(font=list(family = "sans serif",
                             size = 15),
                   legend = list(x = 100, y = 0.5))
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
