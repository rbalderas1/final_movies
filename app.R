#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries in
library(shiny)
library(tidyverse)

# Read data in
movies_data <- read_csv("data/movies.csv")

# List of genres
genre_option <- movies_data %>% 
    distinct(genre)

# Filtering out the genres that have na values in gross col
movies_gross <- movies_data %>% 
    filter(!is.na(gross)) %>% 
    group_by(genre) %>% 
    summarize(total_gross = sum(gross)) %>% 
    arrange(desc(total_gross))

# List of genres that do not have na values
genre_option_gross <- movies_gross %>% 
    distinct(genre)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("IMDb Movie Dashboard"),
    
    # Sidebar with a panel that allows user to choose tab
    sidebarLayout(
        sidebarPanel(
            h4("Select the type of plot you want to display using the tabs"),
            h5("Data is from 'Movie Industry' on Kaggle.
               The data contains 6280 movies released 1980-2020 and was scraped from IMDb."),
            h6("https://www.kaggle.com/datasets/danielgrijalvas/movies")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Timeline",
                                 selectInput("genre_choice",
                                             "Choose genre to highlight",
                                             choices = genre_option),
                                 plotOutput("timeline_plot")),
                        tabPanel("Bar Plot",
                                 selectInput("genre1",
                                             "Select Genre:",
                                             choices = genre_option_gross, selected = genre_option_gross[1,1]),
                                 selectInput("genre2",
                                             "Select Genre:",
                                             choices = genre_option_gross, selected = genre_option_gross[2,1]),
                                 selectInput("genre3",
                                             "Select Genre:",
                                             choices = genre_option_gross, selected = genre_option_gross[3,1]),
                                 plotOutput("bar_plot")),
                        tabPanel("Scatter Plot",
                                 selectInput("genre_choice_score",
                                             "Choose genre to highlight",
                                             choices = genre_option),
                                 plotOutput("scatter_plot")),
                        tabPanel("Bar Plot 2",
                                 sliderInput("genre_count",
                                             "Select how many genres to display:",
                                             min = 1,
                                             max = 18,
                                             value = 5),
                                 plotOutput("bar_plot_2")))
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$timeline_plot <- renderPlot({
        
        movies_runtime <- movies_data %>%
            filter(!is.na(runtime)) %>% 
            group_by(genre, year) %>% 
            summarize(average_runtime = mean(runtime)) %>% 
            arrange(desc(average_runtime))
        
        highlighted_genre <- movies_runtime %>%
            filter(genre == input$genre_choice)
        
        movies_runtime %>%
            ggplot(aes(y = average_runtime,
                       x = year,
                       group = genre)) +
            geom_line(color = "grey") +
            geom_point(data = highlighted_genre,
                       color = "#ba1f1c") +
            geom_line(data = highlighted_genre,
                      color = "#ba1f1c") +
            theme_light() +
            labs(y = "Average Runtime for the Genre (minutes)",
                 x = "Year",
                 title = "Average Runtime for Movie Genres Throughout the Years") +
            theme(plot.background = element_rect(fill = "#bfdbd0"),
                  text = element_text(size = 15))
        
    })
    
    output$bar_plot <- renderPlot({
        
        movies_gross %>% 
            # I got this to work with other data, don't know why it acts weird with this data :(
            # Still wanna keep it in cause I put a lot of effort in it
            filter(genre == c(input$genre1, input$genre2, input$genre3)) %>% 
            ggplot(aes(x = total_gross,
                       y = genre)) +
            geom_col(fill = "#ba1f1c") +
            theme_light() +
            labs(y = "Genre",
                 x = "Total Gross Revenue",
                 title = "Total Gross Revenue for Movie Genre") +
            theme(plot.background = element_rect(fill = "#bfdbd0"),
                  text = element_text(size = 15))
    })
    
    output$scatter_plot <- renderPlot({
        
        highlighted_genre <- movies_data %>%
            filter(genre == input$genre_choice_score)
        
        movies_data %>% 
            group_by(genre) %>% 
            ggplot(aes(x = year,
                       y = score,
                       group = genre)) +
            geom_point(color = "grey") +
            geom_point(data = highlighted_genre,
                       color = "#ba1f1c") +
            geom_smooth(data = highlighted_genre,
                        method = "lm") +
            theme_light() +
            labs(y = "IMDb Score",
                 x = "Year",
                 title = "IMDb Scores for each Movie Genre Throughout the Years") +
            theme(plot.background = element_rect(fill = "#bfdbd0"),
                  text = element_text(size = 15))
        
        
    })
    
    output$bar_plot_2 <- renderPlot({
        
        # Extra plot cause the second one acts funky
        movies_gross %>% 
            top_n(-as.numeric(input$genre_count)) %>% 
            ggplot(aes(y = reorder(genre, total_gross),
                       x = total_gross)) +
            geom_col(fill = "#ba1f1c") +
            theme_light() +
            labs(y = "Genre",
                 x = "Total Gross Revenue",
                 title = "Total Gross Revenue for Each Movie Genre") +
            theme(plot.background = element_rect(fill = "#bfdbd0"),
                  text = element_text(size = 15))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
