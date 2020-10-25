#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(janitor)
library(ggimage)

# Fundy National Park Moose Data
moose_data <- read_csv("moose_data.csv")

moose_data <- moose_data %>%
    janitor::clean_names() %>%
    slice(-1) %>%
    mutate(total_count = as.numeric(total_count)) %>% 
    group_by(year) %>%
    summarise(total_count = sum(total_count, na.rm = TRUE))

# moose_data %>%
#     ggplot() +
#     geom_bar(aes(x = year, y = total_count), stat = "identity") +
#     ggimage::geom_image(aes(x = year, y = total_count + 4, image = "moose.png"), size = 0.15) +
#     ylab("Number of Moose Spotted") +
#     xlab("Year of Moose Survey") +
#     ggtitle("How many moose seen in Fundy National Park, Canada") +
#     theme_minimal()
    


# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("How many Moose in Fundy National Park"),

    # Sidebar with a slider input
            sliderInput("years",
                        "Year range:",
                        min = 1993,
                        max = 2016,
                        value = c(1993, 2016),
                        sep = "", width = 600),

        # Show a plot
           plotOutput("mooseplot", width = 600, height = 600)

)

# Define server logic required
server <- function(input, output) {

    output$mooseplot <- renderPlot({
        moose_data %>%
            filter(year %in% seq(input$years[1], input$years[2], 1)) %>%
            ggplot() +
            geom_bar(aes(x = year, y = total_count), stat = "identity") +
            ggimage::geom_image(aes(x = year, y = total_count + 4, image = "moose.png"), size = 0.15) +
            ylab("Number of Moose Spotted") +
            xlab("Year of Moose Survey") +
            ggtitle("How many moose seen in Fundy National Park, Canada") +
            theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
