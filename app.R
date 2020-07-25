# COVIDGRAPHICS
# https://github.com/rnnh/covidgraphics

# Loading libraries
library(shinydashboard)
library(tidyverse)
library(gganimate)
library(COVID19)
library(gifski)
library(shiny)

# Loading Covid-19 data
covid19.df <- as.data.frame(covid19(verbose = FALSE))

# Defining shiny user interface function
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "covidgraphics"),
  
  # Sidebar
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(
                     menuItem("Line charts", tabName = "linecharts", 
                              icon = icon("line-chart"))),
                   sidebarMenu(
                     menuItem("Animated graphs", tabName = "animatedgraphs", 
                              icon = icon("spinner"))),
                   sidebarMenu(
                     menuItem("Source code", icon = icon("file-code-o"), 
                              href = "https://github.com/rnnh/covidgraphics"))
                   ),
  
  # Body
  dashboardBody(
    tabItems(
      tabItem(tabName = "linecharts",
              fluidRow(
                box(title = "Covid-19 graph controls",
                    "The graphs below will update automatically according to
                    these inputs.", br(),
                    "Enter the 3 letter ISO code for a country to display its
                    covid-19 data.", br(),
                    "Country codes must be in capitals, and separated with 
                    spaces.", br(),
                    "Use the slider to select the date range to display.", br(),
                    textInput("countries_lc", "3 letter ISO country codes",
                              "USA BRA RUS PER ITA"),
                    sliderInput(inputId = "date_lc", label = "Date range",
                                min = min(covid19.df$date),
                                max = max(covid19.df$date), 
                                value = c(max(covid19.df$date) - 60,
                                          max(covid19.df$date)), step = 1))
                ),
              fluidRow(
                box(plotOutput("lp_death_time")),
                box(plotOutput("lp_case_time"))
              ),
              fluidRow(
                box(plotOutput("lp_death_time_pc")),
                box(plotOutput("lp_case_time_pc"))
              )),
      
      tabItem(tabName = "animatedgraphs",
              fluidRow(
                box(title = "Covid-19 animated graph controls",
                    'This animated graph will update according to these inputs 
                    when the "Create animated graph" below is pressed.', br(),
                    "This graph will take a few moments to render, please be 
                    patient.", br(),
                    "Enter the 3 letter ISO code for a country to display its
                    covid-19 data.", br(),
                    "Country codes must be in capitals, and separated with 
                    spaces.", br(),
                    "Use the slider to select the date range to display.", br(),
                    sliderInput(inputId = "date", label = "Date range",
                                min = min(covid19.df$date),
                                max = max(covid19.df$date), 
                                value = c(max(covid19.df$date) - 60,
                                          max(covid19.df$date)), step = 1),
                    textInput("countries", "Select countries (ISO codes)",
                              "USA BRA RUS PER ITA"),
                    actionButton('goPlot', 'Create animated graph (please wait)')
                ),
                box(title = "Covid-19: Cases vs. Deaths",
                    plotOutput("gifPlot", height = 600)
                    ))
              )
      )
    )
  )


# Define shiny server function
server <- function(input, output) {
  
  # Function to create Covid-19 subsets in reaction to inputs
  covid19_reactive <- reactive({
    country_codes <- unlist(strsplit(input$countries_lc, " "))
    covid19_countries.df <- covid19.df[covid19.df$id %in% country_codes, ]
    covid19_subset.df <- covid19_countries.df[
      covid19_countries.df$date >= input$date_lc[1] &
        covid19_countries.df$date <= input$date_lc[2], ]
  })
  
  # Function to create Covid-19 subsets in response to a button press
  covid19_button <- eventReactive(input$goPlot, {
    country_codes <- unlist(strsplit(input$countries, " "))
    covid19_countries.df <- covid19.df[covid19.df$id %in% country_codes, ]
    covid19_subset.df <- covid19_countries.df[
      covid19_countries.df$date >= input$date[1] &
        covid19_countries.df$date <= input$date[2], ]
  })
  
  # Line plot: deaths vs. time
  output$lp_death_time <- renderPlot({
    covid19_timeframe.df <- covid19_reactive()
    ggplot(covid19_timeframe.df, aes(x = date, y = deaths, color = id)) +
      geom_line() + 
      geom_point() +
      labs(title = "Covid-19: Deaths vs. Time",
           x = "Time",
           y = "Deaths",
           color = "Country") +
      theme_bw() +
      theme(text = element_text(size = 16))})
  
  # Line plot: cases vs. time
  output$lp_case_time <- renderPlot({
    covid19_timeframe.df <- covid19_reactive()
    ggplot(covid19_timeframe.df, aes(x = date,
                                     y = confirmed - (deaths + recovered),
                                     color = id)) +
      geom_line() + 
      geom_point() +
      labs(title = "Covid-19: Cases vs. Time",
           x = "Time",
           y = "Cases",
           color = "Country") +
      theme_bw() +
      theme(text = element_text(size = 16))})
  
  # Line plot: deaths per capita vs. time
  output$lp_death_time_pc <- renderPlot({
    covid19_timeframe.df <- covid19_reactive()
    ggplot(covid19_timeframe.df, aes(x = date, y = deaths / population,
                                     color = id)) +
      geom_line() + 
      geom_point() +
      labs(title = "Covid-19: Deaths per capita vs. Time",
           x = "Time",
           y = "Deaths per capita",
           color = "Country") +
      theme_bw() +
      theme(text = element_text(size = 16))})
  
  # Line plot: cases per capita vs. time
  output$lp_case_time_pc <- renderPlot({
    covid19_timeframe.df <- covid19_reactive()
    ggplot(covid19_timeframe.df, aes(
      x = date,
      y = (confirmed - (deaths + recovered)) / population,
      color = id)) +
      geom_line() + 
      geom_point() +
      labs(title = "Covid-19: Cases per capita vs. Time",
           x = "Time",
           y = "Cases per capita",
           color = "Country") +
      theme_bw() +
      theme(text = element_text(size = 16))})
  
  # Animated plot: cases per capita vs. deaths per capita
  output$gifPlot <- renderImage({
    covid19_timeframe.df <- covid19_button()
    
    outfile <- tempfile(fileext='.gif')
    
    attach(covid19_timeframe.df)
    x <- confirmed / population
    y <- deaths / population
    detach(covid19_timeframe.df)
    
    p <- ggplot(covid19_timeframe.df, aes(x, y, size = population, color = id)) +
      geom_point() +
      theme_bw() +
      theme(text = element_text(size = 16)) +
      labs(title = "Covid-19: Cases vs. Deaths",
           subtitle = "Points scaled to population. Date: {frame_time}",
           x = "Confirmed cases per capita",
           y = "Deaths per capita",
           color = "Country") +
      guides(size = FALSE) +
      transition_time(date) +
      ease_aes("linear")
    
    # Setting GIF dimensions
    options(gganimate.dev_args = list(width = 600, height = 600)) 
    
    # Creating GIF
    anim_save("outfile.gif", animate(p))
    
    # List containing the GIF filename
    list(src = "outfile.gif",
         contentType = "image/gif",
         alt = "Covid-19: Cases vs Deaths"
    )}, deleteFile = TRUE)
  
}

# Run the application
shinyApp(ui, server)