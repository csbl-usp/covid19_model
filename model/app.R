library(shiny)

# Parameters :

fatality_rate = .0087
days_from_infection_to_death = 17.3
doubling_time = 4

# functions

source("model_functions.R")


# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Covid19 Simple model"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      p('Do you want to use the modelling based on number of deaths or number of cases?'),
      radioButtons("model_choice", "Model to choose:", choices = c("#deaths", "#cases"), selected = "#deaths",
                   inline = FALSE, width = NULL, choiceNames = NULL,
                   choiceValues = NULL),
      numericInput("employees",
                   "How many employees do you have?",
                   250,
                   min = 1),
      numericInput(
        "risk",
        "What risk are you willing to take?",
        1,
        min = 0,
        max = 100
      ),
      p(
        '"I am ok with this probability that one or more of my employees has the coronavirus."'
      ),
      numericInput(
        "deaths",
        "Total deaths as of today",
        1,
        min = 0,
        max = 100
      ),
      numericInput(
        "population",
        "# people in the area of the deaths",
        3096633
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p("Parameters used : "),
      p("Fatality rate = 0.87% "),
      p("Days from infection to death = 17.3 "),
      p("Doubling time = 4 "),
      textOutput("estimated_cases"),
      p("likelyhood_no_infection indicates the likelyhood that none of the employees has the disease"),
      tableOutput("probabilities")
    )
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$estimated_cases <- renderText({
    
    deaths = input$deaths
    estimated_cases_that_caused_deaths <- deaths / fatality_rate
    
    number_of_times_cases_have_doubled <-
      days_from_infection_to_death / doubling_time
    
    cases_today = estimated_cases_that_caused_deaths * (2 ** number_of_times_cases_have_doubled)
    
    result <-
      paste("The number of estimated cases today is",
            cases_today)
    
    return(result)
  })
  
  output$probabilities <- renderTable({
    
    model_to_use = input$model_choice
    deaths = input$deaths
    population = input$population
    employees = input$employees
    
    
    output_dataframe <- calculate_death_model(deaths, population, employees, fatality_rate = .0087,  doubling_time = 4, days_from_infection_to_death = 17.3)

  return(output_dataframe)   
  }, digits = 4)
}

# Run the application
shinyApp(ui = ui, server = server)
