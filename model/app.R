library(shiny)

# Parameters :

fatality_rate = .0087
days_from_infection_to_death = 17.3
doubling_time = 4

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
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
    
    estimated_true_cases_today = estimated_cases_that_caused_deaths * (2 ** number_of_times_cases_have_doubled)
    
    result <-
      paste("The number of estimated cases today is",
            estimated_true_cases_today)
    
    return(result)
  })
  
  output$probabilities <- renderTable({
    
    deaths = input$deaths
    population = input$population
    
    estimated_cases_that_caused_deaths <- deaths / fatality_rate
    
    number_of_times_cases_have_doubled <-
      days_from_infection_to_death / doubling_time
    
    estimated_true_cases_today = estimated_cases_that_caused_deaths * (2 ** number_of_times_cases_have_doubled)
    
    estimated_true_cases_tomorrow
    estimated_true_cases_in_a_week = 
    
    
    
    current_infection_rate = estimated_true_cases_today / population
    print(current_infection_rate)
    
    
    return(data.frame(date = ("today", "tomorrow", "in a week"), infection_rate))
  }, digits = 4)
}

# Run the application
shinyApp(ui = ui, server = server)
