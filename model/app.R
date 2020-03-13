library(shiny)

# Parameters :

fatality_rate = .0087
days_from_infection_to_death = 17.3
doubling_time = 4

# functions

source("model_functions.R")
require(readxl)

average_case_progression = c(12, 26, 43, 64, 103, 148, 215, 310, 383, 475, 624, 807, 1019, 
                             1256, 1373, 1508, 2019, 2480, 2998, 3625, 4384, 5300, 6409, 7749, 
                             9369, 11329, 13698, 16562, 20025, 24213, 29277, 35399)

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
        "What risk are you willing to take (in percentage)?",
        1,
        min = 0,
        max = 100
      ),
      p(
        '"I am ok with this probability that one or more of my employees has the coronavirus."'
      ),
      
      numericInput(
        "population",
        "# people in your area",
        3096633
      ),
      
      h4("Model #deaths"),
      numericInput(
        "deaths",
        "Total deaths as of today",
        1,
        min = 0,
        max = 100
      ),
      h4("Model #cases"),
      numericInput(
        "cases",
        "Total known cases in your area as of today",
        30,
        min = 0)

    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p("Parameters used : "),
      p("Fatality rate = 0.87% "),
      p("Days from infection to death = 17.3 "),
      p("Doubling time = 4 "),
      h3(textOutput("model_used")),
      textOutput("estimated_cases"),
      br(),
      p("\n likelyhood_no_infection indicates the likelyhood that none of the employees has the disease"),
      tableOutput("probabilities"),
      dataTableOutput("recommendation")
    )
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$model_used <- renderText(paste("Model used:", input$model_choice))

  output$estimated_cases <- renderText({
    
    deaths = input$deaths
    cases = input$cases
    model_to_use = input$model_choice
    
    if (model_to_use == "#deaths"){
      cases_today = estimate_cases_by_deaths(deaths, fatality_rate, doubling_time, days_from_infection_to_death)
    }
    
    if (model_to_use == "#cases"){
      share_of_foreign_spread <- get_proportion_of_foreign_cases(cases)
      cases_today = cases/share_of_foreign_spread
    }
    
    
    
    result <-
      paste("The number of estimated true cases today is",
            cases_today)
    
    return(result)
  })
  
  output$probabilities <- renderTable({
    
    model_to_use = input$model_choice
    deaths = input$deaths
    cases = input$cases
    population = input$population
    employees = input$employees
    
    if (model_to_use == "#deaths"){
      output_dataframe <- calculate_death_model(deaths, population, employees, fatality_rate = .0087,  doubling_time = 4, days_from_infection_to_death = 17.3)
    }
    
    if (model_to_use == "#cases"){
      output_dataframe <- calculate_cases_model(cases,
                                                population,
                                                employees,
                                                average_case_progression)    }

  return(output_dataframe)   
  }, digits = 4)


  output$recommendation <- DT::renderDataTable({
    
    risk_you_want_to_take <- input$risk/100
    model_to_use = input$model_choice
    deaths = input$deaths
    population = input$population
    employees = input$employees
    cases = input$cases
    
    if (model_to_use == "#deaths"){
      output_dataframe <- calculate_death_model(deaths, population, employees, fatality_rate = .0087,  doubling_time = 4, days_from_infection_to_death = 17.3)
    }
    
    if (model_to_use == "#cases"){
      output_dataframe <- calculate_cases_model(cases,
      population,
      employees,
      average_case_progression)
    }
    datatable_to_return <- give_recommendation(model_table = output_dataframe, risk_you_want_to_take)
    return(datatable_to_return)
    
  })
    
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
