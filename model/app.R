library(shiny)

# Parameters :

fatality_rate = .0087
days_from_infection_to_death = 17.3
doubling_time = 4

# functions

source("model_functions.R")
require(readxl)

average_case_progression = c(
  12,
  26,
  43,
  64,
  103,
  148,
  215,
  310,
  383,
  475,
  624,
  807,
  1019,
  1256,
  1373,
  1508,
  2019,
  2480,
  2998,
  3625,
  4384,
  5300,
  6409,
  7749,
  9369,
  11329,
  13698,
  16562,
  20025,
  24213,
  29277,
  35399
)


#We can choose from severak shiny themes in the link below
#https://shiny.rstudio.com/gallery/shiny-theme-selector.html
ourtheme <- "cerulean"

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel(h1("Coronavirus: Why You Must Act Now")),
  theme = shinythemes::shinytheme(ourtheme),
  br(),
  br(),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      p(
        h4('Do you want to use the modelling based on number of deaths or number of cases?')
      ),
      radioButtons(
        "model_choice",
        "Model to choose:",
        choices = c("#deaths", "#cases"),
        selected = "#deaths",
        inline = FALSE,
        width = NULL,
        choiceNames = NULL,
        choiceValues = NULL
      ),
      numericInput("employees",
                   h4("How many employees/students do you have?"),
                   250,
                   min = 1),
      numericInput(
        "risk",
        h4("What risk are you willing to take (in percentage)?"),
        1,
        min = 0,
        max = 100
      ),
      p(
        '"I am ok with this probability that one or more of my employees/students has the coronavirus."'
      ),
      
      numericInput("population",
                   h4("How many people have in your area"),
                   3096633),
      conditionalPanel(
        condition = "input.model_choice == '#deaths'",
        
        numericInput(
          "deaths",
          h4("Total deaths as of today"),
          1,
          min = 0,
          max = 100
        )
      ),
      conditionalPanel(
        condition = "input.model_choice == '#cases'",
        h4("Model #cases"),
        numericInput("cases",
                     h4("Total known cases in your area as of today"),
                     30,
                     min = 0)
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #h3(textOutput("model_used")),
      conditionalPanel(
        condition = "input.model_choice == '#cases'",
        p(h4("Parameters used : ")),
        p("Fatality rate = 0.87% "),
        p("Days from infection to death = 17.3 "),
        p("Doubling time = 4 ")
      ),
      conditionalPanel(
        condition = "input.model_choice == '#deaths'",
        p(
          h4("Model was based on average progression of case numbers in affected countries. ")
        )
      ),
      textOutput("estimated_cases"),
      br(),
      tableOutput("probabilities"),
      plotOutput("cases_barplot", width = "90%"),
      dataTableOutput("recommendation", width = "60%"),
      br(),
      br(),
      p(
        "* Adapted from medium.com/@tomaspueyo/coronavirus-act-today-or-people-will-die-f4d3d9cd99ca"
      
        ),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$model_used <-
    renderText(paste("Model used:", input$model_choice))
  
  output$estimated_cases <- renderText({
    deaths = input$deaths
    cases = input$cases
    model_to_use = input$model_choice
    
    if (model_to_use == "#deaths") {
      cases_today = estimate_cases_by_deaths(deaths,
                                             fatality_rate,
                                             doubling_time,
                                             days_from_infection_to_death)
    }
    
    if (model_to_use == "#cases") {
      share_of_foreign_spread <- get_proportion_of_foreign_cases(cases)
      cases_today = cases / share_of_foreign_spread
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
    
    if (model_to_use == "#deaths") {
      output_dataframe <-
        calculate_death_model(
          deaths,
          population,
          employees,
          fatality_rate = .0087,
          doubling_time = 4,
          days_from_infection_to_death = 17.3
        )
    }
    
    if (model_to_use == "#cases") {
      output_dataframe <- calculate_cases_model(cases,
                                                population,
                                                employees,
                                                average_case_progression)
    }
    
    return(output_dataframe)
  }, digits = 4)
  
  
  output$recommendation <- DT::renderDataTable({
    risk_you_want_to_take <- input$risk / 100
    model_to_use = input$model_choice
    deaths = input$deaths
    population = input$population
    employees = input$employees
    cases = input$cases
    
    if (model_to_use == "#deaths") {
      output_dataframe <-
        calculate_death_model(
          deaths,
          population,
          employees,
          fatality_rate = .0087,
          doubling_time = 4,
          days_from_infection_to_death = 17.3
        )
    }
    
    if (model_to_use == "#cases") {
      output_dataframe <- calculate_cases_model(cases,
                                                population,
                                                employees,
                                                average_case_progression)
    }
    datatable_to_return <-
      give_recommendation(model_table = output_dataframe, risk_you_want_to_take)
    return(datatable_to_return)
    
  })
  
  
  
  output$cases_barplot <- renderPlot({
    risk_you_want_to_take <- input$risk / 100
    model_to_use = input$model_choice
    deaths = input$deaths
    population = input$population
    employees = input$employees
    cases = input$cases
    
    if (model_to_use == "#deaths") {
      output_plot <-
        calculate_death_model(
          deaths,
          population,
          employees,
          fatality_rate = .0087,
          doubling_time = 4,
          days_from_infection_to_death = 17.3,
          output_format = "plot"
        )
    }
    
    if (model_to_use == "#cases") {
      output_plot <- calculate_cases_model(cases,
                                                population,
                                                employees,
                                                average_case_progression,
                                                output_format = "plot")
    }
    
    return(output_plot)
    
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
