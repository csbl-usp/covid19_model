library(shiny)
library(shiny.i18n)

library(data.table)
##### Functions and parameters #####

fatality_rate = .0087
days_from_infection_to_death = 17.3
doubling_time = 4
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


##################################


#We can choose from severak shiny themes in the link below
#https://shiny.rstudio.com/gallery/shiny-theme-selector.html
ourtheme <- "cerulean"




# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel(     fluidRow(
    column(8, textOutput("title")),
    column(3,       tags$div(HTML('<div id="lang" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
  <label class="control-label" for="lang"></label>
                                      <div class="shiny-options-group">

                                      <label class="radio-inline">
                                      <input type="radio" name="lang" value="pt" checked="checked"/>
                                      <span><img src="brasil.png" style="width:28px;height:20px;"/></span>

                                      </label>
                                      <label class="radio-inline">
                                      <input type="radio" name="lang" value="en" />
                                      <span><img src="uk.png" style="width:28px;height:20px;"/></span>

                                      </label>
                                      </div>
                                      </div> ')))
  )),
  theme = shinythemes::shinytheme(ourtheme),
  # Sidebar with a slider input for number of bins #####
  sidebarLayout(
    sidebarPanel(
      textOutput("which_model"),
      uiOutput("radio_model")
      ,
      uiOutput("employ"),
      uiOutput("risk_to_take"),
      textOutput("risk_explained"),
      
      uiOutput("people"),
      conditionalPanel(
        condition = "input.model_choice == '#deaths'",
        
        textOutput("model_deaths"),
        
        numericInput(
          "deaths",
          textOutput("total_deaths"),
          1,
          min = 0,
          max = 100
        )
      ),
      conditionalPanel(condition = "input.model_choice == '#cases'",
                       textOutput(h4("model_cases")),
                       uiOutput("cases"))
      
    ),
    
    # Main panel ##########
    mainPanel(
      #h3(textOutput("model_used")),
      conditionalPanel(
        condition = "input.model_choice == '#deaths'",
        textOutput("param"),
        textOutput("fatality"),
        textOutput("days_from_infection"),
        uiOutput("doubling_time")
      ),
      conditionalPanel(condition = "input.model_choice == '#cases'",
                       textOutput(h4("progression"))),
      textOutput("estimated_cases"),
      br(),
      tableOutput("probabilities"),
      plotOutput("cases_barplot", width = "90%"),
      dataTableOutput("recommendation", width = "60%"),
      br(),
      br(),
      textOutput("adapted_from")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #### Texts #####
  output$title <- renderText(tr(input, "title"))
  output$model_used <-
    renderText(paste("Model used:", input$model_choice))
  
  output$which_model <-
    renderText(tr(input, "which_model"))
  
  output$risk_explained <-
    renderText(tr(input, "risk_explained"))
  
  output$model_deaths <-
    renderText(tr(input, "model_deaths"))
  
  output$estimated_cases <- renderText(tr(input, 'estimated_cases'))
  output$fatality <- renderText(tr(input, 'fatality'))
  output$model_cases <- renderText(tr(input, 'model_cases'))
  output$model_deaths <- renderText(tr(input, 'model_deaths'))
  output$modelbutton <- renderText(tr(input, 'modelbutton'))
  output$number_of_employees <-
    renderText(tr(input, 'number_of_employees'))
  output$param <- renderText(tr(input, 'param'))
  output$progression <- renderText(tr(input, 'progression'))
  output$risk_explained <- renderText(tr(input, 'risk_explained'))
  output$risk_to_take <- renderText(tr(input, 'risk_to_take'))
  output$title <- renderText(tr(input, 'title'))
  output$total_cases <- renderText(tr(input, 'total_cases'))
  output$total_deaths <- renderText(tr(input, 'total_deaths'))
  output$which_model <- renderText(tr(input, 'which_model'))
  
  #### UI #####
  
  output$employ <- renderUI({
    numericInput("employees",
                 tr(input, "number_of_employees"),
                 250,
                 min = 1)
  })
  
  output$radio_model <- renderUI({
    deaths = tr(input, "#deaths")
    cases = tr(input, "#cases")
    choice_vector = c("#deaths", "#cases")
    names(choice_vector) = c(deaths, cases)
    
    radioButtons(
      "model_choice",
      tr(input, "model_choice_header"),
      choices = choice_vector,
      selected = "#deaths",
      inline = FALSE,
      width = NULL,
      choiceNames = NULL,
      choiceValues = NULL
    )
    
  })
  
  output$risk_to_take <- renderUI({
    numericInput("risk",
                 h4(tr(input, "risk_to_take")),
                 1,
                 min = 0,
                 max = 100)
  })
  
  output$people <- renderUI({
    numericInput("population",
                 h4(tr(input, "#people")),
                 3096633)
  })
  
  output$cases <- renderUI({
    numericInput("cases",
                 tr(input, "model_cases"),
                 30,
                 min = 0)
  })
  
  output$doubling_time <- renderUI({
    numericInput("doubling_time",
                 tr(input, "doubling_text"),
                 4,
                 min = 0,
                 max = 100)
  })
  
  #### Tables and plots #####
  
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
    doubling_time = input$doubling_time
    lang = input$lang
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
          lang = lang,
          fatality_rate = .0087,
          doubling_time = doubling_time,
          days_from_infection_to_death = 17.3
        )
    }
    
    if (model_to_use == "#cases") {
      output_dataframe <- calculate_cases_model(cases,
                                                population,
                                                employees,
                                                lang = lang,
                                                average_case_progression)
    }
    
    return(output_dataframe)
  }, digits = 4)
  
  
  output$recommendation <- DT::renderDataTable({
    risk_you_want_to_take <- input$risk / 100
    model_to_use = input$model_choice
    lang = input$lang
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
          lang = "en",
          fatality_rate = .0087,
          doubling_time = 4,
          days_from_infection_to_death = 17.3
        )
    }
    
    if (model_to_use == "#cases") {
      output_dataframe <- calculate_cases_model(cases,
                                                population,
                                                employees,
                                                lang = "en",
                                                average_case_progression)
    }
    datatable_to_return <-
      give_recommendation(model_table = output_dataframe, risk_you_want_to_take, lang)
    return(datatable_to_return)
    
  })
  
  
  
  output$cases_barplot <- renderPlot({
    risk_you_want_to_take <- input$risk / 100
    model_to_use = input$model_choice
    deaths = input$deaths
    lang = input$lang
    doubling_time = input$doubling_time
    population = input$population
    employees = input$employees
    cases = input$cases
    
    if (model_to_use == "#deaths") {
      output_plot <-
        calculate_death_model(
          deaths,
          population,
          employees,
          lang = lang,
          fatality_rate = .0087,
          doubling_time = doubling_time,
          days_from_infection_to_death = 17.3,
          output_format = "plot"
        )
    }
    
    if (model_to_use == "#cases") {
      output_plot <- calculate_cases_model(cases,
                                           population,
                                           employees,
                                           lang = lang,
                                           average_case_progression,
                                           output_format = "plot")
    }
    
    return(output_plot)
    
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
