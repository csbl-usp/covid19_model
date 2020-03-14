library(shiny)

#Defining parameters
alfa1 <- -257.580667091408
alfa2 <- 67698.802805002
beta <- 0.4375 #beta = gama*R0
gama <- 0.15625
R0 <- 2.8 #beta/gama
R0_min <- 1.02

#Define time
time_range <- seq(from = 0, to = 3600, by = 0.01)

#How can I get all the values 0, 0.01, 0.02, 0.03, 0.04.......? Like the column of time (Column A) of George's table.... 
# for (i in time_range){
#
#   }

#Defining UI ------
ui <- fluidPage(titlePanel("Covid-19 Prediction Model"),
                
                sidebarLayout(
                  #Total population
                  sidebarPanel(
                    numericInput(
                      "total_pop",
                      "Total population",
                      value = 21571281,
                      min = 1
                    ),
                    numericInput(
                      #Infected number
                      "infected",
                      "Number of infected",
                      value = 151,
                      min = 1
                    ),
                    numericInput(
                      #Recovered
                      "recovered",
                      "Number of recovered",
                      value = 0,
                      min = 1
                    ),
                    numericInput(
                      "quarantine_gov",
                      "After the first case, which day the goverment will start the quarentine?",
                      value = 15,
                      min = 1
                    ),
                    numericInput(
                      "time_to_start_quarantine_pop",
                      "Assume that peolpe needs some time (days) to start the quarentine effectively:",
                      value = 10,
                      min = 1
                    )
                  ),
                  
                  mainPanel(textOutput("susceptible"),          #Print susceptibles
                            textOutput("suscept_percent"))
                ))

#Defining server logic -----
server <- function(input, output, session) {
  ########Defining rates
  susceptible_calc <-
    reactive({
      #Number of susceptibles
      input$total_pop - input$infected - input$recovered
    })
  
  susceptible_rate <-
    reactive({
      #Rate of susceptibles
      ((input$total_pop - input$infected - input$recovered) / input$total_pop)
    })
  
  infected_rate <-
    reactive({
      #Rate of infected
      input$infected / input$total_pop
    })
  
  recovered_rate <-
    reactive({
      #rate of recovered
      1 - input$infected_rate - input$susceptible_rate
    })
  
  ######Defining predictions
  susceptible_prediction <- reactive({
    (input$susceptible_rate - gama) * R0 * input$susceptible_rate * infected_rate *
      (time_range - 0)
  })
  
  infected_prediction <-
    reactive({
      input$infected_rate + 
        (gama * R0 * input$susceptible_rate * input$infected_rate - gama * input$infected_rate) * (time_range - 0)
    })
  
  recovered_prediction <- reactive({
    1 - input$susceptible_prediction - input$infected_prediction
  })
  
  
  #####Outputs
  output$susceptible <- renderText({
    print_susceptible_message <-
      paste("There are",
            susceptible_calc(),
            "people susceptible to the disease in your city.")
    
    return(print_susceptible_message)
  })
  
  # output$suscept_percent <- renderText({
  #   print_susceptible_percent <-
  #     paste("This is",
  #           susceptible_percent(),
  #           "% of the total population.")
  #
  #   return(print_susceptible_percent)
  # })
  
}

#Run application -----
shinyApp(ui, server)
