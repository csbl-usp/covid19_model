library(shiny)

#Defining UI ------
ui <- fluidPage(titlePanel("Covid-19 Prediction Model"),
                
                sidebarLayout(                   #Total population
                  sidebarPanel(
                    numericInput(
                      "total_pop",
                      "Total population",
                      value = 21571281,
                      min = 1
                    ),
                    numericInput(                #Infected number
                      "infected",
                      "Number of infected",
                      value = 151,
                      min = 1
                    ),
                    numericInput(                #Recovered
                      "recovered",
                      "Number of recovered",
                      value = 0,
                      min = 1
                    )
                  ),
                  
                  mainPanel(textOutput("susceptible"),          #Print susceptibles
                            textOutput("suscept_percent"))
                ))

#Defining server logic -----
server <- function(input, output, session) {
  susceptible_calc <- reactive({                                #Number of susceptibles
    input$total_pop - input$infected - input$recovered
  })
  
  susceptible_percent <- reactive({                              #Rate of susceptibles
   ((input$total_pop - input$infected - input$recovered) / input$total_pop)
  })
  
  infected_percent <- reactive({                              #Rate of infected
    
  })
  
  output$susceptible <- renderText({
    print_susceptible_message <-
      paste("There are",
            susceptible_calc(),
            "people susceptible to the disease in your city.")
    
    return(print_susceptible_message)
  })
  
  output$suscept_percent <- renderText({
    print_susceptible_percent <-
      paste("This is",
            susceptible_percent(),
            "% of the total population.")

    return(print_susceptible_percent)
  })
  
}

#Run application -----
shinyApp(ui, server)
