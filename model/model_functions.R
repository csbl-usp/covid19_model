library(DT)
library(ggplot2)

#' @param number_of_deaths
#' @param population
#' @param employees
#' @param lang The chosen language
#' @param output_format Either "dataframe" or "plot". Defaults to "dataframe"
#' @param fatality_rate Defaults to .0087
#' @param doubling_time Defaults to 4
#' @param days_from_infection_to_death Defaults to 17.3
#' @return
calculate_death_model <-
  function(deaths,
           population,
           employees,
           lang,
           fatality_rate = .0087,
           doubling_time = 4,
           days_from_infection_to_death = 17.3,
           output_format = "dataframe") {
    estimated_cases_that_caused_deaths <- deaths / fatality_rate
    
    number_of_times_cases_have_doubled <-
      days_from_infection_to_death / doubling_time
    
    cases_today = estimated_cases_that_caused_deaths * (2 ** number_of_times_cases_have_doubled)
    
    cases_tomorrow = cases_today * (2 ** (1 / doubling_time))
    cases_in_a_week = cases_today * (2 ** (7 / doubling_time))
    
    
    estimated_cases = round(c(cases_today, cases_tomorrow, cases_in_a_week))
    infection_rates = estimated_cases /  population
    
    # Obs: number of cases output by the model can currently be bigger than population!
    likelihoods = (1 - infection_rates) ** employees
    
    if (lang == "en"){
      dates_vector =  c("Today", "Tomorrow", "In a week")
    } else if (lang == "pt"){
      dates_vector =  c("Hoje", "Amanhã", "Em uma semana")
    }
    
    output_dataframe = data.frame(
      date = dates_vector,
      estimated_n_cases = estimated_cases ,
      likelyhood_no_infection = likelihoods
    )
    
    
    
    if (output_format == "dataframe") {
      
      if (lang == "en"){
        return(output_dataframe)
      } else if (lang == "pt"){
        colnames(output_dataframe) =  c("Data", "Casos estimados", "Chance de 0 infecções")
        return(output_dataframe)
      }
      
    }
    
    if (output_format == "plot") {
      if (lang == "en"){
        output_dataframe$date <-
          factor(output_dataframe$date,
                 levels = c("In a week", "Tomorrow", "Today"))
        
        p <-plot_estimated_cases_barplot(output_dataframe)
        
        return(p)
        
      } else if (lang == "pt"){
        output_dataframe$date <-
          factor(output_dataframe$date,
                 levels = c("Em uma semana", "Amanhã", "Hoje"))
        
        p <-plot_estimated_cases_barplot(output_dataframe) +
          xlab("Data") +
          ylab("Número de casos estimado")
        
        return(p)
        
      }
      
      return(p)
    }
    
    
    
  }


#' @param cases
#' @param population
#' @param employees
#' @param lang
#' @param average_case_progression How cases develop through time (on average).
#' @param output_format Either "dataframe" or "plot". Defaults to "dataframe"
#' @return aata.frame with model results
calculate_cases_model <-
  function(cases,
           population,
           employees,
           lang,
           average_case_progression,
           output_format = "dataframe") {
    share_of_foreign_spread <- get_proportion_of_foreign_cases(cases)
    
    cases_today = cases / share_of_foreign_spread
    
    
      # The sum of the days in the model with less than we have today.
    estimated_day_of_epidemy = sum(average_case_progression < cases_today)

    cases_today = cases_today
    
    cases_tomorrow = average_case_progression[estimated_day_of_epidemy + 1]
    
    cases_in_a_week = average_case_progression[estimated_day_of_epidemy + 7]
    
    
    estimated_cases = round(c(cases_today, cases_tomorrow, cases_in_a_week))
    infection_rates = estimated_cases /  population
    
    # Obs: number of cases output by the model can currently be bigger than population!
    likelihoods = (1 - infection_rates) ** employees
    
    if (lang == "en"){
      dates_vector =  c("Today", "Tomorrow", "In a week")
    } else if (lang == "pt"){
      dates_vector =  c("Hoje", "Amanhã", "Em uma semana")
    }
    
    output_dataframe = data.frame(
      date = dates_vector,
      estimated_n_cases = estimated_cases ,
      likelyhood_no_infection = likelihoods
    )
    
    
    
    if (output_format == "dataframe") {
      
      if (lang == "en"){
        return(output_dataframe)
      } else if (lang == "pt"){
        colnames(output_dataframe) =  c("Data", "Casos estimados", "Chance de 0 infecções")
        return(output_dataframe)
      }

    }
    
    if (output_format == "plot") {

            
      if (lang == "en"){
        output_dataframe$date <-
          factor(output_dataframe$date,
                 levels = c("In a week", "Tomorrow", "Today"))
        p <-plot_estimated_cases_barplot(output_dataframe)       
        return(p)
      } else if (lang == "pt"){
        output_dataframe$date <-
          factor(output_dataframe$date,
                 levels = c("Em uma semana","Amanhã", "Hoje"))
        p <-plot_estimated_cases_barplot(output_dataframe)+
          xlab("Data") +
          ylab("Número de casos estimado")
        return(p)
      }
      
      
      return(p)
    }
  }


plot_estimated_cases_barplot <- function(output_dataframe){
  
  p <-
    ggplot(output_dataframe, aes(y = estimated_n_cases, x = date)) +
    geom_bar(stat = 'identity', fill = "brown") +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 19),
          axis.text = element_text(size = 19)
    ) +
    ylab("Estimated number of cases") +
    xlab("Date")
  
  return(p)
}

#' @return datatable with recommendation
give_recommendation <-  function(model_table, risk_you_want_to_take, lang) {
    prob_of_at_least_one_infected = 1 - model_table$likelyhood_no_infection
    
    recomendation_level = 1
    backgroundcolor = ""
    color = "white"
    
    if (prob_of_at_least_one_infected[1] > risk_you_want_to_take) {
      recomendation_level = 4
      backgroundcolor = "red"
    } else if (prob_of_at_least_one_infected[2] > risk_you_want_to_take) {
      recomendation_level = 3
      backgroundcolor = "orange"
    } else if (prob_of_at_least_one_infected[3] > risk_you_want_to_take) {
      recomendation_level = 2
      backgroundcolor = "orange"
    } else {
      backgroundcolor = ""
      color = "black"
    }
    
    recommendations <- list()
    recommendations[["en"]] <- c("No specific reccommendation","Close within a week!", "Close before tomorrow!", "Close immediately!")
    recommendations[["pt"]] <- c("Nenhuma recomendação específica","Feche em até uma semana!", "Feche antes de amanhã!", "Feche imediatamente!")

    rec_name <- list()
    rec_name[["en"]] <- "Recommendation:"
    rec_name[["pt"]] <- "Recomendação:"  
    output_df = data.frame("Recommendation" = paste0("<b>",recommendations[[lang]][recomendation_level], "</b>"))
    colnames(output_df) <- rec_name[[lang]]
    
    todo <- list()
    todo[["en"]] <- "What to do:"
    todo[["pt"]] <- "O que fazer:"   
    rownames(output_df) <- c(todo[[lang]])
    
    
    output_dt = datatable(output_df, options = list(dom = 't'), escape = FALSE) %>% formatStyle(1,
                                                                                color = color,
                                                                                backgroundColor = backgroundcolor
                                                                                )
    return(output_dt)
  }


estimate_cases_by_deaths <-
  function(deaths,
           fatality_rate,
           doubling_time,
           days_from_infection_to_death) {
    estimated_cases_that_caused_deaths <- deaths / fatality_rate
    
    number_of_times_cases_have_doubled <-
      days_from_infection_to_death / doubling_time
    
    cases_today = estimated_cases_that_caused_deaths * (2 ** number_of_times_cases_have_doubled)
    
    return(round(cases_today))
    
  }



get_proportion_of_foreign_cases <- function(cases) {
  internal_cases_proportion <-
    c(
      0,
      0,
      0,
      0,
      0,
      0.166666666666667,
      0.142857142857143,
      0.125,
      0.222222222222222,
      0.3,
      0.363636363636364,
      0.416666666666667,
      0.384615384615385,
      0.428571428571429,
      0.4,
      0.4375,
      0.470588235294118,
      0.5,
      0.526315789473684,
      0.55,
      0.571428571428571,
      0.590909090909091,
      0.565217391304348,
      0.541666666666667,
      0.56,
      0.576923076923077,
      0.592592592592593,
      0.571428571428571,
      0.586206896551724,
      0.6,
      0.612903225806452,
      0.625,
      0.636363636363636,
      0.647058823529412,
      0.657142857142857,
      0.666666666666667,
      0.675675675675676,
      0.68421052631579,
      0.692307692307692,
      0.7,
      0.707317073170732,
      0.714285714285714,
      0.72093023255814,
      0.727272727272727,
      0.733333333333333,
      0.739130434782609,
      0.74468085106383,
      0.75,
      0.755102040816326,
      0.76,
      0.764705882352941,
      0.769230769230769,
      0.773584905660377,
      0.777777777777778,
      0.781818181818182,
      0.785714285714286,
      0.789473684210526,
      0.793103448275862,
      0.796610169491525,
      0.8,
      0.80327868852459,
      0.806451612903226,
      0.80952380952381,
      0.8125,
      0.815384615384615,
      0.818181818181818,
      0.82089552238806,
      0.823529411764706,
      0.826086956521739,
      0.828571428571429,
      0.830985915492958,
      0.833333333333333,
      0.835616438356164,
      0.837837837837838,
      0.84,
      0.842105263157895,
      0.844155844155844,
      0.846153846153846,
      0.848101265822785,
      0.85,
      0.851851851851852,
      0.853658536585366,
      0.855421686746988,
      0.857142857142857,
      0.858823529411765,
      0.86046511627907,
      0.862068965517241,
      0.863636363636364,
      0.865168539325843,
      0.866666666666667,
      0.868131868131868,
      0.869565217391304,
      0.870967741935484,
      0.872340425531915,
      0.873684210526316,
      0.875,
      0.876288659793814,
      0.877551020408163,
      0.878787878787879,
      0.88,
      0.881188118811881,
      0.882352941176471,
      0.883495145631068,
      0.884615384615385,
      0.885714285714286,
      0.886792452830189,
      0.88785046728972,
      0.888888888888889,
      0.889908256880734,
      0.890909090909091,
      0.891891891891892,
      0.892857142857143,
      0.893805309734513,
      0.894736842105263,
      0.895652173913044,
      0.896551724137931,
      0.897435897435897,
      0.898305084745763,
      0.899159663865546,
      0.9,
      0.900826446280992,
      0.901639344262295,
      0.902439024390244,
      0.903225806451613,
      0.904,
      0.904761904761905,
      0.905511811023622,
      0.90625,
      0.906976744186046,
      0.907692307692308,
      0.908396946564885,
      0.909090909090909,
      0.909774436090225,
      0.91044776119403,
      0.911111111111111,
      0.911764705882353,
      0.912408759124088,
      0.91304347826087,
      0.913669064748201,
      0.914285714285714,
      0.914893617021277,
      0.915492957746479,
      0.916083916083916,
      0.916666666666667,
      0.917241379310345,
      0.917808219178082,
      0.918367346938775,
      0.918918918918919,
      0.919463087248322,
      0.92,
      0.920529801324503,
      0.921052631578947,
      0.92156862745098,
      0.922077922077922,
      0.92258064516129,
      0.923076923076923,
      0.923566878980892,
      0.924050632911392,
      0.924528301886793,
      0.925,
      0.925465838509317,
      0.925925925925926,
      0.926380368098159,
      0.926829268292683,
      0.927272727272727,
      0.927710843373494,
      0.92814371257485,
      0.928571428571429,
      0.928994082840237,
      0.929411764705882,
      0.929824561403509,
      0.930232558139535,
      0.930635838150289,
      0.931034482758621,
      0.931428571428571,
      0.931818181818182,
      0.932203389830508,
      0.932584269662921,
      0.932960893854749,
      0.99
    )
  
  
  if (cases < length(internal_cases_proportion)) {
    return(1 - internal_cases_proportion[cases])
  } else {
    return(0.01)
  }
  
}

load("translation.bin")

tr <- function(input, text){ # translates text into current language
  sapply(text,function(s) translation[[s]][[input$lang]], USE.NAMES=FALSE)
}
