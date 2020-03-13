library(DT)


#' @param number_of_deaths
#' @param population
#' @param employees
#' @param fatality_rate Defaults to .0087
#' @param doubling_time Defaults to 4
#' @param days_from_infection_to_death Defaults to 17.3
#' @return

calculate_death_model <-
  function(deaths,
           population,
           employees,
           fatality_rate = .0087,
           doubling_time = 4,
           days_from_infection_to_death = 17.3) {
    estimated_cases_that_caused_deaths <- deaths / fatality_rate
    
    number_of_times_cases_have_doubled <-
      days_from_infection_to_death / doubling_time
    
    cases_today = estimated_cases_that_caused_deaths * (2 ** number_of_times_cases_have_doubled)
    
    cases_tomorrow = cases_today * (2 ** (1 / doubling_time))
    cases_in_a_week = cases_today * (2 ** (7 / doubling_time))
    
    
    estimated_cases = c(cases_today, cases_tomorrow, cases_in_a_week)
    infection_rates = estimated_cases /  population
    
    # Obs: number of cases output by the model can currently be bigger than population!
    likelihoods = (1 - infection_rates) ** employees
    
    output_dataframe = data.frame(
      date = c("today", "tomorrow", "in a week"),
      estimated_n_cases = estimated_cases ,
      infection_rate = infection_rates,
      likelyhood_no_infection = likelihoods
    )
    
    return(output_dataframe)
    
  }


#' @param number_of_deaths
#' @param population
#' @param employees
#' @param community_external_estimate Estimate of how many cases are local and how many are external.
#' @param average_case_progression How cases develop through time (on average).
#' @return aata.frame with model results
calculate_cases_model <-
  function(cases,
           population,
           employees,
           community_external_estimate,
           average_case_progression) {
    
    
    if (cases < ncol(community_external_estimate)){
      share_of_foreign_spread = as.numeric(community_external_estimate[5,cases+1])
    }
    
    cases_today = cases/share_of_foreign_spread
    print(cases_today)
    

    
    
    # The sum of the days in the model with less than we have today.
    estimated_day_of_epidemy = sum(average_case_progression < cases_today)
    
    average_case_progression
    
    
    
    
    cases_today = cases_today
    
    cases_tomorrow = average_case_progression[estimated_day_of_epidemy + 1]
    
    cases_in_a_week = average_case_progression[estimated_day_of_epidemy + 7]
    
    
    estimated_cases = c(cases_today, cases_tomorrow, cases_in_a_week)
    infection_rates = estimated_cases /  population
    
    # Obs: number of cases output by the model can currently be bigger than population!
    likelihoods = (1 - infection_rates) ** employees
    
    output_dataframe = data.frame(
      date = c("today", "tomorrow", "in a week"),
      estimated_n_cases = estimated_cases ,
      infection_rate = infection_rates,
      likelyhood_no_infection = likelihoods
    )
    
    return(output_dataframe)
    
  }

#' @return datatable with recommendation
give_recommendation <- function(model_table, risk_you_want_to_take) {
  
  prob_of_at_least_one_infected = 1 - model_table$likelyhood_no_infection
  
  recomendation = ""
  color = ""

  if (prob_of_at_least_one_infected[1] > risk_you_want_to_take) {
    recomendation = "Close immediately!"
    color = "red"
  } else if (prob_of_at_least_one_infected[2] > risk_you_want_to_take) {
    recomendation = "Close before tomorrow!"
    color = "orange"
  } else if (prob_of_at_least_one_infected[3] > risk_you_want_to_take) {
    recomendation = "Close within a week!"
    color = "orange"
  } else {
    recomendation = "No specific recommendation"
    color = ""
  }
  
  output_df = data.frame("Recommendation" = recomendation )
  
  output_dt = datatable(output_df) %>% formatStyle(1,backgroundColor = color)
  return(output_dt)
}


estimate_cases_by_deaths <- function(deaths, fatality_rate, doubling_time, days_from_infection_to_death){
  
  estimated_cases_that_caused_deaths <- deaths / fatality_rate
  
  number_of_times_cases_have_doubled <-
    days_from_infection_to_death / doubling_time
  
  cases_today = estimated_cases_that_caused_deaths * (2 ** number_of_times_cases_have_doubled)
  
  return(round(cases_today))
  
}