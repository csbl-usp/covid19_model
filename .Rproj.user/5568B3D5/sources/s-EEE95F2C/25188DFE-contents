



#' @param number_of_deaths
#' @param population
#' @param employees
#' @param fatality_rate Defaults to .0087
#' @param doubling_time Defaults to 4
#' @param days_from_infection_to_death Defaults to 17.3
#' @return 

calculate_death_model <- function(deaths, population, employees, fatality_rate = .0087,  doubling_time = 4, days_from_infection_to_death = 17.3){

    estimated_cases_that_caused_deaths <- deaths / fatality_rate
  
  number_of_times_cases_have_doubled <-
    days_from_infection_to_death / doubling_time
  
  cases_today = estimated_cases_that_caused_deaths * (2 ** number_of_times_cases_have_doubled)
  
  cases_tomorrow = cases_today * (2 ** (1/doubling_time)  )
  cases_in_a_week = cases_today * (2 ** (7/doubling_time)  )
  
  
  estimated_cases = c( cases_today, cases_tomorrow, cases_in_a_week)
  infection_rates = estimated_cases /  population
  
  # Obs: number of cases output by the model can currently be bigger than population!
  likelihoods = (1 - infection_rates) ** employees
  
  output_dataframe = data.frame(date = c("today", "tomorrow", "in a week"),
                                estimated_n_cases = estimated_cases , 
                                infection_rate = infection_rates,
                                likelyhood_no_infection = likelihoods)
  
  return(output_dataframe)
  
}