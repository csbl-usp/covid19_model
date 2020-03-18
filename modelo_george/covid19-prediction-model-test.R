#####covid19-prediction-model-test -----

#Parameters
alfa1 <- -257.580667091408 #same
alfa2 <- 67698.802805002
beta <- 0.4375 #beta = gama*R0
gama <- 0.15625 
R0 <- 2.8 #beta/gama #maybe would be defining by city
R0_min <- 1.02 #1.2
x0 <- 20
dx <- 4

#Informação usada para o plotagem do modelo#
time_range <- seq(from = 0, to = 50, by = 0.1)
time_range_test <- as.data.frame(time_range)
time_range_test$R0_effective <- 0
time_range_test$infected_prediction <- 0
time_range_test$pop_fraction <- 0

############For the user to input data -------
total_pop <- 9000000 #Popoulation #HUBEI
infected <- 440 #HUBEI
recovered <- 0 #recuperados #HUBEI
quarantine_gov <- 15 
time_start_quarantine_pop <- 10

#Previous calculations
susceptible_calc <- total_pop - infected - recovered
susceptible_rate <- (total_pop - infected - recovered) / total_pop
infected_rate <- infected / total_pop
recovered_rate <- 1 - infected_rate - susceptible_rate

###################### infection
#########################################################
for (i in 1:dim(time_range_test)[1]) {
  #R0 effective
  time_range_test[i, 2] <-
    if (time_range_test[i, 1] > quarantine_gov) {
      R0_min + R0 * exp(-time_range_test[i, 1] / time_start_quarantine_pop)
    } else {
      R0
    }
  #Infected prediction
  {
    time_range_test[i, 3] <-
      infected_rate + (gama * {
        #Re-calculating Ro_effective
        if (time_range_test[i, 1] > quarantine_gov) {
          R0_min + R0 * exp(-time_range_test[i, 1] / time_start_quarantine_pop)
        } else {
          R0
        }
      } * susceptible_rate * infected_rate - gama * infected_rate) * (time_range_test[i, 1] - 0.1)
  }
  #hubei pop_fraction
  {
    time_range_test[i, 4] <-
      ((alfa1 - alfa2) / (1 + exp((time_range_test[i, 1] - x0) / dx)) + alfa2) / total_pop
  }
}


##############PESSOAL ESSAS SÃO AS FÓRMULAS QUE ENCONTREI NA TABELA DO GEORGE 
##########JUNTEI ALGUMAS DELAS NO FOR APLICADO ACIMA! QUE CONTEM AS INFORMAÇÕES PARA A PLOTAGEM DOS GRÁFICOS TAMBÉM ESTÃO NA TABELA
#############R0_effective -----
  R0_effective <- 
    if (time_range_test[i, 1] > quarantine_gov) {
      R0_min + R0 * exp(-time_range_test[i, 1] / time_start_quarantine_pop)
    } else {
      R0
    }

#############Susceptible_prediction -----
  susceptible_prediction <-
      susceptible_rate - gama * R0_effective * susceptible_rate * infected_rate * (time_range_test[i, 1] - 0.1)

#############Infected_prediction -----      
  #Informação usada para o plotagem do modelo#
  infected_prediction <-
      #put this information on the df time_range_test in infected_prediction column
    infected_rate + (gama * R0_effective * susceptible_rate * infected_rate - gama * infected_rate) * (time_range_test[i, 1] - 0.1)

#############Hubei -----  
  hubei <-
      (alfa1 - alfa2) / (1 + exp((time_range_test[i, 1] - x0) / dx)) + alfa2 #it's calculated a little bit different than the excel
    hubei

#############Recovered_prediction -----
  recovered_prediction <-
  1 - susceptible_prediction - infected_prediction

############Hubei_pop_fraction -----  
#Informação usada para o plotagem do modelo#
hubei_pop_fraction <- hubei[1] / total_pop

    
    
par(mfrow=c(1,1))
#R0 effective
plot(R0_effective~time_range, 
     data = time_range_test, 
     ylim=c(0,3), 
     col = 'blue',
     main = "Ro Effective",
     cex.main = 2)
#abline(time_range_test$R0_effective~time_range_test$time_range, col = "red")

# install.packages("plotly")
library(plotly)

#defining order
time_range_test$time_range <- factor(time_range_test$time_range, levels = time_range_test[["time_range"]])

fig1 <- plot_ly(time_range_test, x = ~time_range, y = ~infected_prediction, name = 'Prediction of infected people', type = 'scatter', mode = 'lines',
               line = list(color = 'rgb(205, 12, 24)', width = 4))
fig1 <- fig1 %>% add_trace(y = ~pop_fraction, name = 'Fração populacional de Hubei', line = list(color = 'rgb(22, 96, 167)', width = 4))
fig1 <- fig1 %>% layout(title = "Infected e Hubei_pop_fraction",
                        xaxis = list(title = "Infected_prediction"),
                        yaxis = list(title = "Fração populacional de Hubei"))

fig1
fig1=NULL
