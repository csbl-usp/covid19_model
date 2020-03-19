library(readxl)
setwd("~/covid19_model/modelo_george")
time_cumulativo <- read_excel("time_series_19-covid_March12_v2xls.xls",
                              sheet = "Confirmed_cases")





parsItaly <- c(0.1001,1.0882,1.3197,2.0000,25.4713,50.0000)
parsIran <- c(0.1001  ,  1.0841   , 1.3151  ,  2.0000 ,  25.4713  , 50.0000)
parsHubei <- c(0.4135 ,   1.1003 ,   0.5005 ,   2.0000 ,  25.4655 ,  50.0000)
parsKorea <- c(0.1000 ,   1.2552 ,   1.0990 ,   2.0000 ,  25.4710 ,  50.0000)

locations <- c("pars1","pars2","pars3","pars4","pars5","pars6")
locDF <- data.frame(parsItaly,parsHubei,parsKorea,parsIran)
#locDF <- t(locDF)
#colnames(locDF) <- locations
rownames(locDF) <- locations
write.csv(x=locDF,file = "model_parameters_by_region.tsv",sep = "\t")

data <- time_cumulativo[102,5:54] #Italy
country <- 'Italia5M'
TotalPop <- 5*10^6
# SAO PAULO POPULACAO
#TotalPop <- 12.18*10^6
InitCases <- 0.1
Ro3 <- 2

parsguess <- c(0.1, 1, 2, Ro3, 0, 50)
lb <- c(0.1, 1, 0.5, Ro3, 1, 50)
ub <- c(0.7, 5, 4, Ro3, 50, 50)

N = length(data)

#Para ser usado na interpolação Rdata2
#x = c(1:N)

Rdata <- as.numeric((data[1, 1:50])) / TotalPop

Si = (TotalPop-InitCases)/TotalPop
Ii = InitCases/TotalPop
x <- seq(from = 0.01, to = N, by = 0.01)


########################################
########### VERY SPECIAL FUNCTION #####
#######################################
FunLoopSIR3RoSIR <- function(Si, Ii, gamma,Ro1, Ro2,Ro3, iquarant,durationq, x){
  S = c()
  I = c()
  R = c()
  beta1 <- Ro1*gamma;    beta2 <- Ro2*gamma; beta3 <- Ro3*gamma
  N <- length(x)
  S[1] <- Si;  I[1] <- Ii;  R[1] <- 1 - S[1] - I[1]
  for (i in 1:(N-1)){
    dt <- 10^-1
    S[i+1] <- S[i] -  beta1*S[i]*I[i]*dt
    I[i+1] <- 1 - S[i+1] - R[i]
    R[i+1] <- R[i] + gamma*I[i]*dt
    if (i  > iquarant*(1/0.01)) {
      S[i+1] <- S[i] -  beta2*S[i]*I[i]*dt
      I[i+1] <- 1 - S[i+1] - R[i]
      R[i+1] <- R[i] + gamma*I[i]*dt
    }
    if (i  > (iquarant+durationq)*(1/0.01)){
      S[i+1] <- S[i] -  beta3*S[i]*I[i]*dt
      I[i+1] <- 1 - S[i+1] - R[i]
      R[i+1] <- R[i] + gamma*I[i]*dt
    }
  }

  dataframe <- data.frame(col1=S, col2 = I, col3 = R)
  colnames(dataframe) = c("S","I","R")
  return(dataframe)
}


dataframe = FunLoopSIR3RoSIR(1, 100/10^7, parsguess[1], parsguess[2], parsguess[3], parsguess[3], parsguess[5], parsguess[6], x);


############# PLOT DATA #############################

plot(x, 100 * dataframe$R,'l', col="red",
     ylab ="Cumulativo dos casos (R) (% populacao)",
     xlab = "% Dias desde o inicio da contagem",
     main = 'Sao Paulo de acordo com Italia');

plot( x,100 * dataframe$I,'l', col="blue",
      ylab = "% População Doente Simultaneamente",
      xlab = "% Dias desde o inicio da contagem",
      main = 'Sao Paulo de acordo com Italia'
      )
