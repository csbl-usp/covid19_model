library(readxl)
setwd("~/covid19_model/modelo_george")
time_cumulativo <- read_excel("time_series_19-covid_March12_v2xls.xls",
                              sheet = "Confirmed_cases")


data <- time_cumulativo[102,5:54] #Italy
country <- 'Italia5M'
TotalPop <- 5*10^6
# SAO PAULO POPULACAO
#TotalPop <- 12.18*10^6
InitCases <- 0.1
Ro3 <- 2


####### SCRIPT ANTIGO
parsguess <- c(0.1, 1, 2, Ro3, 0, 50)
lb <- c(0.1, 1, 0.5, Ro3, 1, 50)
ub <- c(0.7, 5, 4, Ro3, 50, 5Q0)

N = length(data)

#Para ser usado na interpolação Rdata2
#x = c(1:N)

Rdata <- as.numeric((data[1, 1:50])) / TotalPop

Si = (TotalPop-InitCases)/TotalPop
Ii = InitCases/TotalPop
x <- seq(from = 0.01, to = 120, by = 0.01)


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

parsguess <- c(0.1001, 1.0882, 1.3197, 2.0000, 25.4713, 50.0000)
dataframe = FunLoopSIR3RoSIR(0.999, 440/10^7, parsguess[1], parsguess[2], parsguess[3], parsguess[3], parsguess[5], parsguess[6], x);


############# PLOT DATA #############################
par(mfrow = c(1,1))

plot(x, 100 * dataframe$R,'l', col="blue",
     ylab ="Cumulativo dos casos (R) (% populacao)",
     xlab = "% Dias desde o inicio da contagem",
     main = 'Sao Paulo de acordo com Italia');
par(new=TRUE)
plot(x, 100 * dataframe$I,'l', col="red",
      ylab = "% População Doente Simultaneamente",
      xlab = "% Dias desde o inicio da contagem",
      main = 'Sao Paulo de acordo com Italia'
      )
