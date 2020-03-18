library(readxl)

time_cumulativo <- read_excel("matlab-george-scripts/time_series_19-covid_March12_v2xls.xls", 
                              sheet = "Confirmed_cases")
data <- time_cumulativo[17,5:54] #Italy
country <- 'Italia5M'
TotalPop <- 5*10^6
InitCases <- 0.1
Ro3 <- 2

parsguess <- c(0.1, 1, 2, Ro3, 0, 50) 
lb <- c(0.1, 1, 0.5, Ro3, 1, 50)
ub <- c(0.7, 5, 4, Ro3, 50, 50)

N = length(data)

#Para ser usado na interpolação Rdata2
x = c(1:N)

Rdata <- as.numeric((data[1, 1:50])) / TotalPop

Si = (TotalPop-InitCases)/TotalPop
Ii = InitCases/TotalPop
# Rdata2 = interp1(x, Rdata, 0.01:0.01:N,'spline') 
# f = @(pars,x) FunLoopSIR3Ro(Si,Ii,pars(1), pars(2), pars(3),pars(4),pars(5),pars(6), x)

x <- seq(from = 0.01, to = N, by = 0.01)
# x = 0.01:0.01:N

# y = Rdata2

# options = optimoptions('lsqcurvefit','Display','iter','Algorithm', 'levenberg-marquardt','FiniteDifferenceType',...
                       # 'central', 'MaxFunctionEvaluations', 1000, 'StepTolerance', 10^-8)

# [pars,resnorm,residual,~,output,~,J] =lsqcurvefit(f,parsguess,x,y,lb,ub,options);
# %x = lsqcurvefit(fun,x0,xdata,ydata,lb,ub)
# %[pars, resid, J] = nlinfit(x,y,f,parsguess,opts);
# 
# J=full(J);
# alpha = 0.2; % this is for 95% confidence intervals
# pars_ci = nlparci(pars,residual,'jacobian',J,'alpha',alpha);
# uncertainty=(pars_ci(:,2)-pars_ci(:,1))/2;
# 
# R_fit=f(pars,x); 
# sigma_fitUP = norm(R_fit - f(pars+uncertainty',x) );
#                              sigma_fitDN = norm(R_fit - f(pars-uncertainty',x) );
# 
# figure
# steps =100;
# %plot(x,y,'k', x, R_fit,'b', x, R_fit-sigma_fitUP,'m-',x, R_fit+sigma_fitUP,'m-' );%, x,R_fit - sigma_fitDN)
# plot(downsample(x,steps),100*Rdata,'*k', x, 100*R_fit,'b' );%, x,R_fit - sigma_fitDN)
# xlabel("Dias desde o inicio da contagem","fontweight","bold");
# ylabel("Cumulativo dos casos (R% populacao)","fontweight","bold");
# txt = strcat('    ', num2str( [pars(1)]), ';    ',num2str([ pars(2),pars(3), pars(4),    pars(6) ],   '%.2g') , ' ' );
# 
# title({[strcat('Regi?o= ',country), ' DiasDoenteInfeccioso= ', num2str(1/pars(1),'%.2g')],...
#   [strcat(...
#           'Ro= ', num2str(pars(2),'%.2g' ),...
#           '  DiaDaAtitude= ', num2str(pars(4)-1/pars(1),'%.2g' ),...
#           '  Novo Ro = ', num2str(pars(3),'%.2g' )...
#   )...
#     ]});
# 
# % [S I R]= FunSIRLoopSIR(Si, Ii,0.1,  2,   2,   10, x);
# % figure
# % plot(x,S,'b',x,I,'r',x,R,'g');
# pars
# 'gamma,     Ro1,       Ro2,      Ro3,   iquarant,  durationq'
# 
# %%
#   x = 0.01:0.01:120;
# figure
# %Rout= FunLoopSIR3Ro(Si, Ii, gamma,Ro1, Ro2,Ro3, iquarant,durationq, x)
# % pars(1) = 1/14;
# % pars(2) = 2;
# % pars(3)  = 2;
# % pars(5)  = 120;
# % pars(6)  = 0;

FunLoopSIR3RoSIR <- function(Si, Ii, gamma,Ro1, Ro2,Ro3, iquarant,durationq, x){
  S = c()
  I = c()
  R = c()
  beta1 <- Ro1*gamma;    beta2 <- Ro2*gamma; beta3 <- Ro3*gamma
  N <- length(x)
  S[1] <- Si;  I[1] <- Ii;  R[1] <- 1 - S[1] - I[1]
  for (i in 1:N-1){
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
  # Rout <- downsample(R,steps)
  
  dataframe <- data.frame(col1=S, col2 = I, col3 = R)  
  return(dataframe)
}

dataframe = FunLoopSIR3RoSIR(1, 100/10^7, parsguess[1], parsguess[2], parsguess[3], parsguess[3], parsguess[5], parsguess[6], x);

yyaxis left
plot(x, 100 * R,'b');
ylim([0 inf]);
ylabel("Cumulativo dos casos (R) (% populacao)","fontweight","bold");

yyaxis right
plot( x,100*I,'r');
ylim([0 inf]);

ylabel("% Popula??o Doente Simultaneamente","fontweight","bold");
xlabel("% Dias desde o in?cio da contagem","fontweight","bold");

txt = strcat('    ', num2str( [pars(1)]), ';    ',num2str([ pars(2),pars(3), pars(4),    pars(6) ],   '%.2g') , ' ' );

title({[strcat('SaoPaulo de acordo com = ',country), ' DiasDoenteInfeccioso= ', num2str(1/pars(1),'%.2g')],...
  [strcat(...
          'Ro= ', num2str(pars(2),'%.2g' ),...
          '  DiaDaAtitude=  ', num2str(pars(4)-1/pars(1),'%.2g' ),...
          '  Novo Ro = ', num2str(pars(3),'%.2g' )...
  )...
    ]});




