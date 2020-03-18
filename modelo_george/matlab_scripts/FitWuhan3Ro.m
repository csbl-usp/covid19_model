%% Adjusts R from modified SIR differential Eqs. to historial epidemiological data
% Approximation: the historical cumulative number of infected is represented by R
% of the SIR model. The approximation is plausible because cases are
% identified closer to the moment the infected becomes recovered than to
% the onset of the infection.
% 
%Written by George C. Cardoso, GCC@USP.BR
%March, 15 2020

%Current Issues: convergence of the fitting parameters Ro, gamma, etc.
%The fitting quality has to be visually inspected and the parameters's seed
%must be reset to plausible values. The process is repeated until an acception fit is obtained. 

%FUNCTIONS NEEDED
%FunLoopSIR3Ro.m    to adjust the parameters of the diff. eqs to  "Recovered" Rdata
%FunLoopSIR3RoSIR.m  to use the parameters found in the fit for predictions


close all; clear all;

filename = '/home/icaro/covid19_model/time_series_19-covid_March12_v2xls.xls'
%data=xlsread(filename,'Confirmed_cases','E158:BB158'); %Wuhan [1/10  2  1  2  11 50]!
data=xlsread(filename,'Confirmed_cases','E18:BB18'); %Italy Modelworks!
%data=xlsread(filename,'Confirmed_cases','E13:BB13'); %Germany? Exogenous?
%data=xlsread(filename,'Confirmed_cases','E161:BB161'); %=France?
%Exogenous?
%data=xlsread(filename,'Confirmed_cases','E162:BB162');%Guangdong/Guanzhou56M' 0.1 2 2  2  10 50]; Io = 26 !!
%data=xlsread(filename,'Confirmed_cases','AN37:BB37'); %Brazil ? IMPORTED CASES. NOT WORKING
%data=xlsread(filename,'Confirmed_cases','E2:BB2');%Thailand ?? 
%data=xlsread(filename,'Confirmed_cases','E3:BB3'); %Japan +/-
%data=xlsread(filename,'Confirmed_cases','E160:BB160');%Korea/Daegu2.5M !
%data=xlsread(filename,'Confirmed_cases','E159:BB159');%Iran/Teeran9M !
country ='Italia5M';
%% 
TotalPop = 5*10^6; % Region/Country total population (see wikipedia, use common sense)
InitCases = 0.1;     % Actual value or if shown as zero in excel spreasheet, values < are allowed. 
Ro3 = 2 % Not used -- would be the end of the emergency behavior/ return to higher Ro
%[gamma,Ro1, Ro2,Ro3, iquarant,durationq 
parsguess =... %Parameters's guesses. Need tweaking otherwise fit will not be good.
     [0.1   1       2         Ro3     0   50]; % tentative parameters
lb = [0.1,  1 ,      0.5,     Ro3,    1,  50];  %lower bounds 
ub = [0.7,   5 ,      4 ,      Ro3,   50,  50 ]; %upper bounds


 close all; 
 N = length(data);
 x = 1:N;

Rdata = data/TotalPop;
Si = (TotalPop-InitCases)/TotalPop; Ii = InitCases/TotalPop; 
Rdata2 = interp1(x,Rdata,0.01:0.01:N,'spline');


f = @(pars,x) FunLoopSIR3Ro(Si,Ii,pars(1), pars(2), pars(3),pars(4),pars(5),pars(6), x)  ; 
%Rout= FunLoopSIR3Ro(Si, Ii, gamma,Ro1, Ro2,Ro3, iquarant,durationq, x)
x = 0.01:0.01:N;
%y = f(parsguess,x)+ normrnd(1,1,[1 5000])/10^2; %teste da funcao com ruido pra fitar garantidamente
y = Rdata2;

options = optimoptions('lsqcurvefit','Display','iter','Algorithm', 'levenberg-marquardt','FiniteDifferenceType',...
                        'central', 'MaxFunctionEvaluations', 1000, 'StepTolerance', 10^-8);
[pars,resnorm,residual,~,output,~,J] =lsqcurvefit(f,parsguess,x,y,lb,ub,options);
%x = lsqcurvefit(fun,x0,xdata,ydata,lb,ub)
%[pars, resid, J] = nlinfit(x,y,f,parsguess,opts);

J=full(J);
alpha = 0.2; % this is for 95% confidence intervals
pars_ci = nlparci(pars,residual,'jacobian',J,'alpha',alpha);
uncertainty=(pars_ci(:,2)-pars_ci(:,1))/2;

R_fit=f(pars,x); 
sigma_fitUP = norm(R_fit - f(pars+uncertainty',x) );
sigma_fitDN = norm(R_fit - f(pars-uncertainty',x) );

figure
steps =100;
%plot(x,y,'k', x, R_fit,'b', x, R_fit-sigma_fitUP,'m-',x, R_fit+sigma_fitUP,'m-' );%, x,R_fit - sigma_fitDN)
plot(downsample(x,steps),100*Rdata,'*k', x, 100*R_fit,'b' );%, x,R_fit - sigma_fitDN)
 xlabel("Dias desde o inicio da contagem","fontweight","bold");
 ylabel("Cumulativo dos casos (R% populacao)","fontweight","bold");
    txt = strcat('    ', num2str( [pars(1)]), ';    ',num2str([ pars(2),pars(3), pars(4),    pars(6) ],   '%.2g') , ' ' );
 
 title({[strcat('Regi�o= ',country), ' DiasDoenteInfeccioso= ', num2str(1/pars(1),'%.2g')],...
        [strcat(...
                'Ro= ', num2str(pars(2),'%.2g' ),...
                '  DiaDaAtitude= ', num2str(pars(4)-1/pars(1),'%.2g' ),...
                '  Novo Ro = ', num2str(pars(3),'%.2g' )...
                )...
         ]});

% [S I R]= FunSIRLoopSIR(Si, Ii,0.1,  2,   2,   10, x);
% figure
% plot(x,S,'b',x,I,'r',x,R,'g');
pars
'gamma,     Ro1,       Ro2,      Ro3,   iquarant,  durationq'

%%
x = 0.01:0.01:120;
figure
%Rout= FunLoopSIR3Ro(Si, Ii, gamma,Ro1, Ro2,Ro3, iquarant,durationq, x)
% pars(1) = 1/14;
% pars(2) = 2;
% pars(3)  = 2;
% pars(5)  = 120;
% pars(6)  = 0;
[S I R] = FunLoopSIR3RoSIR(1, 100/10^7, pars(1),pars(2), pars(3), pars(3), pars(5),pars(6), x);

yyaxis left
plot(x,100*R,'b');
ylim([0 inf]);
ylabel("Cumulativo dos casos (R) (% populacao)","fontweight","bold");
 
yyaxis right
plot( x,100*I,'r');
ylim([0 inf]);

ylabel("% Popula��o Doente Simultaneamente","fontweight","bold");
xlabel("% Dias desde o in�cio da contagem","fontweight","bold");

    txt = strcat('    ', num2str( [pars(1)]), ';    ',num2str([ pars(2),pars(3), pars(4),    pars(6) ],   '%.2g') , ' ' );
 
 title({[strcat('SaoPaulo de acordo com = ',country), ' DiasDoenteInfeccioso= ', num2str(1/pars(1),'%.2g')],...
        [strcat(...
                'Ro= ', num2str(pars(2),'%.2g' ),...
                '  DiaDaAtitude=  ', num2str(pars(4)-1/pars(1),'%.2g' ),...
                '  Novo Ro = ', num2str(pars(3),'%.2g' )...
                )...
         ]});


% Ro1 = 2.8;  Ro2 = 0.5; gamma = 1/3;
% Ii = 444/10^7;  Si=1-444/10^7;  tq = 9;  MaxTime=50;
%pars = [3,1,0.1,444/10^7,1,15];
%Ii = 444/10^7;
%[t,S,I,R]=FunSIR(3,1,0.1,Ii,1,15, 50);
% Recov = FunSIRLoop(1-10^-3, 10^-3, 0.1,1.1,50);
% %FunSIRLoop(Si, Ii, gamma,Ro,N)
% plot(Recov,'o');

% plot(t,I,'-r',t,R,'-b', t, data/10^7,'o');
% ylim([-0.02 max(R)+0.02])
% xlabel("Days","fontweight","bold")
% ylabel("%","fontweight","bold")
% legend('I', 'R','Cummulative for Wuhan');
% 
% txt = strcat(' \rightarrow Peak Percentage Infected =   ', num2str(max(I),   '%.2g' ), ' %' );
% text(5,-0.01,txt, 'FontSize',14)








