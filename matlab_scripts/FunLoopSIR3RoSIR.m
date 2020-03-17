function [S I R]= FunLoopSIR3RoSIR(Si, Ii, gamma,Ro1, Ro2,Ro3, iquarant,durationq, x)

beta1 = Ro1*gamma;    beta2 = Ro2*gamma; beta3 = Ro3*gamma;
N = length(x);
S(1) = Si;  I(1) = Ii;  R(1) = 1 - S(1) - I(1);

for i = 1:N-1
    dt = 10^-1;
    S(i+1) = S(i) -  beta1*S(i)*I(i)*dt;
    I(i+1) = 1 - S(i+1) - R(i) ;
    R(i+1) = R(i) + gamma*I(i)*dt; 
    
 
    if i  > iquarant*(1/0.01)
      S(i+1) = S(i) -  beta2*S(i)*I(i)*dt;
      I(i+1) = 1 - S(i+1) - R(i) ;
      R(i+1) = R(i) + gamma*I(i)*dt;
    end
    
    if i  > (iquarant+durationq)*(1/0.01)
      S(i+1) = S(i) -  beta3*S(i)*I(i)*dt;
      I(i+1) = 1 - S(i+1) - R(i) ;
      R(i+1) = R(i) + gamma*I(i)*dt;
    end
    
end

% % Rout = downsample(R,steps);


end

