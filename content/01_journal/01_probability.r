T_S<-0.2
T0_S<-0.8
T_S0<-0.6
T0_S0<-0.4
S<-0.3
S0<-0.7

P_T_S<-T_S * S
P_T_S0<-T_S0 * S0
P_T0_S<-T0_S * S
P_T0_S0<-T0_S0 * S0

print(P_T_S)
print(P_T_S0)
print(P_T0_S)
print(P_T0_S0)

P_A<-0.04
P_A0<-1-P_A
P_BA<-0.97
P_BA0<-0.01

P_B<-P_BA*P_A+P_BA0*P_A0
P_AB<-(P_BA*P_A)/P_B
P_A0B<-(P_BA0*P_A0)/P_B

print(P_B)
print(P_AB)
print(P_A0B)