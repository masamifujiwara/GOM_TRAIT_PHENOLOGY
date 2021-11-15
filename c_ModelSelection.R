# The best model for each species is selected based on AIC.
#   Take the results from GAM_ANALYSIS.R (ResultsGAM.Rdata) and saved in "Seine_Best.Rdata".

rm(list=ls())  
suppressMessages(library("tidyverse"))
suppressMessages(library("mgcv"))
load('ResultsGAM.Rdata')

MODEL<-NULL
SP<-NULL
COV<-NULL
change<-NULL
SUMM<-NULL
fitVal<-NULL
RESULTS<-RESULTS_Seine
X<-c("temperature","salinity","diss_oxygen","msl","constant","temperature","salinity","diss_oxygen","msl","constant")
for (k in 1:length(RESULTS)){
  IC<-NULL
  IC[1]<-AIC(RESULTS[[k]][[2]])
  IC[2]<-AIC(RESULTS[[k]][[3]])
  IC[3]<-AIC(RESULTS[[k]][[4]])
  IC[4]<-AIC(RESULTS[[k]][[5]])
  IC[5]<-AIC(RESULTS[[k]][[6]])
  IC[6]<-AIC(RESULTS[[k]][[7]])
  IC[7]<-AIC(RESULTS[[k]][[8]])
  IC[8]<-AIC(RESULTS[[k]][[9]])
  IC[9]<-AIC(RESULTS[[k]][[10]])
  IC[10]<-AIC(RESULTS[[k]][[11]])
  MODEL[[k]]<-RESULTS[[k]][[which(IC==min(IC))+1]]
  SP[k]<-RESULTS[[k]][1]
  COV[k]<-X[which(IC==min(IC))]
  fitVal[[k]]<-plot(RESULTS[[k]][[which(IC==min(IC))+1]],pages=1)
  SUMM[[k]]<-summary(RESULTS[[k]][[which(IC==min(IC))+1]])
}

save("MODEL","SP","COV","fitVal","SUMM",file="Seine_Best.Rdata")
