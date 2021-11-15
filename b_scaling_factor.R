# Calculate the scaling factors used for scaling explanatory variables
# These were calculated in a_GAM_ANALYSIS.R, but I forgot to save them in the script.
# I just copied a part of the script and pasted here and saved the scaling factors.  

rm(list=ls())
suppressMessages(library("tidyverse"))

load("STATION.Rdata")

# CUSTOM SCALING FUNCTION TO BE USED WITHIN DPLYR 
scale_var <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# CALCULATE THE MEAN COVARIATES
STATION<-STATION %>%                       
  select(-c('station_id','day','date','DoY',"month")) %>% 
  # CALCULATE the mean per sample unit
  group_by(major_area,year,season) %>%
  summarise(temperature=mean(temperature,na.rm=TRUE),
            salinity=mean(salinity,na.rm=TRUE),
            diss_oxygen=mean(diss_oxygen,na.rm=TRUE),
            latitude=mean(latitude,na.rm=TRUE),
            msl=mean(msl,na.rm=TRUE),
            N=n(), # Total Number of Sampling Occasions
            .groups="drop") 

SCALE_FACTOR<-NULL

VARIABLES<-c("year", "temperature","salinity","diss_oxygen","latitude","msl")

SCALE_FACTOR<-lapply(VARIABLES,function(x){
  SCALE_FACTOR[["MEAN"]]<-mean(unlist(STATION[,x],use.names=FALSE),na.rm=TRUE)
  SCALE_FACTOR[["SD"]]<-sd(unlist(STATION[,x],use.names=FALSE),na.rm=TRUE)
  return(SCALE_FACTOR)
})
names(SCALE_FACTOR)<-VARIABLES

save('SCALE_FACTOR', file = 'SCALE_FACTORS_S.Rdata')

