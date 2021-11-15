# Calculate the scaled explanatory variables
# These were calculated in a_GAM_ANALYSIS.R, but I forgot to save them in the script.
# I just copied a part of the script and pasted here and saved the explanatory variables.  

rm(list=ls())
suppressMessages(library("tidyverse"))
suppressMessages(library("mgcv"))
suppressMessages(library("parallel"))

load("DATA.Rdata")
load("STATION.Rdata")

X1<-"DoY"
X2<-"year"
X3<-"major_area"
X4<-"latitude"

# CUSTOM SCALING FUNCTION TO BE USED WITHIN DPLYR 
scale_var <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


# CREATE Species ID with scientific names and TWPD ID
SP_ID<-DATA %>%
  group_by(sci_name,.drop=TRUE) %>%
  summarize(TPWD_ID=first(TPWD_ID),.groups="drop")


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


# Organize CATCH DATA
CATCH<-DATA %>%  
  select(major_area,year,season,sci_name,catch) %>% 
  group_by(major_area,year,season,sci_name) %>% 
  summarize (catch=n(),.groups="drop") %>% 
  spread(sci_name,catch) %>% 
  # Select those that were sighted more than 100 sampling occasions 
  select(which(colSums(.,na.rm=TRUE)>100)) 

CATCH<-merge(STATION,CATCH, all.x=TRUE) %>% 
  # SCALE NUMERICAL COVARIATES
  mutate_at(vars(year,temperature,salinity,diss_oxygen,latitude,msl),
            scale_var) %>% 
  # Replace NA with 0
  mutate_at(vars(-temperature,-salinity,-diss_oxygen,
                 -msl,-year,-season,-major_area,-N),
            ~replace(.,is.na(.),0))

EXP_VARS<-CATCH[,c("major_area","year","season","temperature","salinity","diss_oxygen","latitude","msl")]

save("EXP_VARS",file="ExpVars.rdata")