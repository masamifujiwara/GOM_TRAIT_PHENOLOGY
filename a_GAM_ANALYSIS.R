# GAM is fit to presence-absence data using binomial model
#
# Environmental variables are the residuals after fitting a model with smooth trends of year and season
#
# Four covariates (temperature, salinity, DO, SL) are fitted one at a time
#   Including the no-covariate model, there are ten models for each species
#   The best model for each species is selected with "c_ModelSelection.R"
#
# Input data are
#   DATA.Rdata
#   STATION.Rdata
#   TPWD.Rdata
#
# Outputs are saved in 
#   ResultsGAM.Rdata
rm(list=ls())
suppressMessages(library("tidyverse"))
suppressMessages(library("mgcv"))
suppressMessages(library("parallel"))

load("DATA.Rdata")
load("STATION.Rdata")
load("TPWD.Rdata")

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

sci_name<-names(CATCH[,!names(CATCH) %in% c("major_area","year","season","temperature","salinity","diss_oxygen","latitude","N","msl")])                      

# Eliminate the species not analyzed (observed less than 100 sampling occasions)
SP_ID<-merge(tibble(sci_name=sci_name), SP_ID, all.x=TRUE)


RESULTS_SP <- function (SP,CATCH2){
  library("dplyr","tibble")
  CATCH<-CATCH2[,c("temperature","salinity","diss_oxygen","msl","year","major_area","season","N")]
  CATCH[,"catch"]<-CATCH2[,SP]
  
  X<-c("temperature","salinity","diss_oxygen","msl")
  results<-vector(mode="list",length=6)
  results[[1]]<-SP
  
  for (j in 1:4){
    CATCH[,"X"]<-CATCH[,X[j]]

    results[[j+1]]<- mgcv::gam(cbind(catch,N-catch) ~
                               major_area+
                               s(season,bs="cc",k=24)+  # This will be a factor for gillnet
                               s(year,k=20)+
                               s(X,k=30),
                             family = binomial,
                             data=CATCH,
                             knots=list(season =c(0.5,24.5)),
                             method = "REML",
                             na.action="na.omit"
    )
  }
 # No Covariate
  results[[6]]<- mgcv::gam(cbind(catch,N-catch) ~
                                    major_area+
                                    s(season,bs="cc",k=24)+  # This will be a factor for gillnet
                                    s(year,k=20),
                                  family = binomial,
                                  data=CATCH,
                                  knots=list(season =c(0.5,24.5)),
                                  method = "REML",
                                  na.action="na.omit"
  )
  for (j in 1:4){
    CATCH[,"X"]<-CATCH[,X[j]]
  results[[j+6]]<- mgcv::gam(cbind(catch,N-catch) ~
                               major_area+
                               s(season,bs="cc",k=24)+  # This will be a factor for gillnet
                               s(year,k=20)+
                               ti(season,year)+
                               s(X,k=30),
                             family = binomial,
                             data=CATCH,
                             # knots=list(season =c(0.5,24.5)),
                             method = "REML",
                             na.action="na.omit"
  )
  }
# No Covariate
results[[11]]<- mgcv::gam(cbind(catch,N-catch) ~
                           major_area+
                           s(season,bs="cc",k=24)+  # This will be a factor for gillnet
                           s(year,k=20)+
                          ti(season,year),
                         family = binomial,
                         data=CATCH,
                         # knots=list(season =c(0.5,24.5)),
                         method = "REML",
                         na.action="na.omit"
  )
  return(results)
}

SP1<-TPWD %>% 
  filter(seine>30) %>% 
  select(sci_name)

RESULTS_Seine<-NULL

## Parallel Version
SP<-as.vector(SP_ID[,1])
cl <- makeCluster(16) # Running on 16 Core Machine
RESULTS_Seine<-parLapply(cl=cl,X=SP,fun=RESULTS_SP,CATCH)
stopCluster(cl)

save('RESULTS_Seine', file = '')
