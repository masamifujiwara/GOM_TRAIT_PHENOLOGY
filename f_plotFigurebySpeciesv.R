## Plot individual fish result

rm(list=ls())  # Clear saved variables
suppressMessages(library("tidyverse"))
suppressMessages(library("gridExtra"))
suppressMessages(library("ggpubr"))

load("clusters.Rdata")
Cluster_Results<-TABLE[,c("sci_name", "year_type"  , "season_type","COV")]

load("Demography.Rdata")

Demog<-map_df (Demography, ~.x[["FB_ESTIM"]][c("TempPrefMin","TempPrefMax","Troph") ]) %>% 
  mutate(sci_name=names(Demography),
         GT= map_dbl(Demography,~.x[["GT1"]]),
         DR= map_dbl(Demography,~.x[["DR1"]]),
         Lamb= map_dbl(Demography,~.x[["Lamb"]]),
         Linf= map_dbl(Demography,~.x[["Linf"]]),
         Lmat= map_dbl(Demography,~.x[["Lmat"]])
  )

load ("SCALE_FACTORS_S.Rdata") # SCALE_FACTOR : List of 6
load ("ExpVars.rdata")         # COVARIATES
load ("Seine_Best.Rdata")      # fitVal, MODEL, SP,  SUMM, COV (include the ones with sufficiant data)
load ("TPWD.Rdata")           

SP2<-data.frame(sci_name=unlist(SP))

TABLE <- merge(Cluster_Results,Demog) # Eliminate invertebrates
INDEX<-1
pdf("BagseineBySpeciesV5.pdf",width=9, height=11)
for (species in unlist(SP2)){
if (species %in% TABLE$sci_name){
  # Select Information for the specific species
  ID2<-which(SP2==species)
  fitVal2<-fitVal[[ID2]]
  MODEL2<-MODEL[[ID2]]
  SUMM2<-SUMM[[ID2]]
  COV2<-COV[ID2]
  DEMOG<-TABLE[TABLE$sci_name==species,]

  RESULTS<-data.frame(fit=MODEL2$fitted.values) %>%
    mutate(year= EXP_VARS[,"year"]*SCALE_FACTOR[['year']]$SD+SCALE_FACTOR[['year']]$MEAN,
           major_area=as.numeric(EXP_VARS[,"major_area"]),
           season=EXP_VARS[,"season"])

  S_B_Plot<-RESULTS %>%
    mutate(month=season/2) %>%
    group_by(major_area, year) %>%
    mutate(fit=mean(fit,na.rm=TRUE))%>%
    ggplot(aes(x=major_area,y=year))+
    geom_raster(aes(fill=fit))+
    scale_fill_gradientn(colours = c("black", "green","yellow","red"), values = c(0,0.05,0.525,1),limits=c(0,1),na.value="white")+
    labs(x="Bay",y="Year",fill="Probability",title="(b)")+scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8),labels=c("SL","GA","MA","SA","AR","CC","UL","LL"))

  S_M_Plot <-RESULTS[,c("major_area","year","season","fit")] %>%
    mutate(month=season/2) %>%
    group_by(month, year) %>%
    mutate(fit=mean(fit,na.rm=TRUE)) %>%
    ggplot(aes(x=month,y=year))+
    geom_raster(aes(fill=fit))+
    scale_fill_gradientn(colours = c("black", "green","yellow","red"), values = c(0,0.05,0.525,1),limits=c(0,1), na.value="white")+
    labs(x="Month",y="Year",fill="Probability",title="(a)")+scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("JA","FE","MR","AP","MY","JN","JL","AU","SP","OC","NO","DE"))
  #
  FIG_S<-data.frame(season_trend=fitVal2[[1]]$fit,
                    season=fitVal2[[1]]$x/2) %>%
    ggplot(aes(x=season,y=season_trend))+
    geom_line()+
    geom_vline(xintercept=3)+
    geom_vline(xintercept=6)+
    geom_vline(xintercept=9)+
    # labs(title="Seasonal Pattern")+
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("JA","FE","MR","AP","MY","JN","JL","AU","SP","OC","NO","DE"))+
    labs(x="Month",y="Seasonal Trend",title="(c)")
  
  #
  FIG_Y<-data.frame(annual_trend=fitVal2[[2]]$fit,
                    year=fitVal2[[2]]$x*SCALE_FACTOR[['year']]$SD+SCALE_FACTOR[['year']]$MEAN) %>%
    ggplot(aes(x=year,y=annual_trend))+
    geom_line()+
    geom_vline(xintercept=1990)+
    geom_vline(xintercept=2000)+
    geom_vline(xintercept=2010)+
    #labs(title="Annual Pattern")
    labs(x="Year",y="Annual Trend",title="(d)")

  Temp1<-ggplot(TABLE,aes(x=TempPrefMax))+
    geom_histogram()+
    geom_vline(xintercept=DEMOG$TempPrefMax,col="red", size=2)+
    geom_vline(xintercept=median(TABLE$TempPrefMax,na.rm = TRUE),col="blue",linetype="dashed") +
    xlab(bquote('Preferred Maximum Temperature' ~C^o))+
    ggtitle("(f)")

  Temp2<-ggplot(TABLE,aes(x=TempPrefMin))+
    geom_histogram()+
    geom_vline(xintercept=DEMOG$TempPrefMin,col="red", size=2)+
    geom_vline(xintercept=median(TABLE$TempPrefMin,na.rm = TRUE),col="blue",linetype="dashed")+
    xlab(bquote('Preferred Minimum Temperature' ~C^o))+
    ggtitle("(e)")

  Troph<-ggplot(TABLE,aes(x=Troph))+
    geom_histogram()+
    geom_vline(xintercept=DEMOG$Troph,col="red", size=2)+
    geom_vline(xintercept=median(TABLE$Troph,na.rm = TRUE),col="blue",linetype="dashed")+
    xlab("Trophic Level")+
    ggtitle("(i)")

  GT<-ggplot(TABLE,aes(x=GT))+
    geom_histogram()+
    geom_vline(xintercept=DEMOG$GT,col="red", size=2)+
    geom_vline(xintercept=median(TABLE$GT,na.rm = TRUE),col="blue",linetype="dashed")+
    xlab("Generation Time (year)")+
    ggtitle("(j)")

  # DR<-ggplot(TABLE,aes(x=DR))+
  #    geom_histogram()+
  #    geom_vline(xintercept=DEMOG$DR)

  Lamb<-ggplot(TABLE,aes(x=Lamb))+
    geom_histogram()+
    geom_vline(xintercept=DEMOG$Lamb,col="red", size=2)+
    geom_vline(xintercept=median(TABLE$Lamb,na.rm = TRUE),col="blue",linetype="dashed")+
    xlab(bquote(~lambda[max]))+
    ggtitle("(k)")

  Linf<-ggplot(TABLE,aes(x=Linf))+
    geom_histogram()+
    geom_vline(xintercept=DEMOG$Linf,col="red", size=2)+
    geom_vline(xintercept=median(TABLE$Linf,na.rm = TRUE),col="blue",linetype="dashed")+
    xlab(bquote(~L[inf]))+
    ggtitle("(g)")

  Lmat<-ggplot(TABLE,aes(x=Lmat))+
    geom_histogram()+
    geom_vline(xintercept=DEMOG$Lmat,col="red", size=2)+
    geom_vline(xintercept=median(TABLE$Lmat,na.rm = TRUE),col="blue",linetype="dashed")+
    xlab("Length at Maturity (cm)")+
    ggtitle("(h)")

  RESULTS<- DEMOG[,c("season_type","year_type","COV")] %>%
    mutate(common_name=TPWD[TPWD[,"sci_name"]==species,"common_name"],sci_name=species) %>%
    relocate(common_name, .before = season_type) %>% 
    relocate(sci_name,.before = common_name)
  RESULTS<-t(RESULTS)
  
  row.names(RESULTS)<-c("Scientific Name","Common Name","Seasonality Type","Year Trend Type","Environment")

  TEXT_TABLE<-ggtexttable(RESULTS,cols = NULL)


  grid.arrange(S_M_Plot,S_B_Plot,FIG_S,FIG_Y,Temp2,Temp1,Linf,Lmat,Troph,
               GT,Lamb,TEXT_TABLE,
               ncol=2,
               top = grid::textGrob(paste("Figure S",INDEX," ",species,sep=""), x = 0, hjust = 0),
               bottom=paste("Page ",INDEX+2,sep=""))
INDEX<-INDEX+1
  
}
}
dev.off()
