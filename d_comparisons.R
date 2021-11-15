# Compare the yearly and seasonal trend patterns and demographic characteristics of fishes
#   Take the information in "Demography.Rdata"
#   Take the information in "Seine_Best.Rdata" from "c_ModelSelection.R"

rm(list=ls())  # Clear saved variables
suppressMessages(library("tidyverse"))
suppressMessages(library("ggpubr"))
suppressMessages(library("gridExtra"))

load("Demography.Rdata")

Demog<-map_df (Demography, ~.x[["FB_ESTIM"]][c("TempPrefMin","TempPrefMax","Troph") ]) %>% 
  mutate(sci_name=names(Demography),
        GT= map_dbl(Demography,~.x[["GT1"]]),
        DR= map_dbl(Demography,~.x[["DR1"]]),
        Lamb= map_dbl(Demography,~.x[["Lamb"]]),
        Linf= map_dbl(Demography,~.x[["Linf"]]),
        Lmat= map_dbl(Demography,~.x[["Lmat"]])
         )

## The following operations were tried within dplyr using mutate_at, but it did not work!
Demog[,"GT"]<-rank(Demog[,"GT"],na.last = "keep",ties.method="average")
Demog[,"DR"]<-rank(Demog[,"DR"],na.last = "keep",ties.method="average")
Demog[,"Lamb"]<-rank(Demog[,"Lamb"],na.last = "keep",ties.method="average")
Demog[,"Linf"]<-rank(Demog[,"Linf"],na.last = "keep",ties.method="average")
Demog[,"Lmat"]<-rank(Demog[,"Lmat"],na.last = "keep",ties.method="average")
Demog[,"Troph"]<-rank(Demog[,"Troph"],na.last = "keep",ties.method="average")
Demog[,"TempPrefMax"]<-rank(Demog[,"TempPrefMax"],na.last = "keep",ties.method="average")
Demog[,"TempPrefMin"]<-rank(Demog[,"TempPrefMin"],na.last = "keep",ties.method="average")

load("Seine_Best.Rdata")

## Cluster the patterns of seasonal trends (take four clusters)
A<-map_dfc(fitVal,~.x[[1]]$fit) %>% mutate_all(scale)
  names(A)<-unlist(SP)
distMatrix<-dist(t(A),method="euclidean")
hc<-cutree(hclust(distMatrix, method="average"),k=4)
Season<-tibble(sci_name=names(hc),season_type=hc)

## Cluster the patterns of yearly trends (take three clusters)
A<-map_dfc(fitVal,~.x[[2]]$fit) %>% mutate_all(scale)
  names(A)<-as.character(unlist(SP))
distMatrix<-dist(t(A),method="euclidean")
hc<-cutree(hclust(distMatrix, method="average"),k=3)
Year<-tibble(sci_name=names(hc),year_type=hc)

COVAR<-tibble(sci_name=unlist(SP),COV=COV)

## The following will only include species that are in both TABLE and Demog
##  Demog include fish species only, but COVAR include both fish and inverte
TABLE<-merge(COVAR,Year,all.x=TRUE)
TABLE<-merge(TABLE,Season,all.x=TRUE)
TABLE<-tibble(merge(TABLE,Demog))

#colSums(is.na(TABLE))
TABLE <- TABLE %>% 
  mutate(COV=factor(COV, ordered=FALSE), year_type=factor(year_type,ordered=FALSE),season_type=factor(season_type, ordered=FALSE)) 

# Rename the patterns based on the cluster analysis
levels(TABLE$year_type)<-c("increase","decrease","fluctuate")
levels(TABLE$season_type)<-c("summer","winter","fall","spring")

save("TABLE","Season","Year","fitVal",file='clusters.Rdata')

ID<-1
FIG<-NULL
RESULTS<-matrix(NA,nrow=3,ncol=8)
colnames(RESULTS)<-c("TempPrefMin","TempPrefMax","Troph",
                     "GT","DR","Lamb","Linf","Lmat")
rownames(RESULTS)<-c("COV","year_type","season_type")
x_lab=c("environmental variables", "annual pattern","seasonal pattern")
y_lab=c("Min Temperature","Max Temperature")

panel_label=c("(a)","(c)","(e)","(b)","(d)","(f)","(g)")

for (j in c(1:2)){
  for (k in c(1:3)){ # COVARIATE, SEASON TYPE, ANNUAL TYPE,
    TABLE2 <- TABLE[,c(k+1,j+4)] 
    # CREATE COMPACT LETTER DISPLAY and PUT THEM into XLABEL STRING "LAB"
    myform <- as.formula(sprintf("%s ~ %s",names(TABLE2)[2],names(TABLE2)[1]))
    mod <- aov(myform, data = TABLE2)
    tuk<-eval(parse(text=paste("multcomp::glht(mod, linfct = multcomp::mcp(",names(TABLE2)[1]," = 'Tukey'))")))
    tuk.cld <- multcomp::cld(tuk)
     LAB<-paste(names(tuk.cld$mcletters$Letters)," (",tuk.cld$mcletters$Letters,")",sep="")
     if (ID %in% c(1,4)){
     LAB<-paste(c('CON','OXY','MSL','SAL','TMP')," (",tuk.cld$mcletters$Letters,")",sep="")
     }
    ## PRODUCE BOXPLOT
    FIG[[ID]]<-ggplot(TABLE,aes_string(x=names(TABLE2)[1],y=names(TABLE2)[2]))+
      geom_boxplot()+
      stat_compare_means(method="kruskal.test")+
      scale_x_discrete(labels=LAB)+
      labs(title=paste(panel_label[ID]),x=x_lab[k],y=y_lab[j])+
      theme_classic()
    ID<-ID+1
  }
}

pdf("PATTERN_DIST.pdf",width=7.5,height=9)
grid.arrange(FIG[[1]],FIG[[4]],FIG[[2]],FIG[[5]],FIG[[3]],FIG[[6]],ncol=2)
dev.off()

panel_label=c("(a)","(b)","(c)","(d)","(e)","(f)","(g)")
y_lab=c("Trophic Level","Generation Time","Damping Ratio",expression(paste("Max.  ", lambda)),"Max. Length", "Length at Maturity")
ID<-1
for (j in c(3:8)){
  for (k in c(1:1)){ # COVARIATE, SEASON TYPE, ANNUAL TYPE,
    TABLE2 <- TABLE[,c(k+1,j+4)] 
    # CREATE COMPACT LETTER DISPLAY and PUT THEM into XLABEL STRING "LAB"
    myform <- as.formula(sprintf("%s ~ %s",names(TABLE2)[2],names(TABLE2)[1]))
    mod <- aov(myform, data = TABLE2)
    tuk<-eval(parse(text=paste("multcomp::glht(mod, linfct = multcomp::mcp(",names(TABLE2)[1]," = 'Tukey'))")))
    tuk.cld <- multcomp::cld(tuk)
    LAB<-paste(c('CON','OXY','MSL','SAL','TMP')," (",tuk.cld$mcletters$Letters,")",sep="")
    
    ## PRODUCE BOXPLOT
    FIG[[ID]]<-ggplot(TABLE,aes_string(x=names(TABLE2)[1],y=names(TABLE2)[2]))+
      geom_boxplot()+
      stat_compare_means(method="kruskal.test")+
      scale_x_discrete(labels=LAB)+
      labs(title=paste(panel_label[ID]),x="",y=y_lab[j-2])+
      theme_classic()
      ID<-ID+1
  }
}

pdf("ENV_DEM.pdf",width=8,height=10)
 grid.arrange(FIG[[1]],FIG[[2]],FIG[[3]],FIG[[4]],FIG[[5]],FIG[[6]],ncol=2, bottom="Environmental Variables")
 dev.off()
