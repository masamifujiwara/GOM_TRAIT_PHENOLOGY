## Plot figures of patterns

rm(list=ls())  # Clear saved variables
suppressMessages(library("tidyverse"))
suppressMessages(library("ggpubr"))
suppressMessages(library("gridExtra"))

load('clusters.Rdata')
load("SCALE_FACTORS_S.Rdata")
panel_label=c("(a)","(b)","(c)","(d)")

# Remove invertebrates
ID1<-which(unlist(Season[,'sci_name']) %in% unlist(TABLE[,'sci_name']))
Season<-Season[ID1,]
Year<-Year[ID1,]
fitVal<-apply(as.array(ID1),1,function(x)fitVal[[x]])

FIG_S<-NULL
FIG_Y<-NULL

for (k in 1:4){
  ID<-as.array(which(Season[,"season_type"]==k)) # Season & Pattern 1
  A1<-apply(ID,1,function(x)fitVal[[x]])
  B1<-map_dfc(A1,~as.data.frame(.x[[1]]$fit))  # Season
  colnames(B1)<-unlist(Season[ID,1],use.names=FALSE) # Season
  B1[,"season"]<-fitVal[[1]][[1]]$x/2  #Season
  B1<-gather(B1,sci_name,season_trend,-season)
  
FIG_S[[k]]<- B1 %>% 
  group_by(sci_name) %>% 
  mutate(season_trend=scale(season_trend)) %>% 
  ggplot(aes(x=season,y=season_trend,lines=sci_name))+
  scale_x_continuous(breaks = 1:12)+
    geom_line()+
    geom_vline(xintercept=3)+
    geom_vline(xintercept=6)+
    geom_vline(xintercept=9)+
    # facet_wrap(~sci_name,scales = "free")+
    # labs(title=paste(panel_label[k],"Pattern ", k),y="",x="") +
   labs(title=paste(panel_label[k]),y="",x="") +
    theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black")) 
}

for (k in 1:3){
  ID<-as.array(which(Year[,"year_type"]==k)) # Year & Pattern 1
  A2<-apply(ID,1,function(x)fitVal[[x]])
  B2<-map_dfc(A2,~as.data.frame(.x[[2]]$fit))  # Year
  colnames(B2)<-unlist(Year[ID,1],use.names=FALSE) # Year
  # B2[,"year"]<-seq(from=1983,to=2019,length.out=100) #year
  B2[,"year"]<-fitVal[[1]][[2]]$x*SCALE_FACTOR[['year']]$SD+SCALE_FACTOR[['year']]$MEAN  #year
  B2<-gather(B2,sci_name,year_trend,-year)
FIG_Y[[k]]<-B2 %>% 
  group_by(sci_name) %>% 
  mutate(year_trend=scale(year_trend)) %>% 
  ggplot(aes(x=year,y=year_trend,lines=sci_name))+
  scale_x_continuous(limits=c(1982,2019),breaks = seq(1984,2018, by=2), expand = c(0, 0.5))+
    geom_line()+
    geom_vline(xintercept=1990)+
    geom_vline(xintercept=2000)+
    geom_vline(xintercept=2010)+
    # facet_wrap(~sci_name,scales = "free")+
    labs(title=paste(panel_label[k]),y="",x="") +
    theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black")) 
}

pdf("SEASON.pdf",width=7.5,height=10)
grid.arrange(FIG_S[[1]],FIG_S[[2]],FIG_S[[3]],FIG_S[[4]],ncol=1,left="Scaled seasonal pattern",bottom="Month")
dev.off()
pdf("YEAR.pdf",width=7.5,height=10)
grid.arrange(FIG_Y[[1]],FIG_Y[[2]],FIG_Y[[3]],ncol=1,left="Scaled annual pattern",bottom="Year")
dev.off()