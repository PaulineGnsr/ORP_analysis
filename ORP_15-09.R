####ORP according to Brayard et al 2006 #####

##data matrix as geography in row and taxa in columns, presence/absence

#-----------------------------
##Preparation
#-----------------------------

library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

##set working directory
setwd("C:/Users/33677/Documents/NEW_WORK/Postdoc LYON/ORP R")

##Load data
Gr <- read.csv("Griesbachian.csv",header=TRUE,sep=";",row.names=1)
Di <- read.csv("Dienerian.csv",header=TRUE,sep=";",row.names=1)
Sm <- read.csv("Smithian.csv",header=TRUE,sep=";",row.names=1)
Sp <- read.csv("Spathian.csv",header=TRUE,sep=";",row.names=1)

Brayard.data <- list(Griesbachian = list(Griesbachian = Gr, name="Griesbachian"),
                     Dienerian = list(Dienerian = Di, name="Dienerian"),
                     Smithian = list(Smithian = Sm, name="Smithian"),
                     Spathian = list(Spathian = Sp, name="Spathian"))

ORP <- function(data, Nite){

#Empiric ORP ####
n.sp <- as.numeric(ncol(data[[1]]))
  
perc <- apply(data[[1]], 2, function(x)sum(x)/length(x)*100)
hist.perc <- hist(perc, breaks=seq(0,100,10), right=TRUE, plot=FALSE)
df.perc <- data.frame("hist.perc.counts"=as.numeric(hist.perc$counts),
                      "hist.perc.perc"=apply(as.data.frame(hist.perc$counts), 1, 
                                             function(x)x/n.sp*100),
                      row.names = hist.perc$mids/100)

l.boot <- lapply(vector("list",Nite), function(x){sample(data[[1]], replace = TRUE)})
l.lot <- lapply(vector("list",Nite), function(x){apply(data[[1]],1,function(x)sample(x))})

#Boostrap
df.boot <- 
  lapply(l.boot, function(data){
    perc.boot <- as.numeric(apply(data, 2, function(x)sum(x)/length(x)*100))
    hist.perc.boot <- hist(perc.boot, breaks=seq(0,100,10), right=TRUE, plot=FALSE)
    
    hist.perc.boot.count <- hist.perc.boot$counts
    hist.perc.boot.perc <- apply(as.data.frame(hist.perc.boot$count), 1,
                           function(x)x/n.sp*100)
    
    list(hist.perc.boot.count, hist.perc.boot.perc)

  }
  )

df.boot.count <- 
  as.data.frame(lapply(df.boot, function(data){data[[1]]}),
                col.names = 1:length(l.boot),
                row.names = seq(0.05,0.95,0.10)
  )
df.boot.perc <- 
  as.data.frame(lapply(df.boot, function(data){data[[2]]}),
                col.names = 1:length(l.boot),
                row.names = seq(0.05,0.95,0.10)
  )


#Lottery
df.lot <-
  lapply(l.lot, function(data){
    perc.lot <- as.numeric(apply(t(data), 2, function(x)sum(x)/nrow(t(data))*100))
    hist.lot <- hist(perc.lot, breaks=seq(0,100,10), right=TRUE, plot=FALSE)
    
    hist.lot.count <- hist.lot$counts
    hist.lot.perc <- apply(as.data.frame(hist.lot$counts), 1, 
                           function(x)x/n.sp*100)
    
    list(hist.lot.count, hist.lot.perc)
  }
  )
df.lot.count <- 
  as.data.frame(lapply(df.lot, function(data){data[[1]]}),
                col.names = 1:length(l.boot),
                row.names = hist.perc$mids/100
  )
df.lot.perc <-
  as.data.frame(lapply(df.lot, function(data){data[[2]]}),
                col.names = 1:length(l.boot),
                row.names = hist.perc$mids/100
  )

#LowerBound and UpperBound
CI.count.min <- as.numeric(apply(df.boot.count,1,function(x)sort(as.numeric(x))[0.025*Nite]))#mean(as.numeric(x))-t*sd(as.numeric(x))/sqrt(N)))
CI.count.max <- as.numeric(apply(df.boot.count,1,function(x)sort(as.numeric(x))[0.975*Nite]))#mean(as.numeric(x))+t*sd(as.numeric(x))/sqrt(N)))
CI.perc.min <- as.numeric(apply(df.boot.perc,1,function(x)sort(as.numeric(x))[0.025*Nite]))#mean(as.numeric(x))-t*sd(as.numeric(x))/sqrt(N)))
CI.perc.max <- as.numeric(apply(df.boot.perc,1,function(x)sort(as.numeric(x))[0.975*Nite]))#mean(as.numeric(x))+t*sd(as.numeric(x))/sqrt(N)))

lot.count.min <- as.numeric(apply(df.lot.count,1,function(x)sort(as.numeric(x))[0.025*Nite]))#mean(as.numeric(x))-t*sd(as.numeric(x))/sqrt(N)))
lot.count.max <- as.numeric(apply(df.lot.count,1,function(x)sort(as.numeric(x))[0.975*Nite]))#mean(as.numeric(x))+t*sd(as.numeric(x))/sqrt(N)))
lot.perc.min <- as.numeric(apply(df.lot.perc,1,function(x)sort(as.numeric(x))[0.025*Nite]))#mean(as.numeric(x))-t*sd(as.numeric(x))/sqrt(N)))
lot.perc.max <- as.numeric(apply(df.lot.perc,1,function(x)sort(as.numeric(x))[0.975*Nite]))#mean(as.numeric(x))+t*sd(as.numeric(x))/sqrt(N)))

#Gather all results ####
df.ORP <- cbind (df.perc, 
                 CI.count.min, CI.count.max, CI.perc.min, CI.perc.max,
                 lot.count.min, lot.count.max, lot.perc.min, lot.perc.max)


#Graphic representation ####
  #Taxa count
plot.count <- 
ggplot(df.ORP, aes(x=as.numeric(rownames(df.ORP)), y=df.ORP$hist.perc.counts))+
  geom_ribbon(aes(ymin=lot.count.min, ymax=lot.count.max),fill="grey90")+
  geom_errorbar(aes(ymin=CI.count.min, ymax=CI.count.max), width=0) +
  geom_point()+
  theme_bw()+
  theme(#text = element_text(size=20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())+
  scale_y_continuous(breaks=seq(0,70,10),limits=c(0,71))+
  scale_x_continuous(breaks=seq(0,1,0.1),limits=c(0,1))+
  labs(x="Occurence ratio", y="Number of taxa", title=data[[2]])

  #Taxa perc
plot.perc <- 
ggplot(df.ORP, aes(x=as.numeric(rownames(df.ORP)), y=df.ORP$hist.perc.perc))+
  geom_ribbon(aes(ymin=lot.perc.min, ymax=lot.perc.max),fill="grey90")+
  geom_errorbar(aes(ymin=CI.perc.min, ymax=CI.perc.max), width=0) +
  geom_point()+
  theme_bw()+
  theme(#text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  scale_y_continuous(breaks=seq(0,70,10),limits=c(0,71))+
  scale_x_continuous(breaks=seq(0,1,0.1),limits=c(0,1))+
  labs(x="Occurence ratio", y="Percentage of taxa", title=data[[2]])

ggarrange(plot.count, plot.perc)

}

multi.ORP <- lapply(Brayard.data, ORP, Nite=1000)
plot.ORP <- ggarrange(plotlist=multi.ORP,ncol=1,nrow=4)
plot.ORP