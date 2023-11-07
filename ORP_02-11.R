##########################################################################
####                     OCCURRENCE RATIO PROFILE                     ####
####                Analysis based on Brayard et al 2006              ####
####      in Palaeogeography, Palaeoclimatology, Palaeoecology        ####
####          (https://doi.org/10.1016/j.palaeo.2006.02.003)          ####
##########################################################################

#### Welcome and thank you for using the ORP!
##set working directory
setwd("C:/Users/33677/Documents/NEW_WORK/Postdoc LYON/ORP R")
library(graphics)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ggplot2)
library(ggpubr)


#### To perform the analysis
#### the raw data is a data frame
  #### taxa in columns
  #### geographic areas in rows
#### Data have to be formatted into a list
  #### Example of formatting with available data in the repository (from Brayard et al 2006)
  data.brayard <- list(Griesbachian = list(Griebachian=read.csv("Griesbachian.csv",header=TRUE,sep=";", row.names=1),
                                           name = "Griebachian"),
                       Dienerian = list(Dienerian=read.csv("Dienerian.csv",header=TRUE,sep=";", row.names=1),
                                        name="Dienerian"),
                       Smithian = list(read.csv("Smithian.csv",header=TRUE,sep=";", row.names=1),
                                       name="Smithian"),
                       Spathian = list(read.csv("Spathian.csv",header=TRUE,sep=";", row.names=1),
                                       name="Spathian"))

#### The script here is made of only one function
  #### ORP(data, Nite, plot)
    #### data is the input list
    #### Nite is the number of iterations for the null model
      #### (default value: Nite = 10.000)
    #### plot (TRUE/FALSE) is allowing ORP graphical representation
  #### The output is either 
    #### if plot=FALSE: a dataframe gathering (in count and percentages)
      #### empiric data, confidence intervals and modeled data
    #### if plot=TRUE: a graphical representation combining
      #### 1. the ORP with taxa count
      #### 2. the ORP with taxa percentages

#Occurrence Ratio Profile
ORP <- function(data, Nite=10000, plot=c("TRUE","FALSE")){
  
  #Empiric ORP
  n.sp <- as.numeric(ncol(data[[1]]))
  
  perc <- apply(data[[1]], 2, function(x)sum(x)/length(x)*100)
  hist.perc <- graphics::hist(perc, breaks=seq(0,100,10), right=TRUE, plot=FALSE)
  df.perc <- data.frame("hist.perc.counts"=as.numeric(hist.perc$counts),
                        "hist.perc.perc"=apply(as.data.frame(hist.perc$counts), 1, 
                                               function(x)x/n.sp*100),
                        row.names = hist.perc$mids/100)
  
  
  #A function for confidence intervals
  l.boot <- lapply(vector("list",Nite), function(x){sample(data[[1]], replace = TRUE)})
  df.boot <- #Boostrap
    lapply(l.boot, function(data){
      perc.boot <- as.numeric(apply(data, 2, function(x)sum(x)/length(x)*100))
      hist.perc.boot <- graphics::hist(perc.boot, breaks=seq(0,100,10), right=TRUE, plot=FALSE)
      
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
  
  #LowerBound and UpperBound
  CI.count.min <- as.numeric(apply(df.boot.count,1,function(x)sort(as.numeric(x))[0.025*Nite]))
  CI.count.max <- as.numeric(apply(df.boot.count,1,function(x)sort(as.numeric(x))[0.975*Nite]))
  CI.perc.min <- as.numeric(apply(df.boot.perc,1,function(x)sort(as.numeric(x))[0.025*Nite]))
  CI.perc.max <- as.numeric(apply(df.boot.perc,1,function(x)sort(as.numeric(x))[0.975*Nite]))
  
  
  #A function for the null model
  l.lot <- lapply(vector("list",Nite), function(x){apply(data[[1]],1,function(x)sample(x))})
  df.lot <- #Lottery
    lapply(l.lot, function(data){
      perc.lot <- as.numeric(apply(t(data), 2, function(x)sum(x)/nrow(t(data))*100))
      hist.lot <- graphics::hist(perc.lot, breaks=seq(0,100,10), right=TRUE, plot=FALSE)
      
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
  lot.count.min <- as.numeric(apply(df.lot.count,1,function(x)sort(as.numeric(x))[0.025*Nite]))
  lot.count.max <- as.numeric(apply(df.lot.count,1,function(x)sort(as.numeric(x))[0.975*Nite]))
  lot.perc.min <- as.numeric(apply(df.lot.perc,1,function(x)sort(as.numeric(x))[0.025*Nite]))
  lot.perc.max <- as.numeric(apply(df.lot.perc,1,function(x)sort(as.numeric(x))[0.975*Nite]))
  
  
  #Gather all results (empiric, CI and null model)
  df.ORP <- cbind (df.perc,
                   CI.count.min, CI.count.max, CI.perc.min, CI.perc.max,
                   lot.count.min, lot.count.max, lot.perc.min, lot.perc.max)
  
  #Graphical representations
  if(plot==TRUE){
    plot.count <- #ORP with taxa count
      ggplot(df.ORP, aes(x=as.numeric(rownames(df.ORP)), y=df.ORP$hist.perc.counts))+
      geom_ribbon(aes(ymin=lot.count.min, ymax=lot.count.max),fill="grey90")+
      geom_errorbar(aes(ymin=CI.count.min, ymax=CI.count.max), width=0) +
      geom_point()+
      theme_bw()+
      theme(#text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
      scale_y_continuous(breaks=seq(0,max(df.ORP),10),limits=c(0,max(df.ORP)))+
      scale_x_continuous(breaks=seq(0,1,0.1),limits=c(0,1))+
      labs(x="Occurence ratio", y="Number of taxa", title=data[[2]])
    
    plot.perc <- #ORP with taxa percentages
      ggplot(df.ORP, aes(x=as.numeric(rownames(df.ORP)), y=df.ORP$hist.perc.perc))+
      geom_ribbon(aes(ymin=lot.perc.min, ymax=lot.perc.max),fill="grey90")+
      geom_errorbar(aes(ymin=CI.perc.min, ymax=CI.perc.max), width=0) +
      geom_point()+
      theme_bw()+
      theme(#text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
      scale_y_continuous(breaks=seq(0,max(df.ORP),10),limits=c(0,max(df.ORP)))+
      scale_x_continuous(breaks=seq(0,1,0.1),limits=c(0,1))+
      labs(x="Occurence ratio", y="Percentage of taxa", title=data[[2]])
    
    ggpubr::ggarrange(plot.count, plot.perc)
  }
  else{return(df.ORP)}
}

multi.ORP <- lapply(data.brayard, ORP, plot=TRUE)
ggpubr::ggarrange(plotlist = multi.ORP, ncol=1)#if you want to display the graphics