###########################################################################
###########################################################################
###                                                                     ###
###                 Cálculo de Riesgos Ejemplos                         ###
###                                                                     ###
###########################################################################
###########################################################################
rm(list=ls())

# Add libraries used
library(XLConnect)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(ggplot2)
library(knitr)
library(tidyverse)
library(lubridate)
df <- read.table("/Users/Admin/Documents/Perugia/Enero/PARES_TODOS.csv",header = TRUE, sep = ",");df
names(df)
#####
c1<-df$fproceso;c1
c2<-df$subdepto;c2
c3<-df$qfabrica;c3
c4<-df$pares;c4
c5<-df$vcestil;c5
c6<-df$requiere;c6
c7<-df$meta;c7
c8<-df$ndepto ;c8

ff<-df[,1];ff
nzd <- data.frame(c1);nzd
nzd$newdate <- strptime(as.character(nzd$c1), "%d/%m/%Y")
nzd$txtdate <- format(nzd$newdate, "%Y-%m-%d");nzd

md<-data.frame(nzd, c4);md

date = as.Date(df[,1], format = "%d.%m.%Y")
date











