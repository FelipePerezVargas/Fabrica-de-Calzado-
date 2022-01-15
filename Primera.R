rm(list=ls())
library(ggplot2)
library(readxl)
library(qcc)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(dplyr)
library(plyr)
library(plotly)
library(hrbrthemes)
library(lubridate)
library(gganimate)
library(qicharts)
library(reshape2)

pp <- read.table("/Users/Admin/Documents/Perugia/pp.csv"
                 ,header = TRUE, sep = ",")
pp

pares<-ggplot(pp, aes(x=pp[,1], y=pp[,2]))+ 
       geom_bar(stat='identity',  fill="steelblue")+
        coord_flip();pares

reprocesos<-ggplot(pp, aes(x=pp[,1], y=pp[,3]))+ 
  geom_bar(stat='identity',  fill="steelblue")+
  coord_flip();reprocesos

primera<-ggplot(pp, aes(x=pp[,1], y=pp[,4]))+ 
  geom_bar(stat='identity',  fill="steelblue")+
  coord_flip();primera



DF <- read.table(text='Fecha 	Reprocesos	Primera
02/01/2020	171	638
03/01/2020	136	370
04/01/2020	47	401
06/01/2020	107	645
07/01/2020	130	646
08/01/2020	91	634
09/01/2020	140	389
10/01/2020	165	235
11/01/2020	94	308
13/01/2020	115	587
14/01/2020	106	622
15/01/2020	85	531
16/01/2020	73	755
17/01/2020	41	959
18/01/2020	127	305
20/01/2020	138	800
21/01/2020	180	688
22/01/2020	252	443
23/01/2020	156	521
24/01/2020	81	728
25/01/2020	94	164
27/01/2020	134	474
28/01/2020	68	739
29/01/2020	70	839', header=TRUE)


DF1 <- melt(DF, id.var="Fecha")

p <- ggplot(DF1, aes(x = Fecha, y = value, fill = variable, decreasing=T)) +
  geom_bar(stat = "identity")+
  labs(x='Días', y='Pares Producidos',
       title =paste( sum(pp[,2]),"Pares producidos ",
                     sum(pp[,3]),"Pares producidos ",
                     sum(pp[,4]),"Pares producidos " ))+
  geom_text(aes(y=value, label=value), vjust=-0.5,hjust=0.5, 
            color="white", size=3.5, angle=0)+
  theme(axis.text.x=element_text(angle=90, hjust=1))
p


p1 <- ggplot(DF1, aes(x = Fecha, y = value, fill = variable, decreasing=T)) +
  geom_bar(stat = "identity")+
  labs(x='Días', y='Pares Producidos', 
       title=paste('Producción Pares por Día Montado 2'))+
  geom_text(aes(y=value, label=value), hjust=0.0, 
            color="white", size=3.5, angle=90)+
  theme(axis.text.x=element_text(angle=90, hjust=-1.2))
p1


