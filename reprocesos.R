rm(list=ls())
library(XLConnect)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(ggplot2)
library(knitr)
library(tidyverse)
library(lubridate)
library(swirl)

df <- read.table("/Users/Admin/Documents/Perugia/Enero/Ensamble_1/E1_Agrupado.csv",header = TRUE, sep = ",");df


#################################################
####### Forma de columnas para los defectos #####
#################################################

ggplot(df,
       aes(y = df[,2], x = df[,3])) +
  geom_point()+
  geom_text(aes(label=df[,3]), vjust=-0.2, color="red", size=3,
            position = position_dodge(0.9))
#################################################
####### Forma de columnas para los defectos #####
#################################################

ggplot(df, aes(x = df[,2], y=df[,3])) + 
  geom_col()+
  coord_flip()+
  geom_text(aes(label=df[,3]), hjust=0.0, color="red", size=3.5,
            position = position_dodge(0.9))

#################################################
####### Forma de columnas para los defectos #####
#######          Mes de Enero               #####
#################################################

dfm <- read.table("/Users/Admin/Documents/Perugia/Enero/PARES_TODOS.csv",header = TRUE, sep = ",");dfm
head(dfm)

#################################################
####### Forma de columnas para los defectos #####
#######    Mes de Enero-Tabla Dinámica      #####
#################################################

fm1 <- read.table("/Users/Admin/Documents/Perugia/vp.txt",header = FALSE, sep = "");fm1

##################### Puntos ########################
m<-ggplot(data = fm1, aes(x = fm1[,1], y = fm1[,2])) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de Pares Producidos",
       title = "Producción de Pares Semanales (Todos los departamentos)",
       subtitle = "Primera Semana")+
  theme(axis.text.x=element_text(angle=90, hjust=1)) 
m
################ Barras ###############
ggplot(data = dfm, aes(x = dfm[,1], y = dfm[,2])) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title ="Producción de Pares Semanales (Todos los departamentos)",
       subtitle = "Enero 2019",
       x = "Fecha", y = "Número de Pares Producidos")+
  theme(axis.text.x=element_text(angle=90, hjust=1)) 

#######################################################################



