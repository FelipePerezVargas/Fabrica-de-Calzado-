#####################################################################
######                                                          #####                                                              
######      Líneas de Tiempo Reprocesos por Departamento        #####
######                                                          #####
#####################################################################
rm(list=ls())
  library(ggplot2)
library(readxl)
library(qcc)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(dplyr)
library(plotly)
library(lubridate)
library(gganimate)

nd <- read.table("E:/Perugia/nn1.csv"
                   ,header = TRUE, sep = ",");nd

nd$Mes <- as.Date(nd$Mes)
#####################################################################
######                                                          #####                                                              
######      Líneas de Tiempo Reprocesos por Departamento        #####
######                       Ensamble 1                         #####
#####################################################################

e2 <- nd %>%
  ggplot( aes(x=Mes , y=nd[,2])) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  labs(ylab='Número de Defectos',
  xlab='Mes',
  title =paste( sum(nd[,2]),"Reprocesos Histórico "))+
  theme_ipsum()

e2<- ggplotly(e2);e2

#####################################################################
######                                                          #####                                                              
######      Líneas de Tiempo Reprocesos por Departamento        #####
######                       Montado 2                          #####
#####################################################################

m2 <- nd %>%
  ggplot( aes(x=Mes , y=nd[,4])) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  labs(ylab='Número de Defectos',
       xlab='Mes',
       title =paste( sum(nd[,4]),"Reprocesos Histórico "))+
  theme_ipsum()


m2<- ggplotly(m2);m2

#####################################################################
######                                                          #####                                                              
######      Líneas de Tiempo Reprocesos por Departamento        #####
######                       Ensamble 1                         #####
#####################################################################

e1 <- datar %>%
  ggplot( aes(x=Mes , y=nd[,2])) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  labs(ylab='Número de Defectos',
       xlab='Mes',
       title =paste( sum(nd[,2]),"Reprocesos Histórico "))+
  theme_ipsum()

e1<- ggplotly(e1);e1

#####################################################################
######                                                          #####                                                              
######      Líneas de Tiempo Reprocesos por Departamento        #####
######                       Ensamble 1                         #####
#####################################################################

e1 <- datar %>%
  ggplot( aes(x=Mes , y=nd[,2])) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  labs(ylab='Número de Defectos',
       xlab='Mes',
       title =paste( sum(nd[,2]),"Reprocesos Histórico "))+
  theme_ipsum()

e1<- ggplotly(e1);e1

#####################################################################
######                                                          #####                                                              
######      Líneas de Tiempo Reprocesos por Departamento        #####
######                       Ensamble 1                         #####
#####################################################################

e1 <- datar %>%
  ggplot( aes(x=Mes , y=nd[,2])) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  labs(ylab='Número de Defectos',
       xlab='Mes',
       title =paste( sum(nd[,2]),"Reprocesos Histórico "))+
  theme_ipsum()

e1<- ggplotly(e1);e1

#####################################################################
######                                                          #####                                                              
######      Líneas de Tiempo Reprocesos por Departamento        #####
######                       Ensamble 1                         #####
#####################################################################

e1 <- datar %>%
  ggplot( aes(x=Mes , y=nd[,2])) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  labs(ylab='Número de Defectos',
       xlab='Mes',
       title =paste( sum(nd[,2]),"Reprocesos Histórico "))+
  theme_ipsum()

e1<- ggplotly(e1);e1

#####################################################################
######                                                          #####                                                              
######      Líneas de Tiempo Reprocesos por Departamento        #####
######                       Ensamble 1                         #####
#####################################################################

e1 <- datar %>%
  ggplot( aes(x=Mes , y=nd[,2])) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  labs(ylab='Número de Defectos',
       xlab='Mes',
       title =paste( sum(nd[,2]),"Reprocesos Histórico "))+
  theme_ipsum()

e1<- ggplotly(e1);e1

#####################################################################
######                                                          #####                                                              
######      Líneas de Tiempo Reprocesos por Departamento        #####
######                       Ensamble 1                         #####
#####################################################################

e1 <- datar %>%
  ggplot( aes(x=Mes , y=nd[,2])) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  labs(ylab='Número de Defectos',
       xlab='Mes',
       title =paste( sum(nd[,2]),"Reprocesos Histórico "))+
  theme_ipsum()

e1<- ggplotly(e1);e1

#####################################################################
######                                                          #####                                                              
######      Líneas de Tiempo Reprocesos por Departamento        #####
######                       Ensamble 1                         #####
#####################################################################

e1 <- datar %>%
  ggplot( aes(x=Mes , y=nd[,2])) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  labs(ylab='Número de Defectos',
       xlab='Mes',
       title =paste( sum(nd[,2]),"Reprocesos Histórico "))+
  theme_ipsum()

e1<- ggplotly(e1);e1


plot(nd[,1], nd[,2])




nd %>% 
  group_by(year(Mes)) %>%                      #Acá va la magia! Vieron que valía la pena aprender a usar dplyr...
  summarise(EEUU=sum(`Estados Unidos`),           #Sumo por grupos=años
            CAN=sum(Canada), CHI=sum(China), 
            CentroAmerica=sum(`Centro America`),
            Sudamerica=sum(`America del Sur`))

