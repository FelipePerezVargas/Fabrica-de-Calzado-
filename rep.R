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

 
##################### Concentrado de Reprocesos Enero ############
#ggsave("renero.png", width = 5, height = 5)
rene <- read.table("/Users/Admin/Documents/Perugia/Enero/rene.csv"
                  ,header = TRUE, sep = ",");rene
ene<-  ggplot(data = rene, aes(x = rene[,1],  y= rene[,2])) + 
  geom_bar(stat='identity',  fill="steelblue")+
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(x = "Departamento",
       y = "Reprocesos",
       title =paste( "Reprocesos Enero=",sum(rene[,2])))+
  geom_text(aes(label=rene[,2]), vjust=1.6, color="red", size=3.5,
            position = position_dodge(0.9));ene
ene<-ggplotly(ene)
 ene
##################### Producción de Pares Enero #####################
ggsave("penero.png", width = 5, height = 5)
par_e <- read.table("/Users/Admin/Documents/Perugia/Enero/par_e.csv"
                   ,header = TRUE, sep = ",");par_e
ene1<-  ggplot(data = par_e, aes(x = par_e[,1],  y= par_e[,2])) + 
  geom_bar(stat='identity', fill="steelblue")+
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(x = "Departamento",
       y = "Pares Producidos",
       title = paste('Pares Producidos Enero=',sum(par_e[,2])),
       subtitle = paste('Eficiencia',(sum(par_e[,2])/66000)*100,'%'))+
  geom_text(aes(label=par_e[,2]), vjust=1.6, color="red", size=3.5)
  ##+ theme_minimal()
ene1
#<-ggplotly(ene1);ene1

####################### Pares vs Reprocesos ###############
ggsave("vs1.png", width = 5, height = 5)
p<-ggplot(data=vs, aes(x=vs[,1], y=vs[,2])) +
  geom_bar(stat="identity",  fill="steelblue")
p + coord_flip()+
  labs(x = "Departamento",
       y = "Reprocesos",
       title =paste( sum(vs[,2]),"Rerpocesos Enero") ,
       subtitle = paste("Total de Reprocesos", sum(rene[,2])))+
  geom_text(aes(label=vs[,2]), hjust=-0.5, color="red", size=3.5,
            position = position_dodge(0.9))

########################## Apilado #######################

df<- read.table("/Users/Admin/Documents/Perugia/Enero/vs.csv"
                ,header = TRUE, sep = ",")
p<-ggplot(data=df, aes(x=df[,1], y=df[,2])) +
  geom_bar(stat="identity",  fill="steelblue")
df2 <- data.frame(Calidad=rep(c("REPROCESOS","PARES"), each=3),
                  Departamento=c("MONTADO2", "MONTADO3", "MONTADO4", "MONTADO2","MONTADO3", "MONTADO4")
                                 , Cantidad=df[,2])
head(df2)
df_sorted <- arrange(df2, Departamento, Cantidad) 
head(df_sorted)
df_cumsum <- ddply(df_sorted, "Departamento",
                   transform, label_ypos=cumsum( Cantidad))
head(df_cumsum)
#ggsave("vs2.png", width = 5, height = 5)
ggplot(data=df_cumsum, aes(x=Departamento, y=Cantidad, fill=Calidad, color=Departamento)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=label_ypos, label=Cantidad), hjust=1.2, 
            color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  coord_flip()+
  labs( title =paste( "Rerpocesos + Pares =", sum(df2[,3]) ))+
    theme_minimal()


#####################################  Pares por día ##############################
fm1 <- read.table("/Users/Admin/Documents/Perugia/vp.txt",header = FALSE, sep = "");fm1
ggsave("pdias.png", width = 5, height = 5)
ggplot(data = fm1, aes(x = fm1[,1], y = fm1[,2])) + 
  geom_bar(stat='identity',color="blue", fill="gray")+
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(x="Día ",
  y="Pares Producidos",
  title=paste('Meta Producción de Pares por Día (85% )'))+
  geom_text(aes(label=fm1[,2]), vjust=-0.2, color="red", size=3,
            position = position_dodge(0.9))+
  geom_errorbar(aes(ymax=fm1[,3], ymin=fm1[,3]), linetype="solid",
                position=position_dodge(), colour='gray10')+  
  geom_hline(yintercept = c(1500, 3000), colour=c("#990000", '#990000'), linetype=c("solid", 'solid'))



################## Reprocesos Detalle Semana 1 #####################

data <- read.table("/Users/Admin/Documents/Perugia/Enero/Ensamble_1/p1.csv", header=TRUE, sep=",")
data %>%
  filter(!is.na(data[,2])) %>%
  arrange(data[,2]) %>%
  tail(length(data[,1])) %>%
  mutate(Defecto=factor(Defecto, Defecto)) %>%
  ggplot( aes(y=Cantidad, x=Defecto, fill=Cantidad) ) +
  geom_bar(stat="identity") +
  scale_fill_viridis() +
  coord_flip() +
  theme_ipsum() +
  labs(x = "",y = "Reprocesos",
       title = "Reprocesos Semanal",
       subtitle = paste("Total Reprocesos Ensamble 1", sum(data[,2])))
ggsave("ds1.png", width = 5, height = 5)

###################### Reprocesos Diarios ######################

data1 <- read.table(file.choose("/Users/Admin/Documents/Perugia/Enero/Ensamble_1/p1.csv"), header=TRUE, sep=",")
data1 %>%
  filter(!is.na(data1[,3])) %>%
  arrange(data1[,3]) %>%
  tail(length(data1[,3])) %>%
  mutate(defecto=factor(data1[,2], data1[,2])) %>%
  ggplot( aes(y=data1[,3], x=defecto, fill=data1[,3]) ) +
  geom_bar(stat="identity") +
  scale_fill_viridis() +
  coord_flip() +
  theme_ipsum() +
  labs(x = "",y = "Reprocesos",
       title = "Reprocesos Mensual Montado 3",
       subtitle = paste("Total Reprocesos=", sum(data1[,3])))


ggsave("ds3.png", width = 5, height = 5)
data2 <- read.table(file.choose(), header=TRUE, sep=",")
data2 %>%
  filter(!is.na(data2[,3])) %>%
  arrange(data2[,3]) %>%
  tail(length(data2[,3])) %>%
  mutate(defecto1=factor(data2[,2], data2[,2])) %>%
  ggplot( aes(y=data2[,3], x=defecto1, fill=data2[,3]) ) +
  geom_bar(stat="identity") +
  scale_fill_viridis() +
  coord_flip() +
  theme_ipsum() +
  labs(x = "",y = "Reprocesos",
       title = "Reprocesos Semanal",
       subtitle = paste("Total Reprocesos Ensamble 1", sum(data2[,3])))

#ggsave("ds4.png", width = 5, height = 5)
data2 <- read.table(file.choose(), header=TRUE, sep=",")
data2 %>%
  filter(!is.na(data2[,3])) %>% #no cuente los que están en ceros
  arrange(data2[,3]) %>% #reordena las filas
  tail(length(data2[,3])) %>%
  mutate(defecto1=factor(data2[,2], data2[,2])) %>%
  ggplot( aes(y=data2[,3], x=defecto1, fill=data2[,3]) ) +
  geom_bar(stat="identity") +
  scale_fill_viridis() +
  coord_flip() +
  theme_ipsum() +
  labs(x = "",y = "Reprocesos",
       title = "Reprocesos Semanal",
       subtitle = paste("Total Reprocesos Ensamble 1", sum(data2[,3])))

###################### Graficos por día  #####################
d1 <- read.table(file.choose(), header=TRUE, sep=",")
ggplot(data = d1, aes(x = d1[,2], y = d1[,3])) +
  geom_bar(stat='identity') + # to draw points
  geom_line()+#+ # to draw a line
  coord_flip() +
theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_text(aes(label=d1[,3]), hjust=0.0, color="red", size=3.5,
            position = position_dodge(0.9))

###################### Otra Grafica #########################


dat2 <- read.table(file.choose(), header=TRUE, sep=",")
dat2 %>%
  filter(!is.na(dat2[,2])) %>%
  arrange(dat2[,2]) %>%
  tail(length(dat2[,2])) %>%
  mutate(defecto2=factor(dat2[,4], dat2[,4])) %>%
  ggplot( aes(y=dat2[,2], x=defecto2, fill=dat2[,2]) ) +
  geom_bar(stat="identity") +
  scale_fill_viridis() +
  coord_flip() +
  theme_ipsum() +
  labs(x = "",y = "Reprocesos",
       title = "Reprocesos Semanal",
       subtitle = paste("Total Reprocesos Ensamble 1", sum(dat2[,4])))


##############################################################################
#####                                                                    #####
#####           Línea de tiempo de pares producidos 2017-2019            #####
#####                                                                    #####
##############################################################################


datap <- read.table(file.choose(), header=T, sep = ',')
datap$Mes <- as.Date(datap$Mes)
p <- datap %>%
  ggplot( aes(x=Mes , y=Pares)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab('') +
  theme_ipsum()
p <- ggplotly(p);p

################### Predicción de producción mes diciembre ######

dec<-c(37119, 29570, 21878,15397)
mes<-c(1,2,3,4)
plot(mes,dec, ylim = c(4500, 50000), xlim = c(1,8)
     ,main = 'Producción de pares', xlab = 'Mes de Producción',
      ylab='Pares producidos')
regresion <- lm(dec ~ mes)
summary(regresion)
abline(regresion)
ndec <- data.frame(mes=seq(1,7))
predict(regresion, ndec)
confint(regresion)
ic <- predict(regresion, ndec, interval = 'confidence')
lines(ndec$mes, ic[, 2], lty = 2, col='blue')
lines(ndec$mes, ic[, 3], lty = 2, col='blue')
points(5, coefficients(regresion)[1]+coefficients(regresion)[2]*5, col='red')
abline(v=5, h=coefficients(regresion)[1]+coefficients(regresion)[2]*5, col='gray', lty=4)
# Intervalos de prediccion
ic <- predict(regresion, ndec, interval = 'prediction')
lines(ndec$mes, ic[, 2], lty = 2, col = 'red')
lines(ndec$mes, ic[, 3], lty = 2, col = 'red')
anova(regresion)
residuos <- rstandard(regresion)
valores.ajustados <- fitted(regresion)
plot(valores.ajustados, residuos)
qqnorm(residuos)
qqline(residuos)



##############################################################################
#####                                                                    #####
#####             Línea de tiempo de reprocesos 2017-2019                #####
#####                                                                    #####
##############################################################################

datar <- read.table(file.choose(), header=T, sep = ',')
datar$Mes <- as.Date(datar$Mes)

r <- datar %>%
  ggplot( aes(x=Mes , y=Reprocesos)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab('') +
  theme_ipsum()

r <- ggplotly(r);r


rc<-c(112101, 174471, 204413)
mes<-c(1,2,3)

plot(mes,rc, ylim = c(100000, 350000), xlim = c(1,8))

regresion1 <- lm(rc ~ mes)
summary(regresion1)
abline(regresion1)
ndec1 <- data.frame(mes=seq(1,7))
predict(regresion1, ndec1)
confint(regresion1)
ic <- predict(regresion1, ndec1, interval = 'confidence')
lines(ndec1$mes, ic[, 2], lty = 2)
lines(ndec1$mes, ic[, 3], lty = 2)
points(6, coefficients(regresion1)[1]+coefficients(regresion1)[2]*6, col='red')
#abline(v=6, h=coefficients(regresion1)[1]+coefficients(regresion1)[2]*6, col='gray', lty=4)
# Intervalos de prediccion
ic1 <- predict(regresion1, ndec1, interval = 'prediction')
lines(ndec1$mes, ic1[, 2], lty = 2, col = 'red')
lines(ndec1$mes, ic1[, 3], lty = 2, col = 'red')
anova(regresion1)
residuos1 <- rstandard(regresion1)
valores.ajustados1 <- fitted(regresion1)
plot(valores.ajustados1, residuos1)
qqnorm(residuos1)
qqline(residuos1)

#################################################################
#####                                                       #####
#####                   Pareto              
#####                                                       #####      
#################################################################

p1<- read.csv("/Users/Admin/Documents/Perugia/Enero/Montado_3/PM3E.csv");p1
attach(p1)
names(p1)
sum(p1[,2])
tipo<-Cantidad
names(tipo)<-Defecto
tipo
Tabla<-pareto.chart(tipo, ylab="Frecuencia", ylab2="Porcentaje Acumulado", 
                    cumperc = seq(0, 100, by = 10), main ='Acumulado semana por defecto',
                    col=heat.colors(length(Defecto)))
Tabla

library(qcc)
#pdf(file ='pa1.pdf', width = 5, height = 5)
defectos <- p1[,2]
names(defectos) <- p1[,1]
color<-rainbow(length(p1[,1]))
pareto.chart(defectos, cumperc = seq(0, 100, by = 10), ylab = "Frecuencia",
             ylab2="Porcentaje acumulado", xlab="Tipos de defectos", 
             main="Defectosde fabricación en un producto", 
             col=color, ylim=c(0,max(p1[,2]+10)))
dev.off()

