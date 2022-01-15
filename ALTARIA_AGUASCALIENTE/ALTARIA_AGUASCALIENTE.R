

################### Configuración de la base de datos ################
rm(list=ls())  
library ('shiny')
library("dplyr")
library('ggplot2')
library(plotly)
library(RMySQL)
setwd('/Users/Admin/Documents/Perugia/Reportes_Tiedas/ALTARIA_AGUASCALIENTE')

database <- dbConnect(MySQL(),
                      dbname = "vincom",
                      host = "50.116.110.193",
                      user = 'vincom',
                      password = '.lyA35C0Ve[cL2',
                      port = 3306
)

v1<-dbGetQuery(database,statement = 'select * FROM ventas');v1
v1$fecha <- as.Date(v1$fecha)

###################### Ventas por Fecha  ##########################

f1<-filter(v1,v1[,3]>='2020/02/27' &  v1[,3]<='2020/03/04', v1[,5]=='DAMA MEXICO', 
           v1[,2]=='ALTARIA AGUASCALIENTES' )


##########################  Ventas por Semana ############################

############################ Fecha Vs Estilo ############################

g <- ggplot(f1, aes(fecha))
e<-g +  geom_bar(aes(fill = estilo), position = position_stack(reverse = TRUE)) 
e<-ggplotly(e);e

############################# Fecha vs Clasificación ##################
g <- ggplot(f1, aes(fecha))
e<-g + geom_bar(aes(fill = clasificacion), position = position_stack(reverse = TRUE)) 
e<-ggplotly(e);e
############################# Fecha vs Tipo ###############
g <- ggplot(f1, aes(fecha))
e<-g + geom_bar(aes(fill = tipo), position = position_stack(reverse = TRUE)) 
e<-ggplotly(e);e

############################Fecha vs Temporada ##############

g <- ggplot(f1, aes(fecha))
e<-g + geom_bar(aes(fill = temporada), position = position_stack(reverse = TRUE)) 
e<-ggplotly(e);e
########################### Tipo Vs clasificación ################



g <- ggplot(f1, aes(tipo))
e<-g + geom_bar(aes(fill = clasificacion), position = position_stack(reverse = TRUE)) 
e<-ggplotly(e);e
##################### Tipo vs Estilo #########################
g <- ggplot(f1, aes(tipo))
e<-g + geom_bar(aes(fill = estilo), position = position_stack(reverse = TRUE)) 
e<-ggplotly(e);e

#######################  Clasificación vs Estilo #################

g <- ggplot(f1, aes(clasificacion))
e<-g + geom_bar(aes(fill = estilo), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e
#######################  Cantidad de Pares vs Estilo ####################
g <- ggplot(f1, aes(cantidad))
e<-g + geom_bar(aes(fill = estilo), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

####################  Temporada vs Estilo #######################

g <- ggplot(f1, aes(temporada))
e<-g + geom_bar(aes(fill = estilo), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

###################### Temporada vs Tipo #######################

g <- ggplot(f1, aes(temporada))
e<-g + geom_bar(aes(fill = tipo), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

###################### Temporada vs Clasificación #######################

g <- ggplot(f1, aes(temporada))
e<-g + geom_bar(aes(fill = clasificacion), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

########################## Estatus vs Clasificación ########################

g <- ggplot(f1, aes(status))
e<-g + geom_bar(aes(fill = clasificacion), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

####################### Estatus vs Tipo  ###################################
g <- ggplot(f1, aes(status))
e<-g + geom_bar(aes(fill = tipo), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

#####################  Estatus vs Estilo #############################
g <- ggplot(f1, aes(status))
e<-g + geom_bar(aes(fill = estilo), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

########################### Talla vs Estilo  #################
g <- ggplot(f1, aes(talla))
e<-g + geom_bar(aes(fill = estilo ), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e
########################### Talla vs Clasificación #################

g <- ggplot(f1, aes(talla))
e<-g + geom_bar(aes(fill = clasificacion ), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

############################  Talla vs Tipo ################
g <- ggplot(f1, aes(talla))
e<-g + geom_bar(aes(fill = tipo ), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e


###########################Venta de Pares por semana ###############################

f2<-ggplot(f1) +
  geom_bar(aes(x =f1[,3]))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(title ="Venta Pares Semana ",
       subtitle = "",
       x = "Fecha", y = "Pares Vendidos")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f2<-ggplotly(f2);f2
sum(f1[,15])
ggsave("semana.png", width = 10, height = 10)
######################   Precios  ####################

precios<-data.frame(f1[,3], round(f1[,17], 0))
f3<-ggplot(precios) +
  geom_bar(aes(x =precios[,2]))+
  labs(title ="Precios de Venta  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Transacciones")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f3<-ggplotly(f3);f3
ggsave("precios.png", width = 10, height = 10)






######################### Descuentos Aplicados ############################

descu<-data.frame(f1[,3], round(f1[,14], 0))
f4<-ggplot(descu) +
  geom_bar(aes(x =descu[,2]))+
  labs(title ="Descuentos Aplicados  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Transacciones")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f4<-ggplotly(f4);f4
ggsave("descuento.png", width = 10, height = 10)

############################   Costo Unitario ###############################
costo<-data.frame(f1[,3], round(f1[,18], 0))
f5<-ggplot(costo) +
  geom_bar(aes(x =costo[,2]))+
  labs(title ="Precios de Costo  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f5<-ggplotly(f5);f5
ggsave("unitario.png", width = 10, height = 10)

############################  Tallas #########################

f6<-ggplot(f1) +
  geom_bar(aes(x =f1[,12]))+
  labs(title ="Venta de Pares por Talla  ",
       subtitle = "",
       x = "Tallas", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f6<-ggplotly(f6);f6
ggsave("tallas.png", width = 10, height = 10)

######################## Estilo ################################################

f7<-ggplot(f1) +
  geom_bar(aes(x =f1[,11]))+
  labs(title ="Venta de Pares por Estilo ",
       subtitle = "",
       x = "Estilo", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f7<-ggplotly(f7);f7
ggsave("estilo.png", width = 10, height = 10)

###########################  Temporada  #######################################

f8<-ggplot(f1) +
  geom_bar(aes(x =f1[,10]))+
  labs(title ="Venta de Pares por Temporada ",
       subtitle = "",
       x = "Temporada", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f8<-ggplotly(f8);f8
ggsave("temporada.png", width = 10, height = 10)
######################### Estatus ###################################

f9<-ggplot(f1) +
  geom_bar(aes(x =f1[,9]))+
  labs(title ="Venta de Pares por Estatus ",
       subtitle = "",
       x = "Estatus", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f9<-ggplotly(f9);f9
ggsave("status.png", width = 10, height = 10)

##########################   Tipo ################################################

f10<-ggplot(f1) +
  geom_bar(aes(x =f1[,7]))+
  labs(title ="Venta de Pares por Tipo ",
       subtitle = "",
       x = "Tipo", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f10<-ggplotly(f10);f10
ggsave("tipo.png", width = 10, height = 10)

##########################  Clasificación ##########################

f11<-ggplot(f1) +
  geom_bar(aes(x =f1[,6]))+
  labs(title ="Venta de Pares por Clasificación-Categoría ",
       subtitle = "",
       x = "Categoria", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f11<-ggplotly(f11);f11
ggsave("clasificacion.png", width = 10, height = 10)

################################################################################
###                                                                          ###                                                         
###                           Diferencia de Venta                            ###
###                                                                          ###  
################################################################################

resta<-round(f1[,17]-f1[,18],0);resta
sum(resta)
sum(resta<'0')
sum(resta=='0')
sum(resta>'0')
min(resta)
max(resta)
resta[3] # Comado de evaluación 




per<-data.frame(f1, resta)
per$fecha <- as.Date(per$fecha)
fp<-filter(per,per[,3]>='2020/02/25' &  per[,3]<='2020/03/01', per[,5]=='DAMA MEXICO', 
           per[,2]=='ALTARIA AGUASCALIENTES' ,per[,19]<='0')
sum(fp[,19])
sum(fp[,15])


##########################  Ventas por Semana Diferencia  ############################

f2p<-ggplot(fp) +
  geom_bar(aes(x =fp[,3]))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(title ="Venta Pares Semana ",
       subtitle = "",
       x = "Fecha", y = "Pares Vendidos")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f2p<-ggplotly(f2p);f2p
sum(fp[,15])
ggsave("ppagos.png", width = 10, height = 10)
######################   Precios Diferencia ####################

precios<-data.frame(fp[,3], floor(fp[,17]))
f3p<-ggplot(precios) +
  geom_bar(aes(x =precios[,2]))+
  labs(title ="Precios de Venta  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Transacciones")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f3p<-ggplotly(f3p);f3p
ggsave("pprecios.png", width = 10, height = 10)

################### Descuentos Aplicados Diferencia #######################

descu<-data.frame(fp[,3], round(fp[,14], 0))
fp4<-ggplot(descu) +
  geom_bar(aes(x =descu[,2]))+
  labs(title ="Descuentos Aplicados  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Transacciones")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp4<-ggplotly(fp4);fp4
ggsave("pdescu.png", width = 10, height = 10)

####################### Costo Unitario Diferencia #########################

costo<-data.frame(fp[,3], round(fp[,18], 0))
fp5<-ggplot(costo) +
  geom_bar(aes(x =costo[,2]))+
  labs(title ="Precios de Costo  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp5<-ggplotly(fp5);fp5
ggsave("punitario.png", width = 10, height = 10)

############################ Tallas Diferencia #########################

fp6<-ggplot(fp) +
  geom_bar(aes(x =fp[,12]))+
  labs(title ="Venta de Pares por Talla  ",
       subtitle = "",
       x = "Tallas", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp6<-ggplotly(fp6);fp6
ggsave("ptallas.png", width = 10, height = 10)

######################## Estilo Diferencia  ############################

fp7<-ggplot(fp) +
  geom_bar(aes(x =fp[,11]))+
  labs(title ="Venta de Pares por Estilo ",
       subtitle = "",
       x = "Estilo", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp7<-ggplotly(fp7);fp7
ggsave("pestilo.png", width = 10, height = 10)

########################### Temporada  Diferencia #######################

fp8<-ggplot(fp) +
  geom_bar(aes(x =fp[,10]))+
  labs(title ="Venta de Pares por Temporada ",
       subtitle = "",
       x = "Temporada", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp8<-ggplotly(fp8);fp8
ggsave("ptemporada.png", width = 10, height = 10)

######################### Estatus Diferencia #############################

fp9<-ggplot(fp) +
  geom_bar(aes(x =fp[,9]))+
  labs(title ="Venta de Pares por Estatus ",
       subtitle = "",
       x = "Estatus", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp9<-ggplotly(fp9);fp9
ggsave("pstatus.png", width = 10, height = 10)

########################## Tipo Diferencia #######################

fp10<-ggplot(fp) +
  geom_bar(aes(x =fp[,7]))+
  labs(title ="Venta de Pares por Tipo ",
       subtitle = "",
       x = "Tipo", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp10<-ggplotly(fp10);fp10
ggsave("ptipo.png", width = 10, height = 10)

####################### Clasificación Diferencia ###################

fp11<-ggplot(fp) +
  geom_bar(aes(x =fp[,6]))+
  labs(title ="Venta de Pares por Clasificación-Categoría ",
       subtitle = "",
       x = "Categoria", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp11<-ggplotly(fp11);fp11
ggsave("pclasificacion.png", width = 10, height = 10)

################################################################################
###                                                                          ###                                                         
###                         Positivo de Venta                                ###
###                                                                          ###  
################################################################################



fg<-filter(per,per[,3]>='2020/02/25' &  per[,3]<='2020/03/01', per[,5]=='DAMA MEXICO', 
           per[,2]=='ALTARIA AGUASCALIENTES' ,per[,19]>='0')
sum(fg[,19])
sum(fg[,15])

######################  Pares Vendidos Positivo ####################

f2g<-ggplot(fg) +
  geom_bar(aes(x =fg[,3]))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(title ="Venta Pares Semana ",
       subtitle = "",
       x = "Fecha", y = "Pares Vendidos")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f2g<-ggplotly(f2g);f2g
sum(fp[,15])
ggsave("gpares.png", width = 10, height = 10)

######################   Precios Positivo ####################

precios<-data.frame(fg[,3], floor(fg[,17]))
f3g<-ggplot(precios) +
  geom_bar(aes(x =precios[,2]))+
  labs(title ="Precios de Venta  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Transacciones")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f3g<-ggplotly(f3g);f3g
ggsave("gprecios.png", width = 10, height = 10)

######################### Descuento Aplicado Positivo  ############################
descu<-data.frame(fg[,3], round(fg[,14], 0))
fg4<-ggplot(descu) +
  geom_bar(aes(x =descu[,2]))+
  labs(title ="Descuentos Aplicados  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Transacciones")+
    theme(axis.text.x=element_text(angle=90, hjust=1))
fg4<-ggplotly(fg4);fg4
ggsave("gdescu.png", width = 10, height = 10)

############################   Costo Unitario Positivo ###############################
costo<-data.frame(fg[,3], round(fg[,18], 0))
fg5<-ggplot(costo) +
  geom_bar(aes(x =costo[,2]))+
  labs(title ="Precios de Costo  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fg5<-ggplotly(fg5);fg5
ggsave("gcosto.png", width = 10, height = 10)


############################  Tallas Positivo #########################

fg6<-ggplot(fg) +
  geom_bar(aes(x =fg[,12]))+
  labs(title ="Venta de Pares por Talla  ",
       subtitle = "",
       x = "Tallas", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fg6<-ggplotly(fg6);fg6
ggsave("gtallas.png", width = 10, height = 10)

######################## Estilo Positivo  ##############################

fg7<-ggplot(fg) +
  geom_bar(aes(x =fg[,11]))+
  labs(title ="Venta de Pares por Estilo ",
       subtitle = "",
       x = "Estilo", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fg7<-ggplotly(fg7);fg7
ggsave("gestilo.png", width = 10, height = 10)

########################### Temporada  Peositivo ######################

fg8<-ggplot(fg) +
  geom_bar(aes(x =fg[,10]))+
  labs(title ="Venta de Pares por Temporada ",
       subtitle = "",
       x = "Temporada", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fg8<-ggplotly(fg8);fg8
ggsave("gtemporada.png", width = 10, height = 10)

######################### Estatus Positivo ###############################

fg9<-ggplot(fg) +
  geom_bar(aes(x =fg[,9]))+
  labs(title ="Venta de Pares por Estatus ",
       subtitle = "",
       x = "Estatus", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fg9<-ggplotly(fg9);fg9
ggsave("gstatus.png", width = 10, height = 10)

##########################  Tipo Positivo ############################

fg10<-ggplot(fg) +
  geom_bar(aes(x =fg[,7]))+
  labs(title ="Venta de Pares por Tipo ",
       subtitle = "",
       x = "Tipo", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fg10<-ggplotly(fg10);fg10
ggsave("gtipo.png", width = 10, height = 10)

######################### Clasificación Positivo   ###################

fg11<-ggplot(fg) +
  geom_bar(aes(x =fg[,6]))+
  labs(title ="Venta de Pares por Clasificación-Categoría ",
       subtitle = "",
       x = "Categoria", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fg11<-ggplotly(fg11);fg11
ggsave("gclasificacion.png", width = 10, height = 10)

#######################################################################
###                                                                 ###
###                 Concentrado de Resultados                       ###  
###                                                                 ###  
#######################################################################

## número de pares vendidos todos ##
pv<-sum(f1[,15]);pv
## Total de Venta con descuento aplicado ##
vt<-sum(f1[,16]);vt




