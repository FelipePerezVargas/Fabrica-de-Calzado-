rm(list=ls())  
library ('shiny')
library("dplyr")
library('ggplot2')
library(plotly)
library(RMySQL)
setwd('/Users/Admin/Documents/Perugia/Reportes_Tiedas/GENERAL')

database <- dbConnect(MySQL(),
                      dbname = "vincom",
                      host = "50.116.110.193",
                      user = 'vincom',
                      password = '.lyA35C0Ve[cL2',
                      port = 3306
)

v1<-dbGetQuery(database,statement = 'select * FROM ventas');v1
v1$fecha <- as.Date(v1$fecha)

###################### Ventas por Fecha y Tienda ##########################

f0<-filter(v1,v1[,3]>='2020/03/03' &  v1[,3]<='2020/03/08')
sum(f0[,15])

#h<-ggplot(fo, aes(Fecha, dias[,i])) +
#  geom_line(color="gray", size=0.5)+
#  geom_point()+
#  ggtitle(paste("Ventas Pares por semana",nm[i] ))+
#  geom_smooth(method="lm", col="firebrick")+
#  labs(x = "Fecha", y = "Número de Pares ")+
#  scale_x_date(date_labels = "%B/%Y", breaks='1 month')+
#  theme(axis.text.x=element_text(angle=90, hjust=1))
#print(ggplotly(h))

##################### Resultados Generales ##########################

g0 <-ggplot(f0, aes(factor(fecha), fill = factor(linea))) +
  labs(title ="Venta General Fecha vs Línea por Tienda ",
       subtitle = "",
       x = "Fecha", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0<-ggplotly(g0);g0

g0a <-ggplot(f0, aes(factor(tienda), fill = factor(linea))) +
  labs(title ="Venta General Tienda vs Línea ",
       subtitle = "",
       x = "Tienda", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a

g0a <-ggplot(f0, aes(factor(tienda), fill = factor(cantidad))) +
  labs(title ="Venta General Tienda vs Línea ",
       subtitle = "",
       x = "Tienda", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a


g0a <-ggplot(f0, aes(factor(tienda), fill = factor(talla))) +
  labs(title ="Venta General Tienda vs Talla ",
       subtitle = "",
       x = "Tienda", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a

g0a <-ggplot(f0, aes(factor(talla), fill = factor(tienda))) +
  labs(title ="Venta General Talla vs Tienda  ",
       subtitle = "",
       x = "Talla", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a


g0a <-ggplot(f0, aes(factor(clasificacion), fill = factor(tipo))) +
  labs(title ="Venta General Clasificación vs Tipo  ",
       subtitle = "",
       x = "Clasificación ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a

g0a <-ggplot(f0, aes(factor(clasificacion), fill = factor(temporada))) +
  labs(title ="Venta General Clasificación vs Temporada  ",
       subtitle = "",
       x = "Clasificación ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a


g0a <-ggplot(f0, aes(factor(clasificacion), fill = factor(status))) +
  labs(title ="Venta General Clasificación vs Estatus  ",
       subtitle = "",
       x = "Clasificación ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a

g0a <-ggplot(f0, aes(factor(estilo), fill = factor(clasificacion))) +
  labs(title ="Venta General Estilo vs Clasificación  ",
       subtitle = "",
       x = "Estilo ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a

g0a <-ggplot(f0, aes(factor(clasificacion), fill = factor(estilo))) +
  labs(title ="Venta General Clasificación vs Estilo  ",
       subtitle = "",
       x = "Estilo ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a

############################  Consultar #######################
f0<-filter(v1,v1[,3]>='2020/03/03' &  v1[,3]<='2020/03/08'
           & v1[11]!='PERUGIA GOLD VARIOS VARIOS')
g0a <-ggplot(f0, aes(factor(fecha), fill = factor(round(importe,0)))) +
  labs(title ="Venta General Fecha vs Precio  ",
       subtitle = "",
       x = "Estilo ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a


g0a <-ggplot(f0, aes(factor(tienda), fill = factor(round(importe,0)))) +
  labs(title ="Venta General Fecha vs Precio  ",
       subtitle = "",
       x = "Estilo ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a

g0a <-ggplot(f0, aes(factor(round(importe,0)), fill = factor(estilo))) +
  labs(title ="Venta General Fecha vs Precio  ",
       subtitle = "",
       x = "Estilo ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a


########## Salida en cero de pares filtrado  ###########
fp<-filter(v1,v1[,3]>='2020/03/03' &  v1[,3]<='2020/03/08',
           round(importe,0)<='0'& v1[11]!='PERUGIA GOLD VARIOS VARIOS',
           v1[,5]=='DAMA MEXICO')

g0a <-ggplot(fp, aes(factor(estilo), fill = factor(round(importe,0)))) +
  labs(title ="Venta General Fecha vs Precio  ",
       subtitle = "",
       x = "Estilo ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a

g0a <-ggplot(fp, aes(factor(tienda), fill = factor(estilo))) +
  labs(title ="Venta General Fecha vs Precio  ",
       subtitle = "",
       x = "Estilo ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a
#saveWidget(g0a, "tiendas.html")


g0a <-ggplot(fp, aes(factor(tienda), fill = factor(clasificacion))) +
  labs(title ="Venta General Fecha vs Precio  ",
       subtitle = "",
       x = "Estilo ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a
saveWidget(g0a, "tiendas.html")

g0a <-ggplot(fp, aes(factor(tienda), fill = factor(tipo))) +
  labs(title ="Venta General Fecha vs Precio  ",
       subtitle = "",
       x = "Estilo ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a
saveWidget(g0a, "tiendas.html")


g0a <-ggplot(fp, aes(factor(tienda), fill = factor(status))) +
  labs(title ="Venta General Fecha vs Precio  ",
       subtitle = "",
       x = "Estilo ", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g0a<-ggplotly(g0a);g0a
saveWidget(g0a, "tiendas.html")


f1<-filter(v1,v1[,3]>='2020/02/28' &  v1[,3]<='2020/03/05', v1[,5]=='DAMA MEXICO')
f2<-filter(v1,v1[,3]>='2020/02/28' &  v1[,3]<='2020/03/05', v1[,5]=='ACCESORIOS DE CALZADO')
f3<-filter(v1,v1[,3]>='2020/02/28' &  v1[,3]<='2020/03/05', v1[,5]=='PERUGIA GOLD')
f4<-filter(v1,v1[,3]>='2020/02/28' &  v1[,3]<='2020/03/05', v1[,5]=='FLETES')
f5<-filter(v1,v1[,3]>='2020/02/28' &  v1[,3]<='2020/03/05', v1[,5]=='BISUTERIA')


###################### Ventas por Departamento #########################

g1 <-ggplot(f1, aes(factor(fecha), fill = factor(tienda))) +
  labs(title ="Venta Pares Enero por Tienda ",
       subtitle = "",
       x = "Fecha", y = "Pares Vendidos")+
  geom_bar()+theme(axis.text.x=element_text(angle=90, hjust=1))
g1<-ggplotly(g1);g1





#saveWidget(m1, "prueba.html")
#saveWidget(as.widget(m1), "index.html")
#saveWidget(m1, "p1.html", selfcontained = F, libdir = "lib")


g <- ggplot(f1, aes(fecha))
e<-g +  geom_bar(aes(fill = estilo), position = position_stack(reverse = TRUE)) 
e<-ggplotly(e);e


g <- ggplot(f1, aes(fecha))
e<-g + geom_bar(aes(fill = clasificacion), position = position_stack(reverse = TRUE)) 
e<-ggplotly(e);e
###########################
g <- ggplot(f0, aes(fecha))
e<-g + geom_bar(aes(fill = talla), position = position_stack(reverse = TRUE)) 
e<-ggplotly(e);e

g <- ggplot(f0, aes(tienda))
e<-g + geom_bar(aes(fill = talla), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

g <- ggplot(f0, aes(tienda))
e<-g + geom_bar(aes(fill = cantidad), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

g <- ggplot(f0, aes(tienda))
e<-g + geom_bar(aes(fill = estilo), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

g <- ggplot(f0, aes(tienda))
e<-g + geom_bar(aes(fill = clasificacion), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

g <- ggplot(f0, aes(tienda))
e<-g + geom_bar(aes(fill = tipo), position = position_stack(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
e<-ggplotly(e);e

##########################  Ventas por Semana ############################

f2<-ggplot(f0) +
  geom_bar(aes(x =f0[,3]))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(title ="Venta Pares Semana ",
       subtitle = "",
       x = "Fecha", y = "Pares Vendidos")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f2<-ggplotly(f2);f2
sum(f0[,15])
ggsave("semana.png", width = 10, height = 10)
######################   Precios  ####################

precios<-data.frame(f0[,3], round(f0[,17], 0))
f3<-ggplot(precios) +
  geom_bar(aes(x =precios[,2]))+
  labs(title ="Precios de Venta  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Transacciones")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f3<-ggplotly(f3);f3
ggsave("precios.png", width = 10, height = 10)
######################### Descuentos Aplicados ############################

descu<-data.frame(f0[,3], round(f0[,14], 0))
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

f7<-ggplot(f0) +
  geom_bar(aes(x =f0[,11]))+
  labs(title ="Venta de Pares por Estilo ",
       subtitle = "",
       x = "Estilo", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f7<-ggplotly(f7);f7
ggsave("estilo.png", width = 10, height = 10)

###########################  Temporada  #######################################

f8<-ggplot(f0) +
  geom_bar(aes(x =f0[,10]))+
  labs(title ="Venta de Pares por Temporada ",
       subtitle = "",
       x = "Temporada", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f8<-ggplotly(f8);f8
ggsave("temporada.png", width = 10, height = 10)
######################### Estatus ###################################

f9<-ggplot(f0) +
  geom_bar(aes(x =f0[,9]))+
  labs(title ="Venta de Pares por Estatus ",
       subtitle = "",
       x = "Estatus", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f9<-ggplotly(f9);f9
ggsave("status.png", width = 10, height = 10)

##########################   Tipo ################################################

f10<-ggplot(f0) +
  geom_bar(aes(x =f0[,7]))+
  labs(title ="Venta de Pares por Tipo ",
       subtitle = "",
       x = "Tipo", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f10<-ggplotly(f10);f10
ggsave("tipo.png", width = 10, height = 10)

##########################  Clasificación ##########################

f11<-ggplot(f0) +
  geom_bar(aes(x =f0[,6]))+
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

resta<-round(f0[,17]-f0[,18],0);resta
sum(resta)
sum(resta<'0')
sum(resta=='0')
sum(resta>'0')
min(resta)
max(resta)
#resta[3] # Comado de evaluación 



per<-data.frame(f0, resta)
per$fecha <- as.Date(per$fecha)
fp1<-filter(per,per[,3]>='2020/03/02' &  per[,3]<='2020/03/08', per[,5]=='DAMA MEXICO', 
            per[,19]<='0')
sum(fp1[,19])
sum(fp1[,15])


##########################  Ventas por Semana Diferencia  ############################

f2p<-ggplot(fp1) +
  geom_bar(aes(x =fp1[,3]))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(title ="Venta Pares Semana ",
       subtitle = "",
       x = "Fecha", y = "Pares Vendidos")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f2p<-ggplotly(f2p);f2p
sum(fp[,15])
ggsave("ppagos.png", width = 10, height = 10)
######################   Precios Diferencia ####################

precios<-data.frame(fp1[,3], floor(fp1[,17]))
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
           per[,19]>='0')
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





