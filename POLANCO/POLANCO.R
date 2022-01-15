rm(list=ls())  
library ('shiny')
library("dplyr")
library('ggplot2')
library(plotly)
library(RMySQL)


database <- dbConnect(MySQL(),
                      dbname = "vincom",
                      host = "50.116.110.193",
                      user = 'vincom',
                      password = '.lyA35C0Ve[cL2',
                      port = 3306
)

v1<-dbGetQuery(database,statement = 'select * FROM ventas');v1
v1$fecha <- as.Date(v1$fecha)
f1<-filter(v1,v1[,3]>='2020/02/25' &  v1[,3]<='2020/03/01', v1[,5]=='DAMA MEXICO', 
           #           v1[,2]=='ALTARIA AGUASCALIENTES' )
           #           v1[,2]=='CUAUHTEMOC CDMX')
           #           v1[,2]=='FORUM BUENAVISTA')
           #           v1[,2]=='GALERIA DEL CALZADO GDL')
           #           v1[,2]=='GALERIA DEL ZAPATO')
           #           v1[,2]=='GALERIAS CELAYA')
           #           v1[,2]=='GRAN PLAZA MAZATLAN')
           #           v1[,2]=='GUATEMALA MAJADA')
           #v1[,2]=='LUNA PARC')
           #           v1[,2]=='MORELIA GALERIA')
           #           v1[,2]=='MUNDO E')
           #           v1[,2]=='OUTLET DELTA')
           #           v1[,2]=='OUTLET LERMA')
           #           v1[,2]=='OUTLET PUNTA NORTE')
           #           v1[,2]=='PLAZA ARAGON')
#           v1[,2]=='PLAZA CUERNAVACA')
v1[,2]=='POLANCO')
#v1[,2]=='QUERETARO CENTRO')
#v1[,2]=='QUERETARO CONSTITUYENTES')
#v1[,2]=='VENTA EN LINEA'
#v1[,2]==''
##########################  Ventas por Semana ############################

f2<-ggplot(f1) +
  geom_bar(aes(x =f1[,3]))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(title ="Venta Pares Semana ",
       subtitle = "",
       x = "Fecha", y = "Pares Vendidos")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f2<-ggplotly(f2);f2
sum(f1[,15])
#ggsave("pagos.png", width = 5, height = 5)
######################   Distribución de Precios  ####################

precios<-data.frame(f1[,3], round(f1[,17], 0))
f3<-ggplot(precios) +
  geom_bar(aes(x =precios[,2]))+
  labs(title ="Precios de Venta  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Transacciones")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f3<-ggplotly(f3);f3
#ggsave("pagos.png", width = 5, height = 5)
######################### Descuentos Aplicados ############################
descu<-data.frame(f1[,3], round(f1[,14], 0))
f4<-ggplot(descu) +
  geom_bar(aes(x =descu[,2]))+
  labs(title ="Descuentos Aplicados  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Transacciones")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f4<-ggplotly(f4);f4
#ggsave("pagos.png", width = 5, height = 5)
f4a<-ggplot(data = descu) +
  geom_bar(mapping = aes(x =descu[,1] , y = descu[,2], group=1), stat = "identity")
f4a<-ggplotly(f4a);f4a
#ggsave("pagos.png", width = 5, height = 5)
############################   Costo Unitario ###############################
costo<-data.frame(f1[,3], round(f1[,18], 0))
f5<-ggplot(costo) +
  geom_bar(aes(x =costo[,2]))+
  labs(title ="Precios de Costo  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f5<-ggplotly(f5);f5
#ggsave("pagos.png", width = 5, height = 5)
ggplot(data = costo) +
  geom_bar(mapping = aes(x =costo[,1] , y = costo[,2], fill= costo[,1]), stat = "identity")

plot_ly(data = costo, x = ~costo[,1], y = ~costo[,2], color = ~costo[,1], type = "bar" )

############################  Venta por Tallas #########################

f6<-ggplot(f1) +
  geom_bar(aes(x =f1[,12]))+
  labs(title ="Venta de Pares por Talla  ",
       subtitle = "",
       x = "Tallas", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f6<-ggplotly(f6);f6
#ggsave("pagos.png", width = 5, height = 5)

######################## Venta por Estilo ################################################

f7<-ggplot(f1) +
  geom_bar(aes(x =f1[,11]))+
  labs(title ="Venta de Pares por Estilo ",
       subtitle = "",
       x = "Estilo", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f7<-ggplotly(f7);f7
#ggsave("pagos.png", width = 5, height = 5)
###########################  Venta por Temporada  #######################################

f8<-ggplot(f1) +
  geom_bar(aes(x =f1[,10]))+
  labs(title ="Venta de Pares por Temporada ",
       subtitle = "",
       x = "Temporada", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f8<-ggplotly(f8);f8
#ggsave("pagos.png", width = 5, height = 5)
#########################   Venta por Estatus #############################################

f9<-ggplot(f1) +
  geom_bar(aes(x =f1[,9]))+
  labs(title ="Venta de Pares por Estatus ",
       subtitle = "",
       x = "Estatus", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f9<-ggplotly(f9);f9
#ggsave("pagos.png", width = 5, height = 5)

##########################   Venta por Tipo ################################################

f10<-ggplot(f1) +
  geom_bar(aes(x =f1[,7]))+
  labs(title ="Venta de Pares por Tipo ",
       subtitle = "",
       x = "Tipo", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f10<-ggplotly(f10);f10
#ggsave("pagos.png", width = 5, height = 5)
##########################  Venta por Clasificación ##########################

f11<-ggplot(f1) +
  geom_bar(aes(x =f1[,6]))+
  labs(title ="Venta de Pares por Clasificación-Categoría ",
       subtitle = "",
       x = "Categoria", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f11<-ggplotly(f11);f11
#ggsave("pagos.png", width = 5, height = 5)
##########################   Venta por Cliente #############################
pd<-data.frame(f1[,3], round(f1[,18], 0))
f12<-ggplot(pd) +
  geom_bar(aes(x =pd[,2]))+
  labs(title ="Precios de Costo  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f12<-ggplotly(f12);f12
#ggsave("pagos.png", width = 5, height = 5)

cf<-filter(v1,v1[,3]>='2020/02/21' &  v1[,3]<='2020/02/27', v1[,5]=='DAMA MEXICO', 
           v1[,2]=='ALTARIA AGUASCALIENTES',v1[,18]>='247'&v1[18]<='248' )


cf1<-filter(v1,v1[,3]>='2020/02/21' &  v1[,3]<='2020/02/27', v1[,5]=='DAMA MEXICO', 
            v1[,2]=='ALTARIA AGUASCALIENTES',v1[,18]>='260'&v1[18]<='261' )



resta<-f1[,17]-f1[,18];resta
sum(resta)
sum(resta<'0')
sum(resta=='0')
sum(resta>'0')
sum(f1[,15])

################################################################################
###                                                                          ###                                                         
###                         Diferencia de Venta                              ###
###                                                                          ###  
################################################################################



per<-data.frame(f1, resta)
per$fecha <- as.Date(per$fecha)
fp<-filter(per,per[,3]>='2020/02/25' &  per[,3]<='2020/03/01', per[,5]=='DAMA MEXICO', 
           per[,2]=='ALTARIA AGUASCALIENTES' ,per[,19]<='0')

##########################  Ventas por Semana Perdidas ############################

f2p<-ggplot(fp) +
  geom_bar(aes(x =fp[,3]))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(title ="Venta Pares Semana ",
       subtitle = "",
       x = "Fecha", y = "Pares Vendidos")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f2p<-ggplotly(f2p);f2p
sum(fp[,15])
#ggsave("pagos.png", width = 5, height = 5)
######################   Distribución de Precios Perdidas ####################

precios<-data.frame(fp[,3], floor(fp[,17]))
f3p<-ggplot(precios) +
  geom_bar(aes(x =precios[,2]))+
  labs(title ="Precios de Venta  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Transacciones")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
f3p<-ggplotly(f3p);f3p
#ggsave("pagos.png", width = 5, height = 5)
######################### Descuentos Aplicados Perdidas############################
descu<-data.frame(fp[,3], round(fp[,14], 0))
fp4<-ggplot(descu) +
  geom_bar(aes(x =descu[,2]))+
  labs(title ="Descuentos Aplicados  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Transacciones")+
  geom_text(aes(label=descu[,2]), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp4<-ggplotly(fp4);fp4
#ggsave("pagos.png", width = 5, height = 5)
#f4pa<-ggplot(data = descu) +
#  geom_bar(mapping = aes(x =descu[,1] , y = descu[,2], group=1), stat = "identity")
#f4pa<-ggplotly(f4pa);f4pa
#ggsave("pagos.png", width = 5, height = 5)
############################   Costo Unitario Perdidas ###############################
costo<-data.frame(fp[,3], round(fp[,18], 0))
fp5<-ggplot(costo) +
  geom_bar(aes(x =costo[,2]))+
  labs(title ="Precios de Costo  ",
       subtitle = "",
       x = "Rango de Precios", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp5<-ggplotly(fp5);fp5
#ggsave("pagos.png", width = 5, height = 5)
#ggplot(data = costo) +
#  geom_bar(mapping = aes(x =costo[,1] , y = costo[,2], fill= costo[,1]), stat = "identity")

#plot_ly(data = costo, x = ~costo[,1], y = ~costo[,2], color = ~costo[,1], type = "bar" )

############################  Venta por Tallas Perdidas #########################

fp6<-ggplot(fp) +
  geom_bar(aes(x =fp[,12]))+
  labs(title ="Venta de Pares por Talla  ",
       subtitle = "",
       x = "Tallas", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp6<-ggplotly(fp6);fp6
#ggsave("pagos.png", width = 5, height = 5)

######################## Venta por Estilo Perdidas################################################

fp7<-ggplot(fp) +
  geom_bar(aes(x =fp[,11]))+
  labs(title ="Venta de Pares por Estilo ",
       subtitle = "",
       x = "Estilo", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp7<-ggplotly(fp7);fp7
#ggsave("pagos.png", width = 5, height = 5)
###########################  Venta por Temporada  Perdidas#######################################

fp8<-ggplot(fp) +
  geom_bar(aes(x =fp[,10]))+
  labs(title ="Venta de Pares por Temporada ",
       subtitle = "",
       x = "Temporada", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp8<-ggplotly(fp8);fp8
#ggsave("pagos.png", width = 5, height = 5)
#########################   Venta por Estatus Perdidas#############################################

fp9<-ggplot(fp) +
  geom_bar(aes(x =fp[,9]))+
  labs(title ="Venta de Pares por Estatus ",
       subtitle = "",
       x = "Estatus", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp9<-ggplotly(fp9);fp9
#ggsave("pagos.png", width = 5, height = 5)

##########################   Venta por Tipo Perdidas################################################

fp10<-ggplot(fp) +
  geom_bar(aes(x =fp[,7]))+
  labs(title ="Venta de Pares por Tipo ",
       subtitle = "",
       x = "Tipo", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp10<-ggplotly(fp10);fp10
#ggsave("pagos.png", width = 5, height = 5)
##########################  Venta por Clasificación Perdidas##########################

fp11<-ggplot(fp) +
  geom_bar(aes(x =fp[,6]))+
  labs(title ="Venta de Pares por Clasificación-Categoría ",
       subtitle = "",
       x = "Categoria", y = "Número de Pares")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
fp11<-ggplotly(fp11);fp11
#ggsave("pagos.png", width = 5, height = 5)
