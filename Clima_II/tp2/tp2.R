rm(list = ls())
gc()

# Llamamos a la librería tidyverse

library(tidyverse)

#------------------
# EJERCICIO 1
data('iris');iris

#a)
iris_tb<-as_tibble(iris)

#b)
iris;iris_tb

#c)
sepal_length_df<-iris$Sepal.Length
sepal_length_tb<-iris_tb$Sepal.Length

class(sepal_length_df)
class(sepal_length_tb) #Se obtiene la misma estructura en ambos casos.

is.vector(sepal_length_df)
is.vector(sepal_length_tb)
is_tibble(sepal_length_df)
is_tibble(sepal_length_tb)

#d)
sepal_length_df<-iris[,1]
sepal_length_tb<-iris_tb[,1]

is.vector(sepal_length_df) #TRUE
is.vector(sepal_length_tb) #FALSE
is_tibble(sepal_length_df) #FALSE
is_tibble(sepal_length_tb) #TRUE

#e)
which(sepal_length_df==4.3)
which(sepal_length_tb==4.3)

 # PREGUNTAR PORQUE ME DIO TODO IGUAL.

#---------------
# EJERCICIO 2
setwd("~/obser-hub/Clima_II/")
data<-read.csv(file="./tp1/Datos.csv") # read con R base
data_rdr<-read_csv(file="./tp1/Datos.csv") # read con readR 

# Leyendo con read_csv nos da mucha info en consola ademas de que distingue las
# fechas.

# a)
class(data) #es un Data Frame
class(data_rdr) #es un Tibble

class(data$Fecha) # character
class(data_rdr$Fecha) # Date

#b)
data$Fecha<-ymd(data$Fecha)
class(data$Fecha)

#c)
mdq<-filter(.data=data, Estacion=="MAR DEL PLATA AERO")
class(mdq)#data frame

#d)
mdq$Logico<-logical(nrow(mdq))

for(i in 1:nrow(mdq)){
  mdq$Logico[i]<-mdq$PP[i] > 0.0
}

#e)
mdq %>%
  select("Estacion","Fecha","PP") %>%
  mutate(Logico=ifelse(mdq$PP>0,"TRUE","FALSE"))

#-----------
#EJERCICIO 3
pp_pergamino<-read_tsv(file="./tp2/Pergamino_INTA.tsv",col_names=F,na ="-99.9")
pp_pergamino<-pp_pergamino[,1:6]

#a)
names(pp_pergamino)<-c("Year","Month","Day","PP","Tmax","Tmin")

#b)
pp_pergamino<-unite(pp_pergamino,"Fecha","Year","Month","Day",sep = "-")
# pp_pergamino<-unite(pp_pergamino,"Fecha","Year","Month","Day",sep = "-") %>% 

pp_pergamino$Fecha<-ymd(pp_pergamino$Fecha)

#c)
pp_1996<-pp_pergamino %>%
  select("Fecha","PP") %>%
  filter(year(Fecha)==1996,PP>0) %>%
  summarise(Acumulado_1996=sum(PP))

#-------------
# EJERCICIO 4
caudales<-read_csv("./tp2/df_Caudales.csv")

#a)
missing_values<- function(vector){
  nas_porcentual<-length(which(is.na(vector)))/length(vector)
  return(nas_porcentual)
}
  
#b)
datos_faltantes<-select(.data=caudales,-Fecha) %>%
  map(.f=missing_values)

mas_datos<-which.min(datos_faltantes);names(mas_datos);datos_faltantes$PasodeIndios

# PASO DE INDIOS NO TIENE NINGÚN DATO FALTANTE.

#c)
caudales_t<-pivot_longer(data=caudales,
                         cols=-Fecha, #Selecciono todo menos la Fecha
                         names_to = "Estacion",
                         values_to = "Caudal")



#d)
caudal_anual_PasoIndios<-filter(.data=caudales_t,Estacion=="PasodeIndios")%>%
  mutate(Año=year(Fecha))%>%
  group_by(Año)%>%
  summarise(Caudal_Medio=mean(Caudal,na.rm=T))

#e)
ggplot(data=caudal_anual_PasoIndios,aes(x=Año,y=Caudal_Medio))+
  geom_point()+
  geom_line(col="salmon")+
  ylab("Caudal medio")+
  xlab("Año")+
  ggtitle("Caudal Medio Anual - Paso de Indios")

#----
# EJERCICIO 5

#a)
caudales_comahue<-read_csv("./tp2/CaudalMedioDiario_Comahue.csv")

Qsep_Andacollo<-caudales_comahue%>%
  filter(Estacion=="Andacollo",month(Fecha)==9)

ggplot(data=Qsep_Andacollo,aes(x=Caudal))+
  geom_histogram(col="blue",fill="blue")+
  scale_x_continuous(breaks=seq(0,1000,by=100))+
  scale_y_continuous(breaks=seq(0,400,by=50))

#b)
Qmensual_Andacollo<-caudales_comahue%>%
  filter(Estacion=="Andacollo")%>%
  mutate(Mes=month(Fecha))%>%
  group_by(Mes)%>%
  summarise(ValorMedio=mean(Caudal,na.rm=T))

ggplot(data=Qmensual_Andacollo,aes(x=factor(Mes),y=ValorMedio))+
  geom_bar(stat="identity",col="black",fill="salmon")+
  xlab("Mes")+
  ylab("Caudal Medio")+
  ggtitle("Caudal Medio Mensual - Andacollo")

#c)
Qmensual_anual_Andacollo<-caudales_comahue%>%
  filter(Estacion=="Andacollo")%>%
  mutate(Año=year(Fecha),Mes=month(Fecha))%>%
  group_by(Año,Mes)%>%
  summarise(ValorMedio=mean(Caudal,na.rm=T))
                         
ggplot(data=Qmensual_anual_Andacollo,aes(x=factor(Mes),y=ValorMedio))+
  geom_boxplot()+
  scale_y_continuous(breaks=seq(0,400,by=50))+
  xlab("Mes")+
  ylab("Caudal Medio")+
  ggtitle("Caudal Medio Anual por Mes - Andacollo")



