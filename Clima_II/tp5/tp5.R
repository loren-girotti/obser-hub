# seteo el entorno
setwd("~/obser-hub/Clima_II/")
library(tidyverse)

#----
# Ejercicio 1
rendimientos<-read_tsv("./tp5/Rendimientos.tsv")

ggplot(rendimientos,aes(y=rendimientos$Rendimiento,x=rendimientos$Acumulado))+
  geom_point()+
  ylab(label = "Rendimiento Soja")+
  xlab(label = "PP Acumulada Anual - Venado Tuerto [mm]")+
  theme_bw()

# Sí hay una relación entre las variables. Tiene mucho más sentido que mi variable
# predictora sea la PP acumulada, ya que tiene un impacto directo en el desarrollo
# agrícola.

#b) 
modelo<-lm(Rendimiento~Acumulado,data = rendimientos)

# El modelo de regresión lineal me devuelve una lista. Tiene información como los
# coeficientes de la recta en la componente de 'coefficients'; los ajustes del modelo
# a los datos originales en 'fitted values', etc.

#c)

ggplot(rendimientos,aes(y=rendimientos$Rendimiento,x=rendimientos$Acumulado))+
  geom_point(aes(col = "Datos"))+
  ylab(label = "Rendimiento Soja")+
  xlab(label = "PP Acumulada Anual - Venado Tuerto [mm]")+
  geom_abline(aes(intercept = modelo$coefficients[1],slope = modelo$coefficients[2],
              col = "Modelo"))+
  scale_color_manual(name="Referencia",values=c("Modelo"="blue","Datos"="black"))+
  theme_bw()
  

#----
# Ejercicio 4
caudales<-read_csv("./tp5/df_CaudalesAcumulados.csv")
indices<-read_csv("./tp5/df_indices.csv")

#a)
indices_est<-indices%>%
  mutate(Estacion=ifelse(Mes %in% c(12,1,2), "Verano",
                         ifelse(Mes %in% 3:5,"Otonio",
                                ifelse(Mes %in% 6:8,"Invierno","Primavera"))),
         Anio_aux=ifelse(Mes==12,Anio+1,Anio))%>% #(*)
  select(-Mes,-Anio)%>%
  pivot_longer(cols = c(-Anio_aux,-Estacion),
               values_to = "Valor",
               names_to = "Indices")%>%
  group_by(Anio_aux,Estacion,Indices)%>%
  summarise(Promedio=mean(Valor))%>%
  filter(Anio_aux<2010)

# (*) Esta línea me garantiza que los veranos coincidan. Los diciembres con los
# eneros y febreros siguientes
# Anio_aux=ifelse(Mes==12,Anio+1,Anio)

#b)
andacollo<-caudales%>%
  filter(Estacion=="Andacollo",Fase=="Fase_max",Anio<2010)%>%
  pull(Acumulado) # (**)

#(**) Uso 'pull' para que me devuelva la columna como vector y no como tibble.

# Utilizamos las estaciones anteriores porque las interacciones océano-atmósfera
# tienen un cierto retardo hasta que puedan hacer efecto en distintos sistemas.
# por lo tanto queremos ver la correlación del caudal máximo que se da en primavera
# con los indices en estaciones anteriores por este retardo.

# Primero creo un dataframe vacío para poder guardar los resultados de las correlaciones

correlaciones<-tibble(Indice=rep(c("iod","nino12","nino34","nino4","sam","tna","tsa"),times=3),
                      Estacion=rep(c("Otonio","Invierno","Primavera"),each=7),
                      Correlacion=NA,
                      Significancia=NA)

# Loopeo 2 veces, una para los indices y otra para las estaciones:

for(i in c("iod","nino12","nino34","nino4","sam","tna","tsa")){
  for(j in c("Otonio","Invierno","Primavera")){
    fila<-which(correlaciones$Indice==i & correlaciones$Estacion==j)
    
    correlaciones$Correlacion[fila]<-cor(andacollo,indices_est%>%filter(Indices==i,Estacion==j)%>%
                                           pull(Promedio))
    
    correlaciones$Significancia[fila]<-cor.test(andacollo,indices_est%>%filter(Indices==i,Estacion==j)%>%
                                                  pull(Promedio))$p.value<0.05 #(***)
  }
}

#(***) con p.value<0.05 indicamos si la significancia es aceptada o no.


