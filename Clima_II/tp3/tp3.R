rm(list =ls())
gc()

library(tidyverse)
#----
# Ejercicio 1

#a)
N<-10000
a<-rnorm(n=N,mean=10,sd=3)
b<-rnorm(n=N,mean=10,sd=6)
c<-rnorm(n=N,mean=10,sd=1)

df<-tibble(a,b,c)
df_t<-pivot_longer(df,cols=c(a,b,c),
                   names_to="Serie",
                   values_to="Valor")

#c)
ggplot(data=df_t,aes(x=Valor,col=Serie))+
  geom_density()

  
#d)
a_mean<-mean(a)
a_median<-median(a)
dif_mean_median<-a_mean-a_median #-0.023

#son prácticamente iguales, debido a la simetría de la distribución normal.

#e) Sí coinciden, es por la simetría. No ocurrirá lo mismo para distribuciones que no sean simétricas.

#----
# EJERCICIO 2
N<-1000
df<-tibble(x=c(rnorm(n=N,mean = 15,sd=3)),y=c(rnorm(n=N,mean = 15,sd=3)))
df$y[(length(df$y)-9):length(df$y)]<-1000

#b)
mean_x<-mean(df$x)
median_x<-median(df$x) # Ambos son similares, pues es una distribución normal

#c)
mean_y<-mean(df$y)
median_y<-median(df$y) 
# Difieren claramente puesto que la media es sensible a valores; en cambio la mediana no.
# La mediana es RESISTENTE con respecto a los valores extremos.

#d)
IQR_x<-IQR(df$x)
sd_x<-sd(df$x)
abs(IQR_x-sd_x)

IQR_y<-IQR(df$y)
sd_y<-sd(df$y)
abs(IQR_y-sd_y)

# hay una amplia diferencia entre ambos casos, debido a que el desvío estándar utiliza
# la media para calcularse, y por ende no es resistente a valores extremos.
# Es conveniente en estos casos utilizar parámetros de posición como lo es la mediana
# y el IQR.

df%>%pivot_longer(cols=c(x,y),
                  names_to="Variable",
                  values_to="Valor")%>%
  ggplot(aes(y=Valor,x=factor(Variable),fill=Variable))+
  geom_boxplot()+
  coord_trans(y="log10")
  
# No se puede ver el gráfico sin agregar la escala logarítmica. Esto se debe a que al tener una gran diferencia
# de valores con los outsiders, la escala se estira para intentar graficarlos a todos.
# Con una escala logarítmica podemos visualizar un poco mejor la distribución de los vectores.

#----
# EJERCICIO 3

#a) 
N<-100000
gamma1<-rgamma(n=N,shape=1,scale=1)
gamma3<-rgamma(n=N,shape=1,scale=3)
gamma6<-rgamma(n=N,shape=1,scale=6)

gammas<-tibble(gamma1,gamma3,gamma6)%>%pivot_longer(cols=c(gamma1,gamma3,gamma6),
                                     names_to="Distribución",
                                     values_to="Valor")

#b)
gammas%>%ggplot(aes(x=Valor,col=Distribución))+
  geom_density()

#La escala distribuye la frecuencia a lo largo de los valores, a mayor escala, una distribución más dispersa.

#c) 
gamma1<-rgamma(n=N,shape=1,scale=1)
gamma3<-rgamma(n=N,shape=3,scale=1)
gamma6<-rgamma(n=N,shape=6,scale=1)

gammas<-tibble(gamma1,gamma3,gamma6)%>%pivot_longer(cols=c(gamma1,gamma3,gamma6),
                                     names_to="Distribución",
                                     values_to="Valor")
gammas%>%ggplot(aes(x=Valor,col=Distribución))+
  geom_density()
  
# El parámetro de forma nos varía el valor de la moda.

#d)
1-pgamma(10,shape=5,scale=1)

#----
# EJERCICIO 4

library(moments)
gamma<-rgamma(n=10000,shape=5,scale=1)

df<-tibble(gamma)
#a)
df%>%
  ggplot(aes(x=gamma))+
  geom_histogram(fill="skyblue",color="black")+
  geom_vline(aes(xintercept = media,color="Media"),size=0.8)+
  scale_color_manual(name = "Referencia",values = c("Media"="red"))+
  labs(title="Histograma de una muestra con tamaño 10000",
       x = "Valor",
       y = "Frecuencia")
#b)
media<-mean(gamma) #media muestral
# se ve claro que no es simétrica la distribución respecto de la media.

#c)
asimetria<-skewness(gamma)
#valor >0, lo cuál tiene sentido porque tiene una cola más larga a la derecha.

#d)
a<-rgamma(n=10000,shape=1,scale=1)
b<-rgamma(n=10000,shape=3,scale=5)
df<-tibble(a,b)%>%pivot_longer(cols=c(a,b),
                               names_to="Distribucion",
                               values_to="Valores")

df%>%
  ggplot(aes(x = Valores, fill = Distribucion)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 50) +
  labs(title = "Comparación de distribuciones Gamma",
       x = "Valor",
       y = "Frecuencia") +
  theme_linedraw()

#e)
a_curt<-kurtosis(a)
b_curt<-kurtosis(b)

# La curtosis de la función gamma se refleja en la concentracíon de valores
# respecto de la media.

#----
# Ejercicio 5
setwd("~/obser-hub/Clima_II/tp3/")
df<-read_csv("./LaQuiacaObservatorio.csv")
df_inv<-df%>%
  mutate(
    mes = month(Fecha),
    anio = year(Fecha)) %>% # Separo los meses y anios en columnas distintas
  filter(mes %in% c(6,7,8)) %>% # Filtro por los meses de invierno
  group_by(anio) %>% # Agrupo por anio
  summarise(tmed_anual=mean(tmed, na.rm=T))

df_inv%>%
  ggplot(aes(x=tmed_anual))+
  geom_density(color="darkblue")+
  labs(
    title="Densidad empírica de la temp media anual del invierno",
    subtitle = "Promedio anual de JJA, desde 1961-2022",
    x="Temp media anual",
    y="Frecuencia")

# La distribución se asemeja a una normal, cuyos parámetros característicos
# son la media y el desvío.

#b)
library(fitdistrplus)
descdist(df_inv$tmed_anual)
# Según el gráfico de Cullen y Frey, la distribución se asemeja bien a la normal.

#c)
parametros<-mledist(df_inv$tmed_anual,"norm") # Cálculo de los parámetros a
# a través del metodo de máxima verosimilitud.

media<-parametros$estimate[1] # guardo la media y el sd desde la lista
sd<-parametros$estimate[2]

#d)

df_inv%>%
  ggplot(aes(x=tmed_anual))+
  geom_density(color="darkblue")+
  stat_function(fun = dnorm,
                args = list(mean=media,sd=sd),
                color = 'red')+
  labs(
    title="Densidad empírica vs Teórica",
    x="Temp media anual",
    y="Frecuencia")

# el ajuste es bueno: se observa que las colas se asemejan, las medias coinciden
# y la simetría es similar.

#----
# EJERCICIO 6
#a)
pp<-read_csv('../tp1/Datos.csv')
pp_aero_nov<-pp%>%
  dplyr::select(-Codigo,-Nobs)%>%
  mutate(Mes=month(Fecha))%>%
  filter(Estacion=='AEROPARQUE AERO',Mes=="11")

pp_aero_nov%>%ggplot(aes(x=PP))+
  geom_histogram(color="black",fill="skyblue")+
  scale_x_continuous(breaks = seq(0,max(pp_aero_nov$PP),by=15))+
  labs(title = "Histograma de pp de noviembre en Estación Aeroparque",
       x= "PP [mm]",
       y= "Frecuencia")+
  theme_bw()
  
# se asemeja más a una distribucón Gamma.
# Según la forma que presenta, tendrá que tener una escala baja y una forma alta

#b)
parametros<-mmedist(pp_aero_nov$PP,distr="gamma")$estimate

#c)

pp_aero_nov%>%ggplot(aes(x=PP))+
  geom_histogram(color="black",fill="skyblue")+
  scale_x_continuous(breaks = seq(0,max(pp_aero_nov$PP),by=15))+
  geom_density()+
  stat_function(fun=dgamma,args=parametros,size=1)+
  labs(title = "Distribución Empírica vs Teórica",
       x= "PP [mm]",
       y= "Frecuencia")+
  theme_bw()

#----
# Ejercicio 7
load("./tp3/Datos.RData")

dias_secos<-datos$PRCP<1
cantidad_dias_secos <- sum(dias_secos, na.rm = TRUE)
rachas <- rle(dias_secos)
longitudes_rachas <-rachas$lengths[rachas$values == TRUE]
longitudes_rachas[is.na(longitudes_rachas)] <- 0

parametros_mme_nbinom<- mmedist(data = longitudes_rachas, distr = "nbinom")$estimate
parametros_mme_gamma<- mmedist(data = longitudes_rachas, distr = "gamma")$estimate

data_frame(longitudes_rachas)%>%
  ggplot(aes(x=longitudes_rachas))+
  geom_histogram(aes(y=..density..),fill="grey",col="black",bins=50)+
  stat_function(fun=dnbinom, args = parametros_mme_nbinom, size=1, alpha=0.5, col='red')+
  stat_function(fun=dgamma, args = parametros_mme_gamma, size=1, alpha=0.5, col='blue')+
  labs(title="Histograma Dias con PP<1mm",
       x="cantidad de dias secos",
       y="Frecuencia")+
  theme_bw()

# Se ajusta mejor la gamma

## Ejercicio 8---------------------------
#a)
datos<-tibble(datos)
datos$MONTH<-month(ymd(datos$YEARMODA))
datos$YEAR<-year(ymd(datos$YEARMODA))
datos$DAY<-day(ymd(datos$YEARMODA))
datos_otonio<-datos%>%
  filter(MONTH%in%3:5)

which(is.na(datos_otonio$MAX))
which(is.na(datos_otonio$MIN))
which(is.na(datos_otonio$DAY))

#NO HAY DATOS FALTANTES EN EL OTOÑO AUSTRAL.

#b)
correlacion_temp<-cor(datos_otonio$MAX,datos_otonio$MIN)

#c)

#PREGUNTAR EL LUNES.
