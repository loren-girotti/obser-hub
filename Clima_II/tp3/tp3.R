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


