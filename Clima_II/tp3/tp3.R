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
  ggplot(aes(y=Valor,x=factor(Variable)))+
  geom_boxplot()+
  coord_trans(y="log10")
