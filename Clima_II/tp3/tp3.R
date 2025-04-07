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

