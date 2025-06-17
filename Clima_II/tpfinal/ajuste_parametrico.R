rm(list = ls())
gc()

library(tidyverse)
setwd("~/obser-hub/Clima_II/tpfinal/")


serie<- read_csv("./temperatura_media_diaria_rosario_1985-2005.csv")

serie <- serie %>% mutate("Dia"=day(ymd(date)),
                          "Mes"=month(ymd(date)),
                          "Anio"=year(ymd(date)),
                          "Anio_Est"=ifelse(Mes==12,Anio+1,Anio))

serie_est_diaria <- serie %>% filter(Mes %in% c(12,1,2), Anio_Est %in% 1986:2005)
serie_est <- serie %>% filter(Mes %in% c(12,1,2), Anio_Est %in% 1986:2005) %>%
  group_by(Anio_Est) %>%
  summarise("Media"=mean(temp))

# GRÁFICO DE LA DISTRIBUCIÓN EMPÍRICA ------------------

plot_distr<-serie_est %>% ggplot(aes(x=Media))+
  geom_density(color="red")+
  labs(title = "Distribución empírica de la temperatura media estival",
       subtitle = "Localidad: Rosario | Período 1986-2005",
       x = "T [°C]",
       y = "Frecuencia")+
  theme_bw();plot_distr


# CÁLCULO DE LOS PARÁMETROS.
library(fitdistrplus)

# Voy a utilizar el método de máxima verosimilitud:

parametros<-mledist(data = serie_est$Media,distr = "norm")
media<-parametros$estimate[1]
desvio<-parametros$estimate[2]

# Grafico la distribución empírica con la teórica:

plot_distr + stat_function(fun = dnorm, args = list(mean = media, sd = desvio),
                           color = "blue")+
  annotate("text",x=25,y=0.4,
           label=paste0("Media = ",signif(media,3)," °C"),
           hjust=0.5,vjust=1,size=4,color="black")+
  annotate("text",x=25,y=0.3,
           label=paste0("Desvío = ",signif(desvio,3)," °C"),
           hjust=0.5,vjust=1,size=4,color="salmon")+
  geom_vline(xintercept = media,lty=2,color="black")

# Podemos ver que el ajuste es visualmente bueno, y en principio mi distribución
# empírica es Platicúrtica: es decir que las colas tienen más peso que la "normal"
# y los valores no están tan centralizados.

# Hay que calcular la curtosis:


