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
  geom_vline(xintercept = media,lty=2,color="black")+
  labs(
    title = "Distribución Empírica vs Teórica",
    subtitle = "Parámetros calculados con Método de Máxima Verosimilitud"
  )

# Podemos ver que el ajuste es visualmente bueno, y en principio mi distribución
# empírica es Platicúrtica: es decir que las colas tienen más peso que la "normal"
# y los valores no están tan centralizados.

# Hay que calcular la curtosis:
library(moments)
curt<-kurtosis(serie_est$Media)
# Entonces es platicúrtica.


# TEST DE LILLIEFORS -------
# Para testear uso Lilliefors

# Calculo la F acumulada Empírica y Teórica:

serie_est_acum<-mutate(serie_est,F_e=rank(serie_est$Media)/nrow(serie_est),
                       F_t=pnorm(serie_est$Media,mean = mean(serie_est$Media),
                                 sd=sd(serie_est$Media)))


# Calculo de D:
D <- max(abs(serie_est_acum$F_e - serie_est_acum$F_t))

# Con alpha=0.1

C <- 1.224/(sqrt(nrow(serie_est))+0.12+(0.11/sqrt(nrow(serie_est))))

# Hago un procedimiento de Monte-Carlo para testear el ajuste, generando 1000000 simulaciones:

D_mc <- replicate(n=100000L, {
  x <- rnorm(n=nrow(serie_est_acum), mean = mean(serie_est_acum$Media),sd=sd(serie_est_acum$Media))
  F_e <- rank(x)/nrow(serie_est_acum)
  F_t <- pnorm(x,mean = mean(x), sd=sd(x))
  max(abs(F_e - F_t)) # <--- Valor de D la simulacion
})

(p_value<-mean(D_mc >= D))

D_ecdf <- ecdf(D_mc)

1-D_ecdf(D)


ks.test(serie_est$Media, "pnorm")
