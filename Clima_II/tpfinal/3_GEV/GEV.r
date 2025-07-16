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
tmax_anual <- serie %>% filter(Mes %in% c(12,1,2), Anio_Est %in% 1986:2005) %>%
  group_by(Anio_Est) %>%
  summarise("Tmax"=max(temp))

# GRÁFICO de los puntos:

tmax_anual %>% ggplot(aes(x=Anio_Est,y=Tmax))+
  geom_line(color="salmon")+
  geom_point(color="red")+
  labs(title="Valores máximos de temperatura la época estival",
       subtitle="Localidad: Rosario. Período: 1986-2005",
       x="Año",
       y="T[°C]")+
  scale_x_continuous(breaks = seq(1986,2005, by=2))+
  theme_bw()

# VER TENDENCIAS


# Ajusto a una distribución GEV:

library(ismev)
ajuste_gev <- gev.fit(tmax_anual$Tmax)

mu<-ajuste_gev$mle[1]
sigma<-ajuste_gev$mle[2]
xi<-ajuste_gev$mle[3]

# Defino el ancho óptimo del bin según el criterio de Scott:
ancho_bin <- 3.5*sd(tmax_anual$Tmax)/(nrow(tmax_anual)^(1/3))
ancho_bin <- 2

# Histograma de máximos:
tmax_anual %>% ggplot(aes(x=Tmax))+
  geom_density(color="red")+
  labs(title = "Densidad de probabilidad empírica de la Temperatura Máxima",
       subtitle = "Localidad: Rosario. Período: 1986-2005.",
       x = "T[°C]",
       y = "Densidad de prob.")+
  theme_bw()
