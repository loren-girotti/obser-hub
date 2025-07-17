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

# EVALUO TENDENCIAS ------------
# Exploro la posibilidad de que haya tendencia:

modelo_tendencia<-lm(Tmax~Anio_Est, data=tmax_anual)
ord_tendencia<-modelo_tendencia$coefficients[1]
pend_tendencia<-modelo_tendencia$coefficients[2]

tmax_anual %>% ggplot(aes(x=Anio_Est,y=Tmax))+
  geom_line(color="salmon")+
  geom_point(color="red")+
  geom_abline(slope = pend_tendencia,intercept = ord_tendencia)+
  labs(title="Valores máximos de temperatura la época estival",
       subtitle="Localidad: Rosario. Período: 1986-2005",
       x="Año",
       y="T[°C]")+
  scale_x_continuous(breaks = seq(1986,2005, by=2))+
  theme_bw()

cor_test_pvalue<-cor.test(tmax_anual$Anio_Est,tmax_anual$Tmax,alternative = "greater")$p.value

# NO HAY TENDENCIA LINEAL.
# Testeo monotona con Man-Kendall

library(trend)
mk.test(tmax_anual$Tmax,alternative = "greater")$p.value
# NO HAY TENDENCIA MONOTONA

# Conclusion: NO HAY TENDENCIAS EN MI SERIE DE MAXIMOS

# AJUSTE GEV -------
# Ajusto a una distribución GEV:

library(ismev)
library(evd)
ajuste_gev <- gev.fit(tmax_anual$Tmax)

mu<-ajuste_gev$mle[1]
sigma<-ajuste_gev$mle[2]
xi<-ajuste_gev$mle[3]


# Densidad empirica máximos:
tmax_anual %>% ggplot(aes(x=Tmax))+
  geom_density(color="red")+
  labs(title = "Densidad de probabilidad empírica de la Temperatura Máxima",
       subtitle = "Localidad: Rosario. Período: 1986-2005.",
       x = "T[°C]",
       y = "Densidad de prob.")+
  theme_bw()

plot(fgev(tmax_anual$Tmax))

# Como el parámetro de forma Xi es negativo, esperamos que ajuste a una GEV de la
# familia Weibull.

# Defino un vector con los valores ordenados de mi temperatura máxima para poder
# construir la curva de densidad de la GEV:

x_seq<-seq(min(tmax_anual$Tmax),max(tmax_anual$Tmax),length.out=1000)

dens_val<-dgev(x_seq, loc = mu, scale = sigma, shape = xi)

tb_gev <- tibble(x=x_seq,y=dens_val)


tmax_anual %>% ggplot(aes(x=Tmax,color="Empírica"))+
  geom_density(linetype=2)+
  geom_line(data=tb_gev,aes(x=x,y=y,color="Weibull"))+
  labs(title = "Ajuste de la Densidad de Máximos con una GEV del tipo Weibull",
       subtitle = "Localidad: Rosario. Período: 1986-2005.",
       x = "T[°C]",
       y = "Densidad de prob.",
       color = "Curva")+
  scale_color_manual(values=c("Empírica"="black","Weibull"="red"))+
  theme_bw()


# Hago un test de Bondad de Ajuste con simulaciones de Monte Carlo.

ks_sim<-function(muestra, loc, scale, shape, Nsim){
  N <- length(muestra)
  vec_sim <- replicate(n=Nsim,{
    muestra <- rgev(n = N, loc = mu, scale = sigma, shape = xi)
    
    params_fit <- gev.fit(muestra)$mle
    
    Dsim <- ks.test(x = muestra, "pgev", loc = params_fit[1], scale = params_fit[2],
                    shape = params_fit[3])$statistic
  })
  
  return(vec_sim)
}

vec_sim <- ks_sim(muestra = tmax_anual$Tmax, loc = mu, scale = sigma, shape = xi,
                  Nsim = 100000)

tb_sim <- tibble(Sim=vec_sim)

Dcalc <- ks.test(x=tmax_anual$Tmax, "pgev", loc = mu, scale = sigma, shape = xi,
                 alternative = "two.sided")$statistic

ggplot(data = tb_sim, aes(x=Sim))+
  geom_histogram(fill="white",color="black")+
  geom_vline(aes(xintercept = Dcalc, color = "D calculado"))+
  geom_vline(aes(xintercept = quantile(vec_sim, probs = 0.025), color ="P2.5 y P97.5"),
             lty = 2)+
  geom_vline(aes(xintercept = quantile(vec_sim, probs = 0.975), color ="P2.5 y P97.5"),
             lty = 2)+
  scale_color_manual(values = c("D calculado" = "blue", "P2.5 y P97.5" = "red"))+
  theme_bw()+
  labs(title="Histograma de los estadisticos D simulados",
       subtitle="Nivel de significancia de 0.05 | 100.000 Simulaciones",
       x="Valores simulados de D",
       y="Frecuencia Absoluta",
       color="Leyenda")
