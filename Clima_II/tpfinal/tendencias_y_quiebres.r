library(tidyverse)
setwd("~/obser-hub/Clima_II/tpfinal/")

serie<- read_csv("./temperatura_media_diaria_rosario_1985-2005.csv")

serie <- serie %>% mutate("Dia"=day(ymd(date)),
                          "Mes"=month(ymd(date)),
                          "Anio"=year(ymd(date)),
                          "Anio_Est"=ifelse(Mes==12,Anio+1,Anio))

which(is.na(serie$date)) # 
which(is.na(serie$temp)) # MI SERIE NO TIENE NAs.

# Necesito filtrar por los meses de DEF (verano austral).
# Como no tengo un verano completo en 1985 porque me falta Diciembre, ni
# en 2005 porque me falta Enero y Febrero, me quedo con los datos comprendidos
# entre Diciembre 1985 y Febrero 2005. (Verano 1986-2005)

serie_est1 <- serie %>% filter(Mes %in% c(12,1,2), Anio_Est %in% 1986:2005)
serie_est <- serie %>% filter(Mes %in% c(12,1,2), Anio_Est %in% 1986:2005) %>%
  group_by(Anio_Est) %>%
  summarise("Media"=mean(temp))

plot<- serie_est %>% ggplot(aes(x=Anio_Est,y=Media))+
  geom_line(color="#cc0000")+
  scale_x_continuous(breaks = seq(1986,2005, by=1))+
  labs(title = "Serie temporal de la Temperatura Media Estival (DEF)",
       subtitle = "Localidad: Rosario",
       x = "",
       y= "T [°C]")+
  theme_bw()

#---ANALISIS DE QUIEBRE Y TENDENCIAS----------

# Busco una tendencia con un modelo de regresión lineal.

modelo <- lm(Media ~ Anio_Est, data = serie_est)
ordenada <- modelo$coefficients[1]
pendiente <- modelo$coefficients[2]

plot<-plot + geom_abline(slope = pendiente,intercept = ordenada)

# A simple vista no parece ser una tendencia significativa, pero para eso realicemos
# un test de correlación con un nivel de significancia de 0.05, bajo la hipótesis
# nula de que la correlación es 0, y la alternativa que es menor que 0:

cor_pvalue<-cor.test(serie_est$Media,serie_est$Anio_Est,alternative = "less")$p.value

# Como vemos, el pvalue es de 0.42 lo cual indica que no puedo rechazar la hipótesis
# nula: "la correlación entre la variable y el tiempo es 0".

plot + annotate("text", x = mean(serie_est$Anio_Est), y = max(serie_est$Media),
                label = paste0("p = ", signif(cor_pvalue,3)),
                hjust=0,vjust=1,size=4,color="black")

