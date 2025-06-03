## 1ER PARCIAL SEGUNDA FECHA ----

# Defino ruta para los archivos
setwd("./obser-hub/Clima_II/Parcial1/2da Fecha/")

# Importo librerias
library(tidyverse)
library(fitdistrplus)

# Leo los datos

PPext<-read_csv("./PPextrema.csv")

Indices<-read_csv("./Indices.csv")

# Inciso a) ----

# Planteo un modelo de regresión lineal para estimar la tendencia de la serie:

modelo<-lm(Centroide~Anio, data = PPext)
ordenada<-modelo$coefficients[1]
pendiente<-modelo$coefficients[2]


# Realización del gráfico:

PPext %>% ggplot(aes(x=Anio,y=Centroide))+
  geom_line()+
  geom_abline(slope = pendiente,intercept = ordenada,color="red")+
  labs(title="Serie temporal del índice de PP extrema - Período de NDE",
       subtitle="En negro la serie, en rojo el modelo de regresión",
       x = "Año",
       y = "Índice")+
  theme_bw()

# Para interpretar si la tendencia es estadísticamente significativa podemos 
# realizar un test de correlación entre el índice y los años.

test<-cor.test(PPext$Centroide, PPext$Anio, conf.level = 0.9, alternative = "greater")
pvalue<-test$p.value

# Filtro la tendencia:

PPext_filtrada <- PPext %>% mutate(Filtrada = mean(Centroide)+modelo$residuals)

# Grafico la serie filtrada:

PPext_filtrada %>% ggplot(aes(x=Anio,y=Filtrada))+
  geom_line()+
  labs(title="Serie temporal filtrada del índice de PP extrema - Período de NDE",
       x = "Año",
       y = "Índice")+
  theme_bw()


# Inciso b) --------
PPext_filtrada %>% ggplot(aes(x=Filtrada))+
  geom_density(color="blue")+
  labs(title= "Función de densidad de probabilidad empírica del índice filtrado",
       x = "Índice",
       y = "Densidad")+
  theme_bw()

# Inciso c) --------
# Obtengo los parámetros a partir del método

ajuste<-fitdist(data = PPext_filtrada$Filtrada,distr = "norm", method = "mle")
media<-ajuste$estimate[1]
desvio<-ajuste$estimate[2]


# Grafico la distribución empírica y la teórica:


PPext_filtrada %>% ggplot(aes(x=Filtrada))+
  geom_density(color="blue")+
  geom_vline(xintercept=media,color="gray")+
  stat_function(fun = dnorm, args = list(mean = media,sd = desvio),color="red")+
  labs(title= "Función de densidad de probabilidad empírica vs teórica",
       subtitle = "En rojo tenemos la teórica y en azul la empírica",
       x = "Índice",
       y = "Densidad")+
  theme_bw()


# Inciso e) ----------

Indices <- Indices %>% pivot_wider(names_from = Indice, values_from = Media)

indices_periodo <- Indices %>% filter(Anio %in% 1990:2020)

pp_periodo <- PPext_filtrada %>% filter(Anio %in% 1991:2020)

cor_test_result<-tibble(c("DMI","Nino12","Nino34","Nino4","SAM"),"p-value")


names(cor_test_result)=c("Indice","p-value")

cor_test_result <- cor_test_result %>% mutate(`p-value`=0)

pvalues<-as.numeric(vector(length = 5))

for (i in pvalues) {
  pvalues[i]<-cor.test(x=pp_periodo$Filtrada,y=indices_periodo[,i+1],method = "spearman")$p.value
}

length(pp_periodo$Filtrada)
length(indices_periodo[,2])

pvDMI<-cor.test(x=pp_periodo$Filtrada,y=indices_periodo$DMI,method = "spearman")$p.value
pvNino12<-cor.test(x=pp_periodo$Filtrada,y=indices_periodo$Niño12,method = "spearman")$p.value
pvNino34<-cor.test(x=pp_periodo$Filtrada,y=indices_periodo$Niño34,method = "spearman")$p.value
pvNino4<-cor.test(x=pp_periodo$Filtrada,y=indices_periodo$Niño4,method = "spearman")$p.value
pvSAM<-cor.test(x=pp_periodo$Filtrada,y=indices_periodo$SAM,method = "spearman")$p.value

for(i in 1:5){
cor_test_result[1,2]<-pvDMI
cor_test_result[2,2]<-pvNino12
cor_test_result[3,2]<-pvNino34
cor_test_result[4,2]<-pvNino4
cor_test_result[5,2]<-pvSAM
}

# SE PUEDE HACER CON UN FOR LOOP pero me trabe y no me daba el tiempo.


