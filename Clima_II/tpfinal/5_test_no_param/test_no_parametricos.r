rm(list = ls())
gc()

library(tidyverse)
library(moments)

setwd("~/obser-hub/Clima_II/tpfinal/")


serie<- read_csv("./temperatura_media_diaria_rosario_1985-2005.csv")

serie <- serie %>% mutate("Dia"=day(ymd(date)),
                          "Mes"=month(ymd(date)),
                          "Anio"=year(ymd(date)),
                          "Anio_Est"=ifelse(Mes==12,Anio+1,Anio))

serie_est <- serie %>% filter(Mes %in% c(12,1,2), Anio_Est %in% 1986:2005) %>%
  group_by(Anio_Est) %>%
  summarise("Media"=mean(temp))

oni <- read_csv("./oni.csv") %>% filter(Anio > 1985)

oni <- oni %>% mutate("Fase"=ifelse(abs(ONI)<=0.5,"Neutra",ifelse(ONI>0.5,"Niño","Niña")))

temp_media <- pull(serie_est,Media)

df <- oni %>% mutate("Tmed"=temp_media)

t_nino <- df %>% filter(Fase=="Niño")
t_neutra <- df %>% filter(Fase=="Neutra")
t_nina <- df %>% filter(Fase=="Niña")

media_nino<-mean(t_nino$Tmed)
media_neutra<-mean(t_neutra$Tmed)
media_nina<-mean(t_nina$Tmed)

asim_nino<-skewness(t_nino$Tmed)
asim_neutra<-skewness(t_neutra$Tmed)
asim_nina<-skewness(t_nina$Tmed)

p90_nino<-quantile(t_nino$Tmed,probs=0.9)
P90_neutra<-quantile(t_neutra$Tmed,probs=0.9)
p90_nina<-quantile(t_nina$Tmed,probs=0.9)

momentos <- df %>% group_by(Fase) %>%
  summarise("Media"=round(mean(Tmed),digits = 2),
            "Asimetría"=round(skewness(Tmed),digits = 2),
            "P90"=round(quantile(Tmed,probs=0.9),digits = 2))

# Grafico las densidades de cada distribución

df %>% ggplot(aes(x=Tmed,color=Fase))+
  geom_density()+
  scale_color_manual(values=c("gray","blue4","red4"))+
  geom_vline(xintercept = media_neutra,color="gray",lty=2)+
  geom_vline(xintercept = media_nina,color="blue4",lty=2)+
  geom_vline(xintercept = media_nino,color="red4",lty=2)+
  labs(title = "Distribución de la temperatura estival, en distintas fases del ENOS",
       subtitle = "Localidad: Rosario | Período 1986-2005.",
       x = "T [°C]",
       y = "Densidad de prob")+
  theme_bw()

# BOXPLOT

df %>% ggplot(aes(x=Fase,y=Tmed))+
  geom_boxplot(fill=c("darkgray","cornflowerblue","brown1"))+
  geom_point(data = momentos,aes(x=Fase,y=Media,shape="media"),color="black",size=3)+
  geom_point(data = momentos,aes(x=Fase,y=P90,shape="p90"),color="black",size=3)+
  scale_shape_manual(values=c("media"=16,"p90"=18))+
  #scale_fill_manual(values=c("darkgray","cornflowerblue","brown1"))+
  labs(title = "Diagrama de cajas de la temperatura estival, según el ENOS",
       subtitle = "Localidad: Rosario | Período 1986-2005",
       y = "T [°C]",
       shape = "Leyenda")+
  theme_bw()


# Para testear las diferencias entre los momentos planteo un remuestreo con reemplazo
# Bootstrapping ----

# Defino mi función:

resample_boot<-function(muestra1,muestra2,momento,Nsim){
  
  salida <- vector(mode = "numeric",length = Nsim)
  
  n1<-length(muestra1)
  n2<-length(muestra2)
  
  N<-n1+n2
  
  vec<-c(muestra1,muestra2)
  
  for(i in 1:Nsim){
    rs<-sample(vec,replace = TRUE)
    
    muestra1_rs<-rs[1:n1]
    muestra2_rs<-rs[(n1+1):n2]
    
    if(momento=="media"){
      salida[i]<-mean(muestra1_rs)-mean(muestra2_rs)
    }
    if(momento=="asimetria"){
      salida[i]<-skewness(muestra1_rs)-skewness(muestra2_rs)
    }
    if(momento=="percentil"){
      salida[i]<-quantile(muestra1_rs,probs=0.9)-quantile(muestra2_rs,probs=0.9)
    }
  }
  return(salida)
}

# Defino mis vectores según las fases
vec_nino <- df %>%
  filter(Fase=="Niño") %>%
  pull(Tmed)

vec_neutra <- df %>%
  filter(Fase=="Neutra") %>%
  pull(Tmed)

vec_nina <- df %>%
  filter(Fase=="Niña") %>%
  pull(Tmed)

# Calculo mis vectores nulos para comparar:
vector_nulo_media<-resample_boot(vec_nino,vec_nina,"media",10000)
vector_nulo_asimetria<-resample_boot(vec_nino,vec_nina,"asimetria",10000)
vec_nulo_percentil<-resample_boot(vec_nino,vec_nina,"percentil",10000)

dif_med_nino_nina<-mean(vec_nino)-mean(vec_nina)
dif_med_nino_neutra<-mean(vec_nino)-mean(vec_neutra)
dif_med_nina_neutra<-mean(vec_nina)-mean(vec_neutra)

vec_nulo_med_nino_neutra<-resample_boot(vec_nino,vec_neutra,"media",10000)
vec_nulo_med_nina_neutra<-resample_boot(vec_nina,vec_neutra,"media",10000)

dif_medias<-tibble("nino-nina"=vector_nulo_media,"nino-neutra"=vec_nulo_med_nino_neutra,
                   "nina-neutra"=vec_nulo_med_nina_neutra)


tibble(Media=vector_nulo_media) %>% ggplot(aes(x=Media))+
  geom_histogram(color="black",fill="white")+
  geom_vline(xintercept = c(quantile(vector_nulo_media,probs=0.025),
                            quantile(vector_nulo_media,probs=0.975)),
             color="red",
             lty=2)+
  geom_vline(xintercept = dif_med_nino_nina,color="blue")+
  labs(title = "Histograma de Remuestreo con Reemplazo",
       subtitle = "Comparación entre diferencias de media entre Niño-Niña | Significancia = 0.05",
       x="Diferencia de Media",
       y="Frecuencia")+
  theme_bw()

dif_medias %>% ggplot(aes(x=`nino-neutra`))+
  geom_histogram(color="black",fill="white")+
  geom_vline(xintercept = c(quantile(vec_nulo_med_nino_neutra,probs=0.025),
                            quantile(dif_medias$`nino-neutra`,probs=0.975)),
             color="red",
             lty=2)+
  geom_vline(xintercept = dif_med_nino_neutra,color="blue")+
  labs(title = "Histograma de Remuestreo con Reemplazo",
       subtitle = "Comparación entre diferencias de media entre Niño-Neutra | Significancia = 0.05",
       x="Diferencia de Media",
       y="Frecuencia")+
  theme_bw()


dif_medias %>% ggplot(aes(x=`nina-neutra`))+
  geom_histogram(color="black",fill="white")+
  geom_vline(xintercept = c(quantile(dif_medias$`nina-neutra`,probs=0.025),
                            quantile(dif_medias$`nina-neutra`,probs=0.975)),
             color="red",
             lty=2)+
  geom_vline(xintercept = dif_med_nina_neutra,color="blue")+
  labs(title = "Histograma de Remuestreo con Reemplazo",
       subtitle = "Comparación entre diferencias de media entre Niña-Neutra | Significancia = 0.05",
       x="Diferencia de Media",
       y="Frecuencia")+
  theme_bw()



# Histograma asimetría----
dif_asim_nino_nina<-skewness(vec_nino)-skewness(vec_nina)
dif_asim_nino_neutra<-skewness(vec_nino)-skewness(vec_neutra)
dif_asim_nina_neutra<-skewness(vec_nina)-skewness(vec_neutra)

vec_nulo_asim_nino_neutra<-resample_boot(vec_nino,vec_neutra,"asimetria",10000)
vec_nulo_asim_nina_neutra<-resample_boot(vec_nina,vec_neutra,"asimetria",10000)

dif_asim <- tibble("nino-nina"=vector_nulo_asimetria,"nino-neutra"=vec_nulo_asim_nino_neutra,
                   "nina-neutra"=vec_nulo_asim_nina_neutra)

dif_asim %>% ggplot(aes(x=`nino-nina`))+
  geom_histogram(color="black",fill="white")+
  geom_vline(xintercept = c(quantile(vector_nulo_asimetria,probs=0.025,na.rm = TRUE),
                            quantile(vector_nulo_asimetria,probs=0.975,na.rm = TRUE)),
             color="red",
             lty=2)+
  geom_vline(xintercept = dif_asim_nino_nina,color="blue")+
  labs(title = "Histograma de Remuestreo con Reemplazo",
       subtitle = "Diferencias de asimetría entre Niño-Niña | Significancia = 0.05",
       x="Diferencia de Asimetría",
       y="Frecuencia")+
  theme_bw()

dif_asim %>% ggplot(aes(x=`nino-neutra`))+
  geom_histogram(color="black",fill="white")+
  geom_vline(xintercept = c(quantile(dif_asim$`nino-neutra`,probs=0.025,na.rm = TRUE),
                            quantile(dif_asim$`nino-neutra`,probs=0.975,na.rm = TRUE)),
             color="red",
             lty=2)+
  geom_vline(xintercept = dif_asim_nino_neutra,color="blue")+
  labs(title = "Histograma de Remuestreo con Reemplazo",
       subtitle = "Diferencias de asimetría entre Niño-Neutra | Significancia = 0.05",
       x="Diferencia de Asimetría",
       y="Frecuencia")+
  theme_bw()

dif_asim %>% ggplot(aes(x=`nina-neutra`))+
  geom_histogram(color="black",fill="white")+
  geom_vline(xintercept = c(quantile(dif_asim$`nina-neutra`,probs=0.025,na.rm = TRUE),
                            quantile(dif_asim$`nina-neutra`,probs=0.975,na.rm = TRUE)),
             color="red",
             lty=2)+
  geom_vline(xintercept = dif_asim_nina_neutra,color="blue")+
  labs(title = "Histograma de Remuestreo con Reemplazo",
       subtitle = "Diferencias de asimetría entre Niña-Neutra | Significancia = 0.05",
       x="Diferencia de Asimetría",
       y="Frecuencia")+
  theme_bw()


# Histograma percentiles ----------
dif_per_nino_nina<-quantile(vec_nino,probs=0.9)-quantile(vec_nina,probs=0.9)
dif_per_nino_neutra<-quantile(vec_nino,probs=0.9)-quantile(vec_neutra,probs=0.9)
dif_per_nina_neutra<-quantile(vec_nina,probs=0.9)-quantile(vec_neutra,probs=0.9)

vec_nulo_per_nino_neutra<-resample_boot(vec_nino,vec_neutra,"percentil",10000)
vec_nulo_per_nina_neutra<-resample_boot(vec_nina,vec_neutra,"percentil",10000)
dif_percentil<-tibble(nino_nina=vec_nulo_percentil,nino_neutra=vec_nulo_per_nino_neutra,
                      nina_neutra=vec_nulo_per_nina_neutra)

vectores_nulos %>% ggplot(aes(x=Percentil))+
  geom_histogram(color="black",fill="white")+
  geom_vline(xintercept = c(quantile(vec_nulo_percentil,probs=0.025,na.rm = TRUE),
                            quantile(vec_nulo_percentil,probs=0.975,na.rm = TRUE)),
             color="red",
             lty=2)+
  geom_vline(xintercept = dif_per_nino_nina,color="blue")+
  labs(title = "Histograma de Remuestreo con Reemplazo",
       subtitle = "Diferencias de P90 entre Niño-Niña | Significancia = 0.05",
       x="Diferencia de P90 [°C]",
       y="Frecuencia")+
  theme_bw()


dif_percentil%>% ggplot(aes(x=nino_neutra))+
  geom_histogram(color="black",fill="white")+
  geom_vline(xintercept = c(quantile(dif_percentil$nino_neutra,probs=0.025,na.rm = TRUE),
                            quantile(dif_percentil$nino_neutra,probs=0.975,na.rm = TRUE)),
             color="red",
             lty=2)+
  geom_vline(xintercept = dif_per_nino_neutra,color="blue")+
  labs(title = "Histograma de Remuestreo con Reemplazo",
       subtitle = "Diferencias de P90 entre Niño-Neutra | Significancia = 0.05",
       x="Diferencia de P90 [°C]",
       y="Frecuencia")+
  theme_bw()

dif_percentil %>% ggplot(aes(x=nina_neutra))+
  geom_histogram(color="black",fill="white")+
  geom_vline(xintercept = c(quantile(dif_percentil$nina_neutra,probs=0.025,na.rm = TRUE),
                            quantile(dif_percentil$nina_neutra,probs=0.975,na.rm = TRUE)),
             color="red",
             lty=2)+
  geom_vline(xintercept = dif_per_nina_neutra,color="blue")+
  labs(title = "Histograma de Remuestreo con Reemplazo",
       subtitle = "Diferencias de P90 entre Niña-Neutra | Significancia = 0.05",
       x="Diferencia de P90 [°C]",
       y="Frecuencia")+
  theme_bw()

