rm(list = ls())
gc()

library(tidyverse)

setwd("~/obser-hub/Clima_II/tpfinal/")


serie<- read_csv("./temperatura_media_diaria_rosario_1985-2005.csv")

serie <- serie %>% mutate("Dia"=day(ymd(date)),
                          "Mes"=month(ymd(date)),
                          "Anio"=year(ymd(date)),
                          "Anio_Est"=ifelse(Mes==12,Anio+1,Anio))


serie_est <- serie %>% filter(Mes %in% c(12,1,2), Anio_Est %in% 1986:2005) %>%
  group_by(Anio_Est) %>%
  summarise("Media"=mean(temp))

# Importo los indices climáticos -----
indices <- read_csv("./df_indices.csv") %>%  mutate("Anio_Est"=ifelse(Mes==12,Anio+1,Anio)) %>%
  filter(Mes %in% c(12,1,2), Anio_Est %in% 1986:2005)

sam <- indices %>% group_by(Anio_Est) %>% summarise("sam"=mean(sam))

oni <- read_csv("./oni.csv") %>% filter(Anio %in% 1986:2005)

dmi <- read_csv("./dmi_had_long.csv") %>% 
  mutate("Mes"=month(ymd(Date)),"Anio"=year(ymd(Date)),"Anio_Est"=ifelse(Mes==12,Anio+1,Anio)) %>%
  filter(Mes %in% c(12,1,2), Anio_Est %in% 1986:2005) %>%
  group_by(Anio_Est) %>% summarise ("dmi"=mean(`DMI HadISST1.1  missing value -9999 https://psl.noaa.gov/data/timeseries/month/`))


sam <- pull(sam,"sam")
oni <- pull(oni,"ONI")
dmi <- pull(dmi,"dmi")

temp_media <- serie_est %>% pull(Media)
Anio_Est <- serie_est$Anio_Est

df <- tibble(Anio_Est,temp_media,oni,sam,dmi) # DATA FRAME CON TODO LO NECESARIO

#Modelo de regresión lineal simple con cada indice

plot_oni <- df %>% ggplot(aes(x=oni,y=temp_media))+
            geom_point(color="red")+
            theme_bw()+
            labs(title="Temp Media Estival vs ONI",
                 subtitle="Localidad: Rosario | Período 1986-2005",
                 y = "T[°C]",
                 x = "Índice ONI")

lm_oni <- lm(temp_media~oni,data=df)

plot_oni + geom_abline(slope = lm_oni$coefficients[2],
                       intercept = lm_oni$coefficients[1])

plot_sam <- df %>% ggplot(aes(x=sam,y=temp_media))+
            geom_point(color="blue4")+
            theme_bw()+
            labs(title="Temp Media Estival vs SAM",
                 subtitle="Localidad: Rosario | Período 1986-2005",
                 y = "T[°C]",
                 x = "Índice SAM")

lm_sam <- lm(temp_media~sam,data=df)

plot_sam + geom_abline(slope = lm_sam$coefficients[2],
                       intercept = lm_sam$coefficients[1])


plot_dmi <- df %>% ggplot(aes(x=dmi,y=temp_media))+
            geom_point(color="green4")+
            theme_bw()+
            labs(title="Temp Media Estival vs DMI",
                 subtitle="Localidad: Rosario | Período 1986-2005",
                 y = "T[°C]",
                 x = "Índice DMI")

lm_dmi <- lm(temp_media~dmi,data=df)

plot_dmi + geom_abline(slope = lm_dmi$coefficients[2],
                       intercept = lm_dmi$coefficients[1])

r_oni <- summary(lm_oni)$r.squared
r_sam <- summary(lm_sam)$r.squared
r_dmi <- summary(lm_dmi)$r.squared

test_cor <- function(indice,alternativa){
  resultado <- cor.test(df$temp_media,indice,alternative = alternativa)$p.value
  
  return(paste0("El pvalue es = ",round(resultado,digits = 4)))
}

test_cor(oni,"less") # No podemos rechazar que no haya correlación negativa
test_cor(sam,"two.sided") # CLARAMENTE NO HAY CORRELACION
test_cor(dmi,"less") # No podemos rechazar que no haya correlación negativa

# EVALUO LA HOMOCEDASTICIDAD DE LOS MODELOS DE ONI Y DMI
# Grafico de dispersión entre los valores predictores y los residuos

df <- df %>% mutate("res_oni"=lm_oni$residuals,
                    "res_dmi"=lm_dmi$residuals)
df %>% ggplot(aes(x=oni,y=res_oni))+
  geom_point(color="red")+
  geom_hline(yintercept = 0)+
  labs(title = "Análisis de homocedasticidad",
       subtitle = "Modelo de regresión lineal simple | Predictor: ONI",
       x="ONI",
       y="Residuos [°C]")+
  theme_bw()  


df %>% ggplot(aes(x=dmi,y=res_dmi))+
  geom_point(color="green4")+
  geom_hline(yintercept = 0)+
  labs(title = "Análisis de homocedasticidad",
       subtitle = "Modelo de regresión lineal simple | Predictor: DMI",
       x="DMI",
       y="Residuos [°C]")+
  theme_bw()  
