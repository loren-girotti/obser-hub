# PARCIAL CLIMA II - FECHA 1
# ----------------------------

setwd("~/obser-hub/Clima_II/Parcial1/")

# Importo librerias
library(tidyverse)


# Ejercicio 1----

pp_dol_bb<-read_csv("./1ra_Fecha/Datos.csv") # IMporto los datos de Dolores y Bahía Blanca
pp_pi<-read_tsv("./1ra_Fecha/PuntaIndio.tsv") # Importo los datos de Punta Indio.

#a)

# Filtro los datos del tibble pp_dol_bb para la estación y el período exigido.

pp_dol_bb_SON<-pp_dol_bb%>%mutate(Anio=year(Fecha),Mes=month(Fecha))%>%
                        filter(Anio%in%1961:1991 & Mes%in%9:11)

# Calculo la precipitación acumulada para SON de ambas:
pp_dol_bb_SON_anualizada<-pp_dol_bb_SON%>%group_by(Nombre,Anio)%>%
  summarise(sum(PP)) # Dolores AERO tiene datos faltantes en el período 1988-1991.

# Busco los datos faltantes de Dolores AERO.

nas<-pp_dol_bb_SON%>%
  filter(is.na(PP))%>% # Tenemos un tibble con todos los datos faltantes de interés
  mutate(Dia=day(Fecha))

# Podemos ver que sólo hay faltantes en Dolores y podemos extraer un vector de 
# fechas de datos faltantes.

fechas_nas<-nas$Fecha # no uso pull porque al ser en formato date ya es un vector.

# copio el mensaje de error cuando quise usar pull:
# Error in UseMethod("pull") : 
# no applicable method for 'pull' applied to an object of class "Date"

#b)

# Utilizo las pp diarias filtradas por año y por mes, para tener una mayor cantidad
# de datos en el scatterplot.

pp_wide<-pp_dol_bb_SON%>%select(-Anio,-Mes)%>%
  pivot_wider(names_from = Nombre, values_from = PP)

pp_wide%>%ggplot(aes(x=`BAHIA BLANCA AERO`,y=`DOLORES AERO`))+
  geom_point() # Imposible ver una relación lineal de este gráfico.

pp_wide_anual<-pp_dol_bb_SON_anualizada%>%
  pivot_wider(names_from = "Nombre", values_from = "sum(PP)")

# Los pivoto en formato wide para poder obtener un vector x y otro vector y para
# realizar el scatter plot.

pp_wide_anual%>%ggplot(aes(x=`BAHIA BLANCA AERO`,y=`DOLORES AERO`))+
  geom_point()+
  labs(title = "Gráfico de dispersión entre la PP anualizada de Bahia Blanca y Dolores",
       subtitle = "Período SON 1961-1991",
       x = "PP Bahía Blanca AERO [mm]",
       y = "PP Dolores AERO [mm]")+
  theme_bw()

# Propongo un modelo de regresión lineal para ajustar los datos.


modelo_anual<-lm(`DOLORES AERO`~`BAHIA BLANCA AERO`, data = pp_wide_anual)
pendiente_anual<-modelo_anual$coefficients[2]
ord_origen_anual<-modelo_anual$coefficients[1]

pp_wide_anual%>%ggplot(aes(x=`BAHIA BLANCA AERO`,y=`DOLORES AERO`))+
  geom_point()+
  geom_abline(slope = pendiente_anual, intercept = ord_origen_anual, color = "red")+
  labs(title = "Gráfico de dispersión entre la PP anualizada de Bahia Blanca y Dolores",
       subtitle = "Período SON 1961-1991. En puntos los datos observados; la línea roja representa el modelo",
       x = "PP Bahía Blanca AERO [mm]",
       y = "PP Dolores AERO [mm]")+
  theme_bw()

# c) --------------------------------------------------
# Agrego los datos de Punta Indio a mi tibble wide anterior.

pp_pi_filtrado<-pp_pi%>%
  filter(Anio%in%1961:1991)

pp_wide_anual<-pp_wide_anual%>%mutate(`PUNTA INDIO`=pp_pi_filtrado$PuntaIndio)

# Realizo el modelo de regresión lineal con Punta Indio.
modelo_pi<-lm(`DOLORES AERO`~`PUNTA INDIO`, data = pp_wide_anual)
pendiente_pi<-modelo_pi$coefficients[2]
ord_origen_pi<-modelo_pi$coefficients[1]


# Hago el scatterplot de DOLORES VS PUNTA INDIO

pp_wide_anual%>%ggplot(aes(x=`PUNTA INDIO`,y=`DOLORES AERO`))+
  geom_point()+
  geom_abline(slope = pendiente_pi, intercept = ord_origen_pi, color="blue")+
  labs(title = "Gráfico de dispersión entre la PP anualizada de Punta Indio y Dolores",
       subtitle = "Período SON 1961-1991. Los puntos son datos observados; la línea azul representa el modelo",
       x = "PP Punta Indio [mm]",
       y = "PP Dolores AERO [mm]")+
  theme_bw()


# d) ------------------------------------
# Para calcular R² voy a utilizar la función summary que me permite evaluar el modelo
# y me brinda información de características clave, como el R².

resumen_bahia_blanca<-summary(modelo_anual)
resumen_punta_indio<-summary(modelo_pi)

# Obtengo el coeficiente de determinación de la lista generada por la función
# summary:

coef_det_bahia_blanca<-round(resumen_bahia_blanca$r.squared, digits = 4)
coef_det_punta_indio<-round(resumen_punta_indio$r.squared, digits = 4)

# e) -----------------------------------------------------------------
# Obtengo un vector de residuos de mi modelo a través de la lista que me devuelve:

residuos<-modelo_pi$residuals

# Los agrego a mi tibble wide para graficar la serie temporal:

pp_wide_anual%>%filter(Anio%in%1961:1987)%>%mutate(Residuos=residuos)%>%
  ggplot(aes(x=Anio,y=Residuos))+
  geom_point(color="red")+
  geom_hline(yintercept = 0)+
  labs(title = "Serie temporal de los residuos",
       subtitle = "Modelo de regresión lineal utilizando Punta Indio como predictora.",
       x = "Año",
       y = "Resiudos")+
  theme_bw()

# f) ---------------------------------------------------------------------
indices_na<-which(is.na(pp_wide_anual$`DOLORES AERO`)) # obtengo los indices de los datos que faltan


for(i in indices_na){
  pp_wide_anual$`DOLORES AERO`[i]<-round(pendiente_pi*pp_wide_anual$`PUNTA INDIO`[i]+ord_origen_pi,digits = 1)
}

# relleno los datos que faltan con la ecuación redondeandoa 1 decimal.

# EJERCICIO 2 -----------------------------------------------------------------

tmax<-read_csv("./1ra_Fecha/Tmax_LP.csv")





