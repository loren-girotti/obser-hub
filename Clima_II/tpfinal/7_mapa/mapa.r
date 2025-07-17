rm(list = ls())
gc()

library("RColorBrewer")
library("tidyverse")
library("metR")
library("ncdf4")

setwd("~/obser-hub/Clima_II/tpfinal/")

serie <- read_csv("./temperatura_media_diaria_rosario_1985-2005.csv")

serie <- serie %>% mutate("Dia"=day(ymd(date)),
                          "Mes"=month(ymd(date)),
                          "Anio"=year(ymd(date)),
                          "Anio_Est"=ifelse(Mes==12,Anio+1,Anio))

serie_est <- serie %>% filter(Mes %in% c(12,1,2), Anio_Est %in% 1986:2005) %>%
  group_by(Anio_Est) %>%
  summarise("Media"=mean(temp))

#dimensiones

nc[["dim"]]

#extraccion de las dimensiones

lat_vals <- nc$dim$lat$vals
lon_vals <- nc$dim$lon$vals
time_vals <- nc$dim$time$vals

#longitud: Correccion de unidades

lon_vals[lon_vals>180]<-lon_vals[lon_vals>180]-360

## Utilizamos como dominio 30° -100 °W y 10° -60°S

lat.i<-which.min(abs(lat_vals- -10))
lat.f<-which.min(abs(lat_vals- -60))#estan al reves porque la latitud esta de mayor a menor

lon.i<-which.min(abs(lon_vals- -100))
lon.f<-which.min(abs(lon_vals- -30))

#valores de longitud y latitud correspondientes a ese dominio

lon<-lon_vals[lon.i:lon.f]
lat<-lat_vals[lat.i:lat.f]

nc[["dim"]][["time"]][["units"]]

time_vals[1]
time_vals[60]


# extraigo mi variable entre los años 1985 y 2005

gh.850 <- ncvar_get(nc = nc, varid = 'var',
                    start = c(lon.i,lat.i,27), # Aca le digo que empiece en time=26, que es 1985
                    count = c(lon.f-lon.i+1,lat.f-lat.i+1,20)) ## Aca le digo que termine 21 años despues, en el 2005


## Extraigo la media de T como un vector

temp_media<-serie_est%>%pull(Media)

## Creo una matiz para guardar las correlaciones y las significancias
## que quiero graficar

matriz.cor <- matrix(NA, nrow = dim(gh.850)[1], ncol = dim(gh.850)[2])
matriz.sign <- matrix(NA, nrow = dim(gh.850)[1], ncol = dim(gh.850)[2])

## Relleno cada cuadradito de la matriz con una correlacion entre el gh y hr

for (i in 1:dim(gh.850)[1]) {
  for (j in 1:dim(gh.850)[2]) {
    matriz.cor[i, j] <- cor.test(gh.850[i, j, ],hr.media)$estimate
    matriz.sign[i, j] <- cor.test(gh.850[i, j, ],hr.media)$p.value<0.05
  }
}


## Lo guardo en un dataframe para poder graficar

gh.cor.grafico <- expand.grid(lon = lon, lat = lat)
gh.cor.grafico$Correlacion <- as.vector(matriz.cor)
gh.cor.grafico$Significancia<- as.vector(matriz.sign)

## Cargo el mapa

map.world <- map_data('world')


## Grafico

gh.cor.grafico %>%
  ggplot( aes(x = lon, y = as.numeric(lat), z=Correlacion)) + 
  geom_raster(aes(fill = Correlacion),interpolate = T)+
  scale_fill_gradient2(low = "#2166AC",high = "#B2182B")+
  geom_contour_fill(data = filter(gh.cor.grafico, Significancia),
                    aes(x = lon, y = as.numeric(lat)), size = 1, color = "black", linetype = 2,)+
  geom_contour(col='black',binwidth = 0.1)+geom_text_contour(stroke = 0.2)+
  geom_polygon(data=map.world, aes(x=long, y=lat, group=group),inherit.aes = F,
               color="black", linewidth = 0.9,alpha=0)+
  scale_x_longitude(ticks = 10)+
  scale_y_latitude(ticks = 10)+
  coord_fixed(xlim = c(-100,-30), ylim = c(-60,-10), ratio = 1.3)+
  labs(title = "Correlacion entre la humedad relativa y el\ngeopotencial en 850hP")+
  theme_bw()
