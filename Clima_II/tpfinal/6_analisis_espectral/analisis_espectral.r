rm(list = ls())
gc()


library(tidyverse)
library(zoo)


setwd("~/obser-hub/Clima_II/tpfinal/")


serie<- read_csv("./temperatura_media_diaria_rosario_1985-2005.csv")

serie <- serie %>% mutate("Dia"=day(ymd(date)),
                          "Mes"=month(ymd(date)),
                          "Anio"=year(ymd(date)),
                          "Anio_Est"=ifelse(Mes==12,Anio+1,Anio))

serie_est <- serie %>% filter(Mes %in% c(12,1,2), Anio_Est %in% 1986:2005) %>%
  group_by(Anio_Est) %>%
  summarise("Media"=mean(temp))

# Calculo la trasnformada

trans <- fft(z=serie_est$Media)

amplitud <- (2/length(trans)) * Mod(trans[2:(length(trans)%/%2 + 1)]) ### Ak = 2/n sum yt cos(2*pi*k*t/n)

periodos <-  length(serie_est$Media)/1:length(amplitud)

df<- tibble(Periodo=as.factor(round(periodos, digits = 3)),
            Periodo_num= round(as.numeric(periodos), digits = 3),
            C2= amplitud**2)

df %>%
  arrange(desc(C2)) %>%
  filter(row_number()<25) %>%
  ggplot(aes(x=Periodo, y=C2, group=Periodo))+
  geom_bar(stat='identity', fill='brown3', col='black')+
  coord_trans(y='sqrt')+
  labs(title = "Periodograma Temperatura Media Estival",
       subtitle ="Localidad: Rosario | Período: 1986-2005", 
       x= "Periodo [Años]",
       y= "Potencia espectral")+
  theme_bw()


### Funcion generica para el calculo del umbral de rechazo con un nivel de confianza de una serie x que sigue un modelo AR de orden m 

ruido <- function(x,m,alpha){
  # x son los datos y m es el orden del modelo AR
  
  n <- length(x)
  if(m == 0){
    ss_cuad <- rep(sd(x)**2,n%/%2) * 4/n
    ss_alpha <- ss_cuad * qchisq(alpha,df=2) / 2
    
    return(list(ss_cuad = ss_cuad,
                ss_alpha = ss_alpha))
    return(ss)
  }
  
  maux <- m+1
  r <- acf(x,lag.max=m,plot=FALSE) # corelacion de lag variable
  
  ###### Armo la matriz de mi sistema lineal
  a <- matrix(1,m,m) 
  for(j in 1:m){
    for ( k in 1:m){
      if( k < j){
        a[j,k]<-r$acf[j-k+1]
      }
      if( k > j){
        a[j,k]<-r$acf[k-j+1]
      }
    }
  }
  # Resuelvo Yule-Walker ecuacion
  sol_fin <- solve(a,r$acf[2:maux]) # lo resuelvo 
  
  #####calculo de sdex
  se <- matrix(0,maux,1)
  se[1] <- sd(x)# desvio estandar de x, desvio estandar de ruido blanco para AR(0)
  se[2] <- sqrt((1-r$acf[2]**2)*se[1]**2) #desvio estandar de ruido blanco para AR(1)
  
  for(i in 2:m){
    iaux <- i +1
    
    ###### Armo la matriz de mi sistema lineal
    a <- matrix(1,i,i) 
    for(j in 1:i){
      for ( k in 1:i){
        if( k < j){
          a[j,k]<-r$acf[j-k+1]
        }
        if( k > j){
          a[j,k]<-r$acf[k-j+1]
        }
      }
    }
    
    sol <- solve(a,r$acf[2:iaux]) # lo resuelvo 
    phi <- sol[i] # me quedo con el ultimo parametro de mi vector solución
    se[i+1] <- sqrt((1-phi**2)*se[i]**2)#desvio estandar de ruido blanco para AR(i+1)
  }
  
  sdeps <- se[i+1]
  
  z <- 0 - 2i
  k <- 1:m
  kk <- n %/% 2
  ss <- vector(length = kk, mode='double')
  for(ff in 1:kk ){
    ss[ff] <- 1 /abs(1-sum(sol * exp(z*pi*k*ff/n)))
  }
  ss_cuad <- (ss*sdeps)**2 * 4/n
  ss_alpha <- ss_cuad * qchisq(alpha,df=2) / 2
  
  return(list(ss_cuad = ss_cuad,
              ss_alpha = ss_alpha))
}

## Calculo el ruido espectro teórico de orden AR(1):

ruido_0 <- ruido(x=serie_est$Media, m = 0, alpha = 0.95)


# Construyo un Tibble con la data:
datos <-  tibble(Serie = amplitud**2,
                 Periodo = factor(periodos, levels = periodos),
                 Periodos_num = periodos,
                 Rechazo_95_Ar0= ruido_0$ss_alpha)

datos_long<-datos %>%
  pivot_longer(cols = c('Serie', 'Rechazo_95_Ar0'), names_to = 'Referencia', values_to = 'Potencia')

id<- which(datos$Serie>datos$Rechazo_95_Ar0)

datos_long %>%
  ggplot(aes(x=Periodos_num, y=Potencia, col=Referencia, lty=Referencia))+
  geom_line()+
 # scale_x_continuous(trans = trans_reverser('log10'), breaks = round(datos$Periodos_num[id], digits = 2))+
  theme_bw()+
  ylab('Potencia Espectral')+
  xlab('Periodo [Años]')+
  scale_color_manual(values=c('red', 'black'))+
  scale_linetype_manual(values = c(2,1))+
  labs(title="Contraste del espectro empírico contra ruido teórico")
