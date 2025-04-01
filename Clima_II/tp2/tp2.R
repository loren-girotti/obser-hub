rm(list = ls())
gc()

# Llamamos a la librería tidyverse

library(tidyverse)

#------------------
# EJERCICIO 1
data('iris');iris

#a)
iris_tb<-as_tibble(iris)

#b)
iris;iris_tb

#c)
sepal_length_df<-iris$Sepal.Length
sepal_length_tb<-iris_tb$Sepal.Length

class(sepal_length_df)
class(sepal_length_tb) #Se obtiene la misma estructura en ambos casos.

is.vector(sepal_length_df)
is.vector(sepal_length_tb)
is_tibble(sepal_length_df)
is_tibble(sepal_length_tb)

#d)
sepal_length_df<-iris[,1]
sepal_length_tb<-iris_tb[,1]

is.vector(sepal_length_df) #TRUE
is.vector(sepal_length_tb) #FALSE
is_tibble(sepal_length_df) #FALSE
is_tibble(sepal_length_tb) #TRUE

#e)
which(sepal_length_df==4.3)
which(sepal_length_tb==4.3)

 # PREGUNTAR PORQUE ME DIO TODO IGUAL.

#---------------
# EJERCICIO 2
setwd("~/obser-hub/Clima_II/")
data<-read.csv(file="./tp1/Datos.csv") # read con R base
data_rdr<-read_csv(file="./tp1/Datos.csv") # read con readR 

# Leyendo con read_csv nos da mucha info en consola ademas de que distingue las
# fechas.

# a)
class(data) #es un Data Frame
class(data_rdr) #es un Tibble

class(data$Fecha) # character
class(data_rdr$Fecha) # Date

#b)
data$Fecha<-ymd(data$Fecha)
class(data$Fecha)

#c)
mdq<-filter(.data=data, Estacion=="MAR DEL PLATA AERO")
class(mdq)#data frame

#d)
mdq$Logico<-logical(nrow(mdq))

for(i in 1:nrow(mdq)){
  mdq$Logico[i]<-mdq$PP[i] > 0.0
}

#e)
mdq_tb<-filter(.data=data_rdr,Estacion=="MAR DEL PLATA AERO")


