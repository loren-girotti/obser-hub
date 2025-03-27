rm(list = ls())
gc()

# llamamos a la librería tidyverse

library(tidyverse)

data('iris');iris

#a)
iris_tb<-as_tibble(iris)

#b)
iris;iris_tb

