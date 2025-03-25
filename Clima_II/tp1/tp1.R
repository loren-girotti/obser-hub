#----
#EJERCICIO 1
#a)
#x <- c(1,3,5,7,9)
#Otra forma
x <- seq(from = 1,to = 10, by = 2)

#b)
Y <- 4*x

#c) 
Ymax<-max(Y)

#d)
x_ord<-sort(x,decreasing = TRUE)

#---------------------------------------
#EJERCICIO 2

#a)
fib<-c(1,1,2,3)

#b)
class(fib[3])

#c)
fib<-c(1,1,2,3,"Lorenzo")

#d)
class(fib[3])
# El cambio de clase se debe a que R cambió todos los tipos de variable del vector a "CHARACTER" ya que agregué
# un CHARACTER y las variables dentro de un vector deben ser del mismo tipo.

#e)
fib<-as.numeric(fib)
fib
# Al convertir el character en numeric le asigna el valor "NA" que significa Not Available ya que no sabe que valor
# numérico asignarle al string.

#-----------------------------------------
# EJERCICIO 3

#a)
vec<-seq(from =6, to =99, by=3)
#b)
long_vec<-length(vec)
#c)
which(vec==39)
which(vec==52) # Devuelve 'integer(0)' ya que no está 52 en el vector 'vec' pues son todos múltiplos de 3 y 52 no lo es.

#---------------------------------------
# EJERCICIO 4
#a)
enso_xxi<-c(2002,2004,2006,2009,2014,2018,2023)
#b) Considero Niño debil 0.5 a 0.9 de anom SST
#   Niño Moderado 1.0 a 1.4 de anom SST
#   Niño Fuerte entre 1.5 y 1.9 de SST
#   Niño Muy Fuerte de +2.0 de SST.
max_oni<-c("Moderado","Débil","Débil","Fuerte","Muy Fuerte","Débil","Muy Fuerte")

#c)
names(enso_xxi)<-max_oni

#d)
factor(max_oni, levels = c("Débil", "Moderado", "Fuerte", "Muy Fuerte"))

#----------------------------------------
# EJERCICIO 5
#a)
A <- matrix(data=1:8, nrow =2, ncol=4, byrow = TRUE)
#b)
A[2,3]
#c)
At <- t(A) # t() traspone matrices.
At[1,]
rownames(At)<-c("Primera","Segunda","Tercera","Cuarta") #rownames para asignar nombres a las filas; colnames para las columnas.

#---------------------------------------
# EJERCICIO 6
#a)
a <- matrix(data=1:4, nrow=4, ncol=4, byrow = TRUE)
#b)
det(a) #como es 0 no puedo sacar la inversa.
#c)
producto_matrices <- A%*%a # El producto matricial se hace con %*%, si hacemos con * hacemos elemento a elemento.
suma_matrices <- A+a # Da error claramente porque no son del mismo orden.

#----------------------------------------
# EJERCICIO 7
#a)
df <- data.frame(Nombre=c("Lorenzo","Mateo","Narela","Guadalupe","Paola","Gabriel","Matias"),
                 Apellido=c("Girotti","Rigo","Alvarez","Krivohlavy","Carrillo","Barroso","Lorenzo"),
                 Edad=c(24,25,27,21,23,27,27))
#b)
df[[3]]
df$Edad

#c)
factor(df$Edad)

#---------------------------------------
# EJERCICIO 8
data("mtcars");mtcars
#a)
dim(mtcars) #Checkeo las dimnesiones y veo que tiene 11 variables (sin contar el nombre)

#b)
low_hp<-which(mtcars$hp<100)

#c)
gears<-mtcars[mtcars$gear>=4,] #subset que agarra todas las filas con 4 o mas cambios

hp_prom<-mean(gears$hp)

#------------------------------------
# EJERCICIO 9
#a)
lista<-list(enso_xxi,a,df)

#b)
names(lista)<-c("vector","matriz","data frame")

#c)
length(lista[[1]]) #Para acceder al primer elemento de la lista debo utilizar doble corchete [[1]]
nrow(lista[[3]])

lista$vector[2]<-T # Como segundo elemento aparece el 1, indicando que es verdadero pero mantiene la clase de numeric.
lista$vector[3]<-F;lista$vector # Acá vemos como reemplaza el false por un 0.

#-------------------------------------
# EJERCICIO 10
#a)
# Genero la memoria donde voy a guardar los pesos en kg. Le doy el tipo numeric y las dimensiones según las filas que tiene mtcars
peso <- numeric(nrow(mtcars))

#Hago un loop que corre el indice i desde 1 hasta la cantidad de filas que tiene mtcars (coincidiendo con la dim de mi vector)
for(i in 1:nrow(mtcars)){
  peso[i]<-mtcars$wt[i]*1000*0.453592 # Calculo cada componente y la guardo en la correspondiente componente del vector peso.
}

#b)
sort(peso,decreasing = TRUE)

#---------------------------------------
# EJERCICIO 11
data("iris");iris

iris$cat1<-rep(0,nrow(iris))

#a)

for(i in 1:nrow(iris)){
  if(iris$Petal.Length[i]>=5){
    iris$cat1[i]<-"grande"
  }
  if(iris$Petal.Length[i]<5){
    iris$cat1[i]<-"pequeño"
  }
}

#b) Podemos utilizar acá el ifelse()
iris$cat2<-ifelse(iris$Petal.Length>=5,"grande","pequeño")

#-------------------------------------
# EJERCICIO 12
getwd()
setwd("/home/lgirotti/obser-hub/Clima_II/tp1/")

pp<-read.csv(file="./Datos.csv")

#b)
max(pp$Nobs,na.rm=TRUE) #4
min(pp$Nobs,na.rm=TRUE) #3

#Según los resutlados de los maximos y minimos, interpreto que Nobs son el Núm
# de observaciones realizadas en el día, donde NA significa que no hay observación.

factor(pp$Estacion)

#c)
#Consulto los datos de PP no disponibles o "Not Available (NA)"
pp[which(is.na(pp$PP)),]

#d)
dias_pp<-length(pp$PP[pp$PP>0]);dias_pp


