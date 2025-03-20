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
