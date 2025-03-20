### Script introductorio a la programacion en R 

########## ________________ elementos basicos _________________________________
{
  ## Vectores
  #### creo un veector de numeros pares y le asigno el nombre vec
  vec<- c(2,4,6,8,10)
  
  #### Consulto la clase de elementos que contiene vec
  class(vec)
  
  ### Consulto el tipo de estructura de vec
  is.vector(vec)
  
  ### multiplico todos los elementos por 3 y le asigno el nombre mult
  mult<- 3*vec
  
  ### Consulto la cantidad de elementos en mult
  length(mult)
  
  #### Extraigo el tercer elemento de mult
  vec[3]
  
  #### Tomo el maximo valor del vector mult
  max(mult)
  
  ### Obtengo el indice del vector mult cuyo valor es 18
  which(mult==18)
  
  ##### Obtengo el indice del vector mult cuyo valor es 20
  which(mult==20)
  
  ### Convierto el vector vec a caracter
  vec_char<- as.character(vec)
  is.vector(vec_char)
  class(vec_char)
  
  #### Defino un vector x con los primeros 4 numeros naturales y la palabra "Climatologia II". Â¿Que sucedio?
  x<- c(1,2,3,4,'Climatologia II')
  
  ### Consulto cual es el indice que contiene la palabra 'climatologia II' en x. Â¿Como explico esto?
  which(x=='climatologia II')
  
  #### convierto el vector x en numerico. Â¿Que sucedio con el utlimo elemento?
  x<- as.numeric(x)
  
  ### Genero un vector que contenga una secuencia del 85 al 100 de dos formas alternativas
  y<- seq(from= 85, to=100, by=1)
  y2<- 85:100
  
  ## Son todos los elementos de y iguales a y2?
  y==y2
  
  #### Busco los elementos de y mayores o iguales a 90
  y>=90
  which(y>=90)
  
  ### Defino un vector con los elementos mayores a 90 que se llama mayor (Subconjunto por indice)
  mayor<- y[which(y>=90)]
  
  ### Obtengo el mismo vector, pero utilizando otra forma (Subconjunto condicional)
  mayor2<- y[y>=90]
  
  ## Comparo ambos vectores para ver si son iguales 
  mayor
  mayor2
  
  ### Defino un vector con los utlimos 5 eventos de Dipolo del Oceano Indico moderados a intensos 
  iod<- c(2022, 2019, 2016, 2015, 2010)
  
  ## Asigno nombres a cada elemento del vector iod
  nombre<- c('Primero', 'Segundo', 'Tercero', 'Cuarto', 'Quinto')
  names(iod)<- nombre
  iod
  
  ### Extraigo el tercer elemento del vector iod utilizando su nombre (Subconjunto por nombre)
  iod['Tercero']
  
  
  ### Creo un factor llamado fac que contiene la clasificacion del evento de IOD
  evento<- c('Negativo', 'Positivo', 'Negativo', 'Positivo', 'Negativo')
  fac<- factor(evento)
  fac
  
  ### Modifico el orden de los niveles del factor
  levels(fac)<- c('Positivo', 'Negativo')
  fac
  #######
  
  ### Matrices
  ### genero una matriz de 3x2 llamada A
  A<- matrix(data=1:6, nrow = 3, byrow = T)
  A
  
  ### Consulto las dimensiones de A
  dim(A)
  nrow(A)
  ncol(A)
  
  #### Obtengo el elemento de la 3er fila y la 1er columna
  A[3,1]
  
  ### Obtengo todos los elementos de la 2da fila
  A[2,]
  
  ### Traspongo la matriz A y le asigno el nombre B
  B<- t(A) 
  
  ### Le sumo 2 a cada elemento de la matriz B
  2 + B
  
  ### le asigno nombre a las columnas de B
  
  colnames(B)<- c('col1', 'col2', 'col3')
  B
  
  
  ##### Data Frames 
  ## Genero un data frame con el nombre, latitud, longitud y altitud de algunas estaciones del SMN 
  df<- data.frame(Nombre=c('La Quiaca Observatorio', 'Rivadavia', 'Benito Juarez Aero'),
                  Lat=c(-22.10, -24.19, -37.70),
                  Lon=c(-65.60, -62.89, -59.79),
                  H=c(3459, 205, 207))
  
  ### Consulto las dimensiones de df 
  dim(df)
  length(df) ## Â¿Que devuelve length?
  ncol(df)
  nrow(df)
  
  ### estructura del data frame
  str(df)
  
  #### Extraigo la columna de nombres 
  df[,1]  ### Subconjunto por indice
  df[,'Nombre']   ### Subconjunto por nombre
  df$Nombre ### Subconjunto usando $
  df[[1]] ## Subconjunto usando Corchetes dobles
  
  ### Genero un subconjunto del data frame con aquellas estaciones al norte de -30Â°S 
  df[which(df$Lat>-30),]  ### Subconjunto por indice
  df[df$Lat>-30,] ### Subconjunto condicional
  
  #### Que tipo de estructura obtengo si extraigo una columna? Â¿Tiene sentido esto?
  is.matrix(df$Nombre)
  is.data.frame(df$Nombre)
  is.vector(df$Nombre)
  
  
  #### Listas
  
  ### Genero una lista llamada lista1 con datos de distinta clase 
  lista1<- list(1, 2+3i, T, 'elemento')
  
  ### Genero una lista llamada lista 2 que contenga un vector, una matriz y un data frame
  lista2<- list(c(10,20,30),
                A,
                df)
  
  lista2
  
  ### Agrego nombre a los elementos
  names(lista2)<- c('Vector', 'Matriz', 'Data frame')
  lista2
  
  ### extraigo el segundo elemento de lista2
  lista2$Matriz ### subconjunto usando $
  lista2[[2]] ### Subconjunto usando doble corchete
  
}

##### _____________________ Estructuras de control ____________________________

{
  ### Antes de correr analizar que resultado espera en ambos casos
  
  vector<- seq(3, 7)
  for(k in vector){
    
    print(k*3)
    
  }
  
  ### Asignarle un valor a x
  x<- 3+2i
  
  if(is.complex(x)){
    print('X es una variable compleja')
  } else{
    print('X no es una variable compleja')
  }
  
  ### for e if combinados
  
  df<- data.frame(Nombre=c('La Quiaca Observatorio', 'Rivadavia', 'Benito Juarez Aero'),
                  Lat=c(-22.10, -24.19, -37.70),
                  Lon=c(-65.60, -62.89, -59.79),
                  H=c(3459, 205, 207))
  
  ### Que salida espera?
  
  for(ind in 1:nrow(df)){
    if(df$H[ind]<1000){
      print(df$Nombre[ind])
    }
  }
  
}


######  __________________ Lectura de archivos ________________________________
getwd()

caudal<- read.csv(file = '')




