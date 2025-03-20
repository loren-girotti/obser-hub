# CLASE 1 20/03
---

Usamos R para programar.

---

Martes prácticas teóricas con Lolo.
Jueves prácticas con Euge.

---

### Estructura de prácticas:
**Primer Bloque**:
- Programación en R.
- Estadística.
- *Primer parcial*.

**Segundo bloque**
- Métodos estadísticos para análisis climatológico.
- *Trabajo final*.

Para el segundo bloque hay que tener un intercambio fluido con los profes.

---

### Fechas:
**MARTES 06/05 Primera fecha parcial** LLegar piola para esta.

---
Instalar R y R studio.
- Paquetes a instalar:
	1. metR: `install.packages(metR)`. Para llamarla `library(metR)`


---
# Notas de R:
### Tipos de variable:
- *Factor* (`factor`): Sirve para categorizar, podemos asignarles valores posibles.
*R ES SENSIBLE A LAS MAYÚSCULAS*

### Operadores:
-`%in%` sirve para filtrar valores que estan *dentro* de un objeto.
-`which` devuelve el/los indices de un array según la condición que le pase.

### Estructura de los datos:
- Vectores: 
-- Coleciona elementos de la **misma clase**.
-- Se construyen con `vec<-c(1,2,...)`.
-- Accedemos a los elementos con corchetes `vec[i]`

- Matrices y arreglos (*arrays*):
-- Todos los elementos deben ser de la **misma clase**.
-- Sintáxis `mat<-matrix()` o `arr<-array()`.
-- Accedemos a los valores registrados en la memoria con [,].

- Data frames
-- Estructura de datos bidimensional que puede tener elementos de **distinta clase**.
-- Podemos acceder a los elementos con [[]] o con $.

- Listas
-- Estructura de datos *unidimensionales* puede almacenar **distintas estructuras de datos**

### Estructuras de control
-if
Sintaxis:
```
if{logical}{
operación
}
```
-for loop
Sintaxis:
```
for(elemento in objeto){
operacion con elemento
}
```
-while
Sintaxis:
```
while(condicion logica){
operacion
}
```
