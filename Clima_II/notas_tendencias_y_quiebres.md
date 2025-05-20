# TENDENCIAS Y QUIEBRES
---

Si ajustamos un modelo de regresión lineal a una serie temporal,
podemos obtener una tendencia *lineal*.

Necesito testear la tendencia para ver que no sea debida a la naturaleza aleatoria
de la variable.

Podemos hacer un test de correlación entre la variable y el tiempo, o entre
las observaciones y los valores *modelados*.

Hay que definir una Hipótesis Nula. 

## Análisis de tendencias y estacionariedad.
Como haremos estadística inferencial, es decir, hacer inferencias
sobre una población a partir de una muestra; si tengo una tendencia
es decir un cambio de la variable a través del tiempo, la población 
será siempre distinta y mis inferencias, por tanto, erróneas.

---

# Test de Mann-Kendall

```
library(trend)
mk.test(x = data, alternative = "two.sided") # A dos colas
mk.test(x = data, alternative = "greater") # A una cola, asumiendo que
					   # es tendencia positiva

```

---
# Frecuencias de forzantes
Consideramos de alta frecuencia a los forzantes interdecadales.
- Ejemplo: El Niño, la SAM, etc.
A los forzantes de baja frecuencia los consideramos, decadales o superiores.
- La PDO, y la AMO.

---
# Quiebres en series temporales

## Monoquiebres
Para ver puntos de quiebre, existen los tests de SNHT, Pettit, Buishand R, utilizar
la librería `library(BreakPoints)`

Los anteriores test son para *monoquiebres*.

## Multiquiebres


