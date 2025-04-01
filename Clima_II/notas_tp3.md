# CLASE 01/04
---
Las muestras representativas deben ser no solo numerosas sino también aleatorias.

---
## Medidas características de una distribución
*Medidas de centralización:* indican en torno a qué valor se concentran los datos.
- Valor medio
- Mediana
- Moda

*Medidas de disperción:* indican la representatividad de los parámetros de
centralización.
- Varianza y desvío.
- IQR
- Rango.

*Momentos estadístico:* nos da idea de la forma en que los datos se distribuyen
en una distribución.
- Asimetría -> Momento de orden 3
- Curtosis -> Momento de orden 4.

---
sesgo: diferencia entre valor real y valor medio.
---
## "Peso de las colas"
Es la probabilidad asociada a los valores extremos.
Podemos estudiarlo con la curtosis.

## Asimetría
La estudio a través del momento de orden 3 con un enfoque paramétrico;
o puedo graficar la distribución y observar qué cola es más "larga".

---
# Método de máxima verosimilitud
Estimamos los parámetros utilizando la función de *verosimilitud* o *likelihood*

---
# SECCIÓN R
Nos interesa realizar simulaciones.

A través del comando `rnorm` nos devuelve un valor aleatorio que sigue una
distribución normal. Por ejemplo, podemos tomar una muestra de una normal cuya
media sea 0 y su desvio estandar sea 1. *(r de random)*

```
Normal=rnorm(n=10000,mean=0,sd=1)
```


### Calculo de parámetros:
```
library(moments)
```

- Mediana: `median(dataset)`
- p25: `quantile(dataset, probs=0.25)`
- p75: `quantile(dataset, probs=0.75)`
- Curtosis: `kurtosis(dataset)`
- Asimetria: `skewness(dataset)`



### Ajustes:
Por maxima verosimilitud
`mledist(data=dataset, distr='distribucion')`
