# Notas TP5 - Correlación y regresión.

Partimos de una serie de datos reales aleatorios.
Buscamos un *modelo* lineal que me relacione nos variables.

Definimos el *error* como la diferencia entre el valor predicho
y el valor observado por el modelo.

A la variable independiente *x* solemos llamarla **variable predictora**.
A la variable dependiente *y* solemos llamarla **variable objetivo**.

*Definición:*
- Coeficiente de determinación: Compara la proporción entre la suma cuadrática
de la diferencia entre el modelo y la media muestral, con la varianza muestral.
Va entre 0 y 1; mientras más cerca de 1 mejor es el modelo.

---

## Residuos del modelo

Le pedimos que:
- La media del vector resiudo sea 0.
- Se distribuyan de manera simétrica respecto de la media.
- Que tengan igual varianza respecto del predictor (**Homocedasticidad**).
Esto es importante para poder caracterizar con una única distribución a los residuos.

---
# Regresión lineal múltiple
Ahora ajustamos una variable objetivo, pero con varias variables predictoras.
Si tengo 2 variables predictoras, podemos representar el ajuste mediante un plano.

## Selección de predictores:
Hay que encontrar predictores que tengan sentido físico y no sobrecargar de variables
en pos de mejorar el modelo.

Quiero que el modelo aprenda la *relación de las variables* no que estime a la perfección
mis datos observados.

## Criterios de información:
- Criterio de Akaike (AIC): pide el mejor modelo más barato.
```
AIC = 2*k-2*ln(L)
```

- Criterio de Bayes o Schwatz
```
BIC = kln(n)-2*ln(L)
```

Donde `L` es la función de verosimilitud (maximizada);
`k` es el número de parámetros del modelo;
y `n` es el número de observaciones.


---

# Correlación de Pearson.
```
r_xy=cov(x,y)/(sd_x*sd_y)
```

## Coeficientes de correlación no paramétricos
- Correlación rankeada de Spearman:
es un coef de Pearson pero previamente los ordeno de menor a mayor.

- Tau de Kendall:
ordeno mis observaciones en pares **concordantes** y **discordantes**,
decimos que son concordantes cuando se encuentran en las relaciones crecientes,
discordantes entre relaciones decrecientes.
