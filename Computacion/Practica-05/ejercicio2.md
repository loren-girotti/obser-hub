* En el caso del primer bloque a)
```
DO J = 1, 5
  L(J+1) = L(1)
ENDDO
```

Siendo `L` un arreglo con 6 elementos reales; vuelve a todos sus elementos iguales al primero, por ejemplo:

```
L(2.3,4.5,-2.34,1.2,3.0,0.0)
```

Luego del ciclo `DO` queda

```
L(2.3,2.3,2.3,2.3,2.3,2.3)
```
---
* Para el bloque b)

```
DO J = 2, 6
  L(J-1) = L(J)
ENDDO
```
Le asigna a cada elemento el elemento posterior:

Siendo
```
L(1.,2.,3.,4.,5.,6.)
```

Luego del ciclo `DO` queda
```
L(2.,3.,4.,5.,6.,6.)
```
