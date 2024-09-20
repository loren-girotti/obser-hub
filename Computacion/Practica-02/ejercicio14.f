C Este programa calcula las raíces de un polinomio de grado 2
C Para eso vamos a escribir un pseudocódigo en el cual tendremos los 
C siguientes pasos:
C       - Pedir los coeficientes a, b y c del polinomio
C       - Calcular el determinante
C       - Si el determinate es no negativo, calcular las raices x1 y x2
C       con la expresión 1
C       - Imprimir los valores x1 y x2 aclarando que se uso Baskaara
C       - Utilizar la expresión 2 siempre que el discriminante sea 
C       positivo para calcular x1 y x2
C       - Imprimir los valores de x1 y x2 aclarando que se uso la expresión 2.
C   En caso que el determinante sea negativo, aclarar que no tiene solución
C -----------------------------------------------------------------------------
      PROGRAM resolvente
C        real a, b, c, x1, x2, e1, e2, q, det
      write(*,*) 'Coloque los coeficientes a, b y c separados por 
     & espacios'
      read(*,*) a, b, c
        
        det = b**2 - 4*a*c
        
        x1=(-b+sqrt(det))/(2.*a)
        x2=(-b-sqrt(det))/(2.*a)
        
      write(*,*) 'Las raices según Baskhara son: x1 = ',x1,' y x2 = ',
     & x2

      q=(-1./2.)*(b + (sign(1.,b)*sqrt(det)))
        x1=q/a
        x2=c/q

      write(*,*) 'Las raíces según la expresión 2 son: x1 = ',x1,' y
     & x2 = ',x2

        END

C ---------------------------------------------------------------        
C Corroborando con la calculadora, el método 2 es mucho mejor que
C Baskhara.
C ---------------------------------------------------------------









