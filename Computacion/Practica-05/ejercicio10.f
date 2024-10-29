C PROGRAMA QUE INVIERTE LAS COLUMNAS DE UNA MATRIZ DE mxn
      program columnas_invertidas
      implicit none
      integer n,m,i,j
      parameter (m=4,n=3)
      real A(m,n), aux
C LLENADO DE MATRICES CON DATA:
      DATA ( (A(i,j), j=1,n), i=1,m)/n*1.0,n*2.,n*3.,n*4./
C realizamos el llenado con un do implicito a través de DATA, observar
C que se realiza por columna, entonces podemos llenarla con n columnas
C iguales con los datos que definimos entre / / 

      write(*,*)"Tenemos la matriz A:"
10    format(100(f5.2))
      do i=1,m
          write(*,10) (A(i,j), j=1,n)
      enddo

C Para invertir las columnas, utilizo una memoria auxiliar (como tengo
C que invertir los ELEMENTOS de las columnas, tengo que switchear las
C filas.

      do j=1,n
        do i=1,m/2 !voy solo hasta la mitad, porque sino queda igual*
        aux=a(i,j)
        a(i,j)=a(m-i+1,j)
        a(m-i+1,j)=aux
        enddo
      enddo

      write(*,*)"La matriz A con columnas invertidas:"
      do i=1,m
        write(*,10) (A(i,j), j=1,n)
      enddo

C* porque estaríamos cambiando 2 veces todo.


      end

