C Programa para calcular normas de matrices reales mxn
C Una de las normas es la de Frobenius, la otra es la infinita
C ------------------------------------------------------------
      program norm_matrices_reales
      implicit none
      integer i,j,n,m
      real A(100,100),norma,Fro_norm,Inf_norm
      character*3 norm_type

      write(*,*)"Detremine las dimensiones de la matriz"
      read(*,*)m,n ! 4x4 para el caso de la matriz del ej4
C Cargamos la matriz, en este caso la del ejercicio4
      OPEN(30,File="P06-Matriz.dat")
10    format(100(F4.1,X))

      write(*,*)"Leyendo la matriz: P06-Matriz.dat ..."
      
      do i=1,m
         read(30,10,end=40) (A(i,j), j=1,n)
      enddo

40    do i=1,m
        write(*,10) (A(i,j),j=1,n)
      enddo
C---------------------------------------------------
C Elegimos que norma calcluamos
C -----------------------------
      write(*,*)"Elegir que norma se calculara"
      write(*,*)"Escribir FRO para la norma de Frobenius"
      write(*,*)"Escribir INF para la norma infinita"
      read(*,*) norm_type

      if (norm_type.eq."FRO") then
         norma=Fro_norm(A,m,n)

      elseif (norm_type.eq."INF") then
         norma=Inf_norm(A,m,n)

      endif

      write(*,*)"La norma de ", norm_type, " es"
      write(*,10) norma
      end
C ----------------------------------------------
C FUNCION PARA LA NORMA DE FROBENIUS
      real function Fro_norm(matriz,filas,cols)
      real matriz(100,100),sum
      integer filas,cols
      sum=0.
      do i=1,filas
        do j=1,cols
        sum=sum+abs(matriz(i,j))**2
        enddo
      enddo
      Fro_norm=sqrt(sum)

      RETURN
      end
C ---------------------------------------------
C ---------------------------------------------
C FUNCION PARA LA NORMA INFINITA
      real function Inf_norm(matriz,filas,cols)
      real matriz(100,100),max
      integer filas,cols
      max=0.
      do i=1,filas
        do j=1,cols
           if (abs(matriz(i,j)).gt.max) then
             max=abs(matriz(i,j))
           endif
        enddo
      enddo
      
      Inf_norm=max

      RETURN
      end
C ---------------------------------------------
