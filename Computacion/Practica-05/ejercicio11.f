C EN ESTE PROGRAMA DEBEMOS MANIPULAR MATRICES COMPLEJAS
C DEBEMOS SUMAR LAS PARTES REALES DE LA DIAGONAL Y CONSTRUIR
C OTRA MATRIZ PERO DE CARACTERES SEGÚN EL SIGNO DE CADA PARTE REAL.
C ----------------------------------------------------------------- 
C LA FUNCIÓN real(z) DEVUELVE LA PARTE REAL DEL COMPLEJO z.
C-------------------------------------------------------------------
      program complex_matrix
      implicit none
      integer n,m,i,j
      parameter (n=5,m=5)
      complex Q(n,m)
      character*2 C(n,m)
      logical L(n,m)
      real tr
      
C Leemos desde el archivo 'P05-Matriz.dat'
      open(30,file="P05-Matriz.dat")
10    format(x,5(2(f5.2,x),3x))
      do i=1,n
        read(30,10) (Q(i,j),j=1,m)
        write(*,10) (Q(i,j),j=1,m)
      enddo
      


C Si la traza de Q es negativa (checkear si Q es cuadrada), tengo que 
C hacer la matriz de strings con N para los imaginarios neg y P para
C los positivos o reales puros.
C --------------------------------
C Ej: si en Q(1,2) tengo '2 - 3i', entonces en C(1,2) tengo 'N'
C     si en Q(3,3) tengo '-3 + 0i', en C(3,3) tengo 'P'.
C --------------------------------

C Cálculo de la traza:
      if(n.ne.m) then
            write(*,*)"LA MATRIZ NO TIENE DIAGONAL"
      STOP
      elseif(n.eq.m) then
      tr=0.
            do i=1,n
              tr=tr+real(Q(i,i))
            enddo
      endif
      
C CONSTRUYO LA MATRIZ QUE CORRESPONDE SEGÚN LA TRAZA:
      if(tr.lt.0) then
        write(*,*)"La traza fue negativa"
        do i=1,n
          do j=1,m
              if(aimag(Q(i,j)).lt.0)then
              C(i,j)="N"
              else
              C(i,j)="P"
              endif
          enddo
          write(*,11)(C(i,j),j=1,m)!Imprimimos la matriz en pantalla
        enddo
11      format(100(A,X))
      else
        write(*,*)"La traza fue no negativa"
        do i=1,n
          do j=1,m
              if(abs(Q(i,j)).lt.1)then
              L(i,j)=.FALSE.
              else
              L(i,j)=.TRUE.
              endif
          enddo
12        format(2x,100(L,2X))
          write(*,12)(L(i,j),j=1,m)!Imprimimos la pantalla
        enddo
      endif
      

      
      END


