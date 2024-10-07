C Este programa utiliza el metodo de la burbuja para ordenar las
C componentes de los vectores que se hallan en PO5-Vectores.dat
C en orden descendente.
C #############################################################
      program bubble_sort
      integer nmax, N(30), i, m
      parameter (nmax=30)
      real v(nmax)
      open(50,file="P05-Vectores.dat")

      read(50,*)
10    format(X,I2,3X,29(F6.2))
      read(50,10) N(1), (V(i), i=1,30))
      write(*,*) n(1), v(1), v(2)
C Bubble Sort Algorithm in fortran77. ########
C      do i=1,n-1
C            do j=1,n-i
C                  if(v(j).lt.v(j+1)) then
C                  aux=v(j)
C                  v(j)=v(j+1)
C                  v(j+1)=aux 
C                  endif
C            enddo
C      enddo
CC ###########################################
C      write(*,*)"el vector en orden descendente"
C      do i=1,n
C      write(*,*) v(i)
C      enddo
C
      end
