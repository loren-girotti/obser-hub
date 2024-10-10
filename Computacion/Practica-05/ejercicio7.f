C Este programa utiliza el metodo de la burbuja para ordenar las
C componentes de los vectores que se hallan en PO5-Vectores.dat
C en orden descendente.
C #############################################################
      program bubble_sort
      logical desordenado
      integer n, j, i, IO
      real v(100)
      open(50,file="P05-Vectores.dat")

      read(50,*)
      IO=0
      do while(IO.eq.0)
10    format(X,I2,3X,100(F6.2,X))
      read(50,10,IOSTAT=IO) n, (V(i), i=1,n)
C Bubble Sort Algorithm in fortran77. ########
      desordenado=.true.
      i=0
        do while(desordenado)
        i=i+1
        desordenado=.false.
              do j=1,n-i
                    if(v(j).lt.v(j+1)) then
                    aux=v(j)
                    v(j)=v(j+1)
                    v(j+1)=aux 
                    desordenado=.true.
                    endif
              enddo
        enddo
C #########################################################
        write(*,*)"Las componentes del vector ordenado:"
        write(*,'(100(F6.2,X))') (v(j), j=1,n)
      enddo

      end
