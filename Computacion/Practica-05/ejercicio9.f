C Programa para resolver sistemas de ec lineales triangulares infer
      program sistemas_lineales
      implicit none
      integer i,n,j,k
      parameter(n=4)!dimension de las matrices cuadradas
      real A(n,n), B(n), X(n), suma
        

      data A/16*0.0/ !Llenamos la matriz de ceros.
      
      do i=1,n
        do j=1,i
        !INGRESAMOS LOS DATOS DE A      
          write(*,11)"Ingrese el valor",i,j, "de la matriz A"
          read(*,*) A(i,j)
        if((i.eq.j).and.(a(i,j).eq.0)) then
        write(*,*)"ERROR: ELEMENTO NULO EN LA DIAGONAL"
        STOP !Finaliza el programa

        endif 
        enddo
      enddo

11    format(A20,x,"(",i2,",",i2,")",x,A14)
12    format(A20,x,"(",i2,")",x,A13)
C INGRESAMOS LOS DATOS DE B      
        do i=1,n
          write(*,12)"Ingrese el valor",i,"del vector B"
          read(*,*) b(i)
        enddo

C ESCRIBIMOS LAS MATRICES PARA CORROBORAR UN BUEN INGRESO
      write(*,*) "La matriz A:"
        do i=1,n
          write(*,10)(A(i,j),j=1,n)
        enddo
      
      write(*,*) "El vector B:"
        do i=1,n
          write(*,10)B(i)
        enddo

10    format(100(F5.1,2X))
      write(*,*) "Entonces las soluciones X son:"
      do j=1,n
      suma=0.
        do k=1,j-1
        suma = suma+a(j,k)*x(k)
        enddo
        x(j)=(1/a(j,j))*(b(j)-suma)
      write(*,10)x(j)
      enddo
  

      end
