C Este programa calcula el producto de dos matrices ingresadas x teclad
      program prod_matrices
      integer nA,nB,mA,mB
      real A(100,100), B(100,100), C(100,100)
      write(*,*)"Ingrese las dimensiones de la matriz A"
      read(*,*) nA, mA    
      write(*,*)"Ingrese las dimensiones de la matriz B"
      read(*,*) nB, mB    

C CHEQUEAMOS QUE SE PUEDA REALIZAR EL PRODUCTO
      if((mA.ne.nB).or.(mB.ne.nA)) then
      write(*,*)"No se puede realizar el producto."
      else
      
11    format(A20,x,"(",i2,",",i2,")",x,A14)
C INGRESAMOS LOS DATOS DE A      
        do i=1,nA
          do j=1,mA
          write(*,11)"Ingrese el valor",i,j, "de la matriz A"
          read(*,*) A(i,j)
          enddo
        enddo
C INGRESAMOS LOS DATOS DE B
        do i=1,nB
          do j=1,mB
       write(*,11)"Ingrese el valor",i,j, "de la matriz B"
       read(*,*) B(i,j)
          enddo
        enddo

C ESCRIBIMOS LAS MATRICES PARA CORROBORAR UN BUEN INGRESO
      write(*,*) "La matriz A:"
        do i=1,nA
          write(*,10)(A(i,j),j=1,mA)
        enddo
      
      write(*,*) "La matriz B:"
        do i=1,nB
          write(*,10)(B(i,j),j=1,mB)
        enddo

10    format(100(F5.1,2X))
      
C HACEMOS EL PRODUCTO
      do i=1,nA
        do j=1,mB
        c(i,j)=0
          do k=1,mA
          c(i,j)=c(i,j)+a(i,k)*b(k,j)
          enddo
        enddo
      enddo      

C IMPRIMIMOS EL RESULTADO
      write(*,*)"Finalmente C=AB:"
      do i=1,nA
        write(*,10)(C(i,j),j=1,mB)
      enddo
  
      endif
      

            end
