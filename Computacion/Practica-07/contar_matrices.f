C PROGRAMA PARA CONTAR DIMENSIONES DE MATRICES
C234567
      program contador_matrices
      implicit none
      integer i,j,mp,np,m,n
      parameter(mp=100,np=100)
      real A(mp,np)
      
      open(30,file="P06-Matriz.dat")
      open(40,file="P06-Matriz2.dat")
    
10    format(100(f4.1,x))

      do i=1,mp
        do j=1,np
            read(30,10) A(i,j)
        n=j
        enddo
      m=i
      enddo

      write(*,*)"La matriz tiene ",m," filas y ",n," columnas."
      
      end
      
