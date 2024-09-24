C Programa para listar los elementos de la matriz B
       program lista_b
       real B(5,6)
C Listo los primeros 6 elementos de la matriz B
       i=1
       do j=1,6
         write(*,*) b(i,j)
       end do

C Listo el último elmento
       write(*,*) b(5,6)

       end
