C Este programa calcula si un punto est√ a una distancia R o menor del
C origen, ingresando R y el punto por teclado.
      program distancia
      real x, y, z, r, dist_c

      write(*,*) 'Ingrese el radio R de la esfera'
      read (*,*) r
      write(*,*) 'Ingrese las coordenadas del punto'
      read(*,*) x,y,z

C Calculo la distancia al cuadrado y la comparo con R**2
      dist_c = x**2 + y**2 + z**2

      if(dist_c .le. r**2) then
       write(*,*) 'El punto est√ °dentro de la esfera'

      else
       write(*,*) 'El punto est√ °fuera de la esfera'

      endif

      end
