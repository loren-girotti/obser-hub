C este programa devuelve el n�mero complejo de mayor m�dulo dentro de un
C array de M n�meros complejos.
      program complex_mod
      integer nmax,m,indice
      parameter (nmax=100)
      real a,b,z(nmax),max
      complex C(nmax) !nmax es el valor maximo de componentes
      write(*,*) "Ingrese la cantidad de vectores complejos"
      read(*,*) m !m seran las componentes de trabajo

      do i=1,m
      write(*,*) "Indique las componentes del complejo",i
      read(*,*) a,b
      c(i)=complex(a,b)
      write(*,*) "Registrado."

      z(i)=abs(c(i))

      end do

C Recorro los valores de los modulos y los comparo, guardando el maximo de ellos en max
C su indice en 'indice':      
      max=0.0
      
      do i=1,m
              if (z(i).gt.max) then
                      max=z(i)
                      indice=i
              end if
      end do 

C Imprimo, el indice, el numero complejo y el modulo del maximo:
      write(*,*) "En el indice",indice," se aloja el complejo max c =",
     &c(indice),"con modulo ",max 

      
             end
