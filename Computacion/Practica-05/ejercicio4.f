C este programa devuelve el n�mero complejo de mayor m�dulo dentro de un
C array de M n�meros complejos.
      program complex_mod
      integer nmax,m,indice
      parameter (nmax=100)
      real a,b,z(nmax),max
      complex C(nmax) !nmax es el valor maximo de componentes
      read(*,*) m !m seran las componentes de trabajo
      do i=1,m
      write(*,*) "Indique las componentes del complejo",i
      read(*,*) a,b
      c(i)=complex(a,b)
      write(*,*) "Registrado."

      z(i)=abs(c(i))

      end do
      
      max=0.0
      
      do i=1,m
              if (z(i).gt.max) then
                      max=z(i)
                      indice=i
              end if
      end do 
        
      write(*,*) "El complejo de mayor m�dulo es el de �ndice ",indice
      write(*,*) "con un m�dulo de ",max

             end
