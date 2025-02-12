C PROGRAMA PARA HACER UNA FUNCION PARA CALCULAR n!

      program factorial_func
      implicit none
      integer fact, n, r, Cnr
      write(*,*) "Ingrese n, y r para calcualr C(n,r)"
      read(*,*) n,r
      write(*,*) "n es ",n," r es ",r

      Cnr = fact(n)/(fact(r)*fact(n-r))
10    format(A10,I25)
      write(*,10) "C(n,r) = ",Cnr

      end

      integer function fact(n)
      integer n, i 
      fact=1
      do i = 1, n
            fact=i*fact
      enddo
      return
      end

      
