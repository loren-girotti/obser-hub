C Programa que encuentra el complejo maximo o minimo de un vector de
C N numeros complejos.
      program max_min_complex
      implicit none
      integer n,i
      real mod
      complex C(100)
      character*3 string
      write(*,*)'Ingrese la dim del vector de complejos'
      read(*,*) n
      write(*,*)'Ingrese el vector complejo, de a componentes'
      do i=1,n
      read(*,*) C(i)
      write(*,*)'Componente',i,'=',C(i)
      enddo



      end

      real function maxmin(C(n),n,str)
      integer n,i
      complex C(n)
      character*3 str
      
      if(str.eq.'max') then
      maxmin=0.0
            do i=1,n
              if (abs(C(i)).gt.maxmin) then
                maxmin=abs(C(i))
              end if
            end do 
      else
            do i=1,n
              if (abs(C(i)).lt.maxmin) then
                maxmin=abs(C(i))
              end if
            end do 
