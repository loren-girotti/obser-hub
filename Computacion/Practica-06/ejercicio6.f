C Programa que encuentra el complejo maximo o minimo de un vector de
C N numeros complejos.
      program max_min_complex
      implicit none
      integer n,i
      real mod_maxmin, a, b
      complex C(100), maxmin, z
      character*3 string

      write(*,*)'Ingrese la dim del vector de complejos'
      read(*,*) n
      write(*,*)'Ingrese el vector complejo, de a componentes'
        do i=1,n
             read(*,*)a,b 
             C(i)=complex(a,b)
             write(*,*)'Componente',i,'=',C(i)
        enddo
      
      write(*,*)'Escriba max para el maximo'
      write(*,*)'Escriba min para el minimo'
      read(*,*)string   

      z=maxmin(C,n,string)
      mod_maxmin=abs(z)

      write(*,*)'El ',string,' del vector es ',z 
      write(*,*)'cuyo modulo es ',mod_maxmin

      end

      complex function maxmin(C,n,str)
      integer n,i
      complex C(100)
      real mod_maxmin
      character*3 str
      
      if(str.eq.'max') then
      mod_maxmin=0.0
        do i=1,n
           if (abs(C(i)).gt.mod_maxmin) then
              mod_maxmin=abs(C(i))
              maxmin=C(i)
           end if
        end do 

      else
<<<<<<< HEAD
=======
            do i=1,n
              if (abs(C(i)).lt.maxmin) then
                maxmin=abs(C(i))
                      end if
            end do 
      end



>>>>>>> a97a4101152f0960ba441fcc13926fe9252680d0
      mod_maxmin=1e10
        do i=1,n
           if (abs(C(i)).lt.mod_maxmin) then
              mod_maxmin=abs(C(i))
              maxmin=C(i)
           end if
        end do 
      endif

      RETURN
      end
