C Este programa cuenta los caracteres de las palabras.
      program contador_de_letras
      integer letras(12), palabras4
      character*11 P(12)
      palabras4=0
C Sector de registro de palabras:
      write(*,*) "Ingrese 12 palabras de 10 caracteres MAX"
      do i=1,12
        write(*,*) "Palabra ",i,":"
        read(*,*) P(i)
        do while(index(P(i)," ").eq.0)
          write(*,*)"Se excedió el límite de caracteres"
          write(*,*)"Vuelva a ingresar una palabra"
          read(*,*) P(i)
        enddo
        
        write(*,*) "Se guardó ",P(i)," en ",i
        letras(i)=index(P(i)," ")-1
           if (letras(i).eq.4) then
           palabras4=palabras4+1
           endif
      enddo
C -------------------------------------------------------
10    format(2(2X,A7,2X,"|"))
11    format(X,A10,X,"|",5X,I2,5X,"|")

C Sector de manipulación de datos:
      write(*,10) "Palabra","#Letras"
      do i=1,12
        write(*,11) P(i),letras(i)
      enddo

      write(*,*) "Se registraron ",palabras4," palabras con 4 letras."
      end

C FUNCION INDEX: index(a,b) busca "b" en "a" y te devuelve la posicion
C donde lo encontró. Solo con caracteres.
