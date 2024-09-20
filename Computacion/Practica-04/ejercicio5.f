C Vamos a calcular distintas combinaciones de variables con if
      program variables_z
      
      do i=-5,5  
       !Definición de X
       do j=501,605,2
        x=j/100.
C -----------------------------------       
C Calculo z para cada caso
        if(i.lt.0) then !primera rama de la función
         Z = X*2*I

        elseif(I.eq.0) then !segunda rama de la función
         Z=X+1

        else !tercera rama de la función
         Z=X/(2*I)
       
        endif
C ----------------------------------
       
C Imprimo las X y Z correspondientes
        write(*,*) 'X = ',x,' Z = ',z

        enddo !Cierro el ciclo de X

      enddo !Cierro el ciclo de I

              end
