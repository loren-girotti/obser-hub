C Transcribo un loop hecho con IF y GOTO a DOWHILE
      program loop
      sum=0.
      write(*,*) "Ingrese el primer numero"
      read(*,*) P
C Defino un limite para el loop "PL"
      PL = 1000.

      DO WHILE (P.NE.PL)
      SUM=SUM+P
      write(*,*) "Proximo numero"
      read(*,*)  P
        enddo

      end
