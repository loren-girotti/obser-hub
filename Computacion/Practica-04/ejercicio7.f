C Este es un algoritmo que clasifica las notas de un examen en:
C DESAPROBADO, APROBADO, DESTACADO O SOBRESALIENTE.
      program notas
      integer nota
      write(*,*)'Ingrese la nota del examen'
      read(*,*) nota

      if((nota.lt.4).and.(nota.ge.0)) then
      write(*,*)'El examen está DESAPROBADO'

      elseif((nota.ge.4).and.(nota.lt.7)) then
      write(*,*)'El examen está APROBADO'

      elseif((nota.ge.7).and.(nota.le.8)) then
      write(*,*)'El examen es DESTACADO'

      elseif((nota.ge.9).and.(nota.le.10)) then
      write(*,*)'El examen es SOBRESALIENTE'

      else 
      write(*,*)'La nota ingresada no es válida'

      endif

      end
