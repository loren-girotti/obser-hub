C Este programa detecta si un punto está dentro de un rect�ngulo dado
C por su v�rtice superior izquierdo y su v�rtice inferior derecho, ambos
C indicados por el usuario.
      program rectangulo
      real xs,xi,ys,yi,x,y
      logical vertSup, vertInf

      write(*,*) 'Ingrese las coordenadas del vértice sup. izq'
      read(*,*) xs,ys
      write(*,*) 'Ingrese las coordenadas del vértic inf. der.'
      read(*,*) xi,yi
      write(*,*) 'Ingrese las coordenadas del punto a evaluar'
      read(*,*) x,y

C Condiciones lógicas de los vértices a evaluar
      vertSup = x.ge.xs .and. y.le.ys
      vertInf = x.le.xi .and. y.ge.yi
      !---------------------------                             
     
      if(vertSup .and. vertInf) then
      write(*,*) 'El punto está dentro del rectángulo'

      else
      write(*,*) 'El punto está fuera del rectángulo'

      endif
        

        end

