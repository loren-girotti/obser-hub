C ESTE PROGRAMA CALCULA LAS RAICES DE UNA FUNCION CUADRATICA
      program cuadratica
      complex z1, z2
      real a, b, c, d
      
C pido los coeficientes por pantalla
      write(*,*) 'Escriba los coeficientes a, b y c'
      read(*,*) a,b,c

C calculo el discriminante d:
      d = b**2-4*a*c

C Tomo decisiones en base al resultado del discriminante:
      !- Si D es negativo -------------------
      IF(d.lt.0) then
        !complex(d,0) convierte el discriminante en un n√mero complejo
        z1=(-b+sqrt(complex(d,0)))/(2*a) !calculo z1
        z2=(-b-sqrt(complex(d,0)))/(2*a) !calculo z2
        write(*,*) 'Las ra√ces son complejas: z1=',z1,'z2=',z2

       
      !- Si D = 0 ---------------------------     
      elseif(d.eq.0) then
        x=-b/(2*a) !calculo la unica ra√z
        write(*,*) 'Hay solo una ra√z: x=',x
       

      else
        x1=(-b+sqrt(d))/(2*a)
        x2=(-b-sqrt(d))/(2*a)
        write(*,*) 'Hay 2 ra√ces reales: x1=',x1,'x2=',x2
        
      ENDIF

      end
