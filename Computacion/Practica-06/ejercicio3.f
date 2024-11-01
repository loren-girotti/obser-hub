C PROGRAMA PARA CREAR UNA FUNCIÓN EXTERNA QUE ROTE COORDENADAS
      program coordinates_rotation
      implicit none
      real x,y,xr,yr,theta,Rx,Ry,pi
      pi=4*atan(1.)
      write(*,*)"Este programa rota coordenadas de una determinada"
      write(*,*)"manera. Ingrese x, y, theta (en grados). En ese orden."
      read(*,*) x,y,theta
      theta=theta*pi/180
       
      
C LLAMAMOS A LA FUNCIÓN R(x,y,theta) PARA CALCULAR LAS COORDS.
      
      xr=Rx(x,y,theta)
      yr=Ry(x,y,theta)

      write(*,*) "Las coord ",x,y," se vuelven ",xr,yr,"."

      end

      function Rx(x,y,theta)
      real x,y,theta
      Rx=x*cos(theta)+y*sin(theta)
      return
      end

      function Ry(x,y,theta)
      real x,y,theta
      Ry=-x*sin(theta)+y*cos(theta)
      return
      end
