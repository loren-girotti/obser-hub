      program ec_de_movimiento
      implicit none

C     Declaro las variables
     
      REAL alpha, theta, x, y, z, pi
      PARAMETER (pi=3.14159)

C     Pido alpha y theta al usuario
      write(*,*) 'Indique los valores de alpha (real positivo) y de
     &theta (en grados)'
      read(*,*) alpha, theta

C     Paso theta a radianes
      theta = theta*pi/180.0
      
C     Calculo x
      x = alpha * (cos(theta))**3
      write(*,*) 'x = ',x

C     Calculo y
      y = alpha * (sin(theta))**3
      write(*,*) 'y = ',y

C     Calculo z
      z = exp((abs(x+y))**(1./4.))*log10(1+(tan(theta))**2)
      write(*,*) 'z = ',z

      end

