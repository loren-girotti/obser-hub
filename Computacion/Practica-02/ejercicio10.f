      PROGRAM LORENTZ
C     Defino los tipos de variables a mano
      implicit none
C     ---------------------------------------


C     Defino las variables y los parametros.      
      REAL Beta, gamma, delta, rtheta, v, theta, c, pi
      PARAMETER (v=2.91E5, theta=1.22, c=3.0E5, pi=3.14159)
C     -----------------------------------------

C     Paso de grados a radianes para theta
      rtheta=theta*pi/180.
      write(*,*) 'Siendo theta 1.22º, equivale a ',rtheta,' radianes'
C     -----------------------------------------

C     Calculo beta
      Beta = v/c
      write(*,*) 'Beta vale ',Beta

C     -------------------------------------------

C     Calculo gamma
      gamma = 1./(sqrt(1.-(v/c)**2))
      write(*,*) 'gamma vale ',gamma

C     -------------------------------------------
      
C     Calculo delta
      delta = 1./(gamma*(1.-beta*cos(rtheta)))
      write(*,*) 'delta es ', delta

C     -------------------------------------------

      END 
