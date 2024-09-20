C Este programa calcula la raíz cuadrada de un número N natural a
C través de una aproximación lineal reiterativa
      program raiz_cuadrada
      real epsilon,r0,rn,error,aux,aux2
      integer N

      write(*,*) 'Ingrese el N que desea calcular su raiz cuadrada'
      read(*,*) N

      if(N.lt.0) then
      write(*,*) 'N debe ser positivo'

      elseif(N.eq.0) then
      write(*,*) 'La raiz de N es 0'

      else 

      write(*,*)'Ingrese un valor inicial y el error minimo deseado'
      read(*,*) r0, epsilon

      rn=(1./2)*(r0+(N/r0))
      error = abs(rn-r0)/abs(rn)
      write(*,*) 'La primer estimación, rn=',rn
      write(*,*) 'Con un error = ',error

      i=0
      Do while(error.gt.epsilon)
        do j=0,i
        aux2=rn
        aux=(1./2)*(aux2+(N/aux2))
        rn=aux
        enddo
      error = abs(rn-aux2)/abs(rn)
      i = i+1
      enddo
      

10    format(A22,X,I8,x,A4,f7.4)
11    format(E9.4,X,A37,I8,x,A6)
      write(*,10)'Estimamos la raíz de',N,'como',rn
      write(*,11) error,'es el error de la estimación lueg
     &de',i,'ciclos'

      endif

      end
