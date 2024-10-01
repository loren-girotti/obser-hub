C Este programa calcula el promedio de las componentes de un vector, el
C cual es ingresado por el usuario. A su vez, calcula el desvío
C estandar.
      program promedio_y_desvio
      integer nmax, n 
      parameter (nmax=1000) ! limite maximo de componentes
      real Q(nmax), media, varianza, s, D(nmax)
      write(*,*) "Ingrese la cantidad de componentes N"
      read(*,*) n

      do i=1,n
        write(*,*) "Ingrese el componente ",i
        read(*,*) Q(i)
      enddo

      media=0.
      do i=1,n
       media=media+Q(i)
      enddo

      media=media/n

      varianza=0.
      do i=1,n
       D(i)=Q(i)-media !vector de desvíos
       varianza=varianza+D(i)**2
      enddo
      varianza=varianza/n

      s=sqrt(varianza)

      write(*,10) "Indice","Valor","Desvio"
10    format(X,3(A6,X,"|",X))
      
      do i=1,n
      write(*,11) i,Q(i),D(i)
      enddo
11    format(I6,X,"|",X,2(F6.2,X,"|",X))

      end



