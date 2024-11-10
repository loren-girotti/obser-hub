C DEBEMOS ELEGIR LA OPCIÓN CORRECTA DEL ENUNCIADO
C 
C FORMA 1:
C-------------------------------
C REAL FUNCTION TR(A,N)
C REAL A(N,N)
C INTEGER N, J
C TR = 0.
C DO J =1, N
C         TR = TR + A(J,J)
C ENDDO
C RETURN
C END
C ----------------------------
C
C FORMA 2:
C ---------------------------
C REAL FUNCTION TR(A, NP, N)
C REAL A(NP,NP)
C INTEGER N, J, NP
C TR = 0.
C DO J =1, N
C         TR = TR + A(J,J)
C ENDDO
C RETURN
C END
C ---------------------------
C
C La manera correcta de hacerlo es la forma 2, porque se define el
C tamaño de la matriz a partir de las dimensiones del programa
C principal y se adapta a la matriz que se le pase.
      program funcion_traza
      implicit none
      integer n, i, j, np
      parameter(np=20)
      real A(np,np), tr
      
      open(30,file="P06-Matriz.dat")
10    format(100(f4.1),x)
      n=0
      do i = 1, np
            read(30,10) (A(i,j),j=1,np)
            n=i
      enddo
      
      write(*,*) n
      
      end
      



