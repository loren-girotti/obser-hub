C Vamos a programar una función sentencia que convierta temperaturas
C Celcius en Farenheit.
      program convertidor_temp
      implicit none
      real conv, c, x
      integer i
      conv(x)=1.8*x+32
      
10    format(2(X,A4,3X,"|"))
      write(*,10) "°C","°F"

11    format(2(x,f5.1,X,"|"))
      do i=1,100
        c=i
        write(*,11) c,conv(c)
      enddo

      END

      
