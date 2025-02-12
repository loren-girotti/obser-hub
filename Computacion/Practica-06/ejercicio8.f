C PROGRAMA QUE CALCULA EL MCD ENTRE DOS NATURALES
      program Maximo_Comun_Divisor
      implicit none
      integer j0,j,k,k0,d,mcd

      write(*,*)"Ingrese dos enteros positivos para hallar"
      write(*,*)"el MCD entre ellos."
      read(*,*)j,k
      k0=k
      j0=j
      d=mcd(j,k)

      write(*,*)"El MCD entre ",j0,"y",k0," es ",d

      end

      integer function mcd(j,k)
      integer j,k,r
      r=mod(j,k)
      do while(r.ne.0)
        j=k
        k=r
        r=mod(j,k)
      enddo
      mcd=k
      return
      end
      


