C Clasificacion de años bisiestos
      program anos_bisiestos
      integer ano
      write(*,*)'Ingrese el ano que desea clasificar'
      read(*,*)ano

      if((mod(ano,4).eq.0).and.(mod(ano,100).ne.0)) then
      write(*,*)'El ano es bisiesto'

      elseif((mod(ano,100).eq.0).and.(mod(ano,400).eq.0)) then
      write(*,*)'El ano es bisiesto'

      else
      write(*,*)'El ano no es bisiesto'

      endif



      end
