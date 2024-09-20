C Este programa clasifica triángulos dados 3 valores reales, según sus
C lados.
      program clasificador_triangulos
      real l1,l2,l3
      logical TRIANG, EQUIL, ISOS, SCAL
      write(*,*) 'Ingrese tres números'
      read(*,*) l1,l2,l3
      !Es un triang si (l1+l2>l3) ó (l1+l3>l2) ó (l2+l3>l1)
      TRIANG = (l1+l2.gt.l3).and.(l1+l3.gt.l2).and.(l2+l3.gt.l1)
C Es equil si es triang Y (l1=l2=l3)
      EQUIL = TRIANG.and.((l1.eq.l2).and.(l2.eq.l3))
C Es isoseles si es triang y (l1=l2!=l3) ó (l1=l3!=l2) ó (l2=l3!=l1)
      ISOS = TRIANG.and.(((l1.eq.l2).and.(l1.ne.l3)).or.((l2.eq.l3).and.
     &(l2.ne.l1)).or.((l1.eq.l3).and.(l1.ne.l2)))

C Es escaleno si es triang y (l1!=l2!=l3)
      SCAL = TRIANG.and.((l1.ne.l2).and.(l1.ne.l3).and.(l2.ne.l3))

C Imprimo los valores de verdad:
      write(*,*)'TRIANG ES: ', TRIANG
      write(*,*)'EQUIL ES: ',EQUIL
      write(*,*)'ISOS ES: ',ISOS
      write(*,*)'SCAL ES: ',SCAL

      end
