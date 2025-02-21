      subroutine MYODE( DZ , SP , Z )

! Developed by Marcos L. Verstraete
! Date: 7.24.2017
! Grupo de Matemática Aplicada - FI-UNRC
      

!----------------------------------------------------------------------
      use ELASTICMODULE
      use GENERALMODULE
!----------------------------------------------------------------------
      
      implicit none
      
      type (SPRING), intent (IN)                            :: SP
      double precision, dimension(5), intent(IN)            :: Z
      double precision, dimension(5), intent(OUT)           :: DZ
      
      double precision                                :: MT, MW,AUXLU
      
      double precision, dimension(2,2)                     :: MM
      integer, dimension (2)                               :: INDX
      double precision,  dimension (2)                     :: RHS
      integer                                              :: info
      
!----------------------------------------------------------------------
      
      
      ! other version
      
      MT = SP%M + 2.842514014715750D+00
      MW = SP%M
      
      
      DZ(1) = Z(3)
      DZ(2) = Z(4)
      
      MM(1,1) =  MT
      MM(1,2) =  MW * SP%XA * SP%BB
      MM(2,1) =  MW * SP%XA * SP%BB
      MM(2,2) =  SP%IP
      
      RHS(1) = -SP%KHL   * Z(1)  - SP%DH * Z(3) + SP%FS(1,1) + 
     +          SP%Acoup * Z(5)  / SP%span 
     +
      RHS(2) = -SP%KAL   * Z(2) -  SP%DA * Z(4) + SP%FS(2,1)
      
      call dgesv( 2 , 1 , MM , 2 , INDX , RHS , 2 , info)
      
      DZ(3) = RHS(1)
      DZ(4) = RHS(2)
      
      DZ(5) = 1 / SP%Ceq * (-Z(5)/SP%Rl - SP%Acoup * Z(3))
      
      end subroutine MYODE