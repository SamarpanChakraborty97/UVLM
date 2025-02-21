      subroutine VORTEX2D(VEL , G , R0 , RP , l)


      implicit none

      double precision, intent(IN)                  :: G
      double precision, dimension(2), intent(IN)    :: R0, RP
      double precision, dimension(2), intent(OUT)   :: VEL

      double precision, dimension(2)                :: DR

      double precision            :: MOD_DR_2, COEF, l,MOD_DR

      double precision, parameter     :: PI  = .3141592653589793D+01 


      DR = RP - R0

      MOD_DR_2 = DR(1)**2.00D+00 + DR(2)**2.00D+00
      MOD_DR   = dsqrt(MOD_DR_2)
      
      
      ! VERSION 1 .......................................................
      
      if ( MOD_DR .LT. l ) then
      
      COEF = G / ( 2 * PI * (l**2.00D+00) )

      else
      
      COEF = G / (2 * PI * MOD_DR_2)
      
      end if
      
      VEL(1) = - COEF * DR(2) 
      VEL(2) =   COEF * DR(1) 
      
      ! VERSION 2 .......................................................
      
      !COEF = G / (2 * PI * (MOD_DR_2 + (l/(MOD_DR+1.00D-15))**2.00D+00 ))
      !VEL(1) = - COEF * DR(2) 
      !VEL(2) =   COEF * DR(1) 
      


      end subroutine VORTEX2D