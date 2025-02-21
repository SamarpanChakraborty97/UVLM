      subroutine WAKEINFLUENCE (VW , RP , AW , GD)
      
! Developed by Marcos L. Verstraete
! Date: 7.10.2017
! Grupo de Matemática Aplicada - FI-UNRC

!----------------------------------------------------------------------

      use aerodynamicmodule
      use generalmodule

!----------------------------------------------------------------------
      
      implicit none

      type (GeneralData)                                      :: GD
      
      type (WAKE), dimension(GD%NAW), intent(IN)             :: AW
     
      double precision, dimension (2),  intent (IN)          :: RP
	  
      double precision, dimension (2),  intent (OUT)         :: VW
      
      integer                                                :: i, j
	  
      double precision                                       :: l
     	  
      double precision, dimension (2)                        :: VEL
      
!----------------------------------------------------------------------
      
	  l  = GD%CUTOFFW! * GD%LC
	  
	  VW = 0.00D+00
	  
	  VEL = 0.00D+00

      do i = 1, GD%NAW
             
         do j = 2, AW(i)%NVP + 1
		 
		 
             call VORTEX2D(VEL , AW(i)%GW(j) ,AW(i)%XYAUX(:,j) , RP , l)
             
             VW = VW + VEL
			          
		 end do
      
      end do 
	  
      end subroutine WAKEINFLUENCE