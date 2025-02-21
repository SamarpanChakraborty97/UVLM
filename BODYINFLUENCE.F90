      subroutine BODYINFLUENCE (VB , RP , AB , GD)
      
! Developed by Marcos L. Verstraete
! Date: 7.10.2017
! Grupo de Matemática Aplicada - FI-UNRC

!----------------------------------------------------------------------

      use aerodynamicmodule
      use generalmodule

!----------------------------------------------------------------------
      
      implicit none

      type (GeneralData)                                      :: GD
      
      type (AEROBODY), dimension(GD%NAB), intent(IN)             :: AB
     
      double precision, dimension (2),  intent (IN)          :: RP
	  
      double precision, dimension (2),  intent (OUT)         :: VB
      
      integer                                                :: i, j
	  
      double precision                                       :: l
     	  
      double precision, dimension (2)                        :: VEL
      
!----------------------------------------------------------------------
      
	  l  = GD%CUTOFFB! * GD%LC
	  
	  VB = 0.00D+00
	  
	  VEL = 0.00D+00

      do i = 1, GD%NAB
             
         do j = 1 , AB(i)%NVP
		 
		 
             call VORTEX2D(VEL , AB(i)%BG(j) , AB(i)%XYVPN(:,j) , RP ,l)
             
             VB = VB + VEL
			          
		 end do
      
      end do 
	  
      end subroutine BODYINFLUENCE