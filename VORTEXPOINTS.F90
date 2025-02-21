      subroutine VORTEXPOINTS ( OUT1, XY, LM, NVP)

! Developed by Marcos L. Verstraete
! Date: 7.10.2017
! Grupo de Matemática Aplicada - FI-UNRC
    
!---------------------------------------------------------------------!
!                            USED MODULES                             !
!---------------------------------------------------------------------!
   
      use generalmodule
      use aerodynamicmodule
 
!---------------------------------------------------------------------!
!                       VARIABLE DECLARATION                          !
!---------------------------------------------------------------------!
      
      implicit none
            
      integer, intent(IN)                                  :: NVP
      
      double precision, dimension(2,NVP),intent(IN)         :: XY
      
      integer, dimension(2,NVP-1),intent(IN)                :: LM	  
      
      integer                                              :: i, j, k

      double precision, dimension(2)                       :: ri, rj, dr
      
      double precision, dimension(2,NVP), intent(OUT)      :: OUT1
      
!---------------------------------------------------------------------!
!                           BODY OF PROGRAM                           !
!---------------------------------------------------------------------!
   
! Comments
! The variables OUT1 is the output variable
! OUT1: vortex point positions

  
      do k = 1, NVP-1
   
         i = LM(1,k)
         j = LM(2,k)
   
         ri = XY(:,i)
         rj = XY(:,j)	 
	   dr = rj - ri
     
         OUT1(:,k) = 0.75D+00 * ri + 0.25D+00 * rj
   
      end do
   
        OUT1(:,NVP) = -0.25D+00 * ri + 1.25D+00 * rj 
	 
	 
      end subroutine VORTEXPOINTS