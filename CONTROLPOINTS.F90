      subroutine CONTROLPOINTS ( OUT1, OUT2, OUT3, OUT4, 
     +                             XY,   LM, NCP)

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
            
      integer, intent(IN)                                    :: NCP
      
      double precision, dimension(2,NCP+1), intent(IN)       :: XY
      
      integer, dimension(2,NCP), intent(IN)                  :: LM	  
      
      integer                                              :: i, j, k

      double precision, dimension(2)                       :: ri, rj, dr
      
      double precision, dimension(2,NCP), intent(OUT) :: OUT1,OUT2,OUT3
      
      double precision, dimension(NCP), intent(OUT)   :: OUT4
      
      
!---------------------------------------------------------------------!
!                           BODY OF PROGRAM                           !
!---------------------------------------------------------------------!
   
! Comments
! The variables OUTi i=1,..,5 are output variables
! OUT1: Control point positions
! OUT2: Normal Vector
! OUT3: Tangential Vector
! OUT4: Local Chord
  
      do k = 1, NCP
   
         i = LM(1,k)
         j = LM(2,k)
   
         ri = XY(:,i)
         rj = XY(:,j)	 
	   dr = rj - ri
     
         OUT1(:,k) = 0.25D+00 * ri + 0.75D+00 * rj
     
         OUT4(k) = (dr(1)**2 + dr(2)**2)**0.5D+00 
     
         OUT3(:,k) = 1/OUT4(k) * dr

	   OUT2(1,k) =  OUT3(2,k)
         OUT2(2,k) = -OUT3(1,k)
   
      end do
   
     
      end subroutine CONTROLPOINTS