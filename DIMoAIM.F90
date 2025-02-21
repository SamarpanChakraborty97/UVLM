      subroutine DIMoAIM ( N , AB , NAB)

!----------------------------------------------------------------------

      use aerodynamicmodule

!----------------------------------------------------------------------
      
      implicit none
      
      integer, intent(IN)                                      :: NAB
      
      type(AEROBODY),dimension(NAB), intent (IN)               :: AB

      integer                                                  :: i
      
      integer, intent(OUT)                                     :: N
      
!----------------------------------------------------------------------

      N = 0
      
      do  i = 1, NAB
      
         N = N + AB(i)%NVP
      
      end do
                             
      
      end subroutine DIMoAIM