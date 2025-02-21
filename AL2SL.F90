      subroutine AL2SL( FS , AB , q )

! Developed by Marcos L. Verstraete
! Date: 7.24.2017
! Grupo de Matemática Aplicada - FI-UNRC
      

!----------------------------------------------------------------------
      use AERODYNAMICMODULE
      use GENERALMODULE
!----------------------------------------------------------------------
      
      implicit none
      
      type (AEROBODY), intent (IN)                          :: AB
      double precision, dimension(2), intent(IN)            :: q
      integer                                               :: i
      
      double precision, dimension(2,1), intent(OUT)        :: FS 
      
      double precision, dimension(2,AB%NCP)                :: FN
      double precision, dimension(2,AB%NVP)                :: XY
      
      double precision, dimension(2,2)                     :: A
      
      
!----------------------------------------------------------------------
      
      XY = AB%XYVPB
      FN = AB%FN
      
      FS = 0.00D+00
      
      do i = 1, AB%NCP
      
      A(1,1) = 0.00D+00
      A(2,1) = -XY(1,i)*dsin(q(2))
      A(2,2) =  XY(1,i)*dcos(q(2))
      A(1,2) = 1.00D+00
      
      FS(:,1) = FS(:,1) + MATMUL(A,FN(:,i))
      
      end do

	FS(1,1) = - FS(1,1)
      
      end subroutine AL2SL