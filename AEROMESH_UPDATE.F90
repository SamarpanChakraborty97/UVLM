      SUBROUTINE AEROMESH_UPDATE(VSCP,XYNPN,XYNPB,XYCPB,q,dq,R0,n)
      
      
      implicit none
      
      integer, intent(IN)                                  :: n
      double precision, dimension(2), intent(IN)           :: q , dq, R0
      double precision, dimension(2,n), intent(IN)           :: XYNPB
      double precision, dimension(2,n-1), intent(IN)         :: XYCPB
      double precision, dimension(2,n), intent(OUT)          :: XYNPN
      double precision, dimension(2,n-1), intent(OUT)        :: VSCP
      
      double precision, dimension(2,1)                     :: b1,b2,R,DR
      double precision, dimension(2,1)                     :: aux1
      double precision, dimension(2,2)                     :: TNB
      integer                                              :: i
      
      
      
      
      b1(1,1) = 1.00D+00
      b1(2,1) = 0.00D+00
      
      b2(1,1) = 0.00D+00
      b2(2,1) = 1.00D+00
      
      TNB(1,1) = dcos(q(2)) 
      TNB(2,1) = dsin(q(2))
      TNB(1,2) =-dsin(q(2))
      TNB(2,2) = dcos(q(2))
      
      R(1,1) = R0(1) + 0.00D+00
      R(2,1) = R0(2) + (- q(1) )
      
      DR(1,1) = 0.00D+00
      DR(2,1) = (- dq(1))

      
      
      do i = 1, n
      
      XYNPN(:,i) = R(:,1) + MATMUL(TNB,XYNPB(:,i))
      
      end do
      
      do i = 1, n-1
      
      VSCP(:,i) = DR(:,1) + dq(2)*(XYCPB(1,i)*MATMUL(TNB,b2(:,1)) - 
     +                        XYCPB(2,i)*MATMUL(TNB,b1(:,1)))
      
      end do
      
      
      END SUBROUTINE AEROMESH_UPDATE