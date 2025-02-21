      subroutine am2sm ( X1, DX0, DX1, DX2, Dt, n, X2 )
c ......................................................................
c .                                                                    .
c .   P R O G R A M                                                    .
c .                                                                    .
c .       ADAMS-MOULTON TWO-STEP METHOD                                .
c .                                                                    .
c ......................................................................

      implicit none

      integer :: n
      integer :: k

      double precision, dimension (n) :: X1
      double precision, dimension (n) :: X2
      double precision, dimension (n) :: DX0
      double precision, dimension (n) :: DX1
      double precision, dimension (n) :: DX2

      double precision :: Dt

      double precision :: F, A1, A2

      F = Dt / 12.0D+00

      A2 = 5.0D+00
      A1 = 8.0D+00

      do k=1, n

         X2(k) = X1(k) + F * ( A2 * DX2(k) + A1 * DX1(k) - DX0(k) )

      end do

      end subroutine am2sm
