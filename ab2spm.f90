      subroutine ab2spm ( X1, DX0, DX1, Dt, n, X2 )
c ......................................................................
c .                                                                    .
c .   P R O G R A M                                                    .
c .                                                                    .
c .       ADAMS-BASHFORTH TWO-STEP PREDICTOR METHOD                    .
c .                                                                    .
c ......................................................................

      implicit none

      integer :: n
      integer :: k

      double precision, dimension (n) :: X1
      double precision, dimension (n) :: X2
      double precision, dimension (n) :: DX0
      double precision, dimension (n) :: DX1

      double precision :: Dt

      double precision :: F
      double precision :: A1

      F = 0.5D+00 * Dt

      A1 = 3.0D+00

      do k=1, n

         X2(k) = X1(k) + F * ( A1 * DX1(k) - DX0(k) )

      end do

      end subroutine ab2spm
