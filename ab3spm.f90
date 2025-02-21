      subroutine ab3spm ( X2, DX0, DX1, DX2, Dt, n, X3 )
c ......................................................................
c .                                                                    .
c .   P R O G R A M                                                    .
c .                                                                    .
c .       ADAMS-BASHFORTH THREE-STEP PREDICTOR METHOD                  .
c .                                                                    .
c ......................................................................

      implicit none

      integer :: n
      integer :: k

      double precision, dimension (n) :: X2
      double precision, dimension (n) :: X3
      double precision, dimension (n) :: DX0
      double precision, dimension (n) :: DX1
      double precision, dimension (n) :: DX2

      double precision :: Dt

      double precision :: F

      double precision :: A0
      double precision :: A1
      double precision :: A2

      F = Dt / 12.0D+00

      A2 = 23.0D+00
      A1 = 16.0D+00
      A0 =  5.0D+00

      do k=1, n

         X3(k) = X2(k)
     +         + F * ( A2 * DX2(k) - A1 * DX1(k) + A0 * DX0(k) )

      end do

      end subroutine ab3spm
