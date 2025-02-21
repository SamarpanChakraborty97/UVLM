      subroutine am3sm ( X2, DX0, DX1, DX2, DX3, Dt, n, X3 )
c ......................................................................
c .                                                                    .
c .   P R O G R A M                                                    .
c .                                                                    .
c .       ADAMS-MOULTON THREE-STEP METHOD                              .
c .                                                                    .
c ......................................................................


      integer :: n
      integer :: k

      double precision, dimension (n) :: X2
      double precision, dimension (n) :: X3
      double precision, dimension (n) :: DX0
      double precision, dimension (n) :: DX1
      double precision, dimension (n) :: DX2
      double precision, dimension (n) :: DX3

      double precision :: Dt

      double precision :: F, A1, A2, A3

      F = Dt / 24.0D+00

      A3 =  9.0D+00
      A2 = 19.0D+00
      A1 =  5.0D+00

      do k=1, n

        X3(k) = X2(k)
     +        + F * ( A3 * DX3(k) + A2 * DX2(k) - A1 * DX1(k) + DX0(k) )

      end do

      end subroutine am3sm
