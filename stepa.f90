      subroutine stepa ( XJM4, DXJM1, DXJM2, DXJM3, Dt, n, XP )
c ......................................................................
c .                                                                    .
c .   P R O G R A M                                                    .
c .                                                                    .
c .       HAMMING'S FOURTH-ORDER PREDICTOR CORRECTOR METHOD            .
c .                                                                    .
c .       STEP (A)                                                     .
c .                                                                    .
c ......................................................................

      integer :: n
      integer :: k

      double precision, dimension (n) :: XJM4
      double precision, dimension (n) :: DXJM1
      double precision, dimension (n) :: DXJM2
      double precision, dimension (n) :: DXJM3
      double precision, dimension (n) :: XP(n)

      double precision :: Dt

      double precision :: F, AJM1, AJM3

      F = Dt * 4.0D+00 / 3.0D+00

      AJM1 =  2.0D+00
      AJM3 =  2.0D+00

      do k=1, n

        XP(k) = XJM4(k)
     +        + F * ( AJM1 * DXJM1(k) - DXJM2(k) + AJM3 * DXJM3(k) )

      end do

      end subroutine stepa
