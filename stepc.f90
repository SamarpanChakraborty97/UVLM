      subroutine stepc ( XJM1, XJM3, DXJM2, DXJM1, DXJ, Dt, n, XJ )
c ......................................................................
c .                                                                    .
c .   P R O G R A M                                                    .
c .                                                                    .
c .       HAMMING'S FOURTH-ORDER PREDICTOR-CORRECTOR METHOD            .
c .                                                                    .
c .       STEP (C)                                                     .
c .                                                                    .
c ......................................................................

      integer :: n
      integer :: k

      double precision, dimension (n) :: XJM1
      double precision, dimension (n) :: XJM3
      double precision, dimension (n) :: DXJM2
      double precision, dimension (n) :: DXJM1
      double precision, dimension (n) :: DXJ
      double precision, dimension (n) :: XJ

      double precision :: Dt

      double precision :: F1, F2, AJM1, BJM1

      F1 = 1.0D+00 / 8.0D+00
      F2 = 3.0D+00 * Dt

      AJM1 =  2.0D+00
      BJM1 =  9.0D+00

      do k=1, n

         XJ(k) = F1 * ( BJM1 * XJM1(k) - XJM3(k)
     +                  + F2 * ( DXJ(k) + AJM1 * DXJM1(k) - DXJM2(k) ) )

      end do

      end subroutine stepc
