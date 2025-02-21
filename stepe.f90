      subroutine stepe ( XJ, XP, n, TE )

c ......................................................................
c .                                                                    .
c .   P R O G R A M                                                    .
c .                                                                    .
c .       HAMMING'S FOURTH-ORDER PREDICTOR CORRECTOR METHOD            .
c .                                                                    .
c .       STEP (E)                                                     .
c .                                                                    .
c ......................................................................

      implicit none

      integer :: n
      integer :: k

      double precision, dimension (n) :: XJ
      double precision, dimension (n) :: XP
      double precision, dimension (n) :: TE

      double precision :: F

      F = 9.0D+00 / 121.00D+00

      do k=1, n

         TE(k) = F * ( XJ(k) - XP(k) )

      end do

      end subroutine stepe
