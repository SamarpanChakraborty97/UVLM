      subroutine stepb ( XP, TE, n, XJ )
c ......................................................................
c .                                                                    .
c .   P R O G R A M                                                    .
c .                                                                    .
c .       HAMMING'S FOURTH-ORDER PREDICTOR-CORRECTOR METHOD            .
c .                                                                    .
c .       STEP (B)                                                     .
c .                                                                    .
c ......................................................................

      integer :: n
      integer :: k

      double precision, dimension (n) :: XP
      double precision, dimension (n) :: TE
      double precision, dimension (n) :: XJ

      double precision :: F

      F = 112.0D+00 / 9.0D+00

      do k=1, n

         XJ(k) = XP(k) + F * TE(k)

      end do

      end subroutine stepb
