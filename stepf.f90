      subroutine stepf ( X1, TE, n, X2 )
c ......................................................................
c .                                                                    .
c .   P R O G R A M                                                    .
c .                                                                    .
c .       HAMMING'S FOURTH-ORDER PREDICTOR CORRECTOR METHOD            .
c .                                                                    .
c .       STEP (F)                                                     .
c .                                                                    .
c ......................................................................

      integer :: n
      integer :: k

      double precision, dimension (n) :: X1
      double precision, dimension (n) :: TE
      double precision, dimension (n) :: X2

      do k=1, n

         X2(k) = X1(k) - TE(K)

      end do

      end subroutine stepf
