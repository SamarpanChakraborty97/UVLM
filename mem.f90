      subroutine mem ( X0, DX0, DX1, Dt, n, X1 )

c ......................................................................
c .                                                                    .
c .   P R O G R A M                                                    .
c .                                                                    .
c .       MODIFIED EULER METHOD                                        .
c .                                                                    .
c ......................................................................


      integer :: n
      integer :: k

      double precision, dimension (n) :: X0
      double precision, dimension (n) :: X1
      double precision, dimension (n) :: DX0
      double precision, dimension (n) :: DX1

      double precision :: Dt

      double precision :: F

      F = 0.5D+00 * Dt

      do k=1, n

         X1(k) = X0(k) + F * ( DX1(k) + DX0(k) )

      end do

      end subroutine mem
