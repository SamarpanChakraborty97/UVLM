
      subroutine em ( X0, DX0, Dt, n, X1 )

c ......................................................................
c .                                                                    .
c .   P R O G R A M                                                    .
c .                                                                    .
c .       EULER METHOD                                                 .
c .                                                                    .
c ......................................................................

      implicit none

      integer :: n
      integer :: k

      double precision, dimension (n) :: X0
      double precision, dimension (n) :: X1
      double precision, dimension (n) :: DX0

      double precision :: Dt

      do k=1, n

         X1(k) = X0(k) + Dt * DX0(k)

      end do

      end subroutine em
