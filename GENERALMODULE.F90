      module GENERALMODULE
   
      type :: GENERALDATA

      integer                             :: NAB, NAW
      integer                             :: NEB, NSPRING
      integer                             :: NSTEP
      integer                             :: DOP
      integer                             :: FLAG_TEC, FLAG_MATLAB
      double precision                    :: DT, tol
      double precision                    :: VC, TC, LC
      double precision                    :: CL, CD, L,D
      integer,allocatable,dimension(:)    :: IDABC
      double precision                    :: RHO, Lref
      double precision, dimension(2)      :: Uinf, eD, eL
      
      double precision                    :: CUTOFFB
      double precision                    :: CUTOFFW
   
      end type GENERALDATA


      end module GENERALMODULE
