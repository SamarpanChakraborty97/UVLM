      module AUXVARIABLE
      
      implicit none
      
      type :: auxiliarVariable
      
      doubleprecision, allocatable,dimension(:)   :: XJ, XJM1,XJM2 
      doubleprecision, allocatable,dimension(:)   :: XJM3, XJM4
      doubleprecision, allocatable,dimension(:)   :: DXJ , DXJM1
      doubleprecision, allocatable,dimension(:)   :: DXJM2, DXJM3
      doubleprecision, allocatable,dimension(:)   :: TE
      
      end type
      
      end module AUXVARIABLE 