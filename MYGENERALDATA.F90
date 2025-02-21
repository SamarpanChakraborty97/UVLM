      subroutine MYGENERALDATA( GD , suf , idf )
      
! Developed by Marcos L. Verstraete
! Developed by Bruno A. Roccia
! Date: 7.10.2017
! Grupo de Matemática Aplicada - FI-UNRC 
      
      use generalmodule
      
      implicit none
      
!-----------------------------------------------------------------------!
!     VARIABLES DECLARATIONS      
!-----------------------------------------------------------------------!
      
      type(GeneralData)                            :: GD  
      integer                                      :: idf
      character(4)                                 :: suf
      character(50)                                :: FLAG
      double precision                             :: MODUinf

!-----------------------------------------------------------------------!
!                                                                       !      
!-----------------------------------------------------------------------!     
 
      read(idf, *)FLAG
      read(idf, *)FLAG
      read(idf, *)FLAG
      read(idf, *)FLAG
      
      
      read(idf, *)FLAG
      read(idf, *)FLAG
      read(idf, *) GD%LC,GD%VC,GD%NSTEP,GD%RHO, GD%Lref,GD%Uinf(1),
     + GD%Uinf(2), GD%Tol 
      
      read(idf, *)FLAG
      read(idf, *)FLAG
      read(idf, *) GD%CUTOFFB, GD%CUTOFFW, GD%NAB, GD%NAW, GD%NEB,
     + GD%NSPRING, GD%FLAG_TEC, GD%FLAG_MATLAB
           
      read(idf, *)FLAG
      read(idf, *)FLAG   
      
      allocate(GD%IDABC(GD%NAW))
      
      read(idf,*) GD%IDABC
      
!.....................................................................!
      
      GD%TC = GD%LC/GD%VC
	  
      GD%DT = GD%TC        
      
      ! unit vector drag and lift
      
      GD%eD  = 0.00D+00      
      GD%eL  = 0.00D+00
      
      MODUinf = dsqrt(GD%Uinf(1)**2+GD%Uinf(2)**2)
	  
      GD%eD  = GD%Uinf / MODUinf
	  
      GD%eL(1) =  GD%eD(2)
      GD%eL(2) = -GD%eD(1)
      
      
      
      read (idf, * ) FLAG
      
      end subroutine MYGENERALDATA