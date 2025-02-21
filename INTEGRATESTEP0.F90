      subroutine INTEGRATESTEP0( GD , AV , AB, AW , SP)
      
!Version 1.01B               Bruno Roccia     20nov2017
      !Particion de archivos de TECPOLOT      
      
!Modificacion 1.02.01L       Luis Ceballos    27nov2017
      !agregado de LAPACK y paralelizacion OpenMP
      
      
      !modules
      
      use GENERALMODULE
      use AERODYNAMICMODULE
      use ELASTICMODULE
      use AUXVARIABLE
      
      !variables
      
      implicit none
      
      type (GeneralData), intent(INOUT)                       :: GD
      type (AuxiliarVariable), intent(INOUT)                  :: AV
      type (AEROBODY), dimension(GD%NAB), intent(INOUT)       :: AB
      type (WAKE), dimension(GD%NAW), intent(INOUT)           :: AW
      type (SPRING), dimension(GD%NSPRING), intent(INOUT)     :: SP
      
      integer, allocatable, dimension (:)                     :: INDX
      double precision                                        :: AUXLU
      double precision, allocatable, dimension(:,:)           :: AIM
      double precision, allocatable, dimension(:)             :: RHS
      integer                                              :: AUX1, AUX2
      double precision, allocatable, dimension(:)          :: DCP
      double precision, allocatable, dimension(:,:)        :: FN
      double precision                                     ::  CL
      double precision, dimension(2,1)                     :: FS
      double precision, dimension(5)                       :: ZJ, DZJ
      integer                                              :: b
      integer::info
      

      !program
      
      allocate(AIM(GD%DOP,GD%DOP))
      allocate(RHS(GD%DOP))
      allocate(INDX(GD%DOP))
      
      
      allocate(AV%XJ(5*GD%NAB))
      allocate(AV%DXJ(5*GD%NAB))
      allocate(AV%XJM1(5*GD%NAB))
      allocate(AV%DXJM1(5*GD%NAB))
      allocate(AV%XJM2(5*GD%NAB))
      allocate(AV%DXJM2(5*GD%NAB))
      allocate(AV%XJM3(5*GD%NAB))
      allocate(AV%DXJM3(5*GD%NAB))
      allocate(AV%XJM4(5*GD%NAB))
      allocate(AV%TE(5*GD%NAB))
      
      AV%XJ    = 0.00D+00
      AV%DXJ   = 0.00D+00
      AV%XJM1  = 0.00D+00
      AV%DXJM1 = 0.00D+00
      AV%XJM2  = 0.00D+00
      AV%DXJM2 = 0.00D+00
      AV%XJM3  = 0.00D+00
      AV%DXJM3 = 0.00D+00
      AV%XJM4  = 0.00D+00
      AV%TE    = 0.00D+00
      
! aerodynamic loads ..............................................!      
      
      do b = 1, GD%NAB

         call AEROMESH_UPDATE( AB(b)%VSCP  , AB(b)%XYNPN, AB(b)%XYNPB
     +         , AB(b)%XYCPB , SP(b)%q , SP(b)%dq , SP(b)%R0, AB(b)%NNP)
      
  
         call CONTROLPOINTS( AB(b)%XYCPN , AB(b)%nVN ,
     +                       AB(b)%tVN   , AB(b)%Ck  , 
     +                       AB(b)%XYNPN, AB(b)%LM, AB(b)%NCP)

         call VORTEXPOINTS(  AB(b)%XYVPN , AB(b)%XYNPN, 
     +                       AB(b)%LM    , AB(b)%NVP )
     
     
          AW(b)%XYN(:,1) = AB(b)%XYVPN(:,AB(b)%NVP)

      end do
      
      
      call AIMATRIX( AIM, AB , GD)
      
      call RHSVECTOR( RHS, AB , AW , GD)
         
      call dgesv(GD%DOP,1,AIM,GD%DOP,INDX,RHS,GD%DOP,info)
         
      AUX2 = 0
      AUX1 = 0
      do b = 1, GD%NAB
         AUX2 = AUX2 +AB(b)%NVP   
         AUX1 = AUX2 - AB(b)%NVP + 1
         AB(b)%BG  = RHS(AUX1 : AUX2)
         AB(b)%SUMBG = SUM(AB(b)%BG(1:AB(b)%NCP))
         AB(b)%BDG = AB(b)%BG
         AW(b)%GW(1) = AB(b)%BG(AB(b)%NVP)         
      end do
   
      do b = 1, GD%NAB
      
         allocate(DCP(AB(b)%NCP))
         allocate(FN(2,AB(b)%NCP))
      
         call AEROLOADS(DCP, FN, CL, AB, b, GD)
         AB(b)%DCP = DCP
         AB(b)%FN  = FN
         AB(b)%CL  = CL
         deallocate(DCP)
         deallocate(FN)
   
      end do
      
! aerodynamic loads to structural loads ......................................!
      
      do b = 1, GD%NAB
         
      call AL2SL( FS , AB(b), SP(b)%q )
         
      SP(b)%FS = FS
         
      end do
      
! ODE .......................................................................!
      do b = 1, GD%NSPRING
      
          ZJ(1:2) = SP(b)%q(1:2)
          ZJ(3:4) = SP(b)%dq(1:2)
          ZJ(5)   = SP(b)%Voltage
          
          call MYODE( DZJ , SP(b) , ZJ)
          
          AUX1 = 5*(b-1) + 1
          AUX2 = 5*b
          
          AV%XJ(AUX1:AUX2) =  ZJ(1:5)
          AV%DXJ(AUX1:AUX2) = DZJ(1:5)
          
      end do
      
             
      end subroutine INTEGRATESTEP0