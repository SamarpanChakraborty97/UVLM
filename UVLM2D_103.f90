!  UVLM2D_101B.f90 

      program UVLM2D_103

! Modules
      
      use AERODYNAMICMODULE
      use ELASTICMODULE
      use GENERALMODULE
      use AUXVARIABLE
    
    
      implicit none

! Variables
    
      integer, parameter                              :: NIF = 1
      integer, parameter                              :: NOF = 15
      integer, dimension(NIF)                         :: IFS   
      integer, dimension(NOF)                         :: OFS
      character(4)                                    :: suf

      type(GENERALDATA)                               :: GD
      type (auxiliarVariable)                         :: AV
      type(AEROBODY), allocatable, dimension(:)       :: AB
      type(SPRING), allocatable, dimension(:)         :: SP
      type(WAKE), allocatable, dimension(:)           :: AW 
      
      
      double precision, allocatable, dimension(:)     :: RHS

      integer                                         :: IT, b, Zones
      integer                                         :: FileCount, kont
      
      INTEGER, PARAMETER                              :: MAXZONES=10000  !Maximum number of zones allowed by TECPLOT

      integer, dimension ( 8 ) :: dt                !medir tiempo de ejecucion
      integer                  :: IOtimeMes, ti, tf !medir tiempo de ejecucion

!-------------------------------------------------------------------------
      !comenzar la medicion de tiempo
      call date_and_time ( values=dt )
      ti = ( dt(5) * 3600 + dt(6) * 60 + dt(7) ) * 1000 + dt(8)
!-------------------------------------------------------------------------
      
!----------------------------------------------------------------------

      write (*,*) ''
      write (*,*) '----------------------------------------------------'
      write (*,*) '-             UVLM2D Version 1.01B                 -'
      write (*,*) '-     Aeroelastic Code for 2-D simulations         -'
      write (*,*) '-      August 29, 2017 (UNRC-UNC-CONICET)          -'
      
      write (*,*) '----------------------------------------------------'
      write (*,*) ''
      write (*,*) 'Developed by: Dr. Ing. Marcos Verstraete'
      write (*,*) '              Dr. Ing. Bruno Roccia'
      write (*,*) '              Dr. Ing. Luis Ceballos'
      write (*,*) '              Dr. Ing. Sergio Preidikman'
      write (*,*) ''
      write (*,*) 'Contact: mverstraete@ing.unrc.edu.ar'
      write (*,*) ''
     
!----------------------------------------------------------------------      
      

!*************************************************************************!
!                                                                         !
!                           PRE-PROCESSOR                                 !
!                                                                         !
!*************************************************************************!

      call get_command_argument( 1, suf )

      !suf = 'ER04'
      !suf = 'A014'
!-------------------------------------------------------------------------
      !archivo para almacenar el tiempo consumido en la ejecucion
      
      IOtimeMes = 1
      
      open ( UNIT      = IOtimeMes
     +     , FILE      = suf//'_TIME.AUX'
     +     , STATUS    = 'UNKNOWN'
     +     , FORM      = 'FORMATTED'
     +     , ACCESS    = 'SEQUENTIAL' ) 
!-------------------------------------------------------------------------   
      
! OPEN INPUT FILES...
           
      write (*,*)' ~~~~~~~~     P R E - P R O C E S S O R     ~~~~~~~~'
      
      write (*,*)
      
      write (*,*) 'Opening Input and Output Files ... '
    
      call OPENINPUTFILES(  suf  , IFS  , NIF )
      
! General Data...
      
      write (*,*)

      write (*,*) 'Reading General Data ... '
      
      call MYGENERALDATA( GD , suf , IFS(1) )
      
      call OPENOUTPUTFILES( suf  , OFS  , GD, NOF)      

! Aerodynamic Data...

      write (*,*)
      
      write (*,*) 'Generating Aerodynamic Data ... '
      
      allocate(AB(GD%NAB))
      allocate(AW(GD%NAW))

      call AERODYNAMICDATA( AB , AW , GD , IFS(1) )
      
! Elastic Data...
      
      write (*,*)
      
      write (*,*) 'Generating Structural Data ... '
      
      allocate( SP(GD%NSPRING) )

      call ELASTICDATA( SP , GD , IFS(1) )

! Data Report....

      write (*,*) 'Writing Data Report ... '

      call WRITEDATA(GD , AB , AW , SP , suf , OFS(6))

      call DIMoAIM( GD%DOP , AB , GD%NAB)
            
!*************************************************************************!
!                                                                         !
!                              PROCESSOR                                  !
!                                                                         !
!*************************************************************************!
      
      write (*,*)
      write (*,*)' ~~~~~~~~    P R O C E S S O R     ~~~~~~~~ '
      write (*,*)
            
!'*************************************************************************'      
      IT = 0  ! integration at t = 0 (Initial conditions)
      write (*,*)'TIME-STEP', IT
      write (*,*)''
!'*************************************************************************'
      
      call INTEGRATESTEP0(GD,AV, AB, AW, SP)
      
      call OUTPUT(SP, AB,AW,GD,IT,OFS)
      
!'*************************************************************************'      
      IT = 1  ! integration at t = DT
      write (*,*)'TIME-STEP', IT
!'*************************************************************************'      
      
      call CONVECT(AB , AW , GD)
         
      do b = 1, GD%NAW
      
          AW(b)%NVP = AW(b)%NVP + 1
          
      end do     
      
      call INTEGRATESTEP1(GD,AV, AB, AW, SP)
      
      call OUTPUT(SP,AB,AW,GD,IT,OFS)      
      
!'*************************************************************************'
      IT = 2  ! integration at t = 2DT
      write (*,*)'TIME-STEP', IT
!'*************************************************************************'
      
      call CONVECT(AB , AW , GD)
         
      do b = 1, GD%NAW
      
          AW(b)%NVP = AW(b)%NVP + 1
          
      end do
      
      call INTEGRATESTEP2(GD,AV, AB, AW, SP)
      
      call OUTPUT(SP,AB,AW,GD,IT,OFS) 

!'*************************************************************************'      
      IT = 3  ! integration at t = 3DT
      write (*,*)'TIME-STEP', IT
!'*************************************************************************'
      
      call CONVECT(AB , AW , GD)
         
      do b = 1, GD%NAW
      
          AW(b)%NVP = AW(b)%NVP + 1
          
      end do
      
      call INTEGRATESTEP3(GD,AV, AB, AW, SP)
      
      call OUTPUT(SP,AB,AW,GD,IT,OFS)
            
      
!'*************************************************************************'
      Zones = 8
      FileCount = 0
      kont = 0
      do IT = 4, GD%NSTEP

          write (*,*)'TIME-STEP', IT
!'*************************************************************************'
      
          call CONVECT(AB , AW , GD)
         
          do b = 1, GD%NAW
      
              if (AW(b)%NVP.LT. AW(b)%LIMNVP) then
          
                  AW(b)%NVP = AW(b)%NVP + 1
              
              end if
          
          end do
      
0          call INTEGRATESTEPJ(GD,AV, AB, AW, SP)
      
          ! 
          IF (kont .eq. 10) THEN
          call OUTPUT(SP,AB,AW,GD,IT,OFS)
          kont = 0
          ENDIF
          kont = kont+1 
          
          Zones = Zones + 2
          
          call TECOPDIV (GD, MAXZONES, Zones, FileCount, suf, NOF, OFS)
          
      end do
      
!-------------------------------------------------------------------------
      !finalizar con la medicion de tiempo
      call date_and_time ( values=dt )
      tf = ( dt(5) * 3600 + dt(6) * 60 + dt(7) ) * 1000 + dt(8)
      print *, '     ELAPSED TIME = ', tf-ti !imprimir en pantalla el tiempo consumido
      write(IOtimeMes,*)'     ELAPSED TIME = ', tf-ti !imprimir en archivo el tiempo consumido
      close(IOtimeMes)
!-------------------------------------------------------------------------        
      end program UVLM2D_103

