      subroutine TECOPDIV (GD , MAXZONES, Zones, FileCount, suf, NOF,
     +                     OFS)

! Developed by Bruno A. Roccia
! Date: 09.06.2017
! Grupo de Matemática Aplicada - FI-UNRC

!                           DESCRIPTION                               !
!=====================================================================!
!                             MODULES                                 !
      use generalmodule
      
!=====================================================================!
!                        VARIABLE DECLARATION                         !      
      implicit none
      
      type(GeneralData),intent(IN)            :: GD
      integer, intent (IN)                    :: MAXZONES, NOF
      integer, intent (INOUT)                 :: FileCount, Zones
      
      integer, intent (INOUT), dimension(NOF) :: OFS
      
      character(LEN=4), intent(IN)            :: suf
      character(LEN=3)                        :: FILEMod 
      
!=====================================================================!      
            
      if (GD%FLAG_TEC .EQ. 1 .AND. Zones > MAXZONES) then
          
          close (OFS(9))              
          FileCount = FileCount + 1             
          write (FILEMod, "(I3)") FileCount
              
          open( UNIT= OFS(9) , 
     +           FILE='WING_WAKE_'//suf//'_'//trim(FILEMod)//'.TEC', 
     +           STATUS='UNKNOWN' , FORM='FORMATTED' , 
     +           ACCESS='SEQUENTIAL')
              
          write ( OFS(9), 50 )suf
          write ( OFS(9), 51 )
          
          Zones = 0
          
       end if      

!-------------------------------------------------------------------      
      
50    format('TITLE = "UVLM2D - HARVESTING ENERGY: RUN',A4,'"')
51    format('VARIABLES = "X", "Y"')          
      

      end subroutine TECOPDIV