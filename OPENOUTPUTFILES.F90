      subroutine OPENOUTPUTFILES( suf , OFS , GD, NOF)
            
      use generalmodule
      
      implicit none
      
      type( GeneralData), intent(IN)                      :: GD
      integer, intent(IN)                                 :: NOF
      character(4), intent(IN)                            :: suf
      integer, dimension(NOF), intent (OUT)               :: OFS
      
      integer                                             :: i
      
      ! Body of program
      
      do i = 1, NOF
      
          OFS(i) = 200 + i
          
      end do
      
! File #1 ..............................................................!
      
      if (GD%FLAG_MATLAB .EQ. 1) then

          open( UNIT=OFs(1), FILE='WING_WAKE_'//suf//'.SAL',
     +          STATUS='UNKNOWN', FORM='FORMATTED' ,
     +          ACCESS='SEQUENTIAL')
      
      end if
     
! File #2 ..............................................................!

      open( UNIT= OFs(2), FILE='CIRCBODY_'//suf//'.SAL',STATUS='UNKNOWN'
     +    , FORM='FORMATTED' , ACCESS='SEQUENTIAL')
      
! File #3 ..............................................................!     

      open( UNIT= OFs(3),FILE='CIRCWAKE_'//suf//'.SAL',STATUS='UNKNOWN' 
     +   , FORM='FORMATTED' , ACCESS='SEQUENTIAL')
     
! File #4 ..............................................................!

! open( UNIT= OFs(4) , FILE='CP_'//suf//'.SAL' , STATUS='UNKNOWN' 
!+    , FORM='FORMATTED' , ACCESS='SEQUENTIAL')     

! File #5 ..............................................................!

      open( UNIT= OFs(5),FILE='AEROLOADS_'//suf//'.SAL',STATUS='UNKNOWN'
     +    , FORM='FORMATTED' , ACCESS='SEQUENTIAL') 
         
! File #6 ..............................................................!
 
      open( UNIT= OFs(6),FILE='DATAREPORT_'//suf//'.REP', 
     +      STATUS='UNKNOWN' , FORM='FORMATTED' , ACCESS='SEQUENTIAL') 
     
! File #7 ..............................................................!

      open( UNIT= OFs(7) , FILE='DOF_'//suf//'.SAL' , 
     +      STATUS='UNKNOWN' , FORM='FORMATTED' , ACCESS='SEQUENTIAL')
     
! File #8 .............................................................!

      open( UNIT= OFs(8) , FILE='RHSVECTOR_'//suf//'.REP' , 
     +      STATUS='UNKNOWN' , FORM='FORMATTED' , ACCESS='SEQUENTIAL')

! File #9 .............................................................!

      if (GD%FLAG_TEC .EQ. 1) then

          open( UNIT= OFs(9) , FILE='WING_WAKE_'//suf//'.TEC' , 
     +           STATUS='UNKNOWN' , FORM='FORMATTED' , 
     +           ACCESS='SEQUENTIAL')
      
      end if

! Write titles in output files...

            
      !                        W I N G - W A K E
            
      if (GD%FLAG_MATLAB .EQ. 1) then      
       
          write ( OFS(1) , 10 )
          write ( OFS(1) , 11 )
          write ( OFS(1) , 19 )
          write ( OFS(1) , 11 )
          write ( OFS(1) , 20 )
          write ( OFS(1) , 21 )
          write ( OFS(1) , 22 )
          write ( OFS(1) , 23 )
          write ( OFS(1) , 11 )
          write ( OFS(1) , 10 )
      
          write ( OFS(1) ,  * )
          write ( OFS(1) ,  * )'             S I M U L A T I O N: '
     +                        //SUF//''
          write ( OFS(1) ,  * )
      
      end if
          
      
      !                B O D Y   C I R C U L A T I O N S 
      write ( OFS(2) , 10 )
      write ( OFS(2) , 11 )
      write ( OFS(2) , 12 )
      write ( OFS(2) , 11 )
      write ( OFS(2) , 20 )
      write ( OFS(2) , 21 )
      write ( OFS(2) , 22 )
      write ( OFS(2) , 23 )
      write ( OFS(2) , 11 )
      write ( OFS(2) , 10 )
      
      write ( OFS(2) ,  * )
      write ( OFS(2) ,  * )'             S I M U L A T I O N: '//SUF//''
      write ( OFS(2) ,  * )
      
      !               W A K E   C I R C U L A T I O N S
      write ( OFS(3) , 10 )
      write ( OFS(3) , 11 )
      write ( OFS(3) , 13 )
      write ( OFS(3) , 11 )
      write ( OFS(3) , 20 )
      write ( OFS(3) , 21 )
      write ( OFS(3) , 22 )
      write ( OFS(3) , 23 )
      write ( OFS(3) , 11 )
      write ( OFS(3) , 10 )
      
      write ( OFS(3) ,  * )
      write ( OFS(3) ,  * )'             S I M U L A T I O N: '//SUF//''
      write ( OFS(3) ,  * )
!
!
      !                   A E R O D Y N A M I C   L O A D S
      write ( OFS(5) , 10 )
      write ( OFS(5) , 11 )
      write ( OFS(5) , 15 )
      write ( OFS(5) , 11 )
      write ( OFS(5) , 20 )
      write ( OFS(5) , 21 )
      write ( OFS(5) , 22 )
      write ( OFS(5) , 23 )
      write ( OFS(5) , 11 )
      write ( OFS(5) , 10 )
      
      write ( OFS(5) ,  * )
      write ( OFS(5) ,  * )'             S I M U L A T I O N: '//SUF//''
      write ( OFS(5) ,  * )
      
!      ! D E G R E E   OF   F R E E D O M      
      write ( OFS(7) , 10 )
      write ( OFS(7) , 11 )
      write ( OFS(7) , 17 )
      write ( OFS(7) , 11 )
      write ( OFS(7) , 20 )
      write ( OFS(7) , 21 )
      write ( OFS(7) , 22 )
      write ( OFS(7) , 23 )
      write ( OFS(7) , 11 )
      write ( OFS(7) , 10 )
      
      write ( OFS(7) ,  * )
      write ( OFS(7) ,  * )'             S I M U L A T I O N: '//SUF//''
      write ( OFS(7) ,  * )
      
!      ! OUTPUT FILE FOR TECPLOT
      
      if (GD%FLAG_TEC .EQ. 1) then
      
          write ( OFS(9), 50 )suf
          write ( OFS(9), 51 )
      
      
      end if
     
10    format('*************************************************')
11    format('*                                               *')

12    format('*     B O D Y   C I R C U L A T I O N S         *')
13    format('*     W A K E   C I R C U L A T I O N S         *')
15    format('*     A E R O D Y N A M I C   L O A D S         *')
17    format('*     D E G R E E S  OF   F R E E D O M         *')
19    format('*              W I N G and W A K E              *')

20    format('* Version      : UVLM2D 1.03M                   *')
21    format('* Developed by : Dr. M. Verstraete              *')
22    format('*                Dr. B. Roccia                  *')
23    format('*                Dr. L. Ceballos                *')
      
50    format('TITLE = "UVLM2D - HARVESTING ENERGY: RUN',A4,'"')
51    format('VARIABLES = "X", "Y"')      


      endsubroutine OPENOUTPUTFILES