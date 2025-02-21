      subroutine AerodynamicData ( AB , AW , GD , idf )

! Developed by Marcos L. Verstraete
! Date: 7.10.2017
! Grupo de Matemática Aplicada - FI-UNRC


!---------------------------------------------------------------------!
!                            USED MODULES                             !
!---------------------------------------------------------------------!
   
      use generalmodule
      use aerodynamicmodule
 
!---------------------------------------------------------------------!
!                       VARIABLE DECLARATION                          !
!---------------------------------------------------------------------!
      
      implicit none
      
      integer                                             :: idf
            
      integer                                             :: i, j, k
            
      type (GeneralData)                                  :: GD
      
      type (AeroBody), dimension(GD%NAB)                  :: AB
        
      type (Wake), dimension(GD%NAW)                      :: AW
      
      character (50)                                      :: AUX
	  
      double precision, allocatable, dimension(:,:)  :: OUT1, OUT2
      double precision, allocatable, dimension(:,:)  :: OUT3, OUT5
      double precision, allocatable, dimension(:)    :: OUT4
      

      
!---------------------------------------------------------------------!
!                           BODY OF PROGRAM                           !
!---------------------------------------------------------------------!
       

! ***************************** READ ******************************** !

      read (idf, * ) AUX
          
      do i = 1, GD%NAB ! Start to read Body's paremeters 
      
      read (idf, * ) AUX
      read (idf, * ) AUX
        
! Read nodes of Body i
      
      read (idf, * ) AB(i)%NP 
      
      AB(i)%NNP = AB(i)%NP + 1
      AB(i)%NVP = AB(i)%NP + 1
      AB(i)%NCP = AB(i)%NP
                    
      read (idf, * ) AUX
        
! ********************** ALLOCATE BODY ************************** !
  
      allocate( AB(i)%BG(AB(i)%NVP) )
      allocate( AB(i)%BDG(AB(i)%NVP) )

      allocate(AB(i)%XYNPB( 2 , AB(i)%NNP ) )
      allocate(AB(i)%XYNPN( 2 , AB(i)%NNP ) )
     
      allocate(AB(i)%XYVPB( 2 , AB(i)%NVP ) )
      allocate(AB(i)%XYVPN( 2 , AB(i)%NVP ) )
     
      allocate(AB(i)%XYCPB( 2 , AB(i)%NCP ) )
      allocate(AB(i)%XYCPN( 2 , AB(i)%NCP ) )
	 
      allocate(AB(i)%nVB( 2 , AB(i)%NCP ) )
      allocate(AB(i)%nVN( 2 , AB(i)%NCP ) )

      allocate(AB(i)%tVB( 2 , AB(i)%NCP ) )
      allocate(AB(i)%tVN( 2 , AB(i)%NCP ) )
     
      allocate(AB(i)%LM( 2 , AB(i)%NP ))
      allocate(AB(i)%CK( AB(i)%NP ))
	  
      allocate(AB(i)%VSCP( 2 , AB(i)%NCP ) )
      allocate(AB(i)%VWCP( 2 , AB(i)%NCP ) )
      
      allocate( AB(i)%DCP(AB(i)%NCP) )
      allocate( AB(i)%FN(2,AB(i)%NCP) )
	  
! Initialize Variables
      AB(i)%SUMBG = 0.00D+00
	  AB(i)%BG    = 0.00D+00
	  AB(i)%BDG   = 0.00D+00
	  
        
! Read Nodal Coordinates of Body i
		! coordinates
      do j = 1, AB(i)%NNP
        
          read(idf,*) AB(i)%XYNPB(1,j), AB(i)%XYNPB(2,j)
        
      end do
     
      AB(i)%XYNPB = AB(i)%XYNPB     
     
! connectivities
     
      read (idf, * ) AUX
     
      do j = 1, AB(i)%NP
     
         read(idf,*) AB(i)%LM(1,j), AB(i)%LM(2,j) 
     
      end do
     
      end do
 
! ************************* GENERATE ******************************* ! 
! Comments
! OUT1: Control point positions
! OUT2: Normal Vector
! OUT3: Tangential Vector
! OUT4: Local Chord 

      do i = 1, GD%NAB
	 
	 allocate(OUT1(2,AB(i)%NCP))
	 allocate(OUT2(2,AB(i)%NCP))
	 allocate(OUT3(2,AB(i)%NCP))
	 allocate(OUT4(AB(i)%NCP))
	 allocate(OUT5(2,AB(i)%NVP))
  
      call CONTROLPOINTS( OUT1,OUT2,OUT3, OUT4, 
     + AB(i)%XYNPB, AB(i)%LM, AB(i)%NCP)
	 
	 call VORTEXPOINTS(  OUT5, AB(i)%XYNPB, AB(i)%LM , AB(i)%NVP )
	 
	   AB(i)%XYCPB = OUT1
	   AB(i)%nVB   = OUT2
	   AB(i)%tVB   = OUT3
	   AB(i)%Ck    = OUT4
	   AB(i)%XYVPB = OUT5
	   
	   deallocate(OUT1)
	   deallocate(OUT2)
	   deallocate(OUT3)
	   deallocate(OUT4)
	   deallocate(OUT5)
	 
      end do
 


! ********************** ALLOCATE WAKE ************************** !
      	  


	  do i = 1, GD%NAW
        
        read (idf, * ) AUX
        read(idf,*) AW(i)%LIMNVP
      
	    allocate( AW(i)%GW(AW(i)%LIMNVP+1) )
	    allocate( AW(i)%XYN(2,AW(i)%LIMNVP+1) )
          allocate( AW(i)%XYAUX(2,AW(i)%LIMNVP+1) )
		
         AW(i)%GW  = 0.00D+00
		 
         AW(i)%XYN = 0.00D+00
         AW(i)%XYAUX = 0.00D+00
      
         AW(i)%NVP = 1
		
	  end do
 
      
      end subroutine AerodynamicData
      