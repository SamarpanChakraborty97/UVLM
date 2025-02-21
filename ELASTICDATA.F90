      subroutine ELASTICDATA ( SP , GD , idf )

! Developed by Marcos L. Verstraete
! Date: 8.4.2017
! Grupo de Matemática Aplicada - FI-UNRC


!---------------------------------------------------------------------!
!                            USED MODULES                             !
!---------------------------------------------------------------------!
   
      use generalmodule
      use aerodynamicmodule
      use ELASTICMODULE
 
!---------------------------------------------------------------------!
!                       VARIABLE DECLARATION                          !
!---------------------------------------------------------------------!
      
      implicit none
      
      integer                                             :: idf
            
      integer                                             :: i, j, k
            
      type (GeneralData)                                  :: GD
      
      type (SPRING), dimension(GD%NSPRING)                :: SP
      
      character (50)                                      :: AUX
	  
      double precision, allocatable, dimension(:,:)       :: OUT1, OUT2
      double precision, allocatable, dimension(:,:)       :: OUT3, OUT5
      double precision, allocatable, dimension(:)         :: OUT4
      

      
!---------------------------------------------------------------------!
!                           BODY OF PROGRAM                           !
!---------------------------------------------------------------------!
       

! ***************************** READ ******************************** !

      read (idf, * ) AUX
      read (idf, * ) AUX
      
      do i = 1, GD%NSPRING ! Start to read Body's paremeters 
      
      read (idf, * ) AUX
      read (idf, * ) AUX
        
! Read spring coefficients
      
      read (idf, * ) SP(i)%KHL, SP(i)%KHC, SP(i)%KAL, SP(i)%KAC 
                          
      read (idf, * ) AUX
      
! Read mass
      
      read (idf, * ) SP(i)%M, SP(i)%IP, SP(i)%R0(1), SP(i)%R0(2)
      
      read (idf, * ) AUX
      
! Read unablance parameter and semi-chord
      
      read (idf, * )  SP(i)%BB, SP(i)%XA, SP(i)%DH, SP(i)%DA 
      
      read (idf, * ) AUX
      
! Read electric circuit parameters
      
      read (idf, * )  SP(i)%Ceq,  SP(i)%Rl, SP(i)%Acoup, SP(i)%Span 
      
      read (idf, * ) AUX

! Read spring coefficients
      
      read (idf, * ) SP(i)%q(1), SP(i)%dq(1), SP(i)%q(2), SP(i)%dq(2),
     +               SP(i)%Voltage
      
! ********************** ALLOCATE BODY ************************** !
      
      end do
      
      end subroutine ELASTICDATA
      