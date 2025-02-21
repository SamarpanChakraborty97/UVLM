      subroutine AIMATRIX (AIM , AB , GD)

! Developed by Marcos L. Verstraete
! Date: 7.14.2017
! Grupo de Matemática Aplicada - FI-UNRC
      
!     THESE RUTINE CALCULATE THE AERODYNAMIC INFLUENCE MATRIX

!----------------------------------------------------------------------

      use aerodynamicmodule
      use generalmodule
!----------------------------------------------------------------------
      
      implicit none
      
      type (GeneralData)                                   :: GD
       
      type (AEROBODY), dimension(GD%NAB),intent (IN)        :: AB
           
      double precision, dimension (GD%DOP ,GD%DOP),  intent (OUT) :: AIM
      
      integer                                              :: i, j
        
      integer                                  :: AUX1, AUX2, AUX3, AUX4
      
      double precision, allocatable, dimension (:, :)        :: A
      
!----------------------------------------------------------------------

      AUX1 = 0
      AUX2 = 0
             
      do i = 1, GD%NAB
      
             AUX2 = AUX2 + AB(i)%NVP
             AUX1 = AUX2 - AB(i)%NVP
             AUX3 = 0
             AUX4 = 0
             
         do j = 1, GD%NAB
         
             allocate( A ( AB(i)%NVP , AB(j)%NVP ) )
             
             A = 0.00D+00
         
             call MATRIXOFBB( A , AB(i) , AB(j) , GD)
             
             AUX4 = AUX4 + AB(j)%NVP
             AUX3 = AUX4 - AB(j)%NVP
             
             AIM(  AUX1 + 1 : AUX2   ,    AUX3 + 1 :  AUX4 ) = A
         
            deallocate( A )
            
         end do
      
      end do 
	  
! complete with ones in equations of conservation of circulation
      
	  AUX1 = 0
	  AUX2 = 0
	  
	  do i = 1, GD%NAB
	  
	      AUX2 = AUX2 + AB(i)%NVP
		  AUX1 = AUX2 - AB(i)%NVP
	  
	      AIM(  AUX2   ,    AUX1 + 1 :  AUX2 ) = 1.00D+00
		  
	  end do
	  
      end subroutine AIMATRIX