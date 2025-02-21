      subroutine RHSVECTOR (RHS , AB , AW , GD)
      
! Developed by Marcos L. Verstraete
! Date: 7.10.2017
! Grupo de Matemática Aplicada - FI-UNRC

!----------------------------------------------------------------------

       use aerodynamicmodule
      
       use generalmodule

!----------------------------------------------------------------------
      
      implicit none
      
      type (GeneralData)                                  :: GD
      
      type (AEROBODY), dimension(GD%NAB)                  :: AB

      type (WAKE), dimension(GD%NAW), intent(IN)          :: AW
     
      double precision, dimension(GD%DOP),  intent (OUT)    :: RHS
      
      integer                                             :: i, j, k
     	  
      double precision, dimension (2)                     :: VAUX, VWj
      
!----------------------------------------------------------------------

      k = 1
             
      do i = 1, GD%NAB
             
         do j = 1, AB(i)%NCP
		 
         
             call WAKEINFLUENCE( VWj , AB(i)%XYCPN(:,j) , AW , GD)
                          
             AB(i)%VWCP(:,j) = VWj 
			 
			 VAUX = -( VWj + GD%Uinf - AB(i)%VSCP(:,j) )
			 
			 RHS( k ) = VAUX(1)*AB(i)%nVN(1,j)+VAUX(2)*AB(i)%nVN(2,j)
             
			 k = k + 1
         
		 end do
		 
		     RHS( k ) = AB(i)%SUMBG
			 
			 k = k + 1
      
      end do 
	  
      end subroutine RHSVECTOR