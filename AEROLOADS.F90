      subroutine AEROLOADS (DCP,FN, CL, AB,b,GD)

! Developed by Marcos L. Verstraete
! Date: 7.24.2017
! Grupo de Matemática Aplicada - FI-UNRC
      
!     THESE RUTINE CALCULATE THE AERODYNAMIC LOADS

!----------------------------------------------------------------------
      use AERODYNAMICMODULE
      use GENERALMODULE
!----------------------------------------------------------------------
      
      implicit none
      
      type (GeneralData), intent(IN)                        :: GD
      type (AEROBODY), dimension(GD%NAB),intent (IN)        :: AB
           
      integer, intent(IN)                                  :: b
      integer                                              :: i, j, k
      
      double precision, dimension(2)       :: VBCP, AUX3, VEL, R0, RP
      double precision, dimension(2)       :: Vmean, DV
      double precision                     :: AUX1, AUX2, AUX4, l,LIFT
      
      double precision, dimension(2)       :: FT 
      
      double precision,  dimension(AB(b)%NCP), intent(OUT)   :: DCP
      double precision, dimension(2,AB(b)%NCP), intent(OUT)  :: FN
      double precision, intent(OUT)                          :: CL
      
!----------------------------------------------------------------------
      
      l  = GD%CUTOFFB * GD%LC
      
      AUX1 = 0.00D+00
      AUX2 = 0.00D+00
             
      do i = 1, AB(b)%NCP
      
         AUX1 = SUM(AB(b)%BG(1:i))
         AUX2 = SUM(AB(b)%BDG(1:i))
      
          VBCP = 0.00D+00
          
          RP = AB(b)%XYCPN(:,i)
          
          do j = 1 , GD%NAB
              
              VEL = 0.00D+00
              
              do k = 1, AB(j)%NVP
                  
                  R0 = AB(j)%XYVPN(:,k)
                  
                  call VORTEX2D(VEL, AB(j)%BG(k) , R0 , RP , l)
                  
                  VBCP = VBCP + VEL
              
                  end do
              
          end do
          
          
          Vmean = VBCP + AB(b)%VWCP(:,i) + GD%Uinf
          
          Vmean = (Vmean(1)*AB(b)%tVN(1,i)+Vmean(2)*AB(b)%tVN(2,i))
     +            * AB(b)%tVN(:,i)           
          
          AUX3 = Vmean - AB(b)%VSCP(:,i)
              
          DV = AB(b)%BG(i)/AB(b)%Ck(i) * AB(b)%tVN(:,i)
          
          AUX4 = DV(1)*AUX3(1)+DV(2)*AUX3(2)
          
          DCP(i) = GD%RHO*(AUX4 + (AUX1-AUX2)/GD%DT) ! pressure
          
      end do
          
      FT = 0.00D+0
      
      do i = 1, AB(b)%NCP
         
          FN(:,i) = DCP(i)*AB(b)%Ck(i)*AB(b)%nVN(:,i) 
          
          FT = FT + FN(:,i)
          
      end do
      
          LIFT = FT(1)*GD%eL(1) + FT(2)*GD%eL(2)
          
          CL = LIFT / (0.5D+00*GD%RHO*GD%VC*GD%VC*GD%Lref)
      
          DCP = DCP / (0.5D+00*GD%RHO*GD%VC*GD%VC)
	  
      end subroutine AEROLOADS