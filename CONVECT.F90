      subroutine CONVECT (AB , AW , GD)
      
! Developed by Marcos L. Verstraete
! Date: 7.10.2017
! Grupo de Matemática Aplicada - FI-UNRC

!----------------------------------------------------------------------

       use aerodynamicmodule
      
       use generalmodule
       
       use omp_lib

!----------------------------------------------------------------------
      
      implicit none
      
      type (GeneralData), intent(IN)                       :: GD
      
      type (AEROBODY), dimension(GD%NAB), intent(IN)       :: AB

      type (WAKE), dimension(GD%NAW), intent(INOUT)        :: AW
     
      
      integer                                             :: w, i
     	  
      double precision, dimension (2)             :: VEL, VWi,VBi, Ri
      
!----------------------------------------------------------------------

      do w = 1, GD%NAW
         
         AW(w)%XYAUX = AW(w)%XYN

!$omp parallel do private(i,Ri,VBi,VWi,VEL)         
         do i = 1, AW(w)%NVP
         
         VEL = 0.00D+00
         
         Ri = AW(w)%XYAUX(:,i)
         
! BODIES
         
         call BODYINFLUENCE(VBi , Ri , AB , GD)
      
! WAKES
      
         call WAKEINFLUENCE( VWi , Ri , AW , GD)
         
         VEL = VBi + VWi + GD%Uinf
         
         AW(w)%XYN(:,i+1) = Ri + VEL * GD%DT
      
          end do
!$omp end parallel do          
      
      end do
      
      
       do w = 1, GD%NAW
          
          AW(w)%GW(2:AW(w)%LIMNVP) = AW(w)%GW(1:AW(w)%LIMNVP-1)
      
          AW(w)%XYAUX = AW(w)%XYN
       
       end do
 
      
      end subroutine CONVECT