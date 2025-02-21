      subroutine MATRIXOFBB( A , BI , BJ ,GD )
      
!     THESE RUTINE CALCULATE AERODYNAMICS INFLUENCE MATRIX

!Version 1.01B               Bruno Roccia     20nov2017
      !Particion de archivos de TECPOLOT      
      
!Modificacion 1.02.01L       Luis Ceballos    27nov2017
      !agregado de LAPACK y paralelizacion OpenMP

      
!----------------------------------------------------------------------

      use aerodynamicmodule
	use generalmodule
      use omp_lib

!----------------------------------------------------------------------
      
      implicit none
      
       type (GeneralData)                                  :: GD
       
      type(AEROBODY), intent(IN)                           :: BI
      type(AEROBODY), intent(IN)                           :: BJ
      
      double precision, dimension (BI%NVP,BJ%NVP), intent (OUT):: A

      integer                                                :: i, j
      double precision                                       :: l	  
      double precision, dimension (2)                        :: R0, RP
      double precision, dimension(2)                         :: VEL
      
!----------------------------------------------------------------------
     
	 l = GD%CUTOFFB! * GD%LC

	 !l = 0.01D+00

!$omp parallel do private(i,j,RP,VEL,R0)       
      do i = 1, BI%NCP
         
         RP = BI%XYCPN (: , i)
         
         do j = 1, BJ%NVP
         
              VEL = 0.00D+00
			  
			  R0 = BJ%XYVPN (: , j)
                           
              call VORTEX2D(VEL , 1.00D+00 , R0 , RP , l)
			  
			  A(i, j)  = VEL(1) * BI%nVN(1,i) + VEL(2) * BI%nVN(2,i)
         
         end do
        
      end do
!$omp end parallel do
	  
      A(BI%NVP, :) = 0.00D+00
	  
      end subroutine MATRIXOFBB