      
      subroutine OUTPUT (SP,AB,AW,GD,IT,OFS)
      
!-----------------------------------------------------------------------
!     USED MODULES

      use aerodynamicmodule
      use generalmodule
      use ELASTICMODULE
  
      
!------------------------------------------------------------------------

      implicit none
      
      type( GeneralData), intent(IN)                      :: GD
      type (AEROBODY), dimension(GD%NAB), intent (IN)     :: AB
      type (WAKE), dimension(GD%NAB), intent (IN)         :: AW
      type (SPRING), dimension(GD%NSPRING), intent (IN)   :: SP
      integer, dimension(9), intent (IN)                  :: OFS
      
      integer, intent (IN)                                :: IT
      
      double precision                                    :: Power
     
      integer                                             :: b,i, k
      
      integer                                             :: NNT, NLMT, 
     +                                                       NVPT       
      
!-----------------------------------------------------------------------
      ! OFS(1): WING_WAKE 
      ! OFS(2): CIRC BODY
      ! OFS(3): CIRC WAKE
      ! OFS(5): AEROLOADS
      ! OFS(7): DOF
      
      write(OFS(1), 20)
      write(OFS(1), 10) IT, IT*GD%DT
      write(OFS(1), 20)
      
      write(OFS(2), 20)
      write(OFS(2), 10) IT, IT*GD%DT
      write(OFS(2), 20)
      
      write(OFS(3), 20)
      write(OFS(3), 10) IT, IT*GD%DT
      write(OFS(3), 20)
      
      write(OFS(5), 20)
      write(OFS(5), 10) IT, IT*GD%DT
      write(OFS(5), 20)
      
      write(OFS(7), 20)
      write(OFS(7), 10) IT, IT*GD%DT
      write(OFS(7), 20)
      
        
! Write Loads 
        
      do b = 1, GD%NAB
      
         write(OFS(5), 30) b, AB(b)%CL
         
         write(OFS(5), *)'DCP'
         
         do i = 1, AB(b)%NCP
         
           write(OFS(5), 40) i, AB(b)%DCP(i)
         
         enddo
      
      end do
      
! Write circulations body

      !do b = 1, GD%NAB
      !
      !   write(OFS(2), 47) b
      !
      !   write(OFS(3), *)
      !   
      !   do i = 1, AB(b)%NVP
      !   
      !     write(OFS(2), 40) i, AB(b)%BG(i)
      !   
      !   enddo
      !
      !end do
      
! Write circulations WAKE      
      
      !do b = 1, GD%NAW
      !
      !   write(OFS(3), 48) b
      !   
      !   write(OFS(3), *)
      !   
      !   do i = 1, AW(b)%NVP
      !   
      !     write(OFS(3), 40) i, AW(b)%GW(i)
      !   
      !   enddo
      !
      !end do      
      

! Write Wake Evolution for MATLAB

      if (GD%FLAG_MATLAB .EQ. 1) then

         do b = 1, GD%NAB
      
             write(OFS(1), 45) b
         
             do i = 1, AB(b)%NNP
         
                write(OFS(1), 50) i, AB(b)%XYNPN(1,i), AB(b)%XYNPN(2,i)
         
             enddo
      
         end do
      
      
         do b = 1, GD%NAW
      
             write(OFS(1), 46) b
         
             do i = 1, AW(b)%NVP
         
                write(OFS(1), 50) i, AW(b)%XYN(1,i), AW(b)%XYN(2,i)
         
             enddo
      
          end do
      
      end if
      
! Write Wake Evolution for TECPLOT
      
      NNT  = 0
      NLMT = 0
      NVPT = 0
      
      do i = 1, GD%NAB
      
          NNT  = NNT + AB(i)%NNP
          NLMT = NLMT + AB(i)%NP
      
      end do
      
      do i = 1, GD%NAW
      
          NVPT = NVPT + AW(i)%NVP
      
      end do
      
      if (GD%FLAG_TEC .EQ. 1) then
      
          write ( OFS(9), 100 )b, IT, NNT, NLMT
      
          do b = 1, GD%NAB      
             
              do i = 1, AB(b)%NNP
         
                  write(OFS(9), '(e11.4)') AB(b)%XYNPN(1:2,i)
                      
              end do                                                    
         
         end do
             
         do b = 1, GD%NAB
                                    
             do i = 1, AB(b)%NP
         
                  write(OFS(9), 110) AB(b)%LM(1,i)+AB(b)%NNP*(b-1), 
     +              AB(b)%LM(2,i)+AB(b)%NNP*(b-1)
                      
             end do                                               
         
          end do 
          
          write ( OFS(9), 120 )b, IT, NVPT, 1
             
          do b = 1, GD%NAW                              
                 
              do i = 1, AW(b)%NVP
         
                  write(OFS(9), '(e11.4)')AW(b)%XYN(1:2,i)
         
              end do                 
                          
          end do
      
      end if

! Write DEGREE OF FREEDOM

      do b = 1, GD%NSPRING
      
           Power = (SP(b)%Voltage**2D+00) / SP(b)%Rl
           
           write(OFS(7), 52) b, SP(b)%q(1) , SP(b)%q(2)  ,
     +                          SP(b)%dq(1), SP(b)%dq(2) ,
     +                          SP(b)%Voltage , Power
      
      end do

           
10    format('T I M E - S T E P  =  ', i6 ,3x,'Time:',f14.8)
20    format('========================================================')
30    format(' BODY ',3x, i5, 3x, 'CL =', E16.7)
40    format( i6, 3x , E16.7)
45    format( ' BODY ',3x, i5 )
46    format( ' WAKE ',3x, i5 )
49    format( ' SPRING ',3x, i5 )
47    format( ' CIRCULATIONS OF BODY ',3x, i5 )
48    format( ' CIRCULATIONS OF WAKE ',3x, i5 )
50    format( i6, 3x , f16.9, 3x , f16.9)
52    format( i6, 3x , E16.7, 3x , E16.7, 3x , E16.7, 3x , E16.7 
     +          , 3x , E16.7, 3x , E16.7 )
      
100   format ('ZONE T = "AIRFOIL (',i6,')', i10,'",   N = ',i5,
     +        ',  E = ',i5,',   F = FEPOINT,   ET = LINESEG,  C = BLUE')
      
110   format (i5, 2x, i5)
      
120   format ('ZONE T = "WAKE AIRFOIL (',i6,')', i10,'",   I = ',i5,
     +        ', J = ',i5,',   F = POINT, C = GREEN')      
      
      
      

      end subroutine OUTPUT