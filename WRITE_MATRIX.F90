      subroutine WRITEDATA(A,m,n,s)

! Developed by Marcos L. Verstraete
! Date: 7.14.2017
! Grupo de Matemática Aplicada - FI-UNRC

!                           DESCRIPTION                               !
! This subroutine writes a matrix A in a file in order to 
!=====================================================================!
!                             MODULES                                 !
      use generalmodule
      use aerodynamicmodule
      
!=====================================================================!
!                        VARIABLE DECLARATION                         !      
      implicit none
      
      integer                                                 :: m,n
      type(GeneralData),intent(IN)                            :: GD
      type(AeroBody),dimension(GD%NAB),intent(IN)             :: AB
      type(Wake),dimension(GD%NAW),intent(IN)                 :: WC
      
      character(3)                                            :: suf
      
      double precision, parameter         :: PI  = .3141592653589793D+01
      integer                                                 :: b,i
      
      
      
!=====================================================================!
!                       BODY OF SUBROUTINE                            !
!=====================================================================!

      write(file ,*) '*************************************************'
      write(file ,*) '*                                               *'
      write(file ,*) '*           D A T A    R E P O R T              *'
	  write(file ,*) '*                                               *'
      write(file ,*) '* Program      : UVLM2D                         *'
      write(file ,*) '* Version      : 1.00                           *'
      write(file ,*) '* Developed by : M. Verstraete                  *'
      write(file ,*) '* Date         : July 2017                      *'
	  write(file ,*) '*                                               *'
      write(file ,*) '*************************************************'
      
      write(file ,*)
      write(file ,*) '           P R O Y E C T : '//suf//''
      write(file ,*)

      

      write( file , * )'       ~~~~~~~~~~~ GENERAL DATA ~~~~~~~~~~~'
      write(file ,*) 
      
      write( file , 51  ) GD%NAB
      write( file , 52  ) GD%NAW
      write( file ,  *  )
      write( file , 53  ) GD%NSTEP
      write( file , 54  ) GD%DT
      write( file ,  *  ) 
      write( file , 56  ) GD%TC
      write( file , 57  ) GD%LC
      write( file , 58  ) GD%VC
      write( file ,  *  )
      write( file , 60  ) GD%CUTOFFB
      write( file , 61  ) GD%CUTOFFW
       


      write( file ,  *  )
      write( file ,  *  )
      write( file ,  * )'      ~~~~~~~~~~  AERODYNAMIC DATA  ~~~~~~~~~~'
      write( file ,  *  )

      do b = 1, GD%NAB
      
        write( file ,  200  ) b
        write( file , * )
        write( file ,  205  ) AB(b)%NP
        write( file ,  206  ) AB(b)%NNP
        write( file ,  207  ) AB(b)%NCP
        write( file ,  208  ) AB(b)%NVP

       
        write( file , * )
        write(file  ,  *)'                NODAL POINTS   '
	   
        do i = 1, AB(b)%NNP
        write(file,280) i, AB(b)%XYNPB(1,i),AB(b)%XYNPB(2,i)
        end do
       
        write( file , * )
        write(file  , * )'                CONTROL POINTS '
	   
        do i = 1, AB(b)%NCP
        write(file,280) i, AB(b)%XYCPB(1,i),AB(b)%XYCPB(2,i)
     
        end do
		
	    write( file , * )
        write(file  , * )'                 VORTEX POINTS '
	   
        do i = 1, AB(b)%NVP
        write(file,280) i, AB(b)%XYVPB(1,i),AB(b)%XYVPB(2,i)
        end do
        
       
	    write( file , * )
        write( file , * )'               LOCATION MATRIX '
	   
        do i = 1, AB(b)%NP
        write(file,281) i , AB(b)%LM(1,i) , AB(b)%LM(2,i)
        end do
        
        write( file , * )
        write( file , * )'                NORMAL VECTORS '
		
        do i = 1, AB(b)%NCP
        write(file,280) i, AB(b)%nVB(1,i),AB(b)%nVB(2,i)
        end do
		
	    write( file , * )
        write( file , * )'            TANGENTIAL VECTORS '
          
        do i = 1, AB(b)%NCP
        write(file,280) i, AB(b)%tVB(1,i),AB(b)%tVB(2,i)
        end do
      
	  enddo
      
         
 51   format ('NUMBER OF AERODYNAMIC BODY  : ' , 3x ,  i3)
 52   format ('NUMBER OF AERODYNAMIC WAKE  : ' , 3x ,  i3)
 53   format ('TIME STEP                   : ' , 3x ,  i8)
 54   format ('DIMENSIONLESS DT            : ' , 6x ,  E13.6)
 56   format ('CHARACTERISTIC TIME(TC)     : ' , 6x ,  E13.6)
 57   format ('CHARACTERISTIC LENGTH(LC)   : ' , 6x ,  E13.6)
 58   format ('CHARACTERISTIC VELOCITY(VC) : ' , 6x ,  E13.6)
 60   format ('BOUND SHEET CUTTOFF         : ' , 6x ,  E13.6)
 61   format ('FREE SHEET CUTTOFF          : ' , 6x ,  E13.6)
 

 200  format (20x,'BODY #', x , i2)
 205  format ('NUMBER OF PANELS         : ' , 3x ,  i5)
 206  format ('NUMBER OF NODAL POINTS   : ' , 3x ,  i5)
 207  format ('NUMBER OF CONTROL POINTS : ' , 3x ,  i6)
 208  format ('NUMBER OF VORTEX POINTS  : ' , 3x ,  i6)


280   format(i6,3x,f12.8,x,f12.8)
281   format(i6,3x,i8 , 2x , i8)
 
 


   
      end subroutine WRITEDATA