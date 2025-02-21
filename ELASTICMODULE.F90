      module ELASTICMODULE

! Developed by Marcos L. Verstraete
! Date: 7.10.2017
! Grupo de Matemática Aplicada - FI-UNRC
   
   
      type :: ELASTICBODY
       
      end type ELASTICBODY

   
      type :: SPRING

       double precision                                :: KHL
       double precision                                :: KHC
       
       double precision                                :: KAL
       double precision                                :: KAC
       
       double precision                                :: DH
       double precision                                :: DA
       
       double precision                                :: M
       double precision                                :: IP
       
       double precision                                :: BB
       double precision                                :: XA
       
       double precision,  dimension (2,1)              :: FS
       
       double precision, dimension(2)                  :: q
       double precision, dimension(2)                  :: dq
       double precision, dimension(2)                  :: R0
       
       double precision                                :: Ceq
       double precision                                :: span
       double precision                                :: Rl
       double precision                                :: Acoup
       double precision                                :: Voltage
       
       
      end type SPRING
   
   
      end module