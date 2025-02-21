      module AERODYNAMICMODULE

! Developed by Marcos L. Verstraete
! Date: 7.10.2017
! Grupo de Matemática Aplicada - FI-UNRC
   
   
      type :: AEROBODY
       
       integer                                         :: NP
       integer                                         :: NNP
       integer                                         :: NVP
       integer                                         :: NCP
       
       double precision                                :: CL
              
       double precision, allocatable, dimension (:,:)  :: XYNPB       
       double precision, allocatable, dimension (:,:)  :: XYVPB
       double precision, allocatable, dimension (:,:)  :: XYCPB
       double precision, allocatable, dimension (:,:)  :: XYNPN       
       double precision, allocatable, dimension (:,:)  :: XYVPN
       double precision, allocatable, dimension (:,:)  :: XYCPN
	   
	   double precision, allocatable, dimension (:,:)  :: VSCP
	   double precision, allocatable, dimension (:,:)  :: VWCP
	   
	   double precision, allocatable, dimension (:,:)  :: FN

         
         double precision, allocatable, dimension (:,:)  :: nVB
	   double precision, allocatable, dimension (:,:)  :: nVN
	   
	   double precision, allocatable, dimension (:,:)  :: tVB
	   double precision, allocatable, dimension (:,:)  :: tVN
       
      double precision, allocatable, dimension(:)      :: DCP
       double precision, allocatable, dimension(:)     :: BG, Ck
       double precision, allocatable, dimension(:)     :: BDG
       double precision                                :: SUMBG
	   
       integer, allocatable, dimension(:,:)            :: LM
  

      end type AEROBODY

   
      type :: WAKE

       double precision, allocatable, dimension (:,:)  :: XYAUX
       double precision, allocatable, dimension (:,:)  :: XYN
       
       integer                                         :: LIMNVP
       double precision, allocatable, dimension(:)     :: GW
       integer                                         :: NVP

      end type WAKE
   
   
      end module