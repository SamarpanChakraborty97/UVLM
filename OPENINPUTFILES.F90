      subroutine OPENINPUTFILES( suf , IFS , NIF )
      
      
      implicit none
       
      integer, intent(IN)                       :: NIF
      integer, dimension(NIF), intent (OUT)     :: IFS
      character(4), intent(IN)                  :: suf
      
      integer                                   :: i
      character(4)                              :: AUX1 
      character(50)                             :: FLAG
      
      
      do i = 1, NIF
      
         IFS(i) = 100 + i 
      
      end do
      

      open( UNIT = IFS(1) , FILE = 'DATA_'//suf//'.DAT', STATUS = 'OLD',
     +      FORM = 'FORMATTED', ACCESS = 'SEQUENTIAL' )
              
      end subroutine OPENINPUTFILES