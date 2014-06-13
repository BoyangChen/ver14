      module modelmsub
      use modparam
      use modndelm
      
      implicit none
      
      integer,allocatable :: elmsub(:,:,:) ! nsub; nnds, nd1,..; nnds, nd1,..;...
      
      contains
      
      subroutine initelmsub(nsub,subnd)
      ! passed in var
      integer,intent(in) :: nsub,subnd ! max no. of sub elm & max no. of nodes in a sub elm
      ! local var
      integer :: m,n,l
      
      m=0;n=0;l=0
      
      !m=1+nsub*(1+subnd)
      l=1+subnd
      
      m=nsub
      
      n=size(ndelm(1,:))
      
      allocate(elmsub(l,m,n))
      
      elmsub(:,:,:)=0
      
      end subroutine initelmsub
      
      end module modelmsub