      module modelem
      use modparam
!-------------------------------------------------------     
!---- module for elements ------------------------------
! creates a data type elem, and associated initiation 
! and calculation subroutines
! interface modules are needed for linking an element
! to the floating node elem module & the global arrays 
!-------------------------------------------------------
      
      implicit none
      
      type elem
      ! variables that describe this object class
      character(len=10) :: elname ! element name
      real(kind=dp),dimension(:,:),allocatable :: xelem !-coordinates of nodes
      real(kind=dp),dimension(:),allocatable :: u,du,v,a ! nodal disp., disp increment, velocity & accel.
      real(kind=dp),dimension(:,:),allocatable :: amat ! elm stiffness matrix
      real(kind=dp),dimension(:),allocatable :: rhs ! elm rhs vector      
      real(kind=dp),dimension(:),allocatable :: energy ! energy terms
      real(kind=dp),dimension(:,:),allocatable :: xig !-coordinates of integration points
      real(kind=dp),dimension(:,:),allocatable :: sig,eps ! elm stress & strains at intg pnts

      ! elem solution-dependent variables array; passed in from global arrays via 
      real(kind=dp),dimension(:),allocatable :: sdvel 
      real(kind=dp),dimension(:,:),allocatable :: sdvig ! intg pnt sdv array
      real(kind=dp) :: time, dtime
      integer :: kstep, kinc, jelem
      type(material) :: mat
      
      end type elem
      
      contains
      
      
      
!-------------------------------------------------------     
!---- subroutine to initialize this object -------------
!-------------------------------------------------------
      subroutine newelem(el,elname)
      
          ! read input variables (variables passed-in from the interface kernelfs)
          character(len=10),intent(in) :: elname

          ! return a new element with all its variables updated
          type(elem),intent(out) :: el
          
          
          ! create a new element according to its element type
          elm%elname=elname
          
          select case (elname)
          
            case ('tri3')
            
            case ('quad4')
            
            case default
                write(6,*)'unsupported bulk element type'
                call xit
          end select
               
      end subroutine newelem
      
      
!-------------------------------------------------------     
!---- subroutine to operate on this object -------------
!-------------------------------------------------------
      
      
      end module modelem