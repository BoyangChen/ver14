      module geometrymodule
      use parametermodule
!-------------------------------------------------------     
!---- module for geometrical quantities ----------------
!-------------------------------------------------------
      
      implicit none
      
      integer,parameter :: namelength=10 ! length for the name of the geo. quantity
      
      type point
        real(kind=dp) :: x(3)
      end type point
      
      type edge
        type(point) :: points(2)
      end type edge
      
      type surface
        character(len=namelength) :: itstype
        type(point),dimension(:),allocatable :: points
        type(edge),dimension(:),allocatable :: edges    
      end type surface
      
	  type volume
        character(len=namelength) :: itstype
        type(point),dimension(:),allocatable :: points
        type(edge),dimension(:),allocatable :: edges
        type(surface),dimension(:),allocatable :: surfaces
	  end type volume
      
      
      
      
      contains
      
      
      
      
      subroutine initializepoint(thispoint)
      
      	type(point) :: thispoint
      	
      	thispoint%x(:)=zero
      
      end subroutine intializepoint
      
      
      
      subroutine initializeedge(thisedge)
      
      	type(edge) :: thisedge
      	
      	integer :: i
      	
      	i=0
      	
      	do i=1,2
      		call initializepoint(thisedge%point(i))
        enddo
      
      end subroutine intializeedge
      
      
      
      
      subroutine initializesurface(thissurface,itstype)
      
      	type(surface) :: thissurface
      	character(len=namelength) :: itstype
      	
      	integer :: numberofpoints, numberofedges,i
      	
      	numberofpoints=0
      	numberofedges=0
      	i=0
      	
      	thissurface%itstype=itstype
      	
      	select case(itstype)
      	
      		case('triangle')
      			numberofpoints=3
      			numberofedges=3
      			
      		case('quadrilateral')
      			numberofpoints=4
      			numberofedges=4
      			
      		case default
      		! write some msg on msg file, then exit
      			return
      			
        end select
        
        allocate(thissurface%points(numberofpoints),thissurface%edges(numberofedges))
        ! check success
        
      	do i=1,numberofpoints
      		call initialize(thissurface%points(i))
      	enddo
      	
      	do i=1,numberofedges
      		call initialize(thissurface%edges(i))
      	enddo  
      
      return
      end subroutine intializesurface
      
      
      
      
      subroutine initializevolume(thisvolume,itstype)

      	type(volume) :: thisvolume
      	character(len=namelength) :: itstype
      	
      	thisvolume%itstype=itstype
      	
      	select case(itstype)
      	
      		case('tetrahedron')
      		
      		case('wedge')
      		
      		case('brick')
      		
      		case default
      		
        end select      
      
      return
      end subroutine intializevolume
      
      
      
      
      end module geometrymodule