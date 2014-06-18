      module geometry_module
      use parameter_module
!-------------------------------------------------------     
!---- module for geometrical quantities ----------------
!-------------------------------------------------------
      
      implicit none
      
      integer, parameter :: name_length=10 ! length for the name of the geo. quantity
      
      !---- define geometrical quantities ------
      type point
        real(kind=dp) :: x(3)
      end type point
      
      type edge
        type(point) :: points(2)
      end type edge
      
      type surface
        character(len=name_length) :: its_type
        type(point),allocatable :: points(:)
        type(edge),allocatable :: edges(:)  
      end type surface
      
      type volume
        character(len=name_length) :: its_type
        type(point),allocatable :: points(:)
        type(edge),allocatable :: edges(:)
        type(surface),allocatable :: surfaces(:)
      end type volume
      !-----------------------------------------
      
      
      
      
      contains
      
      ! define initiation and assignment procedures
      
      ! prepare a point with zero coordinates
      subroutine prepare_point(this_point)
      
      	type(point),intent(inout) :: this_point
      	
      	this_point%x(:)=zero
      
      end subroutine prepare_point
 
      ! place a point on desired coordinates
      subroutine place_point(this_point,x)
      
      	type(point),intent(inout) :: this_point
        real(kind=dp),intent(in) :: x(:)
      	
      	this_point%x(:)=x(:)
      
      end subroutine place_point 
      
      
      ! prepare an edge
      subroutine prepare_edge(this_edge)
      
      	type(edge),intent(inout) :: this_edge
      	
      	integer :: i
      	
        i=0
      	
      	do i=1,2
            call prepare_point(this_edge%points(i))
        enddo
      
      end subroutine prepare_edge
      
      ! place an edge on desired locations
      subroutine place_edge(this_edge,points)
      
      	type(edge),intent(inout) :: this_edge
        type(point),intent(in) :: points(:)
      	
        this_edge%points(:)=points(:)
      
      end subroutine place_edge   
      
      
      subroutine prepare_surface(this_surface,this_type)
      
      	type(surface),intent(inout) :: this_surface
      	character(len=name_length),intent(in) :: this_type
      	
      	integer :: number_of_points, number_of_edges,i
      	
        ! prepare integer variables
        number_of_points=0
        number_of_edges=0
        i=0
      	
        ! assign the designated type to its type parameter
        this_surface%its_type=this_type
      	
        ! define the numbers of its respective building elements
        ! according to its type
        select case(this_type)   	
            case('triangle')
                number_of_points=3
                number_of_edges=3		
            case('quadrilateral')
                number_of_points=4
                number_of_edges=4
            case default
                write(msg_file,*)'unsupported type name for surface'
                return    			
        end select
        
        ! allocate space for its building elements
        
        ! if this geo. quantity has already been prepared/used
        if(allocated(this_surface%points) .or. &
          allocated(this_surface%edges)) then 
          ! if it has been prepared according to the correct dimensions
          if(size(this_surface%points)==number_of_points .and. &
            size(this_surface%edges)==number_of_edges) then
            ! do nothing
          else
            ! deallocate space associated with these quantities
            deallocate(this_surface%points,this_surface%edges,&
            stat=deallocate_status)
            ! check success/failure of space deallocation
            if (deallocate_status /= 0) &
                stop "*** Deallocation failed ***" 
            
            ! reallocate appropriate space according to the correct dimensions
            allocate(this_surface%points(number_of_points), &
            this_surface%edges(number_of_edges),stat=allocate_status)
            ! check success/failure of space allocation
            if (allocate_status /= 0) &
                stop "*** Not enough memory ***"          
          end if
        
        ! if this geo. quantity has not been prepared
        else
          ! allocate appropriate space according to the correct dimensions
          allocate(this_surface%points(number_of_points), &
          this_surface%edges(number_of_edges),stat=allocate_status)
          ! check success/failure of space allocation
          if (allocate_status /= 0) &
            stop "*** Not enough memory ***"  
        
        end if
        
        ! prepare its points and edges
        do i=1,number_of_points
            call prepare_point(this_surface%points(i))
        end do
        do i=1,number_of_edges
            call prepare_edge(this_surface%edges(i))
        end do  
      
      return
      end subroutine prepare_surface
      
      
      
      
      subroutine prepare_volume(this_volume,this_type)

      	type(volume),intent(inout) :: this_volume
      	character(len=namelength),intent(in) :: this_type
        
      	integer :: number_of_points, number_of_edges, &
        number_of_surfaces, i
      	
        number_of_points=0
        number_of_edges=0
        number_of_surfaces=0
        i=0
      	
      	this_volume%its_type=this_type
      	
      	select case(this_type)   	
            case('tetrahedron')
                number_of_points=4
                number_of_edges=6
                number_of_surfaces=4
            case('wedge')
                number_of_points=6
                number_of_edges=9
                number_of_surfaces=5           
            case('brick')
                number_of_points=8
                number_of_edges=12
                number_of_surfaces=6	
            case default
                write(msg_file,*)'unsupported type name for volume'
                return 
        end select      

        
        ! allocate space for its building elements
        
        ! if this geo. quantity has already been prepared/used
        if(allocated(this_volume%points) .or. &
          allocated(this_volume%edges) .or. &
          allocated(this_volume%surfaces)) then 
          ! if it has been prepared according to the correct dimensions
          if(size(this_volume%points)==number_of_points .and. &
            size(this_volume%edges)==number_of_edges .and. &
            size(this_volume%surfaces)==number_of_surfaces) then
            ! do nothing
          else
            ! deallocate space associated with these quantities
            deallocate(this_volume%points,this_volume%edges,&
            this_volume%surfaces,stat=deallocate_status)
            ! check success/failure of space deallocation
            if (deallocate_status /= 0) &
                stop "*** Deallocation failed ***" 
            
            ! reallocate appropriate space according to the correct dimensions
            allocate(this_volume%points(number_of_points), &
            this_volume%edges(number_of_edges), &
            this_volume%surfaces(number_of_surfaces), &
            stat=allocate_status)
            ! check success/failure of space allocation
            if (allocate_status /= 0) &
                stop "*** Not enough memory ***"          
          end if
        
        ! if this geo. quantity has not been prepared
        else
          ! allocate appropriate space according to the correct dimensions
          allocate(this_volume%points(number_of_points), &
          this_volume%edges(number_of_edges), &
          this_volume%surfaces(number_of_surfaces), &
          stat=allocate_status)
          ! check success/failure of space allocation
          if (allocate_status /= 0) &
              stop "*** Not enough memory ***" 
        
        end if
        
        ! prepare its points, edges and surfaces
        do i=1,number_of_points
            call prepare_point(this_volume%points(i))
        end do
        do i=1,number_of_edges
            call prepare_edge(this_volume%edges(i))
        end do  
        do i=1,number_of_surfaces
            call prepare_surface(this_volume%surfaces(i))
        end do        
        
      return
      end subroutine prepare_volume
      
      
      
      
      end module geometry_module