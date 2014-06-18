      module geometry_module
      use parameter_module
!-------------------------------------------------------     
!---- module for geometrical quantities ----------------
!-------------------------------------------------------
      
      implicit none
      
      !integer, parameter :: name_length=10 ! length for the name of the geo. quantity
      
      !---- define geometrical quantities ------
      type point
        real(kind=dp) :: x(3)
      end type point
      
      type edge
        type(point) :: points(2)
      end type edge
      
      type triangle
        type(point) :: points(3)
        type(edge) :: edges(3)  
      end type triangle
      
      type quadrilateral
        type(point) :: points(4)
        type(edge) :: edges(4)  
      end type quadrilateral
      
      type tetrahedron
        type(point) :: points(4)
        type(edge) :: edges(6)
        type(triangle) :: triangles(4)
      end type tetrahedron
      
      type wedge
        type(point) :: points(6)
        type(edge) :: edges(9)
        type(triangle) :: triangles(2)
        type(quadrilateral) :: quadrilaterals(3)
      end type wedge
      
      type brick
        type(point) :: points(8)
        type(edge) :: edges(12)
        type(quadrilateral) :: quadrilaterals(6)
      end type brick
      
      !-----------------------------------------
      
      
      
      ! define initiation and assignment procedures
      contains
      
      
      
      ! prepare a point with zero coordinates
      subroutine prepare_point(this_point)
      
      	type(point),intent(inout) :: this_point
      	
      	this_point%x(:)=zero
      
      end subroutine prepare_point
    
      
      ! prepare an edge
      subroutine prepare_edge(this_edge)
      
      	type(edge),intent(inout) :: this_edge
      	
      	integer :: i
      	
        i=0
      	
      	do i=1,2
            call prepare_point(this_edge%points(i))
        enddo
      
      end subroutine prepare_edge

   
      ! prepare a triangle
      subroutine prepare_triangle(this_surface)
      
        type(triangle),intent(inout) :: this_surface
        
      	integer :: i
      	
        ! prepare integer variable
        i=0
        
        do i=1,size(this_surface%points)
            call prepare_point(this_surface%points(i))
        end do
        
        do i=1,size(this_surface%edges)
            call prepare_edge(this_surface%edges(i))
        end do
      
      end subroutine prepare_triangle
      

      ! prepare a quadrilateral
      subroutine prepare_quadrilateral(this_surface)
      
        type(quadrilateral),intent(inout) :: this_surface
        
      	integer :: i
      	
        ! prepare integer variable
        i=0
        
        do i=1,size(this_surface%points)
            call prepare_point(this_surface%points(i))
        end do
        
        do i=1,size(this_surface%edges)
            call prepare_edge(this_surface%edges(i))
        end do
      
      end subroutine prepare_quadrilateral
 

      ! prepare a tetrahedron
      subroutine prepare_tetrahedron(this_volume)
      
        type(tetrahedron),intent(inout) :: this_volume
        
      	integer :: i
      	
        ! prepare integer variable
        i=0
        
        do i=1,size(this_volume%points)
            call prepare_point(this_volume%points(i))
        end do
        
        do i=1,size(this_volume%edges)
            call prepare_edge(this_volume%edges(i))
        end do
        
        do i=1,size(this_volume%triangles)
            call prepare_triangle(this_volume%triangles(i))
        end do
      
      end subroutine prepare_tetrahedron

      
      ! prepare a wedge
      subroutine prepare_wedge(this_volume)
      
        type(wedge),intent(inout) :: this_volume
        
      	integer :: i
      	
        ! prepare integer variable
        i=0
        
        do i=1,size(this_volume%points)
            call prepare_point(this_volume%points(i))
        end do
        
        do i=1,size(this_volume%edges)
            call prepare_edge(this_volume%edges(i))
        end do
        
        do i=1,size(this_volume%triangles)
            call prepare_triangle(this_volume%triangles(i))
        end do
        
        do i=1,size(this_volume%quadrilaterals)
            call prepare_quadrilateral(this_volume%quadrilaterals(i))
        end do
      
      end subroutine prepare_wedge
      
      
      ! prepare a brick
      subroutine prepare_brick(this_volume)
      
        type(brick),intent(inout) :: this_volume
        
      	integer :: i
      	
        ! prepare integer variable
        i=0
        
        do i=1,size(this_volume%points)
            call prepare_point(this_volume%points(i))
        end do
        
        do i=1,size(this_volume%edges)
            call prepare_edge(this_volume%edges(i))
        end do
        
        do i=1,size(this_volume%quadrilaterals)
            call prepare_quadrilateral(this_volume%quadrilaterals(i))
        end do
      
      end subroutine prepare_brick
 
 
 
      ! place a point on desired coordinates
      subroutine place_point(this_point,x)
      
      	type(point),intent(inout) :: this_point
        real(kind=dp),intent(in) :: x(:)
      	
        if(size(this_point%x)==size(x)) then
            this_point%x(:)=x(:)
        else
            stop "dimension mismatch in place_point"
        end if
      
      end subroutine place_point 
      
      
      ! place an edge on desired locations
      subroutine place_edge(this_edge,points)
      
      	type(edge),intent(inout) :: this_edge
        type(point),intent(in) :: points(:)
      	
        if(size(this_edge%points)==size(points)) then    
            this_edge%points(:)=points(:)
        else
            stop "dimension mismatch in place_edge"
        end if
      
      end subroutine place_edge
  
  
      ! place a triangle
      subroutine place_triangle(this_surface,edges,points)
      
        type(triangle),intent(inout) :: this_surface
        type(edge),intent(in) :: edges(:)
        type(point),intent(in) :: points(:)
        
        if(size(this_surface%edges)==size(edges) .and. &
           size(this_surface%points)==size(points)) then
            this_surface%edges(:)=edges(:)
            this_surface%points(:)=points(:)
        else
            stop "dimension mismatch in place_triangle"
        end if
      
      end subroutine place_triangle
      

      ! place a quadrilateral
      subroutine place_quadrilateral(this_surface,edges,points)
      
        type(quadrilateral),intent(inout) :: this_surface
        type(edge),intent(in) :: edges(:)
        type(point),intent(in) :: points(:)
        
        if(size(this_surface%edges)==size(edges) .and. &
           size(this_surface%points)==size(points)) then
            this_surface%edges(:)=edges(:)
            this_surface%points(:)=points(:)
        else
            stop "dimension mismatch in place_quadrilateral"
        end if
      
      end subroutine place_quadrilateral
      
      
      ! place a tetrahedron
      subroutine place_tetrahedron(this_volume,triangles,edges,points)
      
        type(tetrahedron),intent(inout) :: this_volume
        type(triangle),intent(in) :: triangles(:)
        type(edge),intent(in) :: edges(:)
        type(point),intent(in) :: points(:)
        
        if(size(this_volume%triangles)==size(triangles) .and. &
           size(this_volume%edges)==size(edges) .and. &
           size(this_volume%points)==size(points)) then
            this_volume%triangles(:)=triangles(:)
            this_volume%edges(:)=edges(:)
            this_volume%points(:)=points(:)
        else
            stop "dimension mismatch in place_tetrahedron"
        end if
      
      end subroutine place_tetrahedron   
      
      
      ! place a wedge
      subroutine place_wedge(this_volume,quadrilaterals,triangles, & 
      edges,points)
      
        type(wedge),intent(inout) :: this_volume
        type(quadrilateral),intent(in) :: quadrilaterals(:)
        type(triangle),intent(in) :: triangles(:)
        type(edge),intent(in) :: edges(:)
        type(point),intent(in) :: points(:)
        
        if(size(this_volume%quadrilaterals)==size(quadrilaterals).and.&
           size(this_volume%triangles)==size(triangles) .and. &
           size(this_volume%edges)==size(edges) .and. &
           size(this_volume%points)==size(points)) then
            this_volume%quadrilaterals(:)=quadrilaterals(:)
            this_volume%triangles(:)=triangles(:)
            this_volume%edges(:)=edges(:)
            this_volume%points(:)=points(:)
        else
            stop "dimension mismatch in place_wedge"
        end if
      
      end subroutine place_wedge
      
      
      ! place a brick
      subroutine place_brick(this_volume,quadrilaterals, & 
      edges,points)
      
        type(wedge),intent(inout) :: this_volume
        type(quadrilateral),intent(in) :: quadrilaterals(:)
        type(edge),intent(in) :: edges(:)
        type(point),intent(in) :: points(:)
        
        if(size(this_volume%quadrilaterals)==size(quadrilaterals).and.&
           size(this_volume%edges)==size(edges) .and. &
           size(this_volume%points)==size(points)) then
            this_volume%quadrilaterals(:)=quadrilaterals(:)
            this_volume%edges(:)=edges(:)
            this_volume%points(:)=points(:)
        else
            stop "dimension mismatch in place_brick"
        end if
      
      end subroutine place_brick
      
      
      
      end module geometry_module