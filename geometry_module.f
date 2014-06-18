      module geometry_module
      use parameter_module
!-------------------------------------------------------     
!---- module for geometrical quantities ----------------
!-------------------------------------------------------
      
      implicit none
      private
      
      
      !---- define geometrical quantities ------
      type, public :: point
        real(kind=dp) :: x(3)
        contains
          procedure :: prepare => prepare_point
          procedure :: build => build_point
      end type point
      
      type, public :: edge
        type(point) :: points(2)
        contains
          procedure :: prepare => prepare_edge
          procedure :: build => build_edge
      end type edge
      
      type, public :: triangle
        type(point) :: points(3)
        type(edge) :: edges(3)  
        contains
          procedure :: prepare => prepare_triangle
          procedure :: build => build_triangle
      end type triangle
      
      type, public :: quadrilateral
        type(point) :: points(4)
        type(edge) :: edges(4)  
        contains
          procedure :: prepare => prepare_quadrilateral
          procedure :: build => build_quadrilateral
      end type quadrilateral
      
      type, public :: tetrahedron
        type(point) :: points(4)
        type(edge) :: edges(6)
        type(triangle) :: triangles(4)
        contains
          procedure :: prepare => prepare_tetrahedron
          procedure :: build => build_tetrahedron
      end type tetrahedron
      
      type, public :: wedge
        type(point) :: points(6)
        type(edge) :: edges(9)
        type(triangle) :: triangles(2)
        type(quadrilateral) :: quadrilaterals(3)
        contains
          procedure :: prepare => prepare_wedge
          procedure :: build => build_wedge
      end type wedge
      
      type, public :: brick
        type(point) :: points(8)
        type(edge) :: edges(12)
        type(quadrilateral) :: quadrilaterals(6)
        contains
          procedure :: prepare => prepare_brick
          procedure :: build => build_brick
      end type brick
      

      
      
      
      ! define initiation and assignment procedures
      contains
      
      
      
      ! prepare a point with zero coordinates
      subroutine prepare_point(this_point)
      
      	class(point),intent(inout) :: this_point
      	
      	this_point%x(:)=zero
      
      end subroutine prepare_point
    
      
      ! prepare an edge
      subroutine prepare_edge(this_edge)
      
      	class(edge),intent(inout) :: this_edge
      	
      	integer :: i
      	
        i=0
      	
      	do i=1,2
            call prepare_point(this_edge%points(i))
        enddo
      
      end subroutine prepare_edge

   
      ! prepare a triangle
      subroutine prepare_triangle(this_surface)
      
        class(triangle),intent(inout) :: this_surface
        
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
      
        class(quadrilateral),intent(inout) :: this_surface
        
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
      
        class(tetrahedron),intent(inout) :: this_volume
        
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
      
        class(wedge),intent(inout) :: this_volume
        
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
      
        class(brick),intent(inout) :: this_volume
        
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
 
 
 
      ! build a point on desired coordinates
      subroutine build_point(this_point,x)
      
      	class(point),intent(inout) :: this_point
        real(kind=dp),intent(in) :: x(:)
      	
        if(size(this_point%x)==size(x)) then
            this_point%x(:)=x(:)
        else
            stop "dimension mismatch in build_point"
        end if
      
      end subroutine build_point 
      
      
      ! build an edge on desired locations
      subroutine build_edge(this_edge,points)
      
      	class(edge),intent(inout) :: this_edge
        type(point),intent(in) :: points(:)
      	
        if(size(this_edge%points)==size(points)) then    
            this_edge%points(:)=points(:)
        else
            stop "dimension mismatch in build_edge"
        end if
      
      end subroutine build_edge
  
  
      ! build a triangle
      subroutine build_triangle(this_surface,edges,points)
      
        class(triangle),intent(inout) :: this_surface
        type(edge),intent(in) :: edges(:)
        type(point),intent(in) :: points(:)
        
        if(size(this_surface%edges)==size(edges) .and. &
           size(this_surface%points)==size(points)) then
            this_surface%edges(:)=edges(:)
            this_surface%points(:)=points(:)
        else
            stop "dimension mismatch in build_triangle"
        end if
      
      end subroutine build_triangle
      

      ! build a quadrilateral
      subroutine build_quadrilateral(this_surface,edges,points)
      
        class(quadrilateral),intent(inout) :: this_surface
        type(edge),intent(in) :: edges(:)
        type(point),intent(in) :: points(:)
        
        if(size(this_surface%edges)==size(edges) .and. &
           size(this_surface%points)==size(points)) then
            this_surface%edges(:)=edges(:)
            this_surface%points(:)=points(:)
        else
            stop "dimension mismatch in build_quadrilateral"
        end if
      
      end subroutine build_quadrilateral
      
      
      ! build a tetrahedron
      subroutine build_tetrahedron(this_volume,triangles,edges,points)
      
        class(tetrahedron),intent(inout) :: this_volume
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
            stop "dimension mismatch in build_tetrahedron"
        end if
      
      end subroutine build_tetrahedron   
      
      
      ! build a wedge
      subroutine build_wedge(this_volume,quadrilaterals,triangles, & 
      edges,points)
      
        class(wedge),intent(inout) :: this_volume
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
            stop "dimension mismatch in build_wedge"
        end if
      
      end subroutine build_wedge
      
      
      ! build a brick
      subroutine build_brick(this_volume,quadrilaterals, & 
      edges,points)
      
        class(wedge),intent(inout) :: this_volume
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
            stop "dimension mismatch in build_brick"
        end if
      
      end subroutine build_brick
      
      
      
      end module geometry_module