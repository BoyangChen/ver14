      module geometry_module
      use parameter_module
!-------------------------------------------------------     
!---- module for geometrical quantities ----------------
!-------------------------------------------------------
      
      implicit none
      private
      
      
      !---- define geometrical quantities ------
      type, public :: node
        real(kind=dp) :: x(3)
        !contains
          !procedure :: prepare => prepare_node
          !procedure :: build => build_node
      end type node
      
      type, public :: edge
        integer :: nodes(2)
        !contains
          !procedure :: prepare => prepare_edge
          !procedure :: build => build_edge
      end type edge
      
      type, public :: triangle
        integer :: nodes(3)
        integer :: edges(3)  
        !contains
          !procedure :: prepare => prepare_triangle
          !procedure :: build => build_triangle
      end type triangle
      
      type, public :: quadrilateral
        integer :: nodes(4)
        integer :: edges(4)  
        !contains
          !procedure :: prepare => prepare_quadrilateral
          !procedure :: build => build_quadrilateral
      end type quadrilateral
      
      type, public :: tetrahedron
        integer :: nodes(4)
        integer :: edges(6)
        integer :: triangles(4)
        !contains
          !procedure :: prepare => prepare_tetrahedron
          !procedure :: build => build_tetrahedron
      end type tetrahedron
      
      type, public :: wedge
        integer :: nodes(6)
        integer :: edges(9)
        integer :: triangles(2)
        integer :: quadrilaterals(3)
        !contains
          !procedure :: prepare => prepare_wedge
          !procedure :: build => build_wedge
      end type wedge
      
      type, public :: brick
        integer :: nodes(8)
        integer :: edges(12)
        integer :: quadrilaterals(6)
        !contains
          !procedure :: prepare => prepare_brick
          !procedure :: build => build_brick
      end type brick
      

      interface prepare
        module procedure prepare_node
        module procedure prepare_edge
        module procedure prepare_triangle
        module procedure prepare_quadrilateral
        module procedure prepare_tetrahedron
        module procedure prepare_wedge
        module procedure prepare_brick
      end interface

      public :: prepare
      
      
      ! define initiation procedures
      contains
      
      
      
      ! prepare a node with zero coordinates
      subroutine prepare_node(this_node)
      
      	type(node),intent(inout) :: this_node
      	
      	this_node%x(:)=zero
        !this_node=node(zero) ! cannot use implicit constructor here
      
      end subroutine prepare_node
    
      
      ! prepare an edge
      subroutine prepare_edge(this_edge)
      
      	type(edge),intent(inout) :: this_edge
      	
        this_edge%nodes(:)=0
      
      end subroutine prepare_edge

   
      ! prepare a triangle
      subroutine prepare_triangle(this_surface)
      
        type(triangle),intent(inout) :: this_surface
 
        this_surface%nodes(:)=0       
        this_surface%edges(:)=0
      
      end subroutine prepare_triangle
      

      ! prepare a quadrilateral
      subroutine prepare_quadrilateral(this_surface)
      
        type(quadrilateral),intent(inout) :: this_surface
        
        this_surface%nodes(:)=0   
        this_surface%edges(:)=0
    
      end subroutine prepare_quadrilateral
 

      ! prepare a tetrahedron
      subroutine prepare_tetrahedron(this_volume)
      
        type(tetrahedron),intent(inout) :: this_volume
        
        this_volume%nodes(:)=0
        this_volume%edges(:)=0
        this_volume%triangles(:)=0
      
      end subroutine prepare_tetrahedron

      
      ! prepare a wedge
      subroutine prepare_wedge(this_volume)
      
        type(wedge),intent(inout) :: this_volume
        
        this_volume%nodes(:)=0       
        this_volume%edges(:)=0      
        this_volume%triangles(:)=0 
        this_volume%quadrilaterals(:)=0
      
      end subroutine prepare_wedge
      
      
      ! prepare a brick
      subroutine prepare_brick(this_volume)
      
        type(brick),intent(inout) :: this_volume
        
      	this_volume%nodes(:)=0
        this_volume%edges(:)=0
        this_volume%quadrilaterals(:)=0
      
      end subroutine prepare_brick
 
      
      
      end module geometry_module
