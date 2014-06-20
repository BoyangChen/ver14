      module xgeometry_module
      use geometry_module
!-------------------------------------------------------     
!---- module for geometrical quantities ----------------
!-------------------------------------------------------
      
      implicit none
      private
      
      
      !---- define geometrical quantities ------
      
      type, public :: xedge
        integer :: end_points(2)
        integer,allocatable :: break_points(:)
        integer :: edge_status
        integer :: parent
        integer,allocatable :: children(:)
      end type xedge
      

      

      interface prepare
        !module procedure prepare_point
        module procedure prepare_xedge
        !module procedure prepare_triangle
        !module procedure prepare_quadrilateral
        !module procedure prepare_tetrahedron
        !module procedure prepare_wedge
        !module procedure prepare_brick
      end interface

      public :: prepare
      
      
      ! define initiation procedures
      contains
      
    
      
      ! prepare a breakable edge
      subroutine prepare_xedge(this_xedge,n_break_points,n_children)
      
      	type(xedge),intent(inout) :: this_xedge
        integer,optional,intent(in) :: n_break_points,n_children

        integer :: allocate_status,deallocate_status
      	
        this_xedge%end_points(:)=0
        this_xedge%edge_status=0
        this_xedge%parent=0

        if(allocated(this_xedge%break_points)) then
        deallocate(this_xedge%break_points,stat=deallocate_status)
            if(deallocate_status/=0) stop"**deallocation error in prepare_xedge**"
        end if

        if(allocated(this_xedge%children)) then
        deallocate(this_xedge%children,stat=deallocate_status)
            if(deallocate_status/=0) stop"**deallocation error in prepare_xedge**"
        end if
      
        if(present(n_break_points)) then
            allocate(this_xedge%break_points(n_break_points),stat=allocate_status)
            if(allocate_status/=0) stop"**allocation error in prepare_xedge break points**"
            this_xedge%break_points(:)=0
        end if

        if(present(n_children)) then
            allocate(this_xedge%children(n_children),stat=allocate_status)
            if(allocate_status/=0) stop"**allocation error in prepare_xedge children**"
            this_xedge%children(:)=0
        end if

      end subroutine prepare_xedge


 
      
      
      end module xgeometry_module
