    module xquad_module
      use parameter_module
      
      implicit none
      private


      type, public :: xquad ! breakable quadrilateral
        integer :: end_nodes(4)
        integer :: edges(4)
        integer,allocatable :: floating_nodes(:)
        integer :: current_status
        integer :: parent
        integer,allocatable :: children(:)
      end type xquad
      
      interface empty
        module procedure empty_xquad
      end interface
      
      interface update
        module procedure update_xquad
      end interface

      public :: empty,update


 
      contains
      
      
      
    end module xquad_module