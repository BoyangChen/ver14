    module xtri_module
      use parameter_module
      
      implicit none
      private


      type, public :: xtri ! breakable triangle
        integer :: end_nodes(3)
        integer :: edges(3)
        integer,allocatable :: floating_nodes(:)
        integer :: current_status
        integer :: parent
        integer,allocatable :: children(:)
      end type xtri
      
      interface empty
        module procedure empty_xtri
      end interface
      
      interface update
        module procedure update_xtri
      end interface

      public :: empty,update


 
      contains
      
      
      
    end module xtri_module