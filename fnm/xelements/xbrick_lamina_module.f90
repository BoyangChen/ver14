    module xbrick_lamina_module
      use parameter_module
      use subelement_module

      implicit none
      private

      integer,parameter :: nnode=8, nedge=8, ntri=0, nquad=2

      type, public :: xbrick_lamina ! breakable brick
        integer :: end_node(nnode) ! cnc to glb node arrays for accessing glb real(vertex) node numbers
        integer :: edge(nedge) ! cnc to glb edge arrays for accessing status var. and glb flo node numbers
        integer :: quad(nquad) ! cnc to glb quad arrays for accessing glb flo node numbers assoc. with the quad face
        integer,allocatable :: flo_node(:) ! assigned to this volume, in addition to the flo nodes on edges and faces
        integer :: curr_status
        type(subelement),allocatable :: sub_elem(:) ! sub element connectivities
      end type xbrick_lamina
      
      type, public :: xbrick_laminate(nplyblock) ! parameterized derived type
        integer :: nplyblock
        type(xbrick_lamina) :: ply(nplyblock)
        type(xbrick_lamina) :: interlayer(nplyblock) ! topology is the same as lamina
      end type 


      interface empty
        module procedure empty_xbrick_lamina
        !module procedure empty_xbrick_laminate
      end interface

      interface update
        module procedure update_xbrick_lamina
        !module procedure update_xbrick_laminate
      end interface

      public :: empty,update



      contains




      ! empty a breakable brick
      subroutine empty_xbrick_lamina(this_xbrick_lamina)

      	type(xbrick_lamina),intent(inout) :: this_xbrick_lamina

        integer :: istat

        this_xbrick_lamina%end_node(:)=0
        this_xbrick_lamina%edge(:)=0
        this_xbrick_lamina%quad(:)=0
        this_xbrick_lamina%curr_status=0

        if(allocated(this_xbrick_lamina%flo_node)) then
        deallocate(this_xbrick_lamina%flo_node,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xbrick_lamina**"
        end if

      end subroutine empty_xbrick_lamina




      ! update a breakable brick
      subroutine update_xbrick_lamina(this_xbrick_lamina,curr_status,end_node,&
      & edge,quad,flo_node)

      	type(xbrick_lamina),intent(inout) :: this_xbrick_lamina
        integer,optional,intent(in) :: curr_status
        integer,optional,intent(in) :: end_node(:),edge(:),quad(:)
        integer,optional,intent(in) :: flo_node(:)

        integer :: istat

        if(present(curr_status)) this_xbrick_lamina%curr_status=curr_status

        if(present(end_node)) then
            if(size(end_node)==size(this_xbrick_lamina%end_node)) then
                this_xbrick_lamina%end_node(:)=end_node(:)
            else
                stop"**wrong size for xbrick_lamina end_node component**"
            end if
        end if

        if(present(edge)) then
            if(size(edge)==size(this_xbrick_lamina%edge)) then
                this_xbrick_lamina%edge(:)=edge(:)
            else
                stop"**wrong size for xbrick_lamina edge component**"
            end if
        end if

        if(present(quad)) then
            if(size(quad)==size(this_xbrick_lamina%quad)) then
                this_xbrick_lamina%quad(:)=quad(:)
            else
                stop"**wrong size for xbrick_lamina quad component**"
            end if
        end if

        if(present(flo_node)) then
            if(allocated(this_xbrick_lamina%flo_node)) then
                if(size(flo_node)==size(this_xbrick_lamina%flo_node)) then
                    this_xbrick_lamina%flo_node(:)=flo_node(:)
                else
                    deallocate(this_xbrick_lamina%flo_node,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xbrick_lamina**"
                    allocate(this_xbrick_lamina%flo_node(size(flo_node)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xbrick_lamina**"
                    this_xbrick_lamina%flo_node(:)=flo_node(:)
                end if
            else
                allocate(this_xbrick_lamina%flo_node(size(flo_node)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xbrick_lamina**"
                this_xbrick_lamina%flo_node(:)=flo_node(:)
            end if
        end if

      end subroutine update_xbrick_lamina


    end module xbrick_lamina_module
