    module xbrick_module
      use parameter_module
      use subelement_module

      implicit none
      private

      integer,parameter :: nnode=8, nedge=12, ntri=0, nquad=6

      type, public :: brick ! breakable brick
        integer :: end_node(nnode)
      end type brick

      type, public :: xbrick ! breakable brick
        integer :: end_node(nnode) ! cnc to glb node arrays for accessing glb real(vertex) node numbers
        integer :: edge(nedge) ! cnc to glb edge arrays for accessing status var. and glb flo node numbers
        integer :: quad(nquad) ! cnc to glb quad arrays for accessing glb flo node numbers assoc. with the quad face
        integer,allocatable :: flo_node(:) ! assigned to this volume, in addition to the flo nodes on edges and faces
        integer :: curr_status
        type(subelement),allocatable :: sub_elem(:) ! sub element connectivities
      end type xbrick


      interface empty
        module procedure empty_xbrick
      end interface

      interface update
        module procedure update_xbrick
      end interface

      public :: empty,update



      contains




      ! empty a breakable brick
      subroutine empty_xbrick(this_xbrick)

      	type(xbrick),intent(inout) :: this_xbrick

        integer :: istat

        this_xbrick%end_node(:)=0
        this_xbrick%edge(:)=0
        this_xbrick%quad(:)=0
        this_xbrick%curr_status=0

        if(allocated(this_xbrick%flo_node)) then
        deallocate(this_xbrick%flo_node,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xbrick**"
        end if

      end subroutine empty_xbrick




      ! update a breakable brick
      subroutine update_xbrick(this_xbrick,curr_status,end_node,&
      & edge,quad,flo_node)

      	type(xbrick),intent(inout) :: this_xbrick
        integer,optional,intent(in) :: curr_status
        integer,optional,intent(in) :: end_node(:),edge(:),quad(:)
        integer,optional,intent(in) :: flo_node(:)

        integer :: istat

        if(present(curr_status)) this_xbrick%curr_status=curr_status

        if(present(end_node)) then
            if(size(end_node)==size(this_xbrick%end_node)) then
                this_xbrick%end_node(:)=end_node(:)
            else
                stop"**wrong size for xbrick end_node component**"
            end if
        end if

        if(present(edge)) then
            if(size(edge)==size(this_xbrick%edge)) then
                this_xbrick%edge(:)=edge(:)
            else
                stop"**wrong size for xbrick edge component**"
            end if
        end if

        if(present(quad)) then
            if(size(quad)==size(this_xbrick%quad)) then
                this_xbrick%quad(:)=quad(:)
            else
                stop"**wrong size for xbrick quad component**"
            end if
        end if

        if(present(flo_node)) then
            if(allocated(this_xbrick%flo_node)) then
                if(size(flo_node)==size(this_xbrick%flo_node)) then
                    this_xbrick%flo_node(:)=flo_node(:)
                else
                    deallocate(this_xbrick%flo_node,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xbrick**"
                    allocate(this_xbrick%flo_node(size(flo_node)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xbrick**"
                    this_xbrick%flo_node(:)=flo_node(:)
                end if
            else
                allocate(this_xbrick%flo_node(size(flo_node)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xbrick**"
                this_xbrick%flo_node(:)=flo_node(:)
            end if
        end if

      end subroutine update_xbrick


    end module xbrick_module
