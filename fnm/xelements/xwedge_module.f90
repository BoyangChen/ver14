    module xwedge_module
      use parameter_module
      use subelement_module

      implicit none
      private

      integer,parameter :: nnode=6, nedge=9, ntri=2, nquad=3

      type, public :: wedge ! breakable wedge
        integer :: end_node(nnode)
      end type wedge

      type, public :: xwedge ! breakable wedge
        integer :: end_node(nnode) ! cnc to glb node arrays for accessing glb real(vertex) node numbers
        integer :: edge(nedge) ! cnc to glb edge arrays for accessing status var. and glb flo node numbers
        integer :: tri(ntri) ! cnc to glb tri arrays for accessing glb flo node numbers assoc. with the tri face
        integer :: quad(nquad)
        integer,allocatable :: flo_node(:) ! assigned to this volume, in addition to the flo nodes on edges and faces
        integer :: curr_status
        type(subelement),allocatable :: sub_elem(:) ! sub element connectivities
      end type xwedge


      interface empty
        module procedure empty_xwedge
      end interface

      interface update
        module procedure update_xwedge
      end interface

      public :: empty,update



      contains




      ! empty a breakable wedge
      subroutine empty_xwedge(this_xwedge)

      	type(xwedge),intent(inout) :: this_xwedge

        integer :: istat

        this_xwedge%end_node(:)=0
        this_xwedge%edge(:)=0
        this_xwedge%tri(:)=0
        this_xwedge%quad(:)=0
        this_xwedge%curr_status=0

        if(allocated(this_xwedge%flo_node)) then
        deallocate(this_xwedge%flo_node,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xwedge**"
        end if

      end subroutine empty_xwedge




      ! update a breakable wedge
      subroutine update_xwedge(this_xwedge,curr_status,end_node,&
      & edge,tri,quad,flo_node)

      	type(xwedge),intent(inout) :: this_xwedge
        integer,optional,intent(in) :: curr_status
        integer,optional,intent(in) :: end_node(:),edge(:),tri(:),quad(:)
        integer,optional,intent(in) :: flo_node(:)

        integer :: istat

        if(present(curr_status)) this_xwedge%curr_status=curr_status

        if(present(end_node)) then
            if(size(end_node)==size(this_xwedge%end_node)) then
                this_xwedge%end_node(:)=end_node(:)
            else
                stop"**wrong size for xwedge end_node component**"
            end if
        end if

        if(present(edge)) then
            if(size(edge)==size(this_xwedge%edge)) then
                this_xwedge%edge(:)=edge(:)
            else
                stop"**wrong size for xwedge edge component**"
            end if
        end if

        if(present(tri)) then
            if(size(tri)==size(this_xwedge%tri)) then
                this_xwedge%tri(:)=tri(:)
            else
                stop"**wrong size for xwedge tri component**"
            end if
        end if
        
        if(present(quad)) then
            if(size(quad)==size(this_xwedge%quad)) then
                this_xwedge%quad(:)=quad(:)
            else
                stop"**wrong size for xwedge quad component**"
            end if
        end if

        if(present(flo_node)) then
            if(allocated(this_xwedge%flo_node)) then
                if(size(flo_node)==size(this_xwedge%flo_node)) then
                    this_xwedge%flo_node(:)=flo_node(:)
                else
                    deallocate(this_xwedge%flo_node,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xwedge**"
                    allocate(this_xwedge%flo_node(size(flo_node)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xwedge**"
                    this_xwedge%flo_node(:)=flo_node(:)
                end if
            else
                allocate(this_xwedge%flo_node(size(flo_node)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xwedge**"
                this_xwedge%flo_node(:)=flo_node(:)
            end if
        end if

      end subroutine update_xwedge


    end module xwedge_module
