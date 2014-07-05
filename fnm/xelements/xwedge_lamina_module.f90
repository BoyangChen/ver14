    module xwedge_lamina_module
      use parameter_module
      use subelement_module

      implicit none
      private

      integer,parameter :: nnode=6, nedge=6, ntri=2, nquad=0

      type, public :: xwedge_lamina ! breakable wedge
        integer :: end_node(nnode) ! cnc to glb node arrays for accessing glb real(vertex) node numbers
        integer :: edge(nedge) ! cnc to glb edge arrays for accessing status var. and glb flo node numbers
        integer :: tri(ntri) ! cnc to glb tri arrays for accessing glb flo node numbers assoc. with the tri face
        integer,allocatable :: flo_node(:) ! assigned to this volume, in addition to the flo nodes on edges and faces
        integer :: curr_status
        type(subelement),allocatable :: sub_elem(:) ! sub element connectivities
      end type xwedge_lamina
      
      type, public :: xwedge_laminate(nplyblock) ! parameterized derived type
        integer :: nplyblock
        type(xwedge_lamina) :: ply(nplyblock)
        type(xwedge_lamina) :: interlayer(nplyblock-1) ! topology is the same as lamina
      end type 


      interface empty
        module procedure empty_xwedge_lamina
      end interface

      interface update
        module procedure update_xwedge_lamina
      end interface

      public :: empty,update



      contains




      ! empty a breakable wedge
      subroutine empty_xwedge_lamina(this_xwedge_lamina)

      	type(xwedge_lamina),intent(inout) :: this_xwedge_lamina

        integer :: istat

        this_xwedge_lamina%end_node(:)=0
        this_xwedge_lamina%edge(:)=0
        this_xwedge_lamina%tri(:)=0
        this_xwedge_lamina%curr_status=0

        if(allocated(this_xwedge_lamina%flo_node)) then
        deallocate(this_xwedge_lamina%flo_node,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xwedge_lamina**"
        end if

      end subroutine empty_xwedge_lamina




      ! update a breakable wedge
      subroutine update_xwedge_lamina(this_xwedge_lamina,curr_status,end_node,&
      & edge,tri,flo_node)

      	type(xwedge_lamina),intent(inout) :: this_xwedge_lamina
        integer,optional,intent(in) :: curr_status
        integer,optional,intent(in) :: end_node(:),edge(:),tri(:)
        integer,optional,intent(in) :: flo_node(:)

        integer :: istat

        if(present(curr_status)) this_xwedge_lamina%curr_status=curr_status

        if(present(end_node)) then
            if(size(end_node)==size(this_xwedge_lamina%end_node)) then
                this_xwedge_lamina%end_node(:)=end_node(:)
            else
                stop"**wrong size for xwedge_lamina end_node component**"
            end if
        end if

        if(present(edge)) then
            if(size(edge)==size(this_xwedge_lamina%edge)) then
                this_xwedge_lamina%edge(:)=edge(:)
            else
                stop"**wrong size for xwedge_lamina edge component**"
            end if
        end if

        if(present(tri)) then
            if(size(tri)==size(this_xwedge_lamina%tri)) then
                this_xwedge_lamina%tri(:)=tri(:)
            else
                stop"**wrong size for xwedge_lamina tri component**"
            end if
        end if

        if(present(flo_node)) then
            if(allocated(this_xwedge_lamina%flo_node)) then
                if(size(flo_node)==size(this_xwedge_lamina%flo_node)) then
                    this_xwedge_lamina%flo_node(:)=flo_node(:)
                else
                    deallocate(this_xwedge_lamina%flo_node,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xwedge_lamina**"
                    allocate(this_xwedge_lamina%flo_node(size(flo_node)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xwedge_lamina**"
                    this_xwedge_lamina%flo_node(:)=flo_node(:)
                end if
            else
                allocate(this_xwedge_lamina%flo_node(size(flo_node)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xwedge_lamina**"
                this_xwedge_lamina%flo_node(:)=flo_node(:)
            end if
        end if

      end subroutine update_xwedge_lamina


    end module xwedge_lamina_module
