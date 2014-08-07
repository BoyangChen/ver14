      module output_module
      use parameter_module
      use lib_node_module
      use lib_elem_module
      use integration_point_module
      
      implicit none
      private
      
      ! define global variable for output directory
      character(len=dirlength), save :: outdir
      
      
      public :: outdir, output
      
      
      contains
      
      
      subroutine output(kstep,kinc,outdir)
         
        ! passed-in variables
        integer,intent(in)          :: kstep    ! current step number
        integer,intent(in)          :: kinc     ! increment number of current step
        character(len=dirlength),intent(in) :: outdir   ! output directory name
        
        ! output file variables
        integer                     :: outunit  ! output file unit
        character(len=dirlength)    :: outfile  ! output file name
        character(len=dirlength)    :: outnum   ! output increment number (embedded in the outfile name)
        character(len=10)           :: fmat, fmatcnc     ! format specs
      
        ! no. of nodes & elem in the mesh
        integer                     :: nnode, nelem
        
        ! no. of elems of each elem type
        integer                     :: ntri, nquad, ntetra, nwedge, nbrick, ncoh2d, ncoh3d6, ncoh3d8
        integer                     :: nsub2d, nsubtri, nsubquad, nsubcoh2d
        integer                     :: nsub3d, nsubwedge, nsubbrick, nsubcoh3d6, nsubcoh3d8
        integer                     :: nxbrick
        
        ! sub element type variable
        character(len=eltypelength) :: subtype
        
        ! nsize: size of vtk element output; elnode: no. nodes in each elem
        integer                     :: nsize, elnode

        ! connectivity of an elem extracted from lib_elem
        integer, allocatable        :: connec(:)
        
        ! nodal coordinates
        real(kind=dp), allocatable  :: x(:)     ! coordinates of nodes extracted from lib_node
        real(kind=dp)               :: x3d(3)   ! coordinates of a node in 3D
        
        ! nodal displacements
        real(kind=dp), allocatable  :: disp(:)  ! displacements of nodes extracted from lib_node
        real(kind=dp)               :: disp3d(3)! displacements of a node in 3D
        
        
        ! sub element arrays
        type(tri_element),allocatable   :: subtri(:)
        type(quad_element),allocatable  :: subquad(:)
        type(wedge_element),allocatable :: subwedge(:)
        type(brick_element),allocatable :: subbrick(:)
        type(coh2d_element),allocatable :: subcoh2d(:)
        type(coh3d6_element),allocatable :: subcoh3d6(:)
        type(coh3d8_element),allocatable :: subcoh3d8(:)
        type(sub2d_element),allocatable :: sub2d(:)
        type(sub3d_element),allocatable :: sub3d(:)
        
        
        ! integration points
        type(integration_point), allocatable :: igpnt(:) ! intg point array
        
        ! stress and strain
        real(kind=dp), allocatable  :: sig(:), eps(:) ! stress & strain arrays extracted from lib_elem ig pnt
        real(kind=dp)               :: sigtsr(3,3), epstsr(3,3) ! stress & strain tensors for vtk output
        
        ! damage/failure variables
        real(kind=dp)               :: fvar    ! temporary failure variable for scalar outpt
        type(sdv_array),allocatable :: fsdv(:)  ! failure variables extracted from ig point sdv array
        
        ! counters
        integer                     :: i, j, l, m, n

      
      
      
      




      
        ! -----------------------------------------------------------------!
        !                       initialize variables
        ! -----------------------------------------------------------------!
        
        outunit=0; outfile=''; outnum=''; fmat=''; subtype=''
        nnode=0; nelem=0
        ntri=0; nquad=0; ntetra=0; nwedge=0; nbrick=0
        ncoh2d=0; ncoh3d6=0; ncoh3d8=0
        nsub2d=0; nsubtri=0; nsubquad=0; nsubcoh2d=0
        nsub3d=0; nsubwedge=0; nsubbrick=0; nsubcoh3d6=0; nsubcoh3d8=0
        nxbrick=0
        nsize=0; elnode=0
        x3d=zero; disp3d=zero
        sigtsr=zero; epstsr=zero
        fvar=zero
        i=0; j=0; l=0; m=0; n=0
        
        ! obtain nnode value from glb libraries
        nnode=size(lib_node)
      
        ! set format for integer output
        fmat='(i5.5)'   ! for increment no.
        fmatcnc='(i10)'  ! for connec node no.
      
        ! write the increment number as a character and store in outnum
        write(outnum,fmat) kinc
        
        
        ! create the output file name
        !~outfile=trim(outdir)//'/outputs/'//trim(outnum)//'.vtk'
        outfile=trim(outdir)//'fnm-'//trim(outnum)//'.vtk'
        outfile=trim(outfile)
        
        ! open the outfile
        open(newunit(outunit), file=outfile,status="replace")
        
        
        ! write header
        write(outunit,'(a)')'# vtk DataFile Version 3.1'
        write(outunit,'(a)')'for Floating Node Method output'
        
        ! write vtk format
        write(outunit,'(a)')'ASCII'      
        
        ! write vtk data type
        write(outunit,'(a)')'DATASET UNSTRUCTURED_GRID'      
        
  





      
        
        
        ! -----------------------------------------------------------------!
        !                     write nodes
        ! -----------------------------------------------------------------!
        
        write(outunit,'(a, i10, a)')'POINTS ',nnode,' FLOAT'    
        do i=1,nnode
            ! empty x3d array
            x3d=zero
            ! extract nodal coords from lib_node
            call extract(lib_node(i),x=x)
            ! pass nodal coords to x3d array
            if(allocated(x)) x3d(1:size(x))=x(:)
            ! write x3d array into output file
            write(outunit,*) x3d(1),x3d(2),x3d(3)
        end do            
        write(outunit,'(a)')''
        
        
        
        





        
        ! -----------------------------------------------------------------!
        !                     write elements 
        !       (order matters: new elem type must join the queue)
        ! -----------------------------------------------------------------!
        
        
        !                   write elements' connec (order matters)
        
        ! get no. of elems of each type
        if(allocated(lib_tri))      ntri=size(lib_tri) ! no. of tri elem in the mesh
        if(allocated(lib_quad))     nquad=size(lib_quad)
        if(allocated(lib_wedge))    nwedge=size(lib_wedge)
        if(allocated(lib_brick))    nbrick=size(lib_brick)
        if(allocated(lib_coh2d))    ncoh2d=size(lib_coh2d)
        if(allocated(lib_coh3d6))   ncoh3d6=size(lib_coh3d6)
        if(allocated(lib_coh3d8))   ncoh3d8=size(lib_coh3d8)
        
        if(allocated(lib_xbrick)) then
            nxbrick=size(lib_xbrick)
            do m=1,nxbrick
                call extract(lib_xbrick(m),subelem=sub3d)
                nsub3d=size(sub3d)
                do j=1,nsub3d
                    call extract(sub3d(j),eltype=subtype)
                    select case(subtype)
                        case('wedge')
                            nsubwedge=nsubwedge+1
                        case('brick')
                            nsubbrick=nsubbrick+1
                        case('coh3d6')
                            nsubcoh3d6=nsubcoh3d6+1
                        case('coh3d8')
                            nsubcoh3d8=nsubcoh3d8+1
                        case default
                            continue
                    end select
                end do
            end do
        end if
        ! .... and other elem types ....
        
        ! total no. of elems
        nelem=ntri+nquad+ntetra+nwedge+nbrick+ncoh2d+ncoh3d6+ncoh3d8+nsubwedge+nsubbrick+nsubcoh3d6+nsubcoh3d8
        
        ! calculate total no. of nodes to print; each row has 1+elnode no. of indices to print
        nsize=ntri*(1+3)+nquad*(1+4)+ntetra*(1+4)+nwedge*(1+6)+nbrick*(1+8) &
        &    +ncoh2d*(1+4)+ncoh3d6*(1+6)+ncoh3d8*(1+8)                      &
        &    +nsubtri*(1+3)+nsubquad*(1+4)+nsubcoh2d*(1+4)                  &
        &    +nsubwedge*(1+6)+nsubbrick*(1+8)+nsubcoh3d6*(1+6)+nsubcoh3d8*(1+8)
        
        ! write a summary of output
        write(outunit,'(a, i10, i10)')'CELLS ', nelem, nsize
        
        if(ntri > 0) then
            do i=1,ntri ! write each element's connec individually
                call extract(lib_tri(i),connec=connec) ! extract connec from lib_tri
                ! print connec in vtk; note that in vtk node no. starts from 0
                connec=connec-1
                write(outunit,*) 3,connec(1),connec(2),connec(3) 
            end do
        end if
        
        if(nquad > 0) then
            do i=1,nquad ! write each element's connec individually
                call extract(lib_quad(i),connec=connec) ! extract connec from lib_tri
                ! print connec in vtk; note that in vtk node no. starts from 0
                connec=connec-1
                write(outunit,*) 4,connec(1),connec(2),connec(3),connec(4) 
            end do
        end if

        if(nwedge > 0) then
            do i=1,nwedge ! write each element's connec individually
                call extract(lib_wedge(i),connec=connec) ! extract connec from lib_tri
                ! print connec in vtk; note that in vtk node no. starts from 0
                connec=connec-1
                write(outunit,*) 6,connec(1),connec(2),connec(3),connec(4),connec(5),connec(6)
            end do
        end if
        
        if(nbrick > 0) then
            do i=1,nbrick ! write each element's connec individually
                call extract(lib_brick(i),connec=connec) ! extract connec from lib_tri
                ! print connec in vtk; note that in vtk node no. starts from 0
                connec=connec-1
                write(outunit,*) 8,connec(1),connec(2),connec(3),connec(4),connec(5),connec(6),connec(7),connec(8)
            end do
        end if
        
        if(ncoh2d > 0) then
            do i=1,ncoh2d ! write each element's connec individually
                call extract(lib_coh2d(i),connec=connec) ! extract connec from lib_tri
                ! print connec in vtk; note that in vtk node no. starts from 0
                connec=connec-1
                write(outunit,*) 4,connec(1),connec(2),connec(3),connec(4) 
            end do
        end if
        
        if(ncoh3d6 > 0) then
            do i=1,ncoh3d6 ! write each element's connec individually
                call extract(lib_coh3d6(i),connec=connec) ! extract connec from lib_tri
                ! print connec in vtk; note that in vtk node no. starts from 0
                connec=connec-1
                write(outunit,*) 6,connec(1),connec(2),connec(3),connec(4),connec(5),connec(6)
            end do
        end if
        
        if(ncoh3d8 > 0) then
            do i=1,ncoh3d8 ! write each element's connec individually
                call extract(lib_coh3d8(i),connec=connec) ! extract connec from lib_tri
                ! print connec in vtk; note that in vtk node no. starts from 0
                connec=connec-1
                write(outunit,*) 8,connec(1),connec(2),connec(3),connec(4),connec(5),connec(6),connec(7),connec(8)
            end do
        end if
        
        
        ! ------ write x elements ---------
        
        
        if(nxbrick > 0) then
            do m=1,nxbrick
                call extract(lib_xbrick(m),subelem=sub3d)
                nsub3d=size(sub3d)
                if(nsub3d > 0) then
                    do i=1,nsub3d ! write each element's connec individually
                        call extract(sub3d(i),glbcnc=connec) ! extract connec
                        ! print connec in vtk; note that in vtk node no. starts from 0
                        connec=connec-1
                        write(outunit,fmatcnc,advance="no") size(connec)
                        do j=1,size(connec)
                            write(outunit,fmatcnc,advance="no") connec(j)
                        end do
                        write(outunit,'(a)')''
                    end do
                end if
            end do
        end if
        
        write(outunit,'(a)')''


         
        
        !                   write elements' types (order matters)
        
        write(outunit,'(a, i10)')'CELL_TYPES ', nelem
        
        if (ntri > 0) then
            do i=1,ntri
                write(outunit,'(i2)') 5 ! 5 for tri
            end do 
        end if
        
        if (nquad > 0) then
            do i=1,nquad
                write(outunit,'(i2)') 9 ! 9 for quad
            end do 
        end if
        
        if (ntetra > 0) then
            do i=1,ntetra
                write(outunit,'(i2)') 10 ! 10 for tetra
            end do 
        end if
        
        if (nwedge > 0) then
            do i=1,nwedge
                write(outunit,'(i2)') 13 ! 13 for wedge
            end do 
        end if
        
        if (nbrick > 0) then
            do i=1,nbrick
                write(outunit,'(i2)') 12 ! 12 for brick
            end do 
        end if
        
        if (ncoh2d > 0) then
            do i=1,ncoh2d
                write(outunit,'(i2)') 9 ! 9 for coh2d
            end do 
        end if
        
        if (ncoh3d6 > 0) then
            do i=1,ncoh3d6
                write(outunit,'(i2)') 13 ! 13 for coh3d6
            end do 
        end if
        
        if (ncoh3d8 > 0) then
            do i=1,ncoh3d8
                write(outunit,'(i2)') 12 ! 12 for coh3d8
            end do 
        end if
        

        if(nxbrick > 0) then
            do m=1,nxbrick
                call extract(lib_xbrick(m),subelem=sub3d)
                nsub3d=size(sub3d)     
                if (nsub3d > 0) then
                    do i=1,nsub3d
                        call extract(sub3d(i),eltype=subtype)
                        select case(subtype)
                            case('wedge')
                                write(outunit,'(i2)') 13 ! 13 for wedge
                            case('brick')
                                write(outunit,'(i2)') 12 ! 12 for brick
                            case('coh3d6')
                                write(outunit,'(i2)') 13 ! 13 for coh3d6
                            case('coh3d8')
                                write(outunit,'(i2)') 12 ! 12 for coh3d8
                            case default
                                continue
                        end select    
                    end do
                end if
            end do
        end if
     
        write(outunit,'(a)')''










        ! -----------------------------------------------------------------!
        !                     write displacements
        ! -----------------------------------------------------------------!


        ! write nodal varibales (disp.)
        write(outunit,'(a, i10)')'POINT_DATA ', nnode
        write(outunit,'(a)')'VECTORS displacement float'
        !~write(outunit,'(a)')'LOOKUP_TABLE default' ! ** this is only for scalar **
        
        do i=1, nnode
            disp3d=zero ! empty disp3d for reuse
            call extract(lib_node(i),u=disp)
            if(allocated(disp)) disp3d(1:size(disp))=disp(:)
            write(outunit,*) disp3d(1),disp3d(2),disp3d(3)
        end do            
        write(outunit,'(a)')''
        
        
        
        
   





        ! -----------------------------------------------------------------!
        !                     write stress (order matters)
        ! -----------------------------------------------------------------!     
        
        ! write element stress
        write(outunit,'(a, i10)')'CELL_DATA ', nelem
        write(outunit,'(a)')'TENSORS stress float'
        
        if (ntri > 0) then
            do i=1,ntri
                sigtsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_tri(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),stress=sig)                   
                    sigtsr(1,1)=sigtsr(1,1)+sig(1)
                    sigtsr(2,2)=sigtsr(2,2)+sig(2)
                    sigtsr(1,2)=sigtsr(1,2)+sig(3)
                    sigtsr(2,1)=sigtsr(2,1)+sig(3)                         
                end do 
                ! average stress in the element
                sigtsr=sigtsr/size(igpnt)  
                do l=1,3
                    write(outunit,*) sigtsr(1,l), sigtsr(2,l), sigtsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element                
            end do 
        end if
        
        if (nquad > 0) then
            do i=1,nquad
                sigtsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_quad(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),stress=sig)                   
                    sigtsr(1,1)=sigtsr(1,1)+sig(1)
                    sigtsr(2,2)=sigtsr(2,2)+sig(2)
                    sigtsr(1,2)=sigtsr(1,2)+sig(3)
                    sigtsr(2,1)=sigtsr(2,1)+sig(3)                         
                end do 
                ! average stress in the element
                sigtsr=sigtsr/size(igpnt)  
                do l=1,3
                    write(outunit,*) sigtsr(1,l), sigtsr(2,l), sigtsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element                
            end do 
        end if
        
        if (nwedge > 0) then
            do i=1,nwedge
                sigtsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_wedge(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),stress=sig)                   
                    sigtsr(1,1)=sigtsr(1,1)+sig(1)
                    sigtsr(2,2)=sigtsr(2,2)+sig(2)
                    sigtsr(3,3)=sigtsr(3,3)+sig(3)
                    sigtsr(1,2)=sigtsr(1,2)+sig(4)
                    sigtsr(1,3)=sigtsr(1,3)+sig(5)
                    sigtsr(2,3)=sigtsr(2,3)+sig(6)
                    sigtsr(2,1)=sigtsr(2,1)+sig(4)
                    sigtsr(3,1)=sigtsr(3,1)+sig(5)
                    sigtsr(3,2)=sigtsr(3,2)+sig(6)   
                end do 
                ! average stress in the element
                sigtsr=sigtsr/size(igpnt)  
                do l=1,3
                    write(outunit,*) sigtsr(1,l), sigtsr(2,l), sigtsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element                
            end do 
        end if
        
        if (nbrick > 0) then
            do i=1,nbrick
                sigtsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_brick(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),stress=sig)                   
                    sigtsr(1,1)=sigtsr(1,1)+sig(1)
                    sigtsr(2,2)=sigtsr(2,2)+sig(2)
                    sigtsr(3,3)=sigtsr(3,3)+sig(3)
                    sigtsr(1,2)=sigtsr(1,2)+sig(4)
                    sigtsr(1,3)=sigtsr(1,3)+sig(5)
                    sigtsr(2,3)=sigtsr(2,3)+sig(6)
                    sigtsr(2,1)=sigtsr(2,1)+sig(4)
                    sigtsr(3,1)=sigtsr(3,1)+sig(5)
                    sigtsr(3,2)=sigtsr(3,2)+sig(6)   
                end do 
                ! average stress in the element
                sigtsr=sigtsr/size(igpnt)  
                do l=1,3
                    write(outunit,*) sigtsr(1,l), sigtsr(2,l), sigtsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element                
            end do 
        end if
        
        if (ncoh2d > 0) then
            do i=1,ncoh2d
                sigtsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh2d(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),stress=sig)  
                    !epstsr(1,1)
                    sigtsr(2,2)=sigtsr(2,2)+sig(1)
                    sigtsr(1,2)=sigtsr(1,2)+sig(2)
                    sigtsr(2,1)=sigtsr(2,1)+sig(2)                           
                end do 
                ! average strain in the element
                sigtsr=sigtsr/size(igpnt)
                do l=1,3
                    write(outunit,*) sigtsr(1,l), sigtsr(2,l), sigtsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element               
            end do 
        end if
        
        if (ncoh3d6 > 0) then
            do i=1,ncoh3d6
                sigtsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh3d6(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),stress=sig)
                    sigtsr(3,3)=sigtsr(3,3)+sig(1)
                    sigtsr(1,3)=sigtsr(1,3)+sig(2)
                    sigtsr(3,1)=sigtsr(3,1)+sig(2) 
                    sigtsr(2,3)=sigtsr(2,3)+sig(3)
                    sigtsr(3,2)=sigtsr(3,2)+sig(3)
                end do 
                ! average strain in the element
                sigtsr=sigtsr/size(igpnt)
                do l=1,3
                    write(outunit,*) sigtsr(1,l), sigtsr(2,l), sigtsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element               
            end do 
        end if
        
        if (ncoh3d8 > 0) then
            do i=1,ncoh3d8
                sigtsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh3d8(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),stress=sig)  
                    sigtsr(3,3)=sigtsr(3,3)+sig(1)
                    sigtsr(1,3)=sigtsr(1,3)+sig(2)
                    sigtsr(3,1)=sigtsr(3,1)+sig(2) 
                    sigtsr(2,3)=sigtsr(2,3)+sig(3)
                    sigtsr(3,2)=sigtsr(3,2)+sig(3)                           
                end do 
                ! average strain in the element
                sigtsr=sigtsr/size(igpnt)
                do l=1,3
                    write(outunit,*) sigtsr(1,l), sigtsr(2,l), sigtsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element               
            end do 
        end if
        
        

        if(nxbrick > 0) then
            do m=1,nxbrick
                call extract(lib_xbrick(m),subelem=sub3d)
                nsub3d=size(sub3d)        
                if (nsub3d > 0) then
                    do i=1,nsub3d
                        call extract(sub3d(i),eltype=subtype)
                        select case(subtype)                
                            case('wedge')
                                ! extract wedge sub elem
                                call extract(sub3d(i),wedge=subwedge)
                                sigtsr=zero ! empty sig & eps tensor for reuse
                                call extract(subwedge(1),ig_point=igpnt)
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),stress=sig)                   
                                    sigtsr(1,1)=sigtsr(1,1)+sig(1)
                                    sigtsr(2,2)=sigtsr(2,2)+sig(2)
                                    sigtsr(3,3)=sigtsr(3,3)+sig(3)
                                    sigtsr(1,2)=sigtsr(1,2)+sig(4)
                                    sigtsr(1,3)=sigtsr(1,3)+sig(5)
                                    sigtsr(2,3)=sigtsr(2,3)+sig(6)
                                    sigtsr(2,1)=sigtsr(2,1)+sig(4)
                                    sigtsr(3,1)=sigtsr(3,1)+sig(5)
                                    sigtsr(3,2)=sigtsr(3,2)+sig(6)
                                end do 
                                ! average stress in the element
                                sigtsr=sigtsr/size(igpnt)  
                                do l=1,3
                                    write(outunit,*) sigtsr(1,l), sigtsr(2,l), sigtsr(3,l)
                                end do
                                write(outunit,'(a)')'' ! separate from next element
                                
                            case('brick')
                                ! extract brick sub elem
                                call extract(sub3d(i),brick=subbrick)
                                sigtsr=zero ! empty sig & eps tensor for reuse
                                call extract(subbrick(1),ig_point=igpnt)
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),stress=sig)                   
                                    sigtsr(1,1)=sigtsr(1,1)+sig(1)
                                    sigtsr(2,2)=sigtsr(2,2)+sig(2)
                                    sigtsr(3,3)=sigtsr(3,3)+sig(3)
                                    sigtsr(1,2)=sigtsr(1,2)+sig(4)
                                    sigtsr(1,3)=sigtsr(1,3)+sig(5)
                                    sigtsr(2,3)=sigtsr(2,3)+sig(6)
                                    sigtsr(2,1)=sigtsr(2,1)+sig(4)
                                    sigtsr(3,1)=sigtsr(3,1)+sig(5)
                                    sigtsr(3,2)=sigtsr(3,2)+sig(6)
                                end do 
                                ! average stress in the element
                                sigtsr=sigtsr/size(igpnt)  
                                do l=1,3
                                    write(outunit,*) sigtsr(1,l), sigtsr(2,l), sigtsr(3,l)
                                end do
                                write(outunit,'(a)')'' ! separate from next element 
            
                            case('coh3d6')
                                ! extract coh3d6 sub elem
                                call extract(sub3d(i),coh3d6=subcoh3d6)
                                sigtsr=zero ! empty sig & eps tensor for reuse
                                call extract(subcoh3d6(1),ig_point=igpnt)
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),stress=sig)  
                                    sigtsr(3,3)=sigtsr(3,3)+sig(1)
                                    sigtsr(1,3)=sigtsr(1,3)+sig(2)
                                    sigtsr(3,1)=sigtsr(3,1)+sig(2) 
                                    sigtsr(2,3)=sigtsr(2,3)+sig(3)
                                    sigtsr(3,2)=sigtsr(3,2)+sig(3)
                                end do 
                                ! average strain in the element
                                sigtsr=sigtsr/size(igpnt)
                                do l=1,3
                                    write(outunit,*) sigtsr(1,l), sigtsr(2,l), sigtsr(3,l)
                                end do
                                write(outunit,'(a)')'' ! separate from next element 
                                
                            case('coh3d8')
                                ! extract coh3d8 sub elem
                                call extract(sub3d(i),coh3d8=subcoh3d8)
                                sigtsr=zero ! empty sig & eps tensor for reuse
                                call extract(subcoh3d8(1),ig_point=igpnt)
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),stress=sig)  
                                    sigtsr(3,3)=sigtsr(3,3)+sig(1)
                                    sigtsr(1,3)=sigtsr(1,3)+sig(2)
                                    sigtsr(3,1)=sigtsr(3,1)+sig(2) 
                                    sigtsr(2,3)=sigtsr(2,3)+sig(3)
                                    sigtsr(3,2)=sigtsr(3,2)+sig(3)
                                end do 
                                ! average strain in the element
                                sigtsr=sigtsr/size(igpnt)
                                do l=1,3
                                    write(outunit,*) sigtsr(1,l), sigtsr(2,l), sigtsr(3,l)
                                end do
                                write(outunit,'(a)')'' ! separate from next element
           
                            case default
                                continue                       
                        end select    
                    end do   
                end if
            end do
        end if
        
        
               
        
        
        ! -----------------------------------------------------------------!
        !                     write strain (order matters)
        ! -----------------------------------------------------------------!  
        
        ! write element strain
        write(outunit,'(a)')'TENSORS strain float'
        
        if (ntri > 0) then
            do i=1,ntri
                epstsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_tri(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),strain=eps)  
                    epstsr(1,1)=epstsr(1,1)+eps(1)
                    epstsr(2,2)=epstsr(2,2)+eps(2)
                    epstsr(1,2)=epstsr(1,2)+eps(3)
                    epstsr(2,1)=epstsr(2,1)+eps(3)                           
                end do 
                ! average strain in the element
                epstsr=epstsr/size(igpnt)
                do l=1,3
                    write(outunit,*) epstsr(1,l), epstsr(2,l), epstsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element               
            end do 
        end if
        
        if (nquad > 0) then
            do i=1,nquad
                epstsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_quad(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),strain=eps)  
                    epstsr(1,1)=epstsr(1,1)+eps(1)
                    epstsr(2,2)=epstsr(2,2)+eps(2)
                    epstsr(1,2)=epstsr(1,2)+eps(3)
                    epstsr(2,1)=epstsr(2,1)+eps(3)                           
                end do 
                ! average strain in the element
                epstsr=epstsr/size(igpnt)
                do l=1,3
                    write(outunit,*) epstsr(1,l), epstsr(2,l), epstsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element               
            end do 
        end if
        
        if (nwedge > 0) then
            do i=1,nwedge
                epstsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_wedge(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),strain=eps)                   
                    epstsr(1,1)=epstsr(1,1)+eps(1)
                    epstsr(2,2)=epstsr(2,2)+eps(2)
                    epstsr(3,3)=epstsr(3,3)+eps(3)
                    epstsr(1,2)=epstsr(1,2)+eps(4)
                    epstsr(1,3)=epstsr(1,3)+eps(5)
                    epstsr(2,3)=epstsr(2,3)+eps(6)
                    epstsr(2,1)=epstsr(2,1)+eps(4)
                    epstsr(3,1)=epstsr(3,1)+eps(5)
                    epstsr(3,2)=epstsr(3,2)+eps(6)   
                end do 
                ! average stress in the element
                epstsr=epstsr/size(igpnt)  
                do l=1,3
                    write(outunit,*) epstsr(1,l), epstsr(2,l), epstsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element                
            end do 
        end if
        
        if (nbrick > 0) then
            do i=1,nbrick
                epstsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_brick(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),strain=eps)                   
                    epstsr(1,1)=epstsr(1,1)+eps(1)
                    epstsr(2,2)=epstsr(2,2)+eps(2)
                    epstsr(3,3)=epstsr(3,3)+eps(3)
                    epstsr(1,2)=epstsr(1,2)+eps(4)
                    epstsr(1,3)=epstsr(1,3)+eps(5)
                    epstsr(2,3)=epstsr(2,3)+eps(6)
                    epstsr(2,1)=epstsr(2,1)+eps(4)
                    epstsr(3,1)=epstsr(3,1)+eps(5)
                    epstsr(3,2)=epstsr(3,2)+eps(6)   
                end do 
                ! average stress in the element
                epstsr=epstsr/size(igpnt)  
                do l=1,3
                    write(outunit,*) epstsr(1,l), epstsr(2,l), epstsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element                
            end do 
        end if    
        
        if (ncoh2d > 0) then
            do i=1,ncoh2d
                epstsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh2d(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),strain=eps)  
                    !epstsr(1,1)
                    epstsr(2,2)=epstsr(2,2)+eps(1)
                    epstsr(1,2)=epstsr(1,2)+eps(2)
                    epstsr(2,1)=epstsr(2,1)+eps(2)                           
                end do 
                ! average strain in the element
                epstsr=epstsr/size(igpnt)
                do l=1,3
                    write(outunit,*) epstsr(1,l), epstsr(2,l), epstsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element               
            end do 
        end if
        
        if (ncoh3d6 > 0) then
            do i=1,ncoh3d6
                epstsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh3d6(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),strain=eps)  
                    epstsr(3,3)=epstsr(3,3)+eps(1)
                    epstsr(1,3)=epstsr(1,3)+eps(2)
                    epstsr(3,1)=epstsr(3,1)+eps(2) 
                    epstsr(2,3)=epstsr(2,3)+eps(3)
                    epstsr(3,2)=epstsr(3,2)+eps(3)                           
                end do 
                ! average strain in the element
                epstsr=epstsr/size(igpnt)
                do l=1,3
                    write(outunit,*) epstsr(1,l), epstsr(2,l), epstsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element               
            end do 
        end if
        
        if (ncoh3d8 > 0) then
            do i=1,ncoh3d8
                epstsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh3d8(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),strain=eps)  
                    epstsr(3,3)=epstsr(3,3)+eps(1)
                    epstsr(1,3)=epstsr(1,3)+eps(2)
                    epstsr(3,1)=epstsr(3,1)+eps(2) 
                    epstsr(2,3)=epstsr(2,3)+eps(3)
                    epstsr(3,2)=epstsr(3,2)+eps(3)                           
                end do 
                ! average strain in the element
                epstsr=epstsr/size(igpnt)
                do l=1,3
                    write(outunit,*) epstsr(1,l), epstsr(2,l), epstsr(3,l)
                end do
                write(outunit,'(a)')'' ! separate from next element               
            end do 
        end if



        if(nxbrick > 0) then
            do m=1,nxbrick
                call extract(lib_xbrick(m),subelem=sub3d)
                nsub3d=size(sub3d)        
                if (nsub3d > 0) then
                    do i=1,nsub3d
                        call extract(sub3d(i),eltype=subtype)
                        select case(subtype)                
                            case('wedge')
                                ! extract wedge sub elem
                                call extract(sub3d(i),wedge=subwedge)
                                epstsr=zero ! empty eps & eps tensor for reuse
                                call extract(subwedge(1),ig_point=igpnt)
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),strain=eps)                   
                                    epstsr(1,1)=epstsr(1,1)+eps(1)
                                    epstsr(2,2)=epstsr(2,2)+eps(2)
                                    epstsr(3,3)=epstsr(3,3)+eps(3)
                                    epstsr(1,2)=epstsr(1,2)+eps(4)
                                    epstsr(1,3)=epstsr(1,3)+eps(5)
                                    epstsr(2,3)=epstsr(2,3)+eps(6)
                                    epstsr(2,1)=epstsr(2,1)+eps(4)
                                    epstsr(3,1)=epstsr(3,1)+eps(5)
                                    epstsr(3,2)=epstsr(3,2)+eps(6)
                                end do 
                                ! average strain in the element
                                epstsr=epstsr/size(igpnt)  
                                do l=1,3
                                    write(outunit,*) epstsr(1,l), epstsr(2,l), epstsr(3,l)
                                end do
                                write(outunit,'(a)')'' ! separate from next element
                                
                            case('brick')
                                ! extract brick sub elem
                                call extract(sub3d(i),brick=subbrick)
                                epstsr=zero ! empty eps & eps tensor for reuse
                                call extract(subbrick(1),ig_point=igpnt)
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),strain=eps)                   
                                    epstsr(1,1)=epstsr(1,1)+eps(1)
                                    epstsr(2,2)=epstsr(2,2)+eps(2)
                                    epstsr(3,3)=epstsr(3,3)+eps(3)
                                    epstsr(1,2)=epstsr(1,2)+eps(4)
                                    epstsr(1,3)=epstsr(1,3)+eps(5)
                                    epstsr(2,3)=epstsr(2,3)+eps(6)
                                    epstsr(2,1)=epstsr(2,1)+eps(4)
                                    epstsr(3,1)=epstsr(3,1)+eps(5)
                                    epstsr(3,2)=epstsr(3,2)+eps(6)
                                end do 
                                ! average strain in the element
                                epstsr=epstsr/size(igpnt)  
                                do l=1,3
                                    write(outunit,*) epstsr(1,l), epstsr(2,l), epstsr(3,l)
                                end do
                                write(outunit,'(a)')'' ! separate from next element 
            
                            case('coh3d6')
                                ! extract coh3d6 sub elem
                                call extract(sub3d(i),coh3d6=subcoh3d6)
                                epstsr=zero ! empty eps & eps tensor for reuse
                                call extract(subcoh3d6(1),ig_point=igpnt)
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),strain=eps)  
                                    epstsr(3,3)=epstsr(3,3)+eps(1)
                                    epstsr(1,3)=epstsr(1,3)+eps(2)
                                    epstsr(3,1)=epstsr(3,1)+eps(2) 
                                    epstsr(2,3)=epstsr(2,3)+eps(3)
                                    epstsr(3,2)=epstsr(3,2)+eps(3)
                                end do 
                                ! average strain in the element
                                epstsr=epstsr/size(igpnt)
                                do l=1,3
                                    write(outunit,*) epstsr(1,l), epstsr(2,l), epstsr(3,l)
                                end do
                                write(outunit,'(a)')'' ! separate from next element 
                                
                            case('coh3d8')
                                ! extract coh3d8 sub elem
                                call extract(sub3d(i),coh3d8=subcoh3d8)
                                epstsr=zero ! empty eps & eps tensor for reuse
                                call extract(subcoh3d8(1),ig_point=igpnt)
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),strain=eps)  
                                    epstsr(3,3)=epstsr(3,3)+eps(1)
                                    epstsr(1,3)=epstsr(1,3)+eps(2)
                                    epstsr(3,1)=epstsr(3,1)+eps(2) 
                                    epstsr(2,3)=epstsr(2,3)+eps(3)
                                    epstsr(3,2)=epstsr(3,2)+eps(3)
                                end do 
                                ! average strain in the element
                                epstsr=epstsr/size(igpnt)
                                do l=1,3
                                    write(outunit,*) epstsr(1,l), epstsr(2,l), epstsr(3,l)
                                end do
                                write(outunit,'(a)')'' ! separate from next element
           
                            case default
                                continue                       
                        end select    
                    end do   
                end if
            end do
        end if





        ! -----------------------------------------------------------------!
        !                     write failure status
        ! -----------------------------------------------------------------! 
        
        ! write element failure status
        write(outunit,'(a)')'SCALARS fstat float'
        write(outunit,'(a)')'LOOKUP_TABLE default'
        
        if (ntri > 0) then
            do i=1,ntri
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_tri(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%i)) fvar=fvar+fsdv(2)%i(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (nquad > 0) then
            do i=1,nquad
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_quad(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%i)) fvar=fvar+fsdv(2)%i(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (nwedge > 0) then
            do i=1,nwedge
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_wedge(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%i)) fvar=fvar+fsdv(2)%i(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (nbrick > 0) then
            do i=1,nbrick
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_brick(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%i)) fvar=fvar+fsdv(2)%i(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (ncoh2d > 0) then
            do i=1,ncoh2d
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh2d(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%i)) fvar=fvar+fsdv(2)%i(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        

        if (ncoh3d6 > 0) then
            do i=1,ncoh3d6
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh3d6(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%i)) fvar=fvar+fsdv(2)%i(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        

        if (ncoh3d8 > 0) then
            do i=1,ncoh3d8
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh3d8(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%i)) fvar=fvar+fsdv(2)%i(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        



        if(nxbrick > 0) then
            do m=1,nxbrick
                call extract(lib_xbrick(m),subelem=sub3d)
                nsub3d=size(sub3d)
                if (nsub3d > 0) then
                    do i=1,nsub3d
                        call extract(sub3d(i),eltype=subtype)
                        select case(subtype)
                            case('wedge')
                                call extract(sub3d(i),wedge=subwedge)
                                call extract(subwedge(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%i)) fvar=fvar+fsdv(2)%i(1)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case('brick')
                                call extract(sub3d(i),brick=subbrick)
                                call extract(subbrick(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%i)) fvar=fvar+fsdv(2)%i(1)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case('coh3d6')
                                call extract(sub3d(i),coh3d6=subcoh3d6)
                                call extract(subcoh3d6(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%i)) fvar=fvar+fsdv(2)%i(1)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case('coh3d8')
                                call extract(sub3d(i),coh3d8=subcoh3d8)
                                call extract(subcoh3d8(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%i)) fvar=fvar+fsdv(2)%i(1)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case default
                                continue
                        end select    
                    end do
                end if
            end do
        end if





        ! -----------------------------------------------------------------!
        !                     write damage variable
        ! -----------------------------------------------------------------! 
        
        ! write element damage variable
        write(outunit,'(a)')'SCALARS dm float'
        write(outunit,'(a)')'LOOKUP_TABLE default'
        
        
        if (ntri > 0) then
            do i=1,ntri
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_tri(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (nquad > 0) then
            do i=1,nquad
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_quad(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (nwedge > 0) then
            do i=1,nwedge
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_wedge(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (nbrick > 0) then
            do i=1,nbrick
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_brick(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
                
        
        if (ncoh2d > 0) then
            do i=1,ncoh2d
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh2d(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        

        if (ncoh3d6 > 0) then
            do i=1,ncoh3d6
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh3d6(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        

        if (ncoh3d8 > 0) then
            do i=1,ncoh3d8
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh3d8(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(1)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        



        if(nxbrick > 0) then
            do m=1,nxbrick
                call extract(lib_xbrick(m),subelem=sub3d)
                nsub3d=size(sub3d)
                if (nsub3d > 0) then
                    do i=1,nsub3d
                        call extract(sub3d(i),eltype=subtype)
                        select case(subtype)
                            case('wedge')
                                call extract(sub3d(i),wedge=subwedge)
                                call extract(subwedge(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(1)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case('brick')
                                call extract(sub3d(i),brick=subbrick)
                                call extract(subbrick(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(1)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case('coh3d6')
                                call extract(sub3d(i),coh3d6=subcoh3d6)
                                call extract(subcoh3d6(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(1)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case('coh3d8')
                                call extract(sub3d(i),coh3d8=subcoh3d8)
                                call extract(subcoh3d8(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(1)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case default
                                continue
                        end select    
                    end do
                end if
            end do
        end if









        ! -----------------------------------------------------------------!
        !                     write cohesive law variable u0
        ! -----------------------------------------------------------------!

        ! write element damage variable
        write(outunit,'(a)')'SCALARS u0 float'
        write(outunit,'(a)')'LOOKUP_TABLE default'
        
        if (ntri > 0) then
            do i=1,ntri
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_tri(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(2)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (nquad > 0) then
            do i=1,nquad
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_quad(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(2)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (nwedge > 0) then
            do i=1,nwedge
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_wedge(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(2)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (nbrick > 0) then
            do i=1,nbrick
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_brick(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(2)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
               
        
        if (ncoh2d > 0) then
            do i=1,ncoh2d
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh2d(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(2)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
       

        if (ncoh3d6 > 0) then
            do i=1,ncoh3d6
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh3d6(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(2)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        

        if (ncoh3d8 > 0) then
            do i=1,ncoh3d8
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh3d8(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(2)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        



        if(nxbrick > 0) then
            do m=1,nxbrick
                call extract(lib_xbrick(m),subelem=sub3d)
                nsub3d=size(sub3d)
                if (nsub3d > 0) then
                    do i=1,nsub3d
                        call extract(sub3d(i),eltype=subtype)
                        select case(subtype)
                            case('wedge')
                                call extract(sub3d(i),wedge=subwedge)
                                call extract(subwedge(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(2)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case('brick')
                                call extract(sub3d(i),brick=subbrick)
                                call extract(subbrick(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(2)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case('coh3d6')
                                call extract(sub3d(i),coh3d6=subcoh3d6)
                                call extract(subcoh3d6(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(2)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case('coh3d8')
                                call extract(sub3d(i),coh3d8=subcoh3d8)
                                call extract(subcoh3d8(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(2)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case default
                                continue
                        end select    
                    end do
                end if
            end do
        end if








        ! -----------------------------------------------------------------!
        !                     write cohesive law variable uf
        ! -----------------------------------------------------------------!

        ! write element damage variable
        write(outunit,'(a)')'SCALARS uf float'
        write(outunit,'(a)')'LOOKUP_TABLE default'
        
        if (ntri > 0) then
            do i=1,ntri
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_tri(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(3)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (nquad > 0) then
            do i=1,nquad
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_quad(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(3)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (nwedge > 0) then
            do i=1,nwedge
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_wedge(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(3)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        
        
        if (nbrick > 0) then
            do i=1,nbrick
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_brick(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(3)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
     
        
        if (ncoh2d > 0) then
            do i=1,ncoh2d
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh2d(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(3)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do
        write(outunit,'(a)')''
        end if


        if (ncoh3d6 > 0) then
            do i=1,ncoh3d6
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh3d6(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(3)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''   
        end if
        

        if (ncoh3d8 > 0) then
            do i=1,ncoh3d8
                fvar=zero ! empty sig & eps tensor for reuse
                call extract(lib_coh3d8(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),sdv=fsdv)
                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(3)
                end do 
                ! average strain in the element
                fvar=fvar/size(igpnt)
                write(outunit,*) fvar
                !write(outunit,'(a)')'' ! separate from next element               
            end do 
        write(outunit,'(a)')''
        end if
        



        if(nxbrick > 0) then
            do m=1,nxbrick
                call extract(lib_xbrick(m),subelem=sub3d)
                nsub3d=size(sub3d)
                if (nsub3d > 0) then
                    do i=1,nsub3d
                        call extract(sub3d(i),eltype=subtype)
                        select case(subtype)
                            case('wedge')
                                call extract(sub3d(i),wedge=subwedge)
                                call extract(subwedge(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(3)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case('brick')
                                call extract(sub3d(i),brick=subbrick)
                                call extract(subbrick(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(3)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case('coh3d6')
                                call extract(sub3d(i),coh3d6=subcoh3d6)
                                call extract(subcoh3d6(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(3)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case('coh3d8')
                                call extract(sub3d(i),coh3d8=subcoh3d8)
                                call extract(subcoh3d8(1),ig_point=igpnt)
                                fvar=zero
                                do j=1,size(igpnt)
                                    call extract(igpnt(j),sdv=fsdv)
                                    if(allocated(fsdv).and.allocated(fsdv(2)%r)) fvar=fvar+fsdv(2)%r(3)
                                end do 
                                ! average strain in the element
                                fvar=fvar/size(igpnt)
                                write(outunit,*) fvar
                                
                            case default
                                continue
                        end select    
                    end do
                end if
            end do
        end if


        


        if(allocated(connec)) deallocate(connec)
        if(allocated(x)) deallocate(x)
        if(allocated(disp)) deallocate(disp)
        if(allocated(subtri)) deallocate(subtri)
        if(allocated(subquad)) deallocate(subquad)
        if(allocated(subwedge)) deallocate(subwedge)
        if(allocated(subbrick)) deallocate(subbrick)
        if(allocated(subcoh2d)) deallocate(subcoh2d)
        if(allocated(subcoh3d6)) deallocate(subcoh3d6)
        if(allocated(subcoh3d8)) deallocate(subcoh3d8)
        if(allocated(sub2d)) deallocate(sub2d)
        if(allocated(sub3d)) deallocate(sub3d)        
        if(allocated(igpnt)) deallocate(igpnt)
        if(allocated(sig)) deallocate(sig)
        if(allocated(eps)) deallocate(eps)   
        if(allocated(fsdv)) deallocate(fsdv)        
        
        
        close(outunit)
        
        end subroutine output
        
        
        
        
        
        
        
        
        
        
        integer function newunit(unit) result(n)
        ! returns lowest i/o unit number not in use
        integer, intent(out), optional :: unit
        logical inuse
        integer, parameter :: nmin=101   ! avoid lower numbers which are sometimes reserved
        integer, parameter :: nmax=999  ! may be system-dependent
        do n = nmin, nmax
            inquire(unit=n, opened=inuse)
            if (.not. inuse) then
                if (present(unit)) unit=n
                return
            end if
        end do
        write(msg_file,*)'newunit error: available unit not found.'
        call exit_function
        end function
      
      end module output_module
