      module output_module
      use parameter_module
      use lib_node_module
      use lib_elem_module
      use integration_point_module
      
      implicit none
      private
      
      public :: output
      
      
      contains
      
      
      subroutine output(kinc,outdir)
      
        integer,intent(in)          :: kinc     ! increment number of current step
        character(len=dirlength),intent(in) :: outdir   ! output directory name
      
        integer                     :: nnode, nelem    ! no. of nodes & elem in the mesh
        integer                     :: ntri, nquad, ntetra, nwedge, nbrick ! no. of elems of each elem type
        integer                     :: i, j, l, u, nsize, elnode
        integer, allocatable        :: connec(:)! connectivity of an elem extracted from lib_elem
        real(kind=dp), allocatable  :: x(:)     ! coordinates of nodes extracted from lib_node
        real(kind=dp)               :: x3d(3)   ! coordinates of a node in 3D
        real(kind=dp), allocatable  :: disp(:)  ! displacements of nodes extracted from lib_node
        real(kind=dp)               :: disp3d(3)! displacements of a node in 3D
        real(kind=dp), allocatable  :: sig(:), eps(:) ! stress & strain arrays extracted from lib_elem ig pnt
        real(kind=dp)               :: sigtsr(3,3), epstsr(3,3) ! stress & strain tensors for vtk output 
        type(integration_point), allocatable :: igpnt(:) ! intg point array
        character(len=dirlength)    :: outfile  ! output file name
        character(len=dirlength)    :: outnum   ! output increment number (embedded in the outfile name)
        character(len=10)           :: fmat     ! format specs
      
        ! initialize variables
        i=0; j=0; l=0; u=0; nnode=0; nelem=0
        ntri=0; nquad=0; ntetra=0; nwedge=0; nbrick=0
        nsize=0; elnode=0
        outfile=''; outnum=''; fmat=''
        
        ! obtain nnode value from glb libraries
        nnode=size(lib_node)
      
        ! set format for integer output
        fmat='(i5.5)'
      
        ! write the increment number as a character and store in outnum
        write(outnum,fmat) kinc
        
        !~! obtain the current working directory
        !~call getcwd(outdir)
        
        ! create the output file name
        outfile=trim(outdir)//'/'//trim(outnum)//'.vtk'
        outfile=trim(outfile)
        
        ! open the outfile
        open(newunit(u), file=outfile,status="replace")
        
        ! write header
        write(u,'(a)')'# vtk DataFile Version 3.1'
        write(u,'(a)')'for Floating Node Method output'
        
        ! write vtk format
        write(u,'(a)')'ASCII'
        
        ! write vtk data type
        write(u,'(a)')'DATASET UNSTRUCTURED_GRID'
        
        ! write points
        write(u,'(a, i5, a)')'POINTS ',nnode,' FLOAT'    
        do i=1,nnode
            ! empty x3d array
            x3d=zero
            ! extract nodal coords from lib_node
            call extract(lib_node(i),x=x)
            ! pass nodal coords to x3d array
            if(allocated(x)) x3d(1:size(x))=x(:)
            ! write x3d array into output file
            write(u,*) x3d(1),x3d(2),x3d(3)
        end do            
        write(u,'(a)')''
        
        ! write elements' connec
        if(allocated(lib_tri)) ntri=size(lib_tri) ! no. of tri elem in the mesh
        !~if(allocated(lib_quad)) nquad=size(lib_quad)
        !~if(allocated(lib_tetra)) ntetra=size(lib_tetra)
        !~if(allocated(lib_wedge)) nwedge=size(lib_wedge)
        !~if(allocated(lib_brick)) nbrick=size(lib_brick)
        ! .... and other elem types ....
        
        ! total no. of elems
        nelem=ntri+nquad+ntetra+nwedge+nbrick
        
        ! calculate total no. of nodes to print; each row has 1+elnode no. of indices to print
        nsize=ntri*(1+3)+nquad*(1+4)+ntetra*(1+4)+nwedge*(1+6)+nbrick*(1+8)
        
        write(u,'(a, i5, i5)')'CELLS ', nelem, nsize ! write a summary of output
        
        if(ntri > 0) then
            do i=1,ntri ! write each element's connec individually
                call extract(lib_tri(i),connec=connec) ! extract connec from lib_tri
                ! print connec in vtk; note that in vtk node no. starts from 0
                connec=connec-1
                write(u,*) 3,connec(1),connec(2),connec(3) 
            end do
        end if
        
        !~if(nquad > 0) then
        !~    do i=1,nquad ! write each element's connec individually
        !~        call extract(lib_quad(i),connec=connec) ! extract connec from lib_tri
        !~        ! print connec in vtk; note that in vtk node no. starts from 0
        !~        connec=connec-1
        !~        write(u,*) 4,connec(1),connec(2),connec(3),connec(4) 
        !~    end do
        !~end if
        !~
        !~if(ntetra > 0) then
        !~    do i=1,ntetra ! write each element's connec individually
        !~        call extract(lib_tetra(i),connec=connec) ! extract connec from lib_tri
        !~        ! print connec in vtk; note that in vtk node no. starts from 0
        !~        connec=connec-1
        !~        write(u,*) 4,connec(1),connec(2),connec(3),connec(4)
        !~    end do
        !~end if
        !~
        !~if(nwedge > 0) then
        !~    do i=1,nwedge ! write each element's connec individually
        !~        call extract(lib_wedge(i),connec=connec) ! extract connec from lib_tri
        !~        ! print connec in vtk; note that in vtk node no. starts from 0
        !~        connec=connec-1
        !~        write(u,*) 6,connec(1),connec(2),connec(3),connec(4),connec(5),connec(6)
        !~    end do
        !~end if
        !~
        !~if(nbrick > 0) then
        !~    do i=1,nbrick ! write each element's connec individually
        !~        call extract(lib_brick(i),connec=connec) ! extract connec from lib_tri
        !~        ! print connec in vtk; note that in vtk node no. starts from 0
        !~        connec=connec-1
        !~        write(u,*) 8,connec(1),connec(2),connec(3),connec(4),connec(5),connec(6),connec(7),connec(8)
        !~    end do
        !~end if
        
        write(u,'(a)')''
        
        
        
        ! write element type
        write(u,'(a, i5)')'CELL_TYPES ', nelem
        
        if (ntri > 0) then
            do i=1,ntri
                write(u,'(i2)') 5 ! 5 for tri
            end do 
        end if
        
        if (nquad > 0) then
            do i=1,nquad
                write(u,'(i2)') 9 ! 9 for quad
            end do 
        end if
        
        if (ntetra > 0) then
            do i=1,ntetra
                write(u,'(i2)') 10 ! 10 for tetra
            end do 
        end if
        
        if (nwedge > 0) then
            do i=1,nwedge
                write(u,'(i2)') 13 ! 13 for wedge
            end do 
        end if
        
        if (nbrick > 0) then
            do i=1,nbrick
                write(u,'(i2)') 12 ! 12 for brick
            end do 
        end if
     
        write(u,'(a)')''


        ! write nodal varibales (disp.)
        write(u,'(a, i5)')'POINT_DATA ', nnode
        write(u,'(a)')'VECTORS displacement float'
        !write(u,'(a)')'LOOKUP_TABLE default' ! ** this is only for scalar **
        
        do i=1, nnode
            disp3d=zero ! empty disp3d for reuse
            call extract(lib_node(i),u=disp)
            if(allocated(disp)) disp3d(1:size(disp))=disp(:)
            write(u,*) disp3d(1),disp3d(2),disp3d(3)
        end do            
        write(u,'(a)')''
        
        
        ! write element stress
        write(u,'(a, i5)')'CELL_DATA ', nelem
        write(u,'(a)')'TENSORS stress float'
        !write(u,'(a)')'LOOKUP_TABLE default'
        
        if (ntri > 0) then
            do i=1,ntri
                sigtsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_tri(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),stress=sig)
                    if(size(sig)==3) then ! 2D stress state                    
                        sigtsr(1,1)=sigtsr(1,1)+sig(1)
                        sigtsr(2,2)=sigtsr(2,2)+sig(2)
                        sigtsr(1,2)=sigtsr(1,2)+sig(3)
                        sigtsr(2,1)=sigtsr(2,1)+sig(3)                       
                    else ! 3D stress state                    
                        sigtsr(1,1)=sigtsr(1,1)+sig(1)
                        sigtsr(2,2)=sigtsr(2,2)+sig(2)
                        sigtsr(3,3)=sigtsr(3,3)+sig(3)
                        sigtsr(1,2)=sigtsr(1,2)+sig(4)
                        sigtsr(1,3)=sigtsr(1,3)+sig(5)
                        sigtsr(2,3)=sigtsr(2,3)+sig(6)
                        sigtsr(2,1)=sigtsr(2,1)+sig(4)
                        sigtsr(3,1)=sigtsr(3,1)+sig(5)
                        sigtsr(3,2)=sigtsr(3,2)+sig(6)                      
                    end if    
                end do 

                ! average stress in the element
                sigtsr=sigtsr/size(igpnt)
   
                do l=1,3
                    write(u,*) sigtsr(1,l), sigtsr(2,l), sigtsr(3,l)
                end do
                write(u,'(a)')'' ! separate from next element
                
            end do 
        end if
        
        !~if (nquad > 0 ) then
        !~! fill in the same ....
        !~end if
        
        
        
        
        ! write element strain
        write(u,'(a)')'TENSORS strain float'
        
        if (ntri > 0) then
            do i=1,ntri
                epstsr=zero ! empty sig & eps tensor for reuse
                call extract(lib_tri(i),ig_point=igpnt)
                do j=1,size(igpnt)
                    call extract(igpnt(j),strain=eps)
                    if(size(eps)==3) then ! 2D stress state   
                        epstsr(1,1)=epstsr(1,1)+eps(1)
                        epstsr(2,2)=epstsr(2,2)+eps(2)
                        epstsr(1,2)=epstsr(1,2)+eps(3)
                        epstsr(2,1)=epstsr(2,1)+eps(3)                        
                    else ! 3D stress state                
                        epstsr(1,1)=epstsr(1,1)+eps(1)
                        epstsr(2,2)=epstsr(2,2)+eps(2)
                        epstsr(3,3)=epstsr(3,3)+eps(3)
                        epstsr(1,2)=epstsr(1,2)+eps(4)
                        epstsr(1,3)=epstsr(1,3)+eps(5)
                        epstsr(2,3)=epstsr(2,3)+eps(6)
                        epstsr(2,1)=epstsr(2,1)+eps(4)
                        epstsr(3,1)=epstsr(3,1)+eps(5)
                        epstsr(3,2)=epstsr(3,2)+eps(6) 
                    end if    
                end do 

                ! average strain in the element
                epstsr=epstsr/size(igpnt)
   
                do l=1,3
                    write(u,*) epstsr(1,l), epstsr(2,l), epstsr(3,l)
                end do
                write(u,'(a)')'' ! separate from next element
                
            end do 
        end if
        
        !~if (nquad > 0 ) then
        !~! fill in the same ....
        !~end if
        
        
        
        
        close(u)
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