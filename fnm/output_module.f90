      module output_module
      use parameter_module
      use lib_node_module
      use lib_elem_module
      
      implicit none
      
      integer :: lenoutdir
      character(len=dirlength) :: outdir
      
      contains
      
      subroutine output(kinc)
      
        integer,intent(in)          :: kinc     ! increment number of current step
      
        integer                     :: nnode, nelem ! no. of nodes and elements in the mesh
        integer                     :: i, u, nsize, elnode
        real(kind=dp), allocatable  :: x(:)     ! coordinates of nodes extracted from lib_node
        real(kind=dp)               :: x3d(3)   ! coordinates of a node in 3D
        character(len=dirlength)    :: outfile  ! output file name
        character(len=dirlength)    :: outnum   ! output increment number (embedded in the outfile name)
        character(len=10)           :: fmat     ! format specs
      
        ! initialize variables
        i=0; u=0; nnode=0; nelem=0; nsize=0; elnode=0
        x3d=zero
        outfile=''; outnum=''; fmat=''
        
        ! obtain nnode and nelem values from glb libraries
        nnode=size(lib_node)
        nelem=size(lib_elem)
      
        ! set format for integer output
        fmat='(i5.5)'
      
        ! write the increment number as a character and store in outnum
        write(outnum,fmat) kinc
        
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
            ! extract nodal coords from lib_node
            call extract(lib_node(i),x=x)
            ! pass nodal coords to x3d array
            if(allocated(x)) x3d(1:size(x))=x(:)
            ! write x3d array into output file
            write(u,*) x3d(1),x3d(2),x3d(3)
        end do            
        write(u,'(a)')''
        
        ! write elements
        elnode=4 ! quad4 elm
        nsize=nelem*(1+elnode)
        write(u,'(a, i5, i5)')'CELLS ', nelem, nsize
        do i=1,nelem
        write(u,*) 4,ndelm(1,i)-1,ndelm(2,i)-1,
     &   ndelm(3,i)-1,ndelm(4,i)-1
        end do            
        write(u,'(a)')''
        
        ! write element type
        write(u,'(a, i5)')'CELL_TYPES ', nelem
        do i=1,nelem
            write(u,'(i2)',advance='no') 9
        end do 
        write(u,'(a)')''
        
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
        write(6,*)'newunit error: available unit not found.'
        call xit
        !call stop_error("newunit error: available unit not found.")
        end function
      
      end module output_module