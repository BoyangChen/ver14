      module modoutput
      use modparam
      use modfnode
      use modndelm
      
      implicit none
      
      integer :: lenoutdir
      character(len=256) :: outdir
      
      contains
      
        subroutine output(kinc)
      
        integer,intent(in)::kinc
      
        integer :: i,u,nnode,nelm,nsize,elnode
        character(len=256) :: outfile,outnum
        character(len=10) :: fmat
      
        i=0;u=0;nnode=0;nelm=0;nsize=0;elnode=0
        outfile='';outnum='';fmat=''
      
        fmat='(i5.5)'
      
        write(outnum,fmat) kinc
        outfile=trim(outdir)//'/'//trim(outnum)//'.vtk'
        outfile=trim(outfile)
        !write(6,*) outfile
        open(newunit(u), file=outfile,status="replace")
        
        ! write header
        write(u,'(a)')'# vtk DataFile Version 3.1'
        write(u,'(a)')'for Floating Node Method output'
        
        ! write vtk format
        write(u,'(a)')'ASCII'
        
        ! write vtk data type
        write(u,'(a)')'DATASET UNSTRUCTURED_GRID'
        
        ! write points
        nnode=size(fnode(1,:))
        write(u,'(a, i5, a)')'POINTS ',nnode,' FLOAT'
        do i=1,nnode
            write(u,*) fnode(2,i),fnode(3,i),fnode(4,i)
        end do            
        write(u,'(a)')''
        
        ! write elements
        nelm=size(ndelm(1,:))
        elnode=4 ! quad4 elm
        nsize=nelm*(1+elnode)
        write(u,'(a, i5, i5)')'CELLS ', nelm, nsize
        do i=1,nelm
        write(u,*) 4,ndelm(1,i)-1,ndelm(2,i)-1,
     &   ndelm(3,i)-1,ndelm(4,i)-1
        end do            
        write(u,'(a)')''
        
        ! write element type
        write(u,'(a, i5)')'CELL_TYPES ', nelm
        do i=1,nelm
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
      
      end module modoutput