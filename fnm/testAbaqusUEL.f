    !***************************************!
    !   Abaqus UEL interface of the FNM     !
    !                                       !
    !                                       !
    !***************************************!


!------ include useful modules--------------------------
    include 'globals/parameter_module.f90'          ! used in all modules
    include 'globals/glb_clock_module.f90'          ! used in main and elem modules
    include 'globals/integration_point_module.f90'  ! used in elem and output modules
    include 'globals/toolkit_module.f90'            ! used in elem and precrack modules
    include 'libraries/lib_node_module.f90'
    include 'libraries/lib_edge_module.f90'
    include 'libraries/lib_mat_module.f90'
    include 'libraries/lib_elem_module.f90'
    include 'outputs/output_module.f90'
!------------------------------------------------------


    !---------------------------------------------------------!
    !   Abaqus user subroutine for I/O to external files
    !---------------------------------------------------------!
    subroutine uexternaldb(lop,lrestart,time,dtime,kstep,kinc)
    use parameter_module
    use glb_clock_module
    use lib_mat_module
    use lib_node_module
    use lib_edge_module
    use lib_elem_module
    use output_module

    implicit none

    real(kind=dp), intent(in)   :: time(2)
    real(kind=dp), intent(in)   :: dtime
    integer,       intent(in)   :: lop,lrestart,kstep,kinc
    
    ! local variables
    character(len=dirlength)    :: workdir
    integer                     :: lenworkdir

        workdir=''
        lenworkdir=0

        write(msg_file,*) 'reach here'

        if (lop .eq. 0) then
!       start of the analysis
            

            ! initialize global clock and libraries
            call initialize_glb_clock
            call initialize_lib_node
            call initialize_lib_edge
            call initialize_lib_elem
!            call initialize_lib_mat 
             
            ! get output directory (global variable defined in output module)
            
!            outdir=''
            
!            call getoutdir(workdir, lenworkdir)
            
!            if(dirlength<lenworkdir+len('/outputs/')) then
!                write(msg_file,*)'increase dirlength parameter to:',lenworkdir+len('/outputs/')
!                call exit_function
!            end if
            
!            outdir=trim(workdir)//'/outputs/'

        else if (lop .eq. 1) then
!       start of the current increment
        
        else if (lop .eq. 2) then
!	    end of the increment 

            ! print element outputs after each increment
!            call output(kstep,kinc,outdir)
        
        else if (lop .eq. 3) then
!	    end of the analysis

        end if


    return
    end subroutine uexternaldb
