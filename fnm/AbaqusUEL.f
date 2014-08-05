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

        if (lop .eq. 0) then
!       start of the analysis

            ! initialize global clock and libraries
            call initialize_glb_clock
            call initialize_lib_node
            call initialize_lib_edge
            call initialize_lib_elem
            call initialize_lib_mat 
             
            ! get output directory (global variable defined in output module)
            
            outdir=''
            
            call getoutdir(workdir, lenworkdir)
            
            if(dirlength<lenworkdir+len('/outputs/')) then
                write(msg_file,*)'increase dirlength parameter to:',lenworkdir+len('/outputs/')
                call exit_function
            end if
            
            outdir=trim(workdir)//'/outputs/'

        else if (lop .eq. 1) then
!       start of the current increment
        
        else if (lop .eq. 2) then
!	    end of the increment 

            ! print element outputs after each increment
            call output(kstep,kinc,outdir)
        
        else if (lop .eq. 3) then
!	    end of the analysis

        end if


    return
    end subroutine uexternaldb





    !---------------------------------------------------------!
    !   Abaqus user element subroutine
    !---------------------------------------------------------!
    subroutine uel(rhs,amatrx,svars,energy,ndofel,nrhs,nsvars, &
     &       props,nprops,coords,mcrd,nnode,u,du,v,a,jtype,time,dtime, &
     &       kstep,kinc,jelem,params,ndload,jdltyp,adlmag,predef,npredf, &
     &       lflags,mlvarx,ddlmag,mdload,pnewdt,jprops,njprop,period)
    ! load FNM modules
    use parameter_module
    use glb_clock_module
    use lib_mat_module
    use lib_node_module
    use lib_elem_module
    
    ! use Abaqus default implict type declaration for passed-in variables only
    include 'aba_param.inc'

    dimension rhs(mlvarx,*),amatrx(ndofel,ndofel),props(*)
    dimension svars(*),energy(8),coords(mcrd,nnode),u(ndofel)
    dimension du(mlvarx,*),v(ndofel),a(ndofel),time(2),params(*)
    dimension jdltyp(mdload,*),adlmag(mdload,*),ddlmag(mdload,*)
    dimension predef(2,npredf,nnode),lflags(*),jprops(*)
    
    
    
    ! explicitly define all local variables
    
    
    
    ! element K matrix and F vector
    real(dp),allocatable    :: Kmat(:,:), Fvec(:)
    
    ! variables of element nodes, to be extracted from uel passed-in variables
    ! u, du, v, a, and to be updated to global node array 'lib_node' according to
    ! the element nodal connectivity 'nodecnc'
    real(dp)                :: uj(mcrd,nnode),duj(mcrd,nnode),vj(mcrd,nnode),aj(mcrd,nnode)
    
    ! create this element, here assuming to be xbrick element, in the future, a generic
    ! xelem should be created which contains all x element types, just like in the case
    ! of sub elements
    type(element)               :: elem
    character(len=elnamelength) :: elname
    character(len=eltypelength) :: eltype
    integer                     :: typekey
    
    ! nodal cnc of this elem
    integer, allocatable    :: cnc(:)
    
    ! counters
    integer :: i,j,l
    
    uj=zero; duj=zero; vj=zero; aj=zero
    i=0; j=0; l=0
    
    
    !---------------------------------------------------------------------------------!
    !           Abaqus UEL interface to FNM codes for variable pass-in
    !---------------------------------------------------------------------------------!
    
    ! extract this element definition from glb library, using the element key jelem
    elem=lib_elem(jelem)
    
    ! extract elname, eltype and typekey
    call extract(elem,elname,eltype,typekey)
    
    ! extract nodal cnc based on element type
    select case(eltype)
        case('xbrick')
            ! extract nodal cnc from this elem
            call extract(lib_xbrick(typekey),nodecnc=cnc)
        case('coh3d8')
            ! extract nodal cnc from this elem
            call extract(lib_coh3d8(typekey),nodecnc=cnc)
        case default
            write(msg_file,*)'unsupported element type'
            call exit_function
    end select
    
    ! extract nodal values from passed in variables
    do j=1, nnode
        uj(1:mcrd,j)    =   u( (j-1)*mcrd+1 : j*mcrd )
        duj(1:mcrd,j)   =   du( (j-1)*mcrd+1 : j*mcrd )
        vj(1:mcrd,j)    =   v( (j-1)*mcrd+1 : j*mcrd )
        aj(1:mcrd,j)    =   a( (j-1)*mcrd+1 : j*mcrd )
    end do
    
    ! update nodal values to glb node library lib_node according to cnc
    do j=1, nnode
        call update(lib_node(cnc(j)),u=uj(:,j),du=duj(:,j),v=vj(:,j),a=aj(:,j))
    end do
    
    
    
    
    
    !---------------------------------------------------------------------------------!
    !           Actual FNM calculation
    !---------------------------------------------------------------------------------!
    ! integrate this element
    select case(eltype)
        case('xbrick')
            call integrate(lib_xbrick(typekey),Kmat,Fvec)
        case('coh3d8')
            call integrate(lib_coh3d8(typekey),Kmat,Fvec)
        case default
            write(msg_file,*)'unsupported element type'
            call exit_function
    end select  
    
    
    
    
    
    !---------------------------------------------------------------------------------!
    !           Abaqus UEL interface to Abaqus Solver for results pass-up 
    !---------------------------------------------------------------------------------!   
 
    ! in the end, pass Kmat and Fvec to Abaqus UEL amatrx and rhs
    amatrx=zero; rhs(:,1)=zero
    amatrx=Kmat; rhs(:,1)=-Fvec(:)
    
    
    return
    end subroutine uel