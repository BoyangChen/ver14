    module coh2d_element_module
    use parameter_module
    use integration_point_module        ! integration point type
    
    implicit none
    private
    
    integer, parameter          :: ndim=2, nst=2, nnode=4, nig=2, ndof=ndim*nnode ! constants for type coh2d_element 
    real(kind=dp), parameter    :: dfail=one                                      ! max. degradation at final failure
    
    
    type, public :: coh2d_element 
        private
        
        integer :: key=0                            ! glb index of this element
        integer :: connec(nnode)=0                  ! node indices in their global arrays
        integer :: matkey=0                         ! material index in the global material arrays
        type(integration_point) :: ig_point(nig)    ! x, xi, weight, stress, strain, sdv; initialize in prepare procedure
        
        ! below are optional terms 
        
        real(kind=dp),allocatable :: rsdv(:)
        integer,allocatable :: isdv(:)
        logical,allocatable :: lsdv(:)
        
    end type
    
    
    
    
    interface empty
        module procedure empty_coh2d_element
    end interface
    
    interface prepare
        module procedure prepare_coh2d_element
    end interface
    
    interface integrate
        module procedure integrate_coh2d_element
    end interface
    
    interface extract
        module procedure extract_coh2d_element
    end interface
    
    


    public :: empty,prepare,integrate,extract
    
    
    
    
    contains
    
    
    ! this subroutine is used to format the element for use
    ! it is used in the initialize_lib_elem procedure in the lib_elem module
    subroutine empty_coh2d_element(elem)
    
        type(coh2d_element),intent(out) ::elem
        
        integer :: i
        i=0
        
        elem%key=0
        elem%connec=0
        elem%matkey=0

        do i=1,nig
            call empty(elem%ig_point(i))
        end do
          
        if(allocated(elem%rsdv)) deallocate(elem%rsdv)       
        if(allocated(elem%isdv)) deallocate(elem%isdv)
        if(allocated(elem%lsdv)) deallocate(elem%lsdv)
    
    end subroutine empty_coh2d_element
    
    
    
    
    ! this subroutine is used to prepare the connectivity and material lib index of the element
    ! it is used in the initialize_lib_elem procedure in the lib_elem module
    subroutine prepare_coh2d_element(elem,key,connec,matkey)
    
        type(coh2d_element),     intent(inout)   :: elem
        integer,                intent(in)      :: connec(nnode)
        integer,                intent(in)      :: key,matkey
        
        real(kind=dp)   :: x(ndim),stress(nst),strain(nst)
        integer         :: i
        x=zero; stress=zero; strain=zero
        i=0
        
        elem%key=key
        elem%connec=connec
        elem%matkey=matkey
        
        do i=1,nig
            call update(elem%ig_point(i),x=x,stress=stress,strain=strain)
        end do
    
    end subroutine prepare_coh2d_element
    
    
    
    
    subroutine extract_coh2d_element(elem,key,connec,matkey,ig_point,rsdv,isdv,lsdv)
    
        type(coh2d_element), intent(in) :: elem
        
        integer,                              optional, intent(out) :: key, matkey
        integer,                 allocatable, optional, intent(out) :: connec(:)
        type(integration_point), allocatable, optional, intent(out) :: ig_point(:)
        real(kind=dp),           allocatable, optional, intent(out) :: rsdv(:)
        integer,                 allocatable, optional, intent(out) :: isdv(:)
        logical,                 allocatable, optional, intent(out) :: lsdv(:)
        
        if(present(key)) key=elem%key
        
        if(present(matkey)) matkey=elem%matkey
        
        if(present(connec)) then
            allocate(connec(nnode))
            connec=elem%connec
        end if
        
        if(present(ig_point)) then
            allocate(ig_point(nig))
            ig_point=elem%ig_point
        end if
        
        if(present(rsdv)) then        
            if(allocated(elem%rsdv)) then
                allocate(rsdv(size(elem%rsdv)))
                rsdv=elem%rsdv
            end if
        end if    
        
        if(present(isdv)) then        
            if(allocated(elem%isdv)) then
                allocate(isdv(size(elem%isdv)))
                isdv=elem%isdv
            end if
        end if 
        
        if(present(lsdv)) then        
            if(allocated(elem%lsdv)) then
                allocate(lsdv(size(elem%lsdv)))
                lsdv=elem%lsdv
            end if
        end if
    
    
    end subroutine extract_coh2d_element


    
    
    
    
    ! the integration subroutine, updates K matrix, F vector, integration point stress and strain
    ! as well as all the solution dependent variables (sdvs) at intg points and element
    subroutine integrate_coh2d_element(elem,K_matrix,F_vector)
        use toolkit_module                  ! global tools for element integration
        use lib_mat_module                  ! global material library
        use lib_node_module                 ! global node library
        use check_progress_module           ! step and increment no. of the analysis, jstep and jinc
    
        type(coh2d_element),intent(inout)       :: elem 
        real(kind=dp),allocatable,intent(out)   :: K_matrix(:,:), F_vector(:)

        
        ! - the rest are all local variables
        
        ! - variables to be extracted from global arrays
        type(xnode)                     :: node(nnode)  ! x, u, du, v, extra dof ddof etc
        type(material)                  :: mat          ! matname, mattype and matkey to glb mattype array
        character(len=matnamelength)    :: matname
        character(len=mattypelength)    :: mattype
        integer                         :: matkey
        
        ! - variables to be extracted from element intg points
        integer, allocatable        :: isdv(:)
        real(kind=dp), allocatable  :: rsdv(:)
        ! - variables derived from isdv and rsdv
        integer         :: fstat
        real(kind=dp)   :: u0_eff,uf_eff,dm,dmeq
        
        ! - variables defined locally
        real(kind=dp),allocatable :: xj(:),uj(:)        ! nodal vars extracted from glb lib_node array
        real(kind=dp)   :: coords(ndim,nnode)           ! coordinates of the element nodes, formed from xj of each node
        real(kind=dp)   :: u(ndof)                      ! element nodal disp. vector, formed from uj of each node
        
        real(kind=dp)   :: igxi(ndim-1,nig),igwt(nig)   ! ig point natural coords and weights
        real(kind=dp)   :: tmpx(ndim), tmpu(ndim)       ! temp. x and u arrays for intg points
               
        real(kind=dp)   :: fn(nnode)                    ! shape functions
        real(kind=dp)   :: Nmatrix(ndim,ndof)           ! obtained from fn, to compute disp. jump acrss intfc: {u}_jump = [N]*{u}
        real(kind=dp)   :: normal(ndim),tangent(ndim)   ! normal and tangent vectors of the interface, obtained from coords
        real(kind=dp)   :: det                          ! determinant of jacobian (=length of element/2); 2 is length of ref. elem
        real(kind=dp)   :: Qmatrix(ndim,ndim)           ! rotation matrix from global to local coordinates, from normal & tangent
        real(kind=dp)   :: ujump(ndim),delta(ndim)      ! {delta}=[Q]*{u}_jump, {delta} is the jump vector in lcl coords.
        real(kind=dp)   :: Dee(ndim,ndim)               ! material stiffness matrix [D]
        real(kind=dp)   :: Tau(ndim)                    ! {Tau}=[D]*{delta}, traction on the interface
        
        real(kind=dp)   :: QN(ndim,ndof),DQN(ndim,ndof)         ! [Q]*[N], [D]*[Q]*[N]
        real(kind=dp)   :: NtQt(ndof,ndim),NtQtDQN(ndof,ndof)   ! [N']*[Q'], [N']*[Q']*[D]*[Q]*[N] 
        real(kind=dp)   :: NtQtTau(ndof),tract2(ndim)           ! [N']*[Q']*{Tau}
        integer         :: i,j,kig, kntr,n
      
        
        
        
        !------------------------------------------------!
        !           initialize variables 
        !------------------------------------------------!
        
        allocate(K_matrix(ndof,ndof),F_vector(ndof))
        K_matrix=zero; F_vector=zero
        
        i=0; j=0; kig=0
        do i=1,nnode
            call empty(node(i))
        end do
        call empty(mat)
        
        igxi=zero; igwt=zero
        coords=zero; u=zero
        
        
        
        
        
        
        
        !------------------------------------------------!
        !   extract variables from global arrays 
        !------------------------------------------------!
        
        ! - extract nodes from global node array 
        node(:)=lib_node(elem%connec(:))
        
        ! - extract material values from global material array
        mat=lib_mat(elem%matkey)
        
        
        
        
        
        
        
        !------------------------------------------------!
        !   assign values to material, coords and u 
        !------------------------------------------------!      
        
        ! - extract values from nodes and assign to local arrays 
        do j=1,nnode
            ! extract x and u values from nodes
            call extract(node(j),x=xj,u=uj)     
            ! assign x to coords matrix
            if(allocated(xj)) then
                coords(:,j)=xj(:)
            else
                write(msg_file,*)'WARNING: x not allocated for node:',elem%connec(j)
            end if
            ! and u to u vector
            if(allocated(uj)) then 
                u((j-1)*ndim+1:j*ndim)=uj(1:ndim)
            else
                write(msg_file,*)'WARNING: u not allocated for node:',elem%connec(j)
            end if    
        end do
        
        ! - extract values from mat (material type) and assign to local vars (matname, mattype & matkey)
        call extract(mat,matname,mattype,matkey) 
        
        
        
        
        
        
        
        !------------------------------------------------!
        !   compute Q matrix (rotation) and determinat 
        !------------------------------------------------!
        
        ! - compute tangent of the interface: node 2 coords - node 1 coords
        tangent(1)=coords(1,2)-coords(1,1)
        tangent(2)=coords(2,2)-coords(2,1)
        
        ! - normalize tangent vector
        call normalize(tangent,det) !-tangent vector normalized
        
        ! - compute determinant of the line element: actual length/reference length
        det=det/two !-ref. line starts from -1 to 1, length=2

        ! - compute normal of the interface
        normal(1)=-tangent(2)
        normal(2)=tangent(1)

        ! - compute Q matrix: rotate global coords to local coords; Q = transpose[normal,tangent]
        do j=1,2
            Qmatrix(1,j)=normal(j)
            Qmatrix(2,j)=tangent(j)
        end do

        
        
        
        
        
        
        !------------------------------------------------!
        !   perform integration at ig points
        !------------------------------------------------!
        
        ! - calculate ig point xi and weight
        call init_ig(igxi,igwt)
         
        !-calculate strain,stress,stiffness,sdv etc. at each int point
      	do kig=1,nig 
        
            ! - empty relevant arrays for reuse
            fn=zero; dn=zero
            tmpx=zero; tmpu=zero; delta=zero; Tau=zero
            fstat=0; dm=zero; dmeq=zero; u0_eff=zero; uf_eff=zero
            
            !- get shape function matrix
            call init_shape(igxi(:,kig),fn) 
            
		    ! Nmatrix: ujump (at each int pnt) = Nmatrix*u
            do i = 1,ndim
                do j = 1,nnode/2
                Nmatrix(i,i+(j-1)*ndim)= -fn(j)
                Nmatrix(i,i+(nnode-j)*ndim)=fn(j)
                end do
            end do
            
	   	    ! calculate ujump: disp. jump of the two crack surface, in global coords
            ujump=matmul(Nmatrix,u)
            
            ! calculate separation delta in local coords: delta=Qmatrix*ujump
            delta=matmul(Qmatrix,ujump)
            
            ! - extract sdvs from integration points
            call extract(elem%ig_point(kig),isdv=isdv,rsdv=rsdv)
            
            ! - update damage variables local arrays
            if(allocated(isdv)) then
                fstat=isdv(1)
            else
                allocate(isdv(1)); isdv=0
                fstat=isdv(1)
            end if
            if(allocated(rsdv)) then
                dm=rsdv(1); dmeq=rsdv(2); u0_eff=rsdv(3); uf_eff=rsdv(4)
            else
                allocate(rsdv(4)); rsdv=zero
                dm=rsdv(1); dmeq=rsdv(2); u0_eff=rsdv(3); uf_eff=rsdv(4)
            end if
            
            
            ! get D matrix dee accord. to material properties, and update intg point stresses
            select case (mattype)
                case ('interface')
                    
                    ! calculate D matrix, update tmpstress
                    call ddsdde(lib_interface(matkey),Dee,strain=delta,stress=Tau,fstat,dm,dmeq,u0_eff,uf_eff) 
                    
                case default
                    write(msg_file,*) 'material type not supported for tri element!'
                    call exit_function
            end select
            



            !------------------------------------------------!
            !  add this ig point contributions to K and F
            !------------------------------------------------!
          
            QN=matmul(Qmatrix,Nmatrix)
            NtQt=transpose(QN)
            DQN=matmul(Dee,QN)
            NtQtDQN=matmul(NtQt,DQN)
		
            do j=1,ndof
                do k=1,ndof
                    K_matrix(j,k) = K_matrix(j,k)+NtQtDQN(j,k)*det*igwt(kig)
                end do
            end do

            NtQtTau=matmul(NtQt,Tau)
		
            do j=1,ndof
                F_vector(j)=F_vector(j)+NtQtTau(j)*igwt(kig)*det
            end do
            
            
            
            
            !------------------------------------------------!
            !   update ig point kig
            !------------------------------------------------!

            !- calculate integration point physical coordinates (initial)
            tmpx=matmul(coords,fn)
            
            ! update x to ig point x array
            call update(elem%ig_point(kig),x=tmpx)
            
            !- calculate integration point displacement
            do j=1,ndim
                do i=1,nnode
                    tmpu(j)=tmpu(j)+fn(i)*u((i-1)*ndim+j)
                end do
            end do
            
            ! update u to ig point u array
            call update(elem%ig_point(kig),u=tmpu)
            
            ! update separation to ig point strain array
            call update(elem%ig_point(kig),strain=delta)
            
            ! update traction to ig point stress array
            call update(elem%ig_point(kig),stress=Tau)
            
       	end do !-looped over all int points. ig=nig
          
    
    end subroutine integrate_coh2d_element
    
    
    
    
    
    
    
    
    
    
    
    
    ! the rest are private subroutines
    
    
    
    
    
    

    
 
    subroutine init_ig(xi,wt,isgauss)

      real(kind=dp), intent(inout)  :: xi(ndim-1,nig), wt(nig)
      logical, optional, intent(in) :: isgauss
      
      real(kind=dp) :: cn=zero
	
        cn=one !-newton cotes
    
        if(present(isgauss)) then
            if(isgauss) cn=0.5773502691896260_dp !-gauss
        end if
    
        if (nig .eq. 2) then          
            xi(1,1)=-cn
            xi(1,2)=cn
            wt=one
        else
            write(msg_file,*) 'no. of integration points incorrect for coh2d_ig!'
            call exit_function
        end if

    end subroutine init_ig
    
    
    
    subroutine init_shape(igxi,f)
      
        real(kind=dp),intent(inout) :: f(nnode)
        real(kind=dp),intent(in) :: igxi(ndim-1)
        
        real(kind=dp) :: xi ! local variables

        xi=igxi(1)
        
        f(1)=half*(one-xi)
        f(2)=half*(one+xi)
        f(3)=f(2)
        f(4)=f(1)

    end subroutine init_shape
    
    
    end module coh2d_element_module