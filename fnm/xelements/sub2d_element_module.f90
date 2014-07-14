    module sub2d_element_module
        use parameter_module
        use xnode_module
        use tri_element_module
        use quad_element_module
        use coh2d_element_module
    
        implicit none
        private
        
        integer, parameter :: ndim=2
    
        type,public :: sub2d_element
            private ! encapsulate components of this type
            
            character(len=namelength)       :: eltype=''    ! can be all types of 2D elements
            integer                         :: matkey=0     ! material index in glb material array
            real(kind=dp)                   :: theta=zero   ! fibre orientation (lamina)
            logical                         :: plstrain=.false. ! true for plane strain analysis
            integer,allocatable             :: glbcnc(:)    ! sub_elem connec to global node library
            integer,allocatable             :: subcnc(:)    ! sub_elem connec to parent elem nodes
            
            type(tri_element), allocatable  :: tri(:)       ! tri sub elements
            type(quad_element),allocatable  :: quad(:)      ! quad sub elements
            type(coh2d_element),allocatable :: coh2d(:)     ! 2D coh. sub elements
            
            real(kind=dp),allocatable       :: Tmatrix(:,:) ! interpolation matrix
            type(xnode),  allocatable       :: mnode(:)     ! interpolated (material domain) nodes
        end type
    
        interface empty
            module procedure empty_sub2d_element
        end interface
      
        interface prepare
            module procedure prepare_sub2d_element
        end interface
        
        interface integrate
            module procedure integrate_sub2d_element
        end interface
        
        interface extract
            module procedure extract_sub2d_element
        end interface
    
    


        public :: empty,prepare,integrate,extract
        
        
        
        
        contains
        
        
        subroutine empty_sub2d_element(elem)
        
            type(sub2d_element), intent(out) :: elem
            
            elem%eltype=''    ! can be all types of 2D elements
            elem%matkey=0     ! material index in glb material array
            elem%theta=zero   ! fibre orientation (lamina)
            elem%plstrain=.false. ! true for plane strain analysis
            
            if(allocated(elem%glbcnc))  deallocate(elem%glbcnc)
            if(allocated(elem%subcnc))  deallocate(elem%subcnc)
            if(allocated(elem%tri))     deallocate(elem%tri)
            if(allocated(elem%quad))    deallocate(elem%quad)
            if(allocated(elem%coh2d))   deallocate(elem%coh2d)
            if(allocated(elem%Tmatrix)) deallocate(elem%Tmatrix)
            if(allocated(elem%mnode))   deallocate(elem%mnode)
         
            
        end subroutine empty_sub2d_element
        
        
        
        subroutine integrate_sub2d_element(elem,Kmatrix,Fvector,cohgauss)
        
            type(sub2d_element), intent(inout) :: elem
            real(dp), allocatable, intent(out) :: Kmatrix(:,:),Fvector(:)
            logical,  optional,  intent(in)    :: cohgauss
            
            ! local variables
            !real(dp), allocatable ::  xi(:), xe(:), ue(:), xm(:), um(:), coords(:,:)
            integer :: i,j,l
            logical :: gauss
            
            i=0; j=0; l=0
            gauss=.false.
            
            if(present(isgauss)) gauss=cohgauss
            
            select case(elem%eltype)
                case('tri')
                    if(.not.allocated(elem%tri)) allocate(elem%tri(1))
                    call empty(elem%tri(1))
                    call prepare(elem%tri(1),key=0,connec=elem%glbcnc,matkey=elem%matkey,theta=elem%theta)
                    call integrate(elem%tri(1),Kmatrix,Fvector)
                    
                case('quad')
                    if(.not.allocated(elem%quad)) allocate(elem%quad(1))
                    call empty(elem%quad(1))
                    call prepare(elem%quad(1),key=0,connec=elem%glbcnc,matkey=elem%matkey,theta=elem%theta)
                    call integrate(elem%quad(1),Kmatrix,Fvector)
                    
                case('coh2d')
                    if(.not.allocated(elem%coh2d)) allocate(elem%coh2d(1))
                    call empty(elem%coh2d(1))
                    
                    ! check if the coh domain is an interpolated domain
                    if(allocated(elem%Tmatrix)) then
                        ! connec array is not needed any more; zero it instead
                        call prepare(elem%coh2d(1),key=0,connec=[0,0,0,0],matkey=elem%matkey)
                        ! material nodes need to be passed into the integration subroutine
                        if(allocated(elem%mnode)) then
                            call integrate(elem%coh2d(1),Kmatrix,Fvector,gauss,elem%mnode)
                        else
                            write(msg_file,*)'interoplated material nodes must be allocated in sub2d element module!'
                            call exit_function
                        end if
                    else
                        call prepare(elem%coh2d(1),key=0,connec=elem%glbcnc,matkey=elem%matkey)
                        call integrate(elem%coh2d(1),Kmatrix,Fvector,gauss)
                    end if
                    
                case default
                    write(msg_file,*)'unsupported elem type in sub2d element module!'
                    call exit_function
            end select
        
        end subroutine integrate_sub2d_element
        
        
        
    
    end sub2d_element_module