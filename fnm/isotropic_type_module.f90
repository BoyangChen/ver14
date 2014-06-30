    module isotropic_type_module
    use parameter_module
    
    implicit none
    private

    type,public :: isotropic_modulus
        real(kind=dp) :: E=zero, nu=zero ! elastic moduli
    end type
    
    type,public :: isotropic_strength
        real(kind=dp) :: Xt=zero, Xc=zero ! tensile and compressive strengths
    end type
    
    type,public :: isotropic_toughness
        real(kind=dp) :: GIc=zero, GIIc=zero, GIIIc=zero ! mode I,II,III fracture toughness
    end type

    type,public :: isotropic_type
        private
        type(isotropic_modulus):: modulus ! value already zeroed
        type(isotropic_strength) :: strength
        type(isotropic_toughness):: toughness 
        logical :: modulus_active=.false.
        logical :: strength_active=.false.
        logical :: toughness_active=.false.
    end type
    
    
    interface empty
        module procedure empty_isotropic
    end interface
      
    interface update
        module procedure update_isotropic
    end interface
    
    interface export
        module procedure export_isotropic
    end interface
    
    interface ddsdde
        module procedure ddsdde_isotropic
    end interface
    
   
    public :: empty,update,export,ddsdde
  


  
    contains
    
    
    
    
      subroutine empty_isotropic(this_isotropic)
      
      	type(isotropic_type),intent(out) :: this_isotropic
        
      	this_isotropic%modulus=isotropic_modulus(E=zero,nu=zero)
        this_isotropic%strength=isotropic_strength(Xt=zero,Xc=zero)
        this_isotropic%toughness=isotropic_toughness(GIc=zero,GIIc=zero,GIIIc=zero)
        
        this_isotropic%modulus_active=.false.
        this_isotropic%strength_active=.false.
        this_isotropic%toughness_active=.false.

      end subroutine empty_isotropic
 


      subroutine update_isotropic(this_isotropic,modulus,strength,toughness)
      
      	type(isotropic_type),intent(inout) :: this_isotropic
        type(isotropic_modulus),optional,intent(in) :: modulus
        type(isotropic_strength),optional,intent(in) :: strength
        type(isotropic_toughness),optional,intent(in) :: toughness
        
         
        if(present(modulus)) then
                this_isotropic%modulus=modulus  
                this_isotropic%modulus_active=.true.
        end if
        
        if(present(strength)) then
                this_isotropic%strength=strength
                this_isotropic%strength_active=.true.
        end if
        
        if(present(toughness)) then
                this_isotropic%toughness=toughness
                this_isotropic%toughness_active=.true.
        end if

      end subroutine update_isotropic  
      
      
      
      subroutine export_isotropic(this_isotropic,modulus,strength,toughness, &
      & modulus_active,strength_active,toughness_active)
      
      	type(isotropic_type),intent(in) :: this_isotropic
        type(isotropic_modulus),optional,intent(out) :: modulus
        type(isotropic_strength),optional,intent(out) :: strength
        type(isotropic_toughness),optional,intent(out) :: toughness
        logical,optional,intent(out) :: modulus_active,strength_active,toughness_active
        
         
        if(present(modulus)) modulus=this_isotropic%modulus
        if(present(modulus_active)) modulus_active=this_isotropic%modulus_active
        
        if(present(strength)) strength=this_isotropic%strength
        if(present(strength_active)) strength_active=this_isotropic%strength_active
               
        if(present(toughness)) toughness=this_isotropic%toughness
        if(present(toughness_active)) toughness_active=this_isotropic%toughness_active

      end subroutine export_isotropic 



!********************************************************************************************
!************************ subroutine ddsdde ************************************************
!***** returns the tangent stiffness matrix of the material ******************************
!********************************************************************************************
      subroutine ddsdde_isotropic(this_mat,dee,PlaneStrain,strain,stress,rsdv,isdv,lsdv)

        type(isotropic_type),intent(in) :: this_mat
        real(kind=dp),intent(out) :: dee(:,:)
        real(kind=dp),optional,intent(out) :: stress(:)
        real(kind=dp),optional,intent(in) :: strain(:)
        real(kind=dp),allocatable,optional,intent(inout) :: rsdv(:)
        integer,allocatable,optional,intent(inout) :: isdv(:)
        logical,allocatable,optional,intent(inout) :: lsdv(:)
        logical,optional,intent(in) :: PlaneStrain
        
        ! local variables
        real(kind=dp) :: E,nu,G,del
        integer :: nst
        
        ! initialize variables
        dee=zero
        E=zero; nu=zero; G=zero; del=zero
        nst=0 
        
        ! find no. of strains
        nst=size(dee(:,1))
        
        if(.not.this_mat%modulus_active) then
            write(msg_file,*) 'isotropic material modulus undefined!'
            call exit_function
        end if
        
        if(this_mat%strength_active) write(msg_file,*) 'isotropic failure criterion not yet supported!'
        
        if(this_mat%toughness_active) write(msg_file,*) 'isotropic damage evolution not yet supported!'
        
        ! calculate the linear elasticity stiffness matrix
        E=this_mat%modulus%E
        nu=this_mat%modulus%nu
        G=half*E/(one+nu) ! shear modulus

        if (nst .eq. 3) then ! 2D problem
            if (PlaneStrain) then
                del=(one+nu)*(one-2*nu)
                dee(1,1)=(one-nu)*E/del
                dee(2,2)=dee(1,1)
                dee(3,3)=G
                dee(1,2)=nu*E/del
                dee(2,1)=dee(1,2)   
            else ! plane stress
                del=one-nu**2
                dee(1,1)=E/del
                dee(2,2)=dee(1,1)
                dee(3,3)=G
                dee(1,2)=nu*E/del
                dee(2,1)=dee(1,2) 
            end if
        else if (nst .eq. 6) then ! 3D problem
            del=(one+nu)*(one-2*nu)
            dee(1,1)=(one-nu)*E/del
            dee(2,2)=dee(1,1)
            dee(3,3)=dee(1,1)
            dee(4,4)=G
            dee(5,5)=G
            dee(6,6)=G
            dee(1,2:3)=nu*E/del
            dee(2:3,1)=nu*E/del
            dee(2,3)=nu*E/del
            dee(3,2)=dee(2,3)
        else
            write(msg_file,*) 'no. of strains not supported for kdeemat!'
            call exit_function
        end if
        
        

      return
      end subroutine ddsdde_isotropic 
      
      
    end module isotropic_type_module