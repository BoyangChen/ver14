    module isotropic_material_module
    use parameter_module
    
    implicit none
    private

    type,public :: isotropic_modulus
        real(kind=dp) :: E, nu ! elastic moduli
    end type
    
    type,public :: isotropic_strength
        real(kind=dp) :: Xt, Xc ! tensile and compressive strengths
    end type
    
    type,public :: isotropic_toughness
        real(kind=dp) :: GIc, GIIc, GIIIc ! mode I,II,III fracture toughness
    end type

    type,public :: isotropic_type
        type(isotropic_modulus):: modulus
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
    
   
    public :: empty,update
  


  
    contains
    
    
    
      subroutine empty_isotropic(this_isotropic)
      
      	type(isotropic_type),intent(inout) :: this_isotropic
        
      	
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
      
      
    end module isotropic_material_module