    module interface_type_module
    use parameter_module
    
    implicit none
    private


    type,public :: interface_modulus
        real(kind=dp) :: Dnn, Dtt, Dll ! penalty stiffness
    end type
    
    type,public :: interface_strength
        real(kind=dp) :: tau_nc, tau_tc, tau_lc ! strengths, t and l dir. values may be different
    end type
    
    type,public :: interface_toughness
        real(kind=dp) :: Gnc, Gtc, Glc ! toughness values, t and l dir. values may be different
        real(kind=dp) :: eta ! mixed-mode law component; eta_nt/nl/tl are the same; may be different in future
    end type
    

    type,public :: interface_type
        type(interface_modulus) :: modulus
        type(interface_strength) :: strength
        type(interface_toughness) :: toughness
        logical :: modulus_active=.false.
        logical :: strength_active=.false.
        logical :: toughness_active=.false.
    end type
    

    
    interface empty
        module procedure empty_interface
    end interface
      
    interface update
        module procedure update_interface
    end interface
    
   
    public :: empty,update
  


  
    contains
    
    
    
      subroutine empty_interface(this_interface)
      
      	type(interface_type),intent(inout) :: this_interface
        
        this_interface%modulus_active=.false.
        this_interface%strength_active=.false.
        this_interface%toughness_active=.false.
      	
      end subroutine empty_interface
 


      subroutine update_interface(this_interface,modulus,strength,toughness)
      
      	type(interface_type),intent(inout) :: this_interface
        type(interface_modulus),optional,intent(in) :: modulus
        type(interface_strength),optional,intent(in) :: strength
        type(interface_toughness),optional,intent(in) :: toughness
        
        if(present(modulus)) then
                this_interface%modulus=modulus  
                this_interface%modulus_active=.true.
        end if
        
        if(present(strength)) then
                this_interface%strength=strength
                this_interface%strength_active=.true.
        end if
        
        if(present(toughness)) then
                this_interface%toughness=toughness
                this_interface%toughness_active=.true.
        end if
        

      end subroutine update_interface   
      
      
    end module interface_type_module