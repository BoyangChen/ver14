    module interlayer_type_module
    use parameter_module
    
    implicit none
    private


    type,public :: interlayer_modulus
        real(kind=dp) :: Dnn, Dtt, Dll ! penalty stiffness
    end type
    
    type,public :: interlayer_strength
        real(kind=dp) :: tau_nc, tau_tc, tau_lc ! strengths, t and l dir. values may be different
    end type
    
    type,public :: interlayer_toughness
        real(kind=dp) :: Gnc, Gtc, Glc ! toughness values, t and l dir. values may be different
        real(kind=dp) :: eta ! mixed-mode law component; eta_nt/nl/tl are the same; may be different in future
    end type
    

    type,public :: interlayer_type
        type(interlayer_modulus) :: modulus
        type(interlayer_strength) :: strength
        type(interlayer_toughness) :: toughness
        logical :: modulus_active=.false.
        logical :: strength_active=.false.
        logical :: toughness_active=.false.
    end type
    

    
    interface empty
        module procedure empty_interlayer
    end interface
      
    interface update
        module procedure update_interlayer
    end interface
    
   
    public :: empty,update
  


  
    contains
    
    
    
      subroutine empty_interlayer(this_interlayer)
      
      	type(interlayer_type),intent(inout) :: this_interlayer
        
        this_interlayer%modulus_active=.false.
        this_interlayer%strength_active=.false.
        this_interlayer%toughness_active=.false.
      	
      end subroutine empty_interlayer
 


      subroutine update_interlayer(this_interlayer,modulus,strength,toughness)
      
      	type(interlayer_type),intent(inout) :: this_interlayer
        type(interlayer_modulus),optional,intent(in) :: modulus
        type(interlayer_strength),optional,intent(in) :: strength
        type(interlayer_toughness),optional,intent(in) :: toughness
        
        if(present(modulus)) then
                this_interlayer%modulus=modulus  
                this_interlayer%modulus_active=.true.
        end if
        
        if(present(strength)) then
                this_interlayer%strength=strength
                this_interlayer%strength_active=.true.
        end if
        
        if(present(toughness)) then
                this_interlayer%toughness=toughness
                this_interlayer%toughness_active=.true.
        end if
        

      end subroutine update_interlayer   
      
      
    end module interlayer_type_module