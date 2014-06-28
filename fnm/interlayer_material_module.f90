    module interlayer_material_module
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
        type(interlayer_modulus),allocatable :: modulus(:)
        type(interlayer_strength),allocatable :: strength(:)
        type(interlayer_toughness),allocatable :: toughness(:)  
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
        
        integer :: istat
      	
        if(allocated(this_interlayer%modulus)) then
            deallocate(this_interlayer%modulus,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_interlayer**"
        end if

        if(allocated(this_interlayer%strength)) then
            deallocate(this_interlayer%strength,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_interlayer**"
        end if
        
        if(allocated(this_interlayer%toughness)) then
            deallocate(this_interlayer%toughness,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_interlayer**"
        end if

      end subroutine empty_interlayer
 


      subroutine update_interlayer(this_interlayer,modulus,strength,toughness)
      
      	type(interlayer_type),intent(inout) :: this_interlayer
        type(interlayer_modulus),optional,intent(in) :: modulus
        type(interlayer_strength),optional,intent(in) :: strength
        type(interlayer_toughness),optional,intent(in) :: toughness
           
        integer :: istat
         
        if(present(modulus)) then
            if(allocated(this_interlayer%modulus)) then
                this_interlayer%modulus(1)=modulus        
            else
                allocate(this_interlayer%modulus(1),stat=istat)
                if(istat/=0) stop"**allocation error in update_interlayer**"
                this_interlayer%modulus(1)=modulus            
            end if
        end if
        
        if(present(strength)) then
            if(allocated(this_interlayer%strength)) then
                this_interlayer%strength(1)=strength        
            else
                allocate(this_interlayer%strength(1),stat=istat)
                if(istat/=0) stop"**allocation error in update_interlayer**"
                this_interlayer%strength(1)=strength            
            end if
        end if
        
        if(present(toughness)) then
            if(allocated(this_interlayer%toughness)) then
                this_interlayer%toughness(1)=toughness       
            else
                allocate(this_interlayer%toughness(1),stat=istat)
                if(istat/=0) stop"**allocation error in update_interlayer**"
                this_interlayer%toughness(1)=toughness
            end if
        end if

      end subroutine update_interlayer   
      
      
    end module interlayer_material_module