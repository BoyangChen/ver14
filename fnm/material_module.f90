    module material_module
    use paramter_module
    
    implicit none
    private
    
    type,public :: isotropic
        real(kind=dp) :: E, nu ! elastic moduli
        real(kind=dp) :: Xt,Xc ! tensile, compressive, and shear strengths
        real(kind=dp) :: GIc, GIIc, GIIIc ! mode I,II,III fracture toughness
    end type
    
    type,public :: lamina
        real(kind=dp) :: theta ! fibre angle w.r.t global x direction
        real(kind=dp) :: E1,E2,G12,G23,nu12,nu23 ! elastic moduli
        real(kind=dp) :: Xt,Xc,Yt,Yc,Sl,phi0 ! tensile, compressive, and shear strengths from Pinho criterion
        real(kind=dp) :: GfIc ! translaminar fracture toughness
        real(kind=dp) :: GmIc, GmIIc ! mode I,II matrix fracture toughness
        real(kind=dp) :: eta ! mixed-mode law constant
    end type
    
    type,public :: cohesive_crack
        real(kind=dp) :: Dnn, Dtt, Dll ! penalty stiffness
        real(kind=dp) :: tau_nc, tau_tc, tau_lc ! strengths, t and l dir. values may be different
        real(kind=dp) :: Gnc, Gtc, Glc ! toughness values, t and l dir. values may be different
        real(kind=dp) :: eta ! mixed-mode law component; eta_nt/nl/tl are the same; may be different in future
    end type
    
    type,public :: ply_block
        integer :: n_plies ! no. of plies in this block
        real(kind=dp) :: theta ! plyblock angle
    end type
    
    type,public :: laminate
        type(lamina) :: ud_properties ! unidirectional lamina properties
        type(ply_block), allocatable :: layup(:)
    end type
    
    
    
    interface empty
        module procedure empty_isotropic
        module procedure empty_lamina
        module procedure empty_cohesive_crack
        module procedure empty_ply_block
        module procedure empty_laminate
    end interface
      
    interface update
        module procedure update_isotropic
        module procedure update_lamina
        module procedure update_cohesive_crack
        module procedure update_ply_block
        module procedure update_laminate
    end interface
    
   
    public :: empty,update
  


  
    contains
    
    
    
    
    end module material_module