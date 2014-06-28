    include 'isotropic_material_module.f90'
    include 'lamina_material_module.f90'
    include 'interlayer_material_module.f90'
    
    module material_module
    use parameter_module
    use isotropic_material_module
    use lamina_material_module
    use interlayer_material_module
    
    implicit none
    
    integer, parameter, private :: matnamelength=20 ! character length of a user-input material name
    integer, parameter, private :: mattypelength=20 ! character length of a system material type name
    
    type :: material ! global material array is on this type
        character(len=matnamelength) :: matname ! user-given name from input
        character(len=mattypelength) :: mattype ! defined material types: isotropic, lamina, interlayer, etc.
        integer :: matkey ! index in the respective array of the associated material type
    end type

    
    end module material_module