    include 'isotropic_type_module.f90'
    include 'lamina_type_module.f90'
    include 'interlayer_type_module.f90'
    
    module material_module
    use parameter_module
    use isotropic_type_module
    use lamina_type_module
    use interlayer_type_module
    
    implicit none
    
    integer, parameter, private :: matnamelength=20 ! character length of a user-input material name
    integer, parameter, private :: mattypelength=20 ! character length of a system material type name
    
    type :: material ! global material array is on this type
        character(len=matnamelength) :: matname ! user-given name from input
        character(len=mattypelength) :: mattype ! defined material types: isotropic, lamina, interlayer, etc.
        integer :: matkey ! index in the respective array of the associated material type
    end type

    interface empty
        module procedure empty_material
    end interface
    
    contains
    
    subroutine empty_material(mat)
        type(material),intent(inout):: mat
        
        mat%matname=''
        mat%mattype=''
        mat%matkey=0
    
    end subroutine
    
    end module material_module