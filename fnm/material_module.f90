    include 'isotropic_type_module.f90'
    include 'lamina_type_module.f90'
    include 'interlayer_type_module.f90'
    
    module material_module
    use parameter_module
    use isotropic_type_module
    use lamina_type_module
    use interlayer_type_module
    
    implicit none
  
    
    
    type :: material ! global material array is on this type
        
        private
        
        character(len=matnamelength) :: matname='' ! user-given name from input
        character(len=mattypelength) :: mattype='' ! defined material types: isotropic, lamina, interlayer, etc.
        integer :: matkey=0 ! index in the respective array of the associated material type
        
    end type
    
    
    

    interface empty
        module procedure empty_material
    end interface
    
    interface update
        module procedure update_material
    end interface
    
    interface export
        module procedure export_material
    end interface
    
    private :: empty_material, update_material, export_material
    
    
    
    contains
    
    
    
    
    ! this subroutine is used in the preprocessing to format the 
    ! global material library lib_mat
    subroutine empty_material(mat)
        type(material),intent(out):: mat
        
        mat%matname=''
        mat%mattype=''
        mat%matkey=0
    
    end subroutine empty_material
    
    
    ! this subroutine is used in the preprocessing to fill in the
    ! material information in the global material library lib_mat
    subroutine update_material(mat,matname,mattype,matkey)
        type(material),intent(inout):: mat
        character(len=matnamelength),optional,intent(in) :: matname 
        character(len=mattypelength),optional,intent(in) :: mattype 
        integer,optional,intent(in) :: matkey
        
        if(present(matname)) mat%matname=matname
        if(present(mattype)) mat%mattype=mattype
        if(present(matkey)) mat%matkey=matkey
    
    end subroutine update_material
    
    
    ! this subroutine is used anywhere to export material information
    subroutine export_material(mat,matname,mattype,matkey)
        type(material),intent(in):: mat
        character(len=matnamelength),optional,intent(out) :: matname 
        character(len=mattypelength),optional,intent(out) :: mattype 
        integer,optional,intent(out) :: matkey
        
        if(present(matname)) matname=mat%matname
        if(present(mattype)) mattype=mat%mattype
        if(present(matkey)) matkey=mat%matkey
    
    end subroutine export_material
    
    
    end module material_module