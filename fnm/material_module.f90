    include 'isotropic_material_module.f90'
    include 'lamina_material_module.f90'
    include 'interlayer_material_module.f90'
    
    module material_module
    use parameter_module
    use isotropic_material_module
    use lamina_material_module
    use interlayer_material_module
    
    implicit none
    
    type :: material_type ! global material array is on this type
        character(len=matnamelength):: matname
        type(isotropic_type),allocatable :: isotropic(:)
        type(lamina_type),allocatable :: lamina(:)
        type(interlayer_type),allocatable :: interlayer(:)
    end type
    
   interface empty
        module procedure empty_material
    end interface
      
    interface update
        module procedure update_material
    end interface
    

  
    contains
    
    
    
      subroutine empty_material(this_material)
      
      	type(material_type),intent(inout) :: this_material
        
        integer :: istat
        
        this_material%matname=''
      	
        if(allocated(this_material%isotropic)) then
            deallocate(this_material%isotropic,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_material**"
        end if

        if(allocated(this_material%lamina)) then
            deallocate(this_material%lamina,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_material**"
        end if
        
        if(allocated(this_material%interlayer)) then
            deallocate(this_material%interlayer,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_material**"
        end if

      end subroutine empty_material
 


      subroutine update_material(this_material,matname,isotropic,lamina,interlayer)
      
      	type(material_type),intent(inout) :: this_material
        character(len=*),optional,intent(in):: matname
        type(isotropic_type),optional,intent(in) :: isotropic
        type(lamina_type),optional,intent(in) :: lamina
        type(interlayer_type),optional,intent(in) :: interlayer
           
        integer :: istat
        
        if(present(matname)) then
            if(len(matname)<=matnamelength) then
                this_material%matname=matname
            else
                write(msg_file,*) 'material name too long! keep its length within', matnamelength
                this_material%matname(1:matnamelength)=matname(1:matnamelength)
            end if
        end if
         
        if(present(isotropic)) then
            if(allocated(this_material%isotropic)) then
                this_material%isotropic(1)=isotropic        
            else
                allocate(this_material%isotropic(1),stat=istat)
                if(istat/=0) stop"**allocation error in update_material**"
                this_material%isotropic(1)=isotropic            
            end if
        end if
        
        if(present(lamina)) then
            if(allocated(this_material%lamina)) then
                this_material%lamina(1)=lamina        
            else
                allocate(this_material%lamina(1),stat=istat)
                if(istat/=0) stop"**allocation error in update_material**"
                this_material%lamina(1)=lamina            
            end if
        end if
        
        if(present(interlayer)) then
            if(allocated(this_material%interlayer)) then
                this_material%interlayer(1)=interlayer       
            else
                allocate(this_material%interlayer(1),stat=istat)
                if(istat/=0) stop"**allocation error in update_material**"
                this_material%interlayer(1)=interlayer
            end if
        end if

      end subroutine update_material
    
    
    
    
    end module material_module