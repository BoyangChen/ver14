       module parameter_module ! useful constants for all subroutines & all elements
        
        implicit none
        
        integer, parameter :: dp=selected_real_kind(14) !-equivalent to f77 double precision
        
        integer, parameter :: msg_file=6 ! I/O file index for message file
        
        integer, parameter :: matnamelength=20 ! character length of a user-input material name
        integer, parameter :: mattypelength=20 ! character length of a system material type name
        integer, parameter :: elnamelength=20 ! character length of a user-input element name
        integer, parameter :: eltypelength=20 ! character length of a system element type name
        integer, parameter :: setnamelength=20 ! character length of a set name
        integer, parameter :: dirlength=256 ! character length of a directory name
        
        ! generic parameter for intact state (elem, edge, material, etc.)
        integer, parameter :: intact=0
        
        
        ! some common real numbers
        real(kind=dp), parameter :: zero=0._dp,one=1._dp,two=2._dp, &
      & three=3._dp,four=4._dp,five=5._dp,six=6._dp,seven=7._dp,      &
      & eight=8._dp,nine=9._dp,ten=10._dp,half=0.5_dp,quarter=0.25_dp,&
      & one_third=one/three,two_third=two/three,one_sixth=one/six,    &
      & one_eighth=one/eight,root3=one/sqrt(three),                   &
      & halfcirc=180._dp,pi=3.14159265359_dp,ninety=90._dp            &
      & ,tolerance=0.1_dp


        ! residual stiffness
        real(kind=dp), parameter :: Kres=one    ! one MPa (or other unit) of residual stiffness after failure, to prevent nodes run wild
      
      
        ! derived data type sdv_array, just to group 3 types of arrays together
        type :: sdv_array   
            real(kind=dp),  allocatable :: r(:)
            integer,        allocatable :: i(:)
            logical,        allocatable :: l(:)
        end type
        
        type :: int_alloc_array
            integer,    allocatable :: array(:)
        end type
        
        type :: real_alloc_array
            real(dp),   allocatable :: array(:)
        end type
      
        contains
        
        subroutine exit_function
            !stop"**exit function reached**"
            write(msg_file,*) "**exit function reached**"
            CALL XIT    ! abaqus exit function
        end subroutine 
     
       end module parameter_module
